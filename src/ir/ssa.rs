
use std::collections::{ BTreeSet, BTreeMap, HashMap, VecDeque };

use petgraph::{
	Direction,
	graphmap::{ DiGraphMap },
	algo::dominators::{ self },
	dot::{ Dot, Config as DotConfig },
};

use super::*;

// ------------------------------------------------------------------------------------------------
// Dominator tree
// ------------------------------------------------------------------------------------------------

// TODO: implement this directly rather than in terms of a DiGraphMap?
pub(super) struct DomTree {
	g: DiGraphMap<IrBBId, ()>,
}

impl DomTree {
	pub(super) fn new(cfg: &IrCfg) -> Self {
		let doms = dominators::simple_fast(&cfg, 0);
		let mut g = DiGraphMap::<IrBBId, ()>::new();

		for n in cfg.nodes() {
			// doing this because Dominators uses a HashMap internally and the child iteration
			// order is nondeterministic.
			let mut children = doms.immediately_dominated_by(n).collect::<Vec<_>>();
			children.sort();

			for m in children.into_iter() {
				// only add edges for non-self domination.
				if n != m {
					g.add_edge(n, m, ());
				}
			}
		}

		// sanity check, this is a strict tree so in-degree should be 1;
		// should also not have any nodes disconnected from the root.
		for n in cfg.nodes() {
			if n != 0 {
				assert_eq!(g.edges_directed(n, Direction::Incoming).count(), 1, "node {}", n);
			}
		}

		Self { g }
	}

	pub(super) fn show(&self) {
		println!("Dominators");
		println!("-------------");
		println!();
		println!("{:?}", Dot::with_config(&self.g, &[DotConfig::EdgeNoLabel]));
	}

	pub(super) fn idom(&self, node: IrBBId) -> Option<IrBBId> {
		self.g.edges_directed(node, Direction::Incoming).map(|(d, _, _)| d).next()
	}

	pub(super) fn immediately_dominated_by(&self, node: IrBBId) -> impl Iterator<Item = IrBBId> + '_ {
		self.g.edges_directed(node, Direction::Outgoing).map(|(_, c, _)| c)
	}

	/// `true` if `x` strictly dominates `b` (directly or indirectly).
	pub(super) fn strictly_dominates(&self, x: IrBBId, mut b: IrBBId) -> bool {
		while let Some(dom) = self.idom(b) {
			if x == dom {
				return true;
			}

			b = dom;
		}

		false
	}
}

// ------------------------------------------------------------------------------------------------
// Dominance frontiers
// ------------------------------------------------------------------------------------------------

pub(super) type DomFrontiers = Vec<BTreeSet<IrBBId>>;

pub(super) fn compute_dominance_frontiers(cfg: &IrCfg, doms: &DomTree) -> DomFrontiers {
	let mut df = cfg.nodes().map(|_| BTreeSet::new()).collect::<DomFrontiers>();

	for (a, b, _) in cfg.all_edges() {
		let mut x = a;

		while !doms.strictly_dominates(x, b) {
			df[x].insert(b);

			match doms.idom(x) {
				Some(d) => x = d,
				None    => break,
			}
		}
	}

	df
}

pub(super) fn show_df(df: &DomFrontiers) {
	println!("Dominance Frontiers");
	println!("-------------------");
	println!();

	for (i, df) in df.iter().enumerate() {
		println!("DF[{}] = {:?}", i, df);
	}
}

// ------------------------------------------------------------------------------------------------
// Reg finder
// ------------------------------------------------------------------------------------------------

pub(super) fn find_all_regs(bbs: &[IrBasicBlock]) -> BTreeSet<IrReg> {
	let mut ret = BTreeSet::new();

	for bb in bbs.iter() {
		// this is done before phi-insertion, so we don't have to check them.
		for inst in bb.insts() {
			inst.regs(|&r| {
				ret.insert(r);
			});
		}
	}

	ret
}

// ------------------------------------------------------------------------------------------------
// Phi-insertion
// ------------------------------------------------------------------------------------------------

pub(super) fn insert_phis(
	bbs:  &mut [IrBasicBlock],
	cfg:  &IrCfg,
	df:   &DomFrontiers,
	regs: impl Iterator<Item = IrReg>
) {
	for reg in regs {
		insert_phis_impl(bbs, cfg, df, reg);
	}
}

fn insert_phis_impl(bbs: &mut [IrBasicBlock], cfg: &IrCfg, df: &DomFrontiers, reg: IrReg) {
	assert!(bbs.len() == cfg.node_count());

	// defs is set of all BBs which contain an assignment to reg
	let defs = cfg.nodes()
		.filter(|&n| bbs[n].has_assignment_to(reg))
		.collect::<BTreeSet<_>>();

	// F in the book, records if a phi func has been inserted for reg at this node
	let mut inserted = bbs.iter()
		.map(|_| false)
		.collect::<Vec<_>>();

	// W in the book, work queue
	let mut work = VecDeque::new();

	for &d in defs.iter() {
		work.push_back(d);
	}

	while let Some(current) = work.pop_front() {
		// for each BB in the dominance frontier of current...
		for &other in df[current].iter() {
			if inserted[other] { continue; }

			// insert a phi function for reg at the start of other,
			// with as many inputs as other has predecessors.
			let num_preds = cfg.neighbors_directed(other, Direction::Incoming).count();
			bbs[other].add_phi(reg, num_preds);

			inserted[other] = true;

			// a phi function for reg counts as a def, so we have
			// to visit other as that may have to be propagated further.

			if !defs.contains(&other) {
				work.push_back(other);
			}
		}
	}
}

// ------------------------------------------------------------------------------------------------
// Register renaming
// ------------------------------------------------------------------------------------------------

pub(super) fn rename_regs(
	bbs: &mut [IrBasicBlock],
	cfg: &IrCfg,
	doms: &DomTree,
	all_regs: impl Iterator<Item = IrReg>
) {
	let mut renamer = RegRenamer::new(all_regs);
	renamer.search(0, bbs, cfg, doms);
}

struct RegRenamer {
	counters: BTreeMap<u16, u32>,
	stacks:   BTreeMap<u16, Vec<u32>>,
}

impl RegRenamer {
	fn new(all_regs: impl Iterator<Item = IrReg>) -> Self {
		let mut counters = BTreeMap::new();
		let mut stacks = BTreeMap::new();

		for reg in all_regs {
			counters.insert(reg.offset(), 1);
			stacks.insert(reg.offset(), vec![]);
		}

		Self { counters, stacks }
	}

	fn predecessor_index_of(&self, succ: IrBBId, pred: IrBBId, cfg: &IrCfg) -> usize {
		cfg.neighbors_directed(succ, Direction::Incoming)
			.position(|p| p == pred)
			.unwrap()
	}

	fn visit_assignment_dst(&mut self, reg: &mut IrReg, to_pop: &mut BTreeMap<u16, usize>) {
		let offset = reg.offset();
		let i = self.counters[&offset];
		let new_reg = reg.sub(i);
		self.stacks.get_mut(&offset).unwrap().push(i);
		*self.counters.get_mut(&offset).unwrap() += 1;
		*to_pop.entry(offset).or_insert(0) += 1;
		*reg = new_reg;
	}

	fn search(&mut self, bbid: IrBBId, bbs: &mut [IrBasicBlock], cfg: &IrCfg, doms: &DomTree) {
		let mut to_pop = BTreeMap::<u16, usize>::new();
		let bb = &mut bbs[bbid];

		// 1. update instructions in this bb
		for phi in bb.phis_mut() {
			// println!("phi dst = {:?}", phi.dst_reg());
			self.visit_assignment_dst(phi.dst_reg_mut(), &mut to_pop);
		}

		for inst in bb.insts_mut() {
			inst.visit_uses_mut(|v| {
				// println!("use = {:?}", v);
				*v = v.sub(*self.stacks[&v.offset()].last().unwrap_or(&0));
			});

			if let Some(v) = inst.dst_reg_mut() {
				self.visit_assignment_dst(v, &mut to_pop);
			}
		}

		// 2. update phi functions in successors
		for succ in cfg.neighbors_directed(bbid, Direction::Outgoing) {
			let j = self.predecessor_index_of(succ, bbid, cfg);
			// println!("pred = {}, succ = {}, j = {}", bbid, succ, j);

			for phi in bbs[succ].phis_mut() {
				let args = phi.args_mut();
				let i = *self.stacks[&args[j].offset()].last().unwrap_or(&0);
				args[j] = args[j].sub(i);
			}
		}

		// 3. recurse into dominator tree children
		for child in doms.immediately_dominated_by(bbid) {
			self.search(child, bbs, cfg, doms);
		}

		// 4. pop stacks for any defs
		for (reg, n) in to_pop.into_iter() {
			let s = self.stacks.get_mut(&reg).unwrap();
			s.truncate(s.len() - n);
		}
	}
}

// ------------------------------------------------------------------------------------------------
// phi-pruning
// ------------------------------------------------------------------------------------------------

pub(super) fn prune_phis(bbs: &mut [IrBasicBlock], doms: &DomTree) {
	let mut pruner = PhiPruner::new();
	pruner.prune(bbs, doms);
}

struct UseInfo {
	used: bool,
	bbid: IrBBId,
}

impl UseInfo {
	fn new(bbid: IrBBId) -> Self {
		Self { used: false, bbid }
	}

	fn used(&self) -> bool {
		self.used
	}

	fn bb(&self) -> IrBBId {
		self.bbid
	}

	fn mark_used(&mut self) {
		self.used = true;
	}
}

type UseMap = HashMap<IrReg, UseInfo>;

struct PhiPruner {
	use_map: UseMap,
	stack:   Vec<IrReg>,
}

impl PhiPruner {
	fn new() -> Self {
		Self {
			use_map: UseMap::new(),
			stack:   vec![],
		}
	}

	fn prune(&mut self, bbs: &mut [IrBasicBlock], doms: &DomTree) {
		// 1. initial marking phase
		self.visit_mark(0, bbs, doms);

		// 2. propagate used info backwards using stack
		while let Some(v) = self.stack.pop() {
			let phi = self.find_phi(&v, bbs);

			for arg in phi.args().iter() {
				// if arg is marked as useless, mark it as useful and push it to the stack.
				if let Some(arg_info) = self.use_map.get_mut(arg) {
					if !arg_info.used() {
						// println!("propagation: marking reg {} as used in bb {}", arg, arg_info.bb());
						arg_info.mark_used();
						self.stack.push(arg.clone());
					}
				}
			}
		}

		// 3. now anything in the map that is marked unused can be pruned!
		for bb in bbs {
			bb.retain_phis(|reg| {
				// unwrap is ok here because every phi in the function was added to the map
				// by visit_mark
				self.use_map.get(reg).unwrap().used()
			})
		}
	}

	fn find_phi<'b>(&self, reg: &IrReg, bbs: &'b [IrBasicBlock]) -> &'b IrPhi {
		// unwrap is ok here because this method is only called on registers popped from the stack,
		// which only got there by also being put into the use map
		let bbid = self.use_map.get(reg).unwrap().bb();

		// unwrap is ok here for the same reason
		bbs[bbid].phi_for_reg(reg).unwrap()
	}

	fn visit_mark(&mut self, bbid: IrBBId, bbs: &mut [IrBasicBlock], doms: &DomTree) {
		// mark all registers declared by phi functions as unused
		for phi in bbs[bbid].phis() {
			// println!("marking reg {} as unused in bb {}", phi.dst_reg(), bbid);
			self.use_map.insert(phi.dst_reg().clone(), UseInfo::new(bbid));
		}

		// mark all registers used by instructions as used
		// (have to use insts_mut and visit_uses_mut because the closure is FnMut...)
		for inst in bbs[bbid].insts_mut() {
			inst.visit_uses_mut(|reg| {
				// "if reg is defined by some phi function" - this reduces to a check to see
				// if reg is in the use map, because we are visiting the BBs in dominance order,
				// so if it was defined by a phi function then it will already have been added.
				if let Some(info) = self.use_map.get_mut(&reg) {
					// mark as used and push it on the stack
					// println!("marking reg {:?} as used in bb {:?}", reg, info.bb());
					info.mark_used();
					self.stack.push(*reg);
				}
			});
		}

		// visit children
		for child in doms.immediately_dominated_by(bbid) {
			self.visit_mark(child, bbs, doms);
		}
	}
}