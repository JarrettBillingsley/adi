// #![allow(dead_code)]

use std::collections::{ BTreeSet, BTreeMap, HashMap, VecDeque };
use std::fmt::{ Debug, Display, Formatter, Result as FmtResult };

use petgraph::{
	Direction,
	graphmap::{ DiGraphMap },
	algo::dominators::{ self },
	dot::{ Dot, Config as DotConfig },
};

// ------------------------------------------------------------------------------------------------
// Types
// ------------------------------------------------------------------------------------------------

type IrBBId = usize;
type CFG = DiGraphMap<IrBBId, ()>;
type DomFrontiers = Vec<BTreeSet<IrBBId>>;

// ------------------------------------------------------------------------------------------------
// Var
// ------------------------------------------------------------------------------------------------

#[derive(Clone, PartialEq, Eq, Hash)]
enum Var {
	Bare(String),
	Sub(String, usize),
}

impl Var {
	fn bare(name: &str) -> Self {
		Self::Bare(name.into())
	}

	fn name(&self) -> &String {
		match self {
			Var::Bare(s) | Var::Sub(s, ..) => s
		}
	}

	fn sub(&self, i: usize) -> Self {
		match self {
			Var::Bare(name) => Var::Sub(name.clone(), i),
			_ => panic!(".sub() called on '{}'", self),
		}
	}
}

impl Display for Var {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		match self {
			Var::Bare(name)   => write!(f, "{}", name),
			Var::Sub(name, i) => write!(f, "{}_{}", name, i),
		}
	}
}

impl Debug for Var {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		write!(f, "{}", self)
	}
}

// ------------------------------------------------------------------------------------------------
// Src
// ------------------------------------------------------------------------------------------------

enum Src {
	Var(Var),
	Const(i32),
}

impl Src {
	fn var(s: &str) -> Self {
		Self::Var(Var::bare(s))
	}
}

impl Display for Src {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		match self {
			Src::Var(v)   => write!(f, "{}", v),
			Src::Const(c) => write!(f, "{}", c),
		}
	}
}

impl Debug for Src {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		write!(f, "{}", self)
	}
}

// ------------------------------------------------------------------------------------------------
// Phi
// ------------------------------------------------------------------------------------------------

struct Phi {
	dst: Var,
	args: Vec<Var>
}

impl Phi {
	fn new(var: &str, num_args: usize) -> Self {
		Self { dst: Var::bare(var), args: vec![Var::bare(var); num_args] }
	}

	fn assigns(&self, var: &str) -> bool {
		match &self.dst {
			Var::Bare(dst) => dst == var,
			_              => false,
		}
	}

	fn dst_var(&self) -> &Var {
		&self.dst
	}

	fn dst_var_mut(&mut self) -> &mut Var {
		&mut self.dst
	}

	fn args(&self) -> &[Var] {
		&self.args
	}

	fn args_mut(&mut self) -> &mut [Var] {
		&mut self.args
	}
}

impl Display for Phi {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		write!(f, "{} = φ", self.dst)?;
		write_args(f, &self.args)
	}
}

impl Debug for Phi {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		write!(f, "{}", self)
	}
}

// ------------------------------------------------------------------------------------------------
// Inst
// ------------------------------------------------------------------------------------------------

enum Inst {
	Dummy  (String),
	Assign { dst: Var, rhs: Src },
	Call   { dst: Option<Var>, name: String, args: Vec<Src> },
	Ret    { val: Src },
}

impl Inst {
	fn dummy(s: &str) -> Self {
		Self::Dummy(s.into())
	}

	fn assign(dst: &str, rhs: Src) -> Self {
		Self::Assign { dst: Var::bare(dst), rhs }
	}

	fn call(dst: Option<&str>, name: &str, args: Vec<Src>) -> Self {
		Self::Call { dst: dst.map(|dst| Var::bare(dst)), name: name.into(), args }
	}

	fn ret(val: &str) -> Self {
		Self::Ret { val: Src::var(val) }
	}

	/// Does this instruction assign this (bare) variable name?
	fn assigns(&self, var: &str) -> bool {
		match self {
			Inst::Assign { dst: Var::Bare(dst), .. } => dst == var,
			_                                        => false,
		}
	}

	/// If this instruction assigns to a variable, returns a mutable reference to it;
	/// otherwise returns `None`.
	fn dst_var_mut(&mut self) -> Option<&mut Var> {
		match self {
			Inst::Assign { dst, .. } => Some(dst),
			Inst::Call   { dst, .. } => dst.as_mut(),
			_ => None,
		}
	}

	fn visit_uses_mut(&mut self, mut f: impl FnMut(&mut Var)) {
		match self {
			Inst::Assign { rhs: Src::Var(var), .. } => f(var),
			Inst::Call { args, .. } => {
				for a in args {
					if let Src::Var(v) = a {
						f(v);
					}
				}
			}
			Inst::Ret { val: Src::Var(var) } => f(var),
			_ => {}
		}
	}
}

fn write_args<T: Display>(f: &mut Formatter, args: &Vec<T>) -> FmtResult {
	write!(f, "(")?;

	let mut args = args.iter();

	if let Some(arg) = args.next() {
		write!(f, "{}", arg)?;

		for arg in args {
			write!(f, ", {}", arg)?;
		}
	}

	write!(f, ")")
}

impl Display for Inst {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		match self {
			Inst::Dummy(s) => write!(f, "{}", s),
			Inst::Assign { dst, rhs } => write!(f, "{} = {}", dst, rhs),
			Inst::Call { dst, name, args } => {
				if let Some(dst) = dst {
					write!(f, "{} = ", dst)?;
				}

				write!(f, "{}", name)?;
				write_args(f, args)
			}
			Inst::Ret { val } => write!(f, "ret {}", val),
		}
	}
}

impl Debug for Inst {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		write!(f, "{}", self)
	}
}

// ------------------------------------------------------------------------------------------------
// BB
// ------------------------------------------------------------------------------------------------

struct BB {
	id:    IrBBId,
	phis:  Vec<Phi>,
	insts: Vec<Inst>,
}

impl BB {
	fn new(id: IrBBId, insts: Vec<Inst>) -> Self {
		Self { id, phis: vec![], insts }
	}

	fn has_assignment_to(&self, var: &str) -> bool {
		self.phis.iter().any(|p| p.assigns(var)) ||
		self.insts.iter().any(|i| i.assigns(var))
	}

	fn phis(&self) -> impl Iterator<Item = &Phi> {
		self.phis.iter()
	}

	fn phis_mut(&mut self) -> impl Iterator<Item = &mut Phi> {
		self.phis.iter_mut()
	}

	fn phi_for_var(&self, var: &Var) -> Option<&Phi> {
		// TODO: this is linear time. is that a problem? (how many phi funcs are there likely
		// to be at the start of a BB?)
		// since phis execute conceptually in parallel, and since we need to look them up by
		// what variable they define, might make sense to use a map { var => phi }.
		for phi in self.phis() {
			if phi.dst_var() == var {
				return Some(phi);
			}
		}

		None
	}

	fn retain_phis(&mut self, p: impl Fn(&Var) -> bool) {
		self.phis.retain(|phi| p(phi.dst_var()))
	}

	fn insts_mut(&mut self) -> impl Iterator<Item = &mut Inst> {
		self.insts.iter_mut()
	}

	fn add_phi(&mut self, v: &str, num_preds: usize) {
		self.phis.push(Phi::new(v, num_preds));
	}
}

impl Display for BB {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		writeln!(f, "bb{}:", self.id)?;

		for p in self.phis.iter() {
			writeln!(f, "    {}", p)?;
		}

		for i in self.insts.iter() {
			writeln!(f, "    {}", i)?;
		}

		Ok(())
	}
}

impl Debug for BB {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		write!(f, "{}", self)
	}
}

// ------------------------------------------------------------------------------------------------
// main
// ------------------------------------------------------------------------------------------------

fn main() {
	let bbs = &mut [
		// 0 (r)
		BB::new(0, vec![
			Inst::dummy("entry"),
		]),

		// 1 (A)
		BB::new(1, vec![
			Inst::dummy("---"),
		]),

		// 2 (B)
		BB::new(2, vec![
			Inst::assign("y", Src::Const(0)),
			Inst::assign("x", Src::Const(0)),
		]),

		// 3 (C)
		BB::new(3, vec![
			Inst::assign("tmp", Src::var("x")),
			Inst::assign("x", Src::var("y")),
			Inst::assign("y", Src::var("tmp")),
		]),

		// 4 (D)
		BB::new(4, vec![
			Inst::call(Some("x"), "f", vec![Src::var("x"), Src::var("y")]),
		]),

		// 5 (E)
		BB::new(5, vec![
			Inst::ret("x"),
		]),
	];

	println!("{:#?}", bbs);

	let cfg = CFG::from_edges(&[
		(0, 1),
		(1, 2), (1, 3),
		(2, 4),
		(3, 4), (3, 5),
		(4, 1), (4, 5),

		// (0, 1),
		// (1, 2), (1, 8),
		// (2, 3), (2, 4),
		// (3, 4),
		// (4, 2), (4, 5),
		// (5, 6), (5, 7),
		// (6, 8),
		// (7, 8),
		// (8, 9),
	]);

	show_graph(&cfg, "CFG");

	let doms = DomTree::new(&cfg);

	doms.show();

	let df = compute_dominance_frontiers(&cfg, &doms);

	show_df(&df);

	let all_vars = &["x", "y", "tmp"];

	insert_phis(bbs, &cfg, &df, all_vars);

	println!("\nAfter φ-insertion\n-------------------");
	println!("{:#?}", bbs);

	rename_vars(bbs, &cfg, &doms, all_vars);

	println!("\nAfter renaming\n-------------------");
	println!("{:#?}", bbs);

	prune_phis(bbs, &doms);

	println!("\nAfter pruning\n-------------------");
	println!("{:#?}", bbs);
}

// ------------------------------------------------------------------------------------------------
// Show stuff
// ------------------------------------------------------------------------------------------------

fn show_graph(g: &CFG, name: &str) {
	println!("{}", name);
	println!("-------------");
	println!();
	println!("{:?}", Dot::with_config(g, &[DotConfig::EdgeNoLabel]));
}

fn show_df(df: &DomFrontiers) {
	println!("Dominance Frontiers");
	println!("-------------------");
	println!();

	for (i, df) in df.iter().enumerate() {
		println!("DF[{}] = {:?}", i, df);
	}
}

// ------------------------------------------------------------------------------------------------
// Dominator tree
// ------------------------------------------------------------------------------------------------

// TODO: implement this directly rather than in terms of a DiGraphMap?
struct DomTree {
	g: DiGraphMap<IrBBId, ()>,
}

impl DomTree {
	fn new(cfg: &CFG) -> Self {
		let doms = dominators::simple_fast(&cfg, 0);
		let mut g = CFG::new();

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

	fn show(&self) {
		show_graph(&self.g, "Dominators");
	}

	fn idom(&self, node: IrBBId) -> Option<IrBBId> {
		self.g.edges_directed(node, Direction::Incoming).map(|(d, _, _)| d).next()
	}

	fn immediately_dominated_by(&self, node: IrBBId) -> impl Iterator<Item = IrBBId> + '_ {
		self.g.edges_directed(node, Direction::Outgoing).map(|(_, c, _)| c)
	}

	/// `true` if `x` strictly dominates `b` (directly or indirectly).
	fn strictly_dominates(&self, x: IrBBId, mut b: IrBBId) -> bool {
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

fn compute_dominance_frontiers(cfg: &CFG, doms: &DomTree) -> DomFrontiers {
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

// ------------------------------------------------------------------------------------------------
// phi-insertion
// ------------------------------------------------------------------------------------------------

fn insert_phis(bbs: &mut [BB], cfg: &CFG, df: &DomFrontiers, all_vars: &[&str]) {
	for v in all_vars.iter() {
		insert_phis_impl(bbs, cfg, df, v);
	}
}

fn insert_phis_impl(bbs: &mut [BB], cfg: &CFG, df: &DomFrontiers, v: &str) {
	assert!(bbs.len() == cfg.node_count());

	// defs is set of all BBs which contain an assignment to v
	let defs = cfg.nodes()
		.filter(|&n| bbs[n].has_assignment_to(v))
		.collect::<BTreeSet<_>>();

	// F in the book, records if a phi func has been inserted for v at this node
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

			// insert a phi function for v at the start of other,
			// with as many inputs as other has predecessors.
			let num_preds = cfg.neighbors_directed(other, Direction::Incoming).count();
			bbs[other].add_phi(v, num_preds);

			inserted[other] = true;

			// a phi function for v counts as a def, so we have
			// to visit other as that may have to be propagated further.

			if !defs.contains(&other) {
				work.push_back(other);
			}
		}
	}
}

// ------------------------------------------------------------------------------------------------
// Variable renaming
// ------------------------------------------------------------------------------------------------

fn rename_vars(bbs: &mut [BB], cfg: &CFG, doms: &DomTree, all_vars: &[&str]) {
	let mut renamer = VarRenamer::new(all_vars);
	renamer.search(0, bbs, cfg, doms);
}

struct VarRenamer {
	counters: BTreeMap<String, usize>,
	stacks:   BTreeMap<String, Vec<usize>>,
}

impl VarRenamer {
	fn new(all_vars: &[&str]) -> Self {
		let mut counters = BTreeMap::new();
		let mut stacks = BTreeMap::new();

		for &v in all_vars {
			counters.insert(v.into(), 1);
			stacks.insert(v.into(), vec![]);
		}

		Self { counters, stacks }
	}

	fn predecessor_index_of(&self, succ: IrBBId, pred: IrBBId, cfg: &CFG) -> usize {
		cfg.neighbors_directed(succ, Direction::Incoming)
			.position(|p| p == pred)
			.unwrap()
	}

	fn visit_assignment_dst(&mut self, v: &mut Var, to_pop: &mut BTreeMap<String, usize>) {
		let v_name = v.name();
		let i = self.counters[v_name];
		let new_v = v.sub(i);
		self.stacks.get_mut(v_name).unwrap().push(i);
		*self.counters.get_mut(v_name).unwrap() += 1;
		*to_pop.entry(v_name.clone()).or_insert(0) += 1;
		*v = new_v;
	}

	fn search(&mut self, bbid: IrBBId, bbs: &mut [BB], cfg: &CFG, doms: &DomTree) {
		let mut to_pop = BTreeMap::<String, usize>::new();
		let bb = &mut bbs[bbid];

		// 1. update instructions in this bb
		for phi in bb.phis_mut() {
			self.visit_assignment_dst(phi.dst_var_mut(), &mut to_pop);
		}

		for inst in bb.insts_mut() {
			inst.visit_uses_mut(|v| {
				*v = v.sub(*self.stacks[v.name()].last().unwrap());
			});

			if let Some(v) = inst.dst_var_mut() {
				self.visit_assignment_dst(v, &mut to_pop);
			}
		}

		// 2. update phi functions in successors
		for succ in cfg.neighbors_directed(bbid, Direction::Outgoing) {
			let j = self.predecessor_index_of(succ, bbid, cfg);
			// println!("pred = {}, succ = {}, j = {}", bbid, succ, j);

			for phi in bbs[succ].phis_mut() {
				let args = phi.args_mut();
				let i = *self.stacks[args[j].name()].last().unwrap_or(&0);
				args[j] = args[j].sub(i);
			}
		}

		// 3. recurse into dominator tree children
		for child in doms.immediately_dominated_by(bbid) {
			self.search(child, bbs, cfg, doms);
		}

		// 4. pop stacks for any defs
		for (var, n) in to_pop.into_iter() {
			let s = self.stacks.get_mut(&var).unwrap();
			s.truncate(s.len() - n);
		}
	}
}

// ------------------------------------------------------------------------------------------------
// phi-pruning
// ------------------------------------------------------------------------------------------------

fn prune_phis(bbs: &mut [BB], doms: &DomTree) {
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
		assert!(!self.used);
		self.used = true;
	}
}

type UseMap = HashMap<Var, UseInfo>;

struct PhiPruner {
	use_map: UseMap,
	stack:   Vec<Var>,
}

impl PhiPruner {
	fn new() -> Self {
		Self {
			use_map: UseMap::new(),
			stack:   vec![],
		}
	}

	fn prune(&mut self, bbs: &mut [BB], doms: &DomTree) {
		// 1. initial marking phase
		self.visit_mark(0, bbs, doms);

		// 2. propagate used info backwards using stack
		while let Some(v) = self.stack.pop() {
			let phi = self.find_phi(&v, bbs);

			for arg in phi.args().iter() {
				// if arg is marked as useless, mark it as useful and push it to the stack.
				if let Some(arg_info) = self.use_map.get_mut(arg) {
					if !arg_info.used() {
						// println!("propagation: marking var {} as used in bb {}", arg, arg_info.bb());
						arg_info.mark_used();
						self.stack.push(arg.clone());
					}
				}
			}
		}

		// 3. now anything in the map that is marked unused can be pruned!
		for bb in bbs {
			bb.retain_phis(|var| {
				// unwrap is ok here because every phi in the function was added to the map
				// by visit_mark
				self.use_map.get(var).unwrap().used()
			})
		}
	}

	fn find_phi<'b>(&self, var: &Var, bbs: &'b [BB]) -> &'b Phi {
		// unwrap is ok here because this method is only called on variables popped from the stack,
		// which only got there by also being put into the use map
		let bbid = self.use_map.get(var).unwrap().bb();

		// unwrap is ok here for the same reason
		bbs[bbid].phi_for_var(var).unwrap()
	}

	fn visit_mark(&mut self, bbid: IrBBId, bbs: &mut [BB], doms: &DomTree) {
		// mark all variables declared by phi functions as unused
		for phi in bbs[bbid].phis() {
			// println!("marking var {} as unused in bb {}", phi.dst_var(), bbid);
			self.use_map.insert(phi.dst_var().clone(), UseInfo::new(bbid));
		}

		// mark all variables used by instructions as used
		// (have to use insts_mut and visit_uses_mut because the closure is FnMut...)
		for inst in bbs[bbid].insts_mut() {
			inst.visit_uses_mut(|v| {
				// "if v is defined by some phi function" - this reduces to a check to see
				// if v is in the use map, because we are visiting the BBs in dominance order,
				// so if it was defined by a phi function then it will already have been added.
				if let Some(info) = self.use_map.get_mut(&v) {
					// mark as used and push it on the stack
					// println!("marking var {} as used in bb {}", v, info.bb());
					info.mark_used();
					self.stack.push(v.clone());
				}
			});
		}

		// visit children
		for child in doms.immediately_dominated_by(bbid) {
			self.visit_mark(child, bbs, doms);
		}
	}
}