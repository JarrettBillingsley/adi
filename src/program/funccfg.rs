
use std::collections::{ HashMap, HashSet };

use lazycell::LazyCell;
use smallvec::{ SmallVec };
use delegate::delegate;
use petgraph::algo::dominators::{ simple_fast as simple_fast_dominators, Dominators };
use petgraph::graphmap::{ DiGraphMap };

use crate::{ BBId, Function, Program };
use crate::dataflow::{ DataflowCfg };

// -------------------------------------------------------------------------------------------------
// Helpers
// -------------------------------------------------------------------------------------------------

fn sorted_vec<'a, T: 'a + Copy + Ord>(c: impl IntoIterator<Item = &'a T>) -> Vec<T> {
	let mut ret = c.into_iter().copied().collect::<Vec<_>>();
	ret.sort();
	ret
}

fn r_overlaps(overlap: &HashSet<BBId>, irred: &HashSet<BBId>) -> bool {
	!overlap.is_empty() && overlap != irred
}

fn dominates(doms: &CfgDominators, a: BBId, b: BBId) -> bool {
	doms.strict_dominators(b).expect("stranded CFG node")
		.any(|dom| dom == a)
}

// -------------------------------------------------------------------------------------------------
// CFG Algorithms
// -------------------------------------------------------------------------------------------------

impl Program {
	/// Begin the analysis of a function's CFG. The returned object has several analysis methods and
	/// caches some of their results. It can also be passed to some other `Program` methods.
	pub fn func_analyze_cfg<'f>(&self, func: &'f Function) -> FunctionCfg<'f> {
		let num_bbs = func.num_bbs();
		let mut cfg = CfgGraph::new(num_bbs, func.head_id());

		// for CFGs with only one node, there are 0 edges, so the loop below will not add
		// the head node.
		cfg.add_node(func.head_id());

		for bbid in func.all_bbs() {
			self.bb_successors_in_function(bbid, |succ_id| {
				cfg.add_edge(bbid, succ_id);
			});
		}

		// should be true... the only way it couldn't be true is if there were a BB that had no
		// successors or predecessors in the function, which seeeeeeeems impossible. FOR NOW.
		// this is going to be the setup for a brick joke, isn't it?
		assert!(cfg.num_nodes() == num_bbs);

		FunctionCfg::new(func, cfg)
	}

	/// Dump the function's CFG as a DOT diagram description to the console. DEBUGGING!
	pub fn func_dump_cfg(&self, cfg: &FunctionCfg) {
		println!("--------------------------------------------------------------");
		println!("function {}", self.name_of_ea(cfg.func.ea()));
		cfg.graph.dump();
	}
}

// -------------------------------------------------------------------------------------------------
// CfgGraph
// -------------------------------------------------------------------------------------------------

/// .0 is the underlying `DiGraphMap`
/// .1 is the head node ID
#[derive(Clone)]
pub struct CfgGraph(DiGraphMap<BBId, ()>, BBId);
pub type CfgDominators   = Dominators<BBId>;
pub type CfgPredecessors = HashMap<BBId, SmallVec<[BBId; 4]>>;

/// Wrapper for underlying `petgraph::DiGraphMap`, to insulate rest of code in case I ever want to
/// use a different underlying graph representation.
///
/// Does not cache anything.
impl CfgGraph {
	fn new(capacity: usize, head: BBId) -> Self {
		Self(DiGraphMap::with_capacity(capacity, capacity), head)
	}

	fn add_node(&mut self, node: BBId) {
		self.0.add_node(node);
	}

	fn remove_node(&mut self, node: BBId) {
		self.0.remove_node(node);
	}

	fn add_edge(&mut self, from: BBId, to: BBId) {
		self.0.add_edge(from, to, ());
	}

	fn num_bbs(&self) -> usize {
		self.0.node_count()
	}

	fn all_bbs(&self) -> impl Iterator<Item = BBId> + use<'_> {
		self.0.nodes()
	}

	fn doms(&self, head: BBId) -> CfgDominators {
		simple_fast_dominators(&self.0, head)
	}

	fn preds(&self, head: BBId) -> CfgPredecessors {
		use petgraph::visit::{ DfsPostOrder, Walker };
		let mut preds = CfgPredecessors::new();

		// "borrowed" from petgraph::algo::dominators::simple_fast_post_order :P
		for pred in DfsPostOrder::new(&self.0, head).iter(&self.0) {
			for succ in self.0.neighbors(pred) {
				preds.entry(succ).or_default().push(pred);
			}
		}

		// head node has no preds
		preds.entry(head).or_default();
		preds
	}

	fn reachable(&self, start: BBId, head: BBId, doms: &CfgDominators) -> ReachableBBs {
		use petgraph::visit::{ depth_first_search, DfsEvent };

		let mut not_dominated = HashSet::new();
		let mut cyclic = false;
		let mut head_cyclic = false;

		depth_first_search(&self.0, Some(start), |event| {
			match event {
				DfsEvent::Discover(n, _) if n != start => {
					not_dominated.insert(n);
					head_cyclic |= n == head;
				}
				DfsEvent::BackEdge(_, n) if n == start => {
					cyclic = true;
				}
				_ => {}
			}
		});

		let dominated: HashSet<BBId> =
			not_dominated
			.extract_if(|n|
				doms.strict_dominators(*n).expect("unreachable from function head")
				.any(|d| d == start))
			.collect();
		ReachableBBs { from: start, cyclic, head_cyclic, dominated, not_dominated }
	}

	pub fn find_irreducible_nodes(&self, head: BBId) -> Option<HashSet<BBId>> {
		use petgraph::{ Direction };

		/// known as T1 in the literature, returns true if any self-edges (X -> X) were removed
		fn remove_self_edges(g: &mut CfgGraph) -> bool {
			let to_remove: HashSet<BBId> =
				g.0.all_edges()
				.filter_map(|(src, dst, _)| (src == dst).then_some(src))
				.collect();

			if to_remove.is_empty() {
				false
			} else {
				for src in to_remove.into_iter() {
					// log::trace!("T1({src:?})");
					g.0.remove_edge(src, src);
				}
				// g.dump();
				true
			}
		}

		/// known as T2 in the literature, returns true if a node with a single predecessor was
		/// merged with its predecessor
		fn merge_node_with_one_pred(g: &mut CfgGraph, head: BBId) -> bool {
			for dst in g.0.nodes() {
				if dst != head {
					let mut preds = g.0.neighbors_directed(dst, Direction::Incoming);
					match (preds.next(), preds.next()) {
						// skip any node with 0 or ≥ 2 in-edges
						(None, None) |
						(_, Some(_)) => continue,

						// exactly 1 in-edge (src, dst).
						(Some(src), None) => {
							// log::trace!("T2({dst:?})");

							// get successors
							let succs: Vec<BBId> =
								g.0.neighbors_directed(dst, Direction::Outgoing).collect();

							// remove dst, edge (src, dst), and all edges (dst, _)
							g.0.remove_node(dst);

							// add edges from src to dst's old successors
							for succ in succs.into_iter() {
								// don't add self-edges (makes less work for T1)
								if src != succ {
									g.0.add_edge(src, succ, ());
								}
							}

							// g.dump();
							return true;
						}
					}
				}
			}

			false
		}

		let mut g = self.clone();
		// g.dump();

		loop {
			if !remove_self_edges(&mut g) && !merge_node_with_one_pred(&mut g, head) {
				break;
			}
		}

		if g.0.node_count() > 1 {
			// log::warn!("AAAAAAAAA IRREDUCIBLE CFG COLLAPSED TO:");
			// g.dump();
			Some(g.0.nodes().collect())
		} else {
			None
		}
	}

	/// The "R set" of a node `N` is the set of all nodes reachable from, but not dominated by, `N`.
	///
	/// Search through all dominator roots of the R set of the node in `r` for any whose R set
	/// overlaps `irred`. If found, returns it; otherwise returns `None`.
	fn dom_root_that_overlaps(&self, r: &ReachableBBs, irred: &HashSet<BBId>, head: BBId,
	doms: &CfgDominators) -> Option<BBId> {
		let roots = r.dom_roots(doms);

		roots.into_iter().find(
			|&root| r_overlaps(&self.reachable(root, head, doms).r_overlap_of(irred),
			irred))
	}

	fn dump(&self) {
		use petgraph::dot::{ Dot, Config };
		// because func_dump_cfg uses println
		println!("{:?}", Dot::with_config(&self.0, &[Config::EdgeNoLabel]));
	}
}

/// Implementation of `DataflowCfg` to allow it to be used with the
/// `DataflowAlgorithm` framework.
impl DataflowCfg<BBId> for CfgGraph {
	fn num_nodes(&self) -> usize {
		self.0.node_count()
	}

	fn initial_order(&self) -> impl Iterator<Item = BBId> {
		let mut rpo = Vec::<BBId>::with_capacity(self.num_nodes());
		let mut postorder = petgraph::visit::DfsPostOrder::new(&self.0, self.1);
		while let Some(id) = postorder.next(&self.0) {
			rpo.push(id);
		}

		rpo.into_iter().rev()
	}

	fn successors(&self, id: BBId) -> impl Iterator<Item = BBId> {
		self.0.edges(id).map(|(_, succ, _)| succ)
	}
}

// ------------------------------------------------------------------------------------------------
// ReachableBBs
// ------------------------------------------------------------------------------------------------

/// Returned by `FunctionCfg::reachable`, results of looking for nodes reachable from a node.
pub struct ReachableBBs {
	/// The node on which `FunctionCfg::reachable` was called.
	pub from:          BBId,

	/// True if `from` is reachable from itself through some cycle.
	pub cyclic:        bool,

	/// True if `from` is reachable from itself through a cycle which contains the function head.
	pub head_cyclic:   bool,

	/// The nodes reachable from *and dominated by* `from`.
	pub dominated:     HashSet<BBId>,

	/// The nodes reachable from *but not dominated by* `from`.
	pub not_dominated: HashSet<BBId>,
}

impl ReachableBBs {
	/// Computes the union of `self.dominated` and `self.not_dominated`.
	pub fn all_reachable(&self) -> HashSet<BBId> {
		self.dominated.union(&self.not_dominated).copied().collect()
	}

	/// Returns `true` if `self.not_dominated` is empty. NOTE: this alone is not enough to know if
	/// a function is actually splittable!
	pub fn splittable(&self) -> bool {
		self.not_dominated.is_empty()
	}

	/// Computes the subset of `not_dominated` whose dominators are not in `not_dominated`. If given
	/// the set of all nodes in the function, the returned set will only contain the head.
	pub fn dom_roots(&self, doms: &CfgDominators) -> HashSet<BBId> {
		let set: HashSet<BBId> = self.not_dominated.iter()
			.filter(|n|
				doms.immediate_dominator(**n)
					// keep only those nodes whose immediate dominator is not in r.not_dominated...
					.map(|dom| !self.not_dominated.contains(&dom))
					// ...or which are the CFG head.
					.unwrap_or(true))
			.copied()
			.collect();

		set
	}

	/// Compute the overlap of the given set of nodes `irred` with the R set of the node in `r`.
	fn r_overlap_of(&self, irred: &HashSet<BBId>) -> HashSet<BBId> {
		self.not_dominated.intersection(irred).copied().collect()
	}

	pub(crate) fn dump(&self) {
		println!("{:?} {} in a cycle with the head",
			self.from, if self.head_cyclic { "is" } else { "is not" });
		println!("{:?} {} in a non-head cycle",
			self.from, if self.cyclic { "is" } else { "is not" });
		println!("    reachable from and dominated by {:?}: {:?}",
			self.from, sorted_vec(&self.dominated));
		println!("reachable from but NOT dominated by {:?}: {:?}",
			self.from, sorted_vec(&self.not_dominated));
		println!("  {:?} {}\n",
			self.from, if self.splittable() { "IS splittable!" } else { "is NOT splittable." });
	}
}

// -------------------------------------------------------------------------------------------------
// UnsplittableError
// -------------------------------------------------------------------------------------------------

#[derive(Debug)]
pub enum UnsplittableError {
	/// The split point is the function's head.
	IsHead,

	/// The split point is in a cycle with the head node of the function.
	InCycleWithHead,

	/// The split point is either a part of, or partially overlaps, an irreducible part of the CFG
	/// (`.1`).
	OverlapsIrreducible(HashSet<BBId>),

	/// Some dom root's (`.0`) R-set partially overlaps an irreducible part of the CFG (`.1`).
	DomRootOverlapsIrreducible(BBId, HashSet<BBId>),

	/// Some dom root (`.0`) dominates the split point, which means the split point is in a cycle
	/// but does not dominate all nodes in the cycle.
	DomRootDominatesSplitPoint(BBId),

	/// None of the dom roots (`.0`) are splittable. This is probably a bug.
	NoSplittableDomRoots(HashSet<BBId>),
}

// -------------------------------------------------------------------------------------------------
// FunctionCfg
// -------------------------------------------------------------------------------------------------

/// Type to hold onto function CFG analysis data structures to avoid having to recompute them
/// during longer analyses. Holds a reference to the function to prevent it from being modified
/// during the analysis.
pub struct FunctionCfg<'f> {
	func:  &'f Function,
	graph: CfgGraph,
	doms:  LazyCell<CfgDominators>,
	preds: LazyCell<CfgPredecessors>,
}

impl<'f> FunctionCfg<'f> {
	fn new(func: &'f Function, graph: CfgGraph) -> Self {
		Self {
			func,
			graph,
			doms:  LazyCell::new(),
			preds: LazyCell::new(),
		}
	}

	pub(crate) fn func(&self) -> &'f Function {
		self.func
	}

	pub(crate) fn cfg(&self) -> &CfgGraph {
		&self.graph
	}

	pub(crate) fn num_bbs(&self) -> usize {
		self.graph.num_bbs()
	}

	pub(crate) fn all_bbs(&self) -> impl Iterator<Item = BBId> + use<'_> {
		self.graph.all_bbs()
	}

	delegate! {
		to self.func {
			pub(crate) fn head_id(&self) -> BBId;
		}
	}

	/// Get or calculate the dominators of all BBs in this function. The result of this analysis is
	/// cached, so calling it a second time will return the previous results.
	///
	/// Panics if the function is multi-entry.
	pub fn dominators(&'f self) -> &'f CfgDominators {
		if !self.doms.filled() {
			assert!(!self.func.is_multi_entry());
			let doms = self.graph.doms(self.func.head_id());
			self.doms.fill(doms).unwrap();
		}

		self.doms.borrow().unwrap()
	}

	/// Get or calculate the predecessors of all BBs in this function. The result of this analysis
	/// is cached, so calling it a second time will return the previous results.
	pub fn bb_predecessors(&'f self) -> &'f CfgPredecessors {
		if !self.preds.filled() {
			let preds = self.graph.preds(self.func.head_id());
			self.preds.fill(preds).unwrap();
		}

		self.preds.borrow().unwrap()
	}

	/// Compute all BBs reachable from **but not including** `start`, and partition them into two
	/// subsets returned as a `ReachableBBs`: those which are `dominated` by `start`, and those
	/// which are `not_dominated`. `ReachableBBs` also has a `bool` of whether `start` is part of a
	/// cycle, and another whether it's in a cycle with the function head.
	pub fn reachable(&self, start: BBId) -> ReachableBBs {
		self.graph.reachable(start, self.func.head_id(), self.dominators())
	}

	/// If `self` is irreducible, returns `Some(set)` of nodes which participate in at least one
	/// irreducible cycle. Otherwise, if it returns `None`, then self is reducible.
	///
	/// Based on:
	/// - "Flow graph reducibility" by Hecht and Ullman, 1972, with clarifications by:
	/// - "Making Graphs Reducible with Controlled Node Splitting" by Janssen and Corporaal, 1997
	pub fn find_irreducible_nodes(&self) -> Option<HashSet<BBId>> {
		self.graph.find_irreducible_nodes(self.func.head_id())
	}

	/// Computes a splitting plan for splitting `cfg` at `split_node`.
	///
	/// A return of `Ok(vec)` is a sequence of splits which would successfully split the CFG into 2
	/// or more separate functions.
	///
	/// A return of `Err(e)` means there is no way to split the `cfg` at `node`; `e` explains why.
	pub fn split_plan_for(&self, split_node: BBId) -> Result<Vec<BBId>, UnsplittableError> {
		let head = self.func.head_id();
		if split_node == head {
			return Err(UnsplittableError::IsHead);
		}

		// we're gonna be removing stuff from it to do our work
		let mut cfg: CfgGraph = self.graph.clone();
		// the returned sequence of operations
		let mut ret = vec![];
		// the dominators (they will be changing as we change cfg)
		let mut doms = cfg.doms(head);

		// the reachable stuff
		let mut r = cfg.reachable(split_node, head, &doms);

		// check for unsplittable conditions
		if r.head_cyclic {
			return Err(UnsplittableError::InCycleWithHead);
		} else if let Some(irred) = cfg.find_irreducible_nodes(head) {
			// NOTE: this is actually a bit conservative because `irred` is *all* irreducible nodes
			// in the CFG, but there *could* be multiple disjoint sets of irreducible nodes. but
			// that seems so incredibly unlikely that I'm not gonna bother.

			if irred.contains(&split_node) {
				// B is part of the IR clump
				return Err(UnsplittableError::OverlapsIrreducible(irred));
			}

			let overlap = r.r_overlap_of(&irred);

			if r_overlaps(&overlap, &irred) {
				// println!("overlap = {:?}", overlap);
				// r.dump();
				// R(B) partially overlaps with the irred clump
				return Err(UnsplittableError::OverlapsIrreducible(irred));
			} else if irred.is_subset(&r.not_dominated) {
				// IR ∈ R(B)
				if let Some(root) = cfg.dom_root_that_overlaps(&r, &irred, head, &doms) {
					// IR overlaps with one or more dom tree's R(T)
					return Err(UnsplittableError::DomRootOverlapsIrreducible(root, irred));
				}
			}
		}

		'outer: loop {
			if r.splittable() {
				// yay! base case
				ret.push(split_node);
				break;
			} else {
				let roots = r.dom_roots(&doms);

				// look for any dom roots which dominate split_node - indicates some cyclical shit
				if let Some(root) = roots.iter().find(|root| dominates(&doms, **root, split_node)) {
					return Err(UnsplittableError::DomRootDominatesSplitPoint(*root));
				}

				for root in roots.iter() {
					let rr = cfg.reachable(*root, head, &doms);
					if rr.splittable() {
						// yay! found a splittable dom root. simulate removing it from the function

						// println!("remove {}", root);
						cfg.remove_node(*root);
						for root_child in rr.dominated {
							// println!("  remove {}", root_child);
							cfg.remove_node(root_child);
						}

						ret.push(*root);
						doms = cfg.doms(head);
						r = cfg.reachable(split_node, head, &doms);
						continue 'outer;
					}
				}

				return Err(UnsplittableError::NoSplittableDomRoots(roots));
			}
		}

		Ok(ret)
	}
}

