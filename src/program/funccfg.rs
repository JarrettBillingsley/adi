
use std::collections::{ HashMap, HashSet };

use lazycell::LazyCell;
use smallvec::{ SmallVec };
use delegate::delegate;
use petgraph::algo::dominators::{ simple_fast as simple_fast_dominators, Dominators };
use petgraph::graphmap::{ DiGraphMap };

use crate::{ BBId, Function, Program };
use crate::dataflow::{ DataflowCfg };

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
		cfg.0.add_node(func.head_id());

		for bbid in func.all_bbs() {
			self.bb_successors_in_function(bbid, |succ_id| {
				cfg.0.add_edge(bbid, succ_id, ());
			});
		}

		// should be true... the only way it couldn't be true is if there were a BB that had no
		// successors or predecessors in the function, which seeeeeeeems impossible. FOR NOW.
		// this is going to be the setup for a brick joke, isn't it?
		assert!(cfg.0.node_count() == num_bbs);

		FunctionCfg::new(func, cfg)
	}

	/// Dump the function's CFG as a DOT diagram description to the console. DEBUGGING!
	pub fn func_dump_cfg(&self, fcfg: &FunctionCfg) {
		println!("--------------------------------------------------------------");
		println!("function {}", self.name_of_ea(fcfg.func.ea()));
		fcfg.cfg.dump();
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

impl CfgGraph {
	fn new(capacity: usize, head: BBId) -> Self {
		Self(DiGraphMap::with_capacity(capacity, capacity), head)
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

// -------------------------------------------------------------------------------------------------
// CfgGraph
// -------------------------------------------------------------------------------------------------

/// Type to hold onto function CFG analysis data structures to avoid having to recompute them
/// during longer analyses. Holds a reference to the function to prevent it from being modified
/// during the analysis.
pub struct FunctionCfg<'f> {
	func:  &'f Function,
	cfg:   CfgGraph,
	doms:  LazyCell<CfgDominators>,
	preds: LazyCell<CfgPredecessors>,
}

impl<'f> FunctionCfg<'f> {
	fn new(func: &'f Function, cfg: CfgGraph) -> Self {
		Self {
			func,
			cfg,
			doms:  LazyCell::new(),
			preds: LazyCell::new(),
		}
	}

	pub(crate) fn func(&self) -> &'f Function {
		self.func
	}

	pub(crate) fn cfg(&self) -> &CfgGraph {
		&self.cfg
	}

	pub(crate) fn num_bbs(&self) -> usize {
		self.cfg.0.node_count()
	}

	pub(crate) fn all_bbs(&self) -> impl Iterator<Item = BBId> + use<'_> {
		self.cfg.0.nodes()
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

			let doms = simple_fast_dominators(&self.cfg.0, self.func.head_id());

			self.doms.fill(doms).unwrap();
		}

		self.doms.borrow().unwrap()
	}

	/// Get or calculate the predecessors of all BBs in this function. The result of this analysis
	/// is cached, so calling it a second time will return the previous results.
	pub fn bb_predecessors(&'f self) -> &'f CfgPredecessors {
		use petgraph::visit::{ DfsPostOrder, Walker };

		if !self.preds.filled() {
			let mut preds = CfgPredecessors::new();

			// "borrowed" from petgraph::algo::dominators::simple_fast_post_order :P
			for pred in DfsPostOrder::new(&self.cfg.0, self.func.head_id()).iter(&self.cfg.0) {
				for succ in self.cfg.0.neighbors(pred) {
					preds.entry(succ).or_default().push(pred);
				}
			}

			// head node has no preds
			preds.entry(self.func.head_id()).or_default();

			self.preds.fill(preds).unwrap();
		}

		self.preds.borrow().unwrap()
	}

	/// Calculates the set of all BBs reachable from the `start` bb in this function, not including
	/// `start`. **The result of this analysis is *not* cached.**
	pub fn reachable_bbs(&self, start: BBId) -> HashSet<BBId> {
		use petgraph::visit::{ DfsPostOrder, Walker };

		let mut reachable = HashSet::new();

		for bb in DfsPostOrder::new(&self.cfg.0, start).iter(&self.cfg.0) {
			reachable.insert(bb);
		}

		reachable.remove(&start);
		reachable
	}

	/// Computes the set of BBs reachable from `start`, then checks if `start` dominates all of
	/// them. If so, returns `Some(reachable)`, the set of reachable nodes. If not, returns `None`.
	///
	/// This is useful to ask if `start` is a sort of "pinch point" in a function through which all
	/// control must flow from the first part of the function to the second. This is used interally
	/// to determine if a function can be split into two functions at `start`.
	pub fn dominates_all_reachable(&self, start: BBId) -> Option<HashSet<BBId>> {
		let doms = self.dominators();
		let reachable = self.reachable_bbs(start);
		// debug!("{:#?}", doms);
		// debug!("reachable: {:#?}", reachable);

		for &n in reachable.iter() {
			let mut doms_of_n = doms.strict_dominators(n).expect("unreachable from function head");

			if !doms_of_n.any(|d| d == start) {
				return None;
			}
		}

		Some(reachable)
	}

	/// If `self` is irreducible, returns `Some(set)` of nodes which participate in at least one
	/// irreducible cycle. Otherwise, if it returns `None`, then self is reducible.
	///
	/// Based on:
	/// - "Flow graph reducibility" by Hecht and Ullman, 1972, with clarifications by:
	/// - "Making Graphs Reducible with Controlled Node Splitting" by Janssen and Corporaal, 1997
	pub fn find_irreducible_nodes(&self) -> Option<HashSet<BBId>> {
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

		let mut g = self.cfg.clone();
		let head = self.head_id();
		// g.dump();

		loop {
			if !remove_self_edges(&mut g) {
				if !merge_node_with_one_pred(&mut g, head) {
					break;
				}
			}
		}

		if g.0.node_count() > 1 {
			log::warn!("AAAAAAAAA IRREDUCIBLE CFG COLLAPSED TO:");
			g.dump();
			Some(g.0.nodes().collect())
		} else {
			None
		}
	}
}

