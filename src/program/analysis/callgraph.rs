
use petgraph::{
	Direction,
	graphmap::{ DiGraphMap },
	dot::{ Dot, Config as DotConfig },
	algo::{ tarjan_scc },
};

use crate::program::{ Program, FuncId, EA };

type CallGraph = DiGraphMap<FuncId, ()>;

pub(crate) struct ProgramCallGraph {
	g: CallGraph,
}

impl ProgramCallGraph {
	fn new(g: CallGraph) -> Self {
		Self {
			g,
		}
	}

	pub(crate) fn sccs(&self) -> Vec<Vec<FuncId>> {
		tarjan_scc(&self.g)
	}

	pub(crate) fn callers_of(&self, fid: FuncId) -> impl Iterator<Item = FuncId> {
		self.g.edges_directed(fid, Direction::Incoming).map(|(_, dst, _)| dst)
	}

	pub(crate) fn callees_of(&self, fid: FuncId) -> impl Iterator<Item = FuncId> {
		self.g.edges_directed(fid, Direction::Outgoing).map(|(_, dst, _)| dst)
	}

	pub(crate) fn is_recursive(&self, fid: FuncId) -> bool {
		self.callees_of(fid).any(|dst| dst == fid)
	}

	// fn is_leaf(&self, fid: FuncId) -> bool {
	// 	self.cg.callees_of(fid).next().is_none()
	// }

	// fn is_root(&self, fid: FuncId) -> bool {
	// 	self.cg.callers_of(fid).next().is_none()
	// }
}

// ------------------------------------------------------------------------------------------------
// Call graph building
// ------------------------------------------------------------------------------------------------

impl Program {
	/// Build the call graph for the whole program.
	pub(crate) fn build_call_graph(&self) -> ProgramCallGraph {
		let mut g = CallGraph::new();

		for (_, func) in self.funcs.all_funcs() {
			let src_fid = func.id();
			// add every function, in case there are any disconnected from the rest of the graph
			g.add_node(src_fid);

			// then add an edge to every outref in every BB which refers to a BB in another func
			for bbid in func.all_bbs() {
				let term = self.bbidx.get(bbid).term();

				let mut maybe_add = |dst: EA, is_call| {
					if let Some(dst_bbid) = self.span_at_ea(dst).bb() {
						let dst_fid = self.bbidx.get(dst_bbid).func();

						// if different funcs, always add an edge.
						if src_fid != dst_fid ||
						// if it's the same func, we only want to add an edge when it's a legit
						// recursive call, and not just a "jump to beginning".
						is_call {
							g.add_edge(src_fid, dst_fid, ());
						}
					}
				};

				for dst in term.explicit_successors() {
					maybe_add(*dst, term.is_call());
				}

				if let Some(dst) = term.continuation_successor() {
					maybe_add(dst, false);
				}
			}
		}

		ProgramCallGraph::new(g)
	}

	pub(crate) fn dump_call_graph(&self, cg: &ProgramCallGraph) {
		println!("----------------------------------------------------------------------------");
		println!("Call Graph");
		println!();
		println!("{:?}", Dot::with_attr_getters(&cg.g,
			&[DotConfig::EdgeNoLabel, DotConfig::NodeNoLabel],
			&|_, _| "".into(),
			&|_, (_, fid)| format!("label = \"{}\"", self.name_of_ea(self.get_func(*fid).ea())),
		));
	}
}