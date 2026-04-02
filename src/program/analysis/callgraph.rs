
use std::marker::{ PhantomData };

use petgraph::{
	graphmap::{ DiGraphMap },
	dot::{ Dot, Config as DotConfig },
	algo::{ tarjan_scc },
};

use crate::program::{ Program, FuncId, EA };

type CallGraph = DiGraphMap<FuncId, ()>;

// TODO: make pub(crate)
pub struct ProgramCallGraph<'a> {
	g: CallGraph,
	_phantom: PhantomData<&'a ()>,
}

impl<'a> ProgramCallGraph<'a> {
	fn new(g: CallGraph) -> Self {
		Self {
			g,
			_phantom: PhantomData::default(),
		}
	}

	// TODO: make pub(crate)
	pub fn sccs(&self) -> Vec<Vec<FuncId>> {
		tarjan_scc(&self.g)
	}
}

// ------------------------------------------------------------------------------------------------
// Call graph building
// ------------------------------------------------------------------------------------------------

impl Program {
	/// Build the call graph for the whole program. The returned call graph has a lifetime tied to
	/// this `Program`, so the `Program` cannot be modified until the call graph is dropped.
	// TODO: make pub(crate)
	pub fn build_call_graph(&self) -> ProgramCallGraph<'_> {
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

	// TODO: make pub(crate)
	pub fn dump_call_graph(&self, cg: &ProgramCallGraph) {
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