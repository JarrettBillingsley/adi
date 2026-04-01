
use std::marker::{ PhantomData };

use petgraph::{
	graphmap::{ DiGraphMap },
	dot::{ Dot, Config as DotConfig },
	// visit::{ DfsPostOrder },
};

use crate::program::{ Program, FuncId };

type CallGraph = DiGraphMap<FuncId, ()>;

pub(crate) struct ProgramCallGraph<'a> {
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
}

// ------------------------------------------------------------------------------------------------
// Call graph building
// ------------------------------------------------------------------------------------------------

impl Program {
	/// Build the call graph for the whole program. The returned call graph has a lifetime tied to
	/// this `Program`, so the `Program` cannot be modified until the call graph is dropped.
	pub(crate) fn build_call_graph(&self) -> ProgramCallGraph<'_> {
		let mut g = CallGraph::new();

		for (_, func) in self.funcs.all_funcs() {
			g.add_node(func.id());
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
			&|_, (_, fid)| self.name_of_ea(self.get_func(*fid).ea()).to_string(),
		));
	}
}