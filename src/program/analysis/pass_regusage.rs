
use crate::program::{ Program, FuncId };
use crate::program::analysis::callgraph::*;

// ------------------------------------------------------------------------------------------------
// Whole-program register usage
// ------------------------------------------------------------------------------------------------

impl Program {
	pub(super) fn reg_usage_pass(&mut self) {
		log::info!("------------------------------------------------------------------------");
		log::info!("- begin register usage pass");

		let mut ru = RegUsagePass::new(self);
		ru.analyze();

		// then apply results of analysis

		// TODO: if a function's reg usage has been analyzed before, its *arguments and clobbers*
		// cannot have changed, but its *return* regs may have.

		// TODO: enqueue state change analysis on every function which changed???
	}
}

struct RegUsagePass<'a> {
	prog: &'a Program,
	cg:   ProgramCallGraph<'a>,
}

impl<'a> RegUsagePass<'a> {
	fn new(prog: &'a Program) -> Self {
		Self {
			cg: prog.build_call_graph(),
			prog,
		}
	}

	fn analyze(&mut self) {
		for scc in self.cg.sccs().iter() {
			match scc[..] {
				[]                              => unreachable!(),
				[fid] if self.is_recursive(fid) => self.analyze_self_recursive(fid),
				[fid]                           => self.analyze_func(fid),
				_                               => self.analyze_mutually_recursive(scc),
			}

			break;
		}
	}

	fn is_recursive(&self, fid: FuncId) -> bool {
		self.cg.callees_of(fid).any(|dst| dst == fid)
	}

	fn is_leaf(&self, fid: FuncId) -> bool {
		self.cg.callees_of(fid).next().is_none()
	}

	fn is_root(&self, fid: FuncId) -> bool {
		self.cg.callers_of(fid).next().is_none()
	}

	fn analyze_func(&mut self, fid: FuncId) {
		let func = self.prog.get_func(fid);
		log::trace!("- begin analyzing function at {}", func.ea());

		// let ir = self.prog.func_to_ir(fid);
	}

	fn analyze_self_recursive(&mut self, fid: FuncId) {
		log::trace!("- TODO: self-recursive functions not yet implemented. {}",
			self.prog.get_func(fid).ea());
	}

	fn analyze_mutually_recursive(&mut self, fids: &[FuncId]) {
		log::trace!("- TODO: mutually-recursive functions not yet implemented. SCC:");

		for fid in fids {
			let ea = self.prog.get_func(*fid).ea();
			let name = self.prog.name_of_ea(ea);
			log::trace!("    {} @ {:?} ({:?})", name, ea, fid);
		}
	}
}