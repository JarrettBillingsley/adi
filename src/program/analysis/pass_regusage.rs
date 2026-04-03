
use crate::program::{ Program, FuncId, RegSet };
use crate::program::analysis::callgraph::*;
use crate::ir::{ RegDbg, DefUseKind };

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

			// break;
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
		let arch = self.prog.plat().arch();
		let ir = self.prog.func_to_ir(fid);

		log::debug!("{:?}", crate::ir::IrFunctionWithNames(
			&ir, &self.prog.plat.arch().new_ir_compiler()));

		let defs = ir.find_defs_and_uses();

		// 1. argument regs are zero-generation registers which are used by real uses, not just
		// dummy uses.
		let mut arg_set = arch.arch_reg_set();

		for reg in arch.arch_ir_regs() {
			let reg = reg.sub(0);
			match defs.get(&reg) {
				Some(usage) => match usage.how_used() {
					DefUseKind::None => {
						// I don't think this is reachable right now, but maybe in the future the
						// find_defs_and_uses algo will change.
						arg_set.remove(reg.offset());
					}
					DefUseKind::OnlyDummy => {
						log::trace!("  {:?} is only used in dummy uses",
							RegDbg(reg, Some(&self.prog.plat().arch().new_ir_compiler())));

						arg_set.remove(reg.offset());
					}
					DefUseKind::Real => {
						log::trace!("  {:?} is used as an argument!",
							RegDbg(reg, Some(&self.prog.plat().arch().new_ir_compiler())));
					}
				}
				None => {
					arg_set.remove(reg.offset());
				}
			}
		}

		log::trace!("    arg_set = {:?}", arg_set);

		// 2. clobber regs are any reg with nonzero generation at any exit point.
		let mut clobber_set = RegSet::new();

		for irbbid in ir.exitpoints().iter().copied() {
			for reg in ir.get_bb(irbbid).dummy_use_regs() {
				if !reg.is_gen0() {
					if clobber_set.insert(reg.offset()) {
						log::trace!("  {:?} is clobbered",
							RegDbg(reg, Some(&self.prog.plat().arch().new_ir_compiler())));
					}
				}
			}
		}

		log::trace!("    clobber_set = {:?}", clobber_set);

		// 3. return regs. for each caller, generate its IR *but when it calls this function,*
		// have it use arg_set and clobber_set around the call. (so, apply the arg and clobber sets
		// to this function? do we need the IR anymore? actually I don't think so...)
		// then do DSE on the caller, and any remaining use of a `<return>` after a call to this
		// function is a "true" return value.
		//   in that case, move the reg from the clobber set to the return set; early out if the
		//   clobber set becomes empty

		// TODO: apply the reg sets to the actual function!
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