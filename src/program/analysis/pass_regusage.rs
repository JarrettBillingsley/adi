
use crate::fxhash::{ FxHashMap as HashMap, FxHashMapEx };

use crate::arch::{ Architecture };
use crate::program::{ EA, Program, FuncId, RegSet, FuncRegUsage };
use crate::program::analysis::callgraph::*;
use crate::program::analysis::to_ir::{ IRewriteCtx };
use crate::ir::{ RegDbg, IrInstKind, IrTarget, IrBBId, IrReg };

// ------------------------------------------------------------------------------------------------
// Whole-program register usage analysis
// ------------------------------------------------------------------------------------------------

struct RegUsageCtx<'a> {
	arch:          &'a Architecture,
	eas_to_fids:   HashMap<EA, FuncId>,
	fids_to_usage: HashMap<FuncId, Option<FuncRegUsage>>,
}

impl<'a> RegUsageCtx<'a> {
	fn new(prog: &'a Program) -> Self {
		let arch = prog.plat().arch();

		let mut eas_to_fids = HashMap::new();

		for func in prog.all_funcs() {
			for bbid in func.entrypoints() {
				eas_to_fids.insert(prog.bbidx.get(*bbid).ea(), func.id());
			}
		}

		Self {
			arch,
			eas_to_fids,
			fids_to_usage: prog.all_funcs().map(|func| (func.id(), func.reg_usage())).collect(),
		}
	}

	fn of_fid(&self, fid: FuncId) -> Option<FuncRegUsage> {
		*self.fids_to_usage.get(&fid).unwrap()
	}

	fn change(&mut self, fid: FuncId, usage: FuncRegUsage) -> bool {
		let new_usage = Some(usage);

		if self.fids_to_usage[&fid] != new_usage {
			log::trace!(" temporarily setting {:?} usage to {:?}", fid, usage);
			self.fids_to_usage.insert(fid, new_usage);
			true
		} else {
			false
		}
	}
}

impl<'a> IRewriteCtx for RegUsageCtx<'a> {
	fn reg_usage_of(&self, ea: EA) -> Option<FuncRegUsage> {
		let fid = self.eas_to_fids.get(&ea)?;
		self.fids_to_usage.get(fid).copied()?
	}

	fn arch_ir_reg(&self, offset: u8) -> IrReg {
		self.arch.arch_ir_reg(offset)
	}

	fn default_regs(&self) -> RegSet {
		self.arch.arch_reg_set()
	}
}

impl Program {
	pub(super) fn reg_usage_pass(&mut self) {
		log::info!("------------------------------------------------------------------------");
		log::info!("- begin register usage pass");

		let cg = self.build_call_graph();
		let sccs = cg.sccs();

		// --------------------------------------------------------------------------------------------
		// pass 1: bottom-up, determine argument and clobber sets for each function

		let mut reg_usage = RegUsageCtx::new(self);

		for scc in sccs.iter() {
			match scc[..] {
				[] => unreachable!("petgraph::algo::tarjan_scc gave a 0-size SCC???"),
				[fid] => {
					log::trace!("- begin analyzing function arguments/clobbers at {}",
						self.get_func(fid).ea());
					let arch = self.plat().arch();
					let all_regs = arch.arch_reg_set();
					let ir = self.func_to_ir(fid);

					log::debug!("before determining clobbers: {:?}",
						crate::ir::IrFunctionWithNames(&ir, &arch));

					// 1. clobber regs are the union of all callee clobbers *except for itself
					// (for recursive funcs)*, plus any reg with nonzero generation at any exit
					// point.
					let mut clobber_set = RegSet::new();

					for callee in cg.callees_of(fid) {
						if callee != fid {
							clobber_set |= reg_usage.of_fid(callee)
								.map(|ru| ru.clobbers())
								.unwrap();
						}

						// early out if all regs are clobbered
						if clobber_set == all_regs {
							break;
						}
					}

					if clobber_set != all_regs {
						for irbbid in ir.exitpoints().iter().copied() {
							for reg in ir.get_bb(irbbid).dummy_use_regs() {
								if !reg.is_gen0() &&
								clobber_set.insert(reg.offset()) &&
								clobber_set == all_regs {
									// early out if all regs are clobbered
									break;
								}
							}
						}
					}

					log::trace!("    clobber_set = {:?}", clobber_set);
					// don't care if usage changed in this phase. If it's been analyzed before, its
					// arguments/clobbers can't have changed anyway.
					let usage = FuncRegUsage::new(all_regs, clobber_set);
					reg_usage.change(fid, usage);

					// doing this breaks the lifetime so we can reborrow below
					let arch = self.plat().arch();

					// regenerate the IR using the newly-determined clobbers
					let ir = self.func_to_ir(fid);

					log::debug!("after determining clobbers: {:?}",
						crate::ir::IrFunctionWithNames(&ir, &arch));

					let defs = ir.find_defs_and_uses();

					// 2. argument regs are zero-generation registers which are used by real uses,
					// not just dummy uses.
					let mut arg_set = all_regs;

					for reg in arch.arch_ir_regs() {
						let reg = reg.sub(0);
						match defs.get(&reg) {
							Some(usage) if usage.is_really_used() => {
								log::trace!("  {:?} is used as an argument!",
									RegDbg(reg, Some(&arch)));
							}
							Some(_) | None => {
								arg_set.remove(reg.offset());

								// early-out if we've eliminated all potential registers
								if arg_set.is_empty() {
									break;
								}
							}
						}
					}

					log::trace!("    arg_set = {:?}", arg_set);

					// don't care if usage changed in this phase. If it's been analyzed before, its
					// arguments/clobbers can't have changed anyway.
					let usage = FuncRegUsage::new(arg_set, clobber_set);
					reg_usage.change(fid, usage);
				}
				_ => {
					log::warn!("- TODO: mutually-recursive function arg/clobber not yet \
						implemented. SCC:");

					for fid in scc {
						let ea = self.get_func(*fid).ea();
						let name = self.name_of_ea(ea);
						log::trace!("    {} @ {:?} ({:?})", name, ea, fid);
					}

					let _ = reg_usage;
				}
			}
		}

		RegUsagePass::new(self, &cg).analyze_returns(&sccs, &mut reg_usage);

		for (fid, usage) in reg_usage.fids_to_usage.into_iter() {
			log::trace!(" setting {:?} usage to {:?}", fid, usage);
			*self.funcs.get_mut(fid).reg_usage_mut() = usage;
		}

		// then apply results of analysis

		// TODO: enqueue state change analysis on every function which changed???
	}
}

struct RegUsagePass<'a> {
	prog: &'a Program,
	cg:   &'a ProgramCallGraph,
}

impl<'a> RegUsagePass<'a> {
	fn new(prog: &'a Program, cg: &'a ProgramCallGraph) -> Self {
		Self {
			prog,
			cg,
		}
	}

	// --------------------------------------------------------------------------------------------
	// pass 2: top-down, determine return sets for each function
	fn analyze_returns(&mut self, sccs: &[Vec<FuncId>], reg_usage: &mut RegUsageCtx) {
		for scc in sccs.iter().rev() {
			match scc[..] {
				[]    => unreachable!(),
				[fid] => self.returns_func(fid, reg_usage),
				_     => self.returns_mutually_recursive(scc),
			}
		}
	}

	fn returns_func(&mut self, fid: FuncId, reg_usage: &mut RegUsageCtx) {
		log::trace!("- begin analyzing function returns at {}", self.prog.get_func(fid).ea());
		let arch = self.prog.plat().arch();
		let mut ir = self.prog.func_to_ir(fid);

		// 1. find calls with external destinations and internal continuations.

		// maps from callees' FuncIds to the IR BB which contains the `mov _, <return>` instructions
		// after calling it.
		let mut callees_to_bbs: HashMap<FuncId, IrBBId> = HashMap::new();

		for irbbid in 0 .. ir.num_bbs() {
			let bb = ir.get_bb(irbbid);

			match bb.term_inst().kind() {
				IrInstKind::Call {
					dst: IrTarget::External(ea),
					cont: IrTarget::Internal(cont_bb),
					..
				} => {
					// self-calls will not be matched by this, since their dst will be Internal.
					if let Some(callee) = self.prog.func_that_contains(ea) {
						callees_to_bbs.insert(callee.id(), cont_bb);
					} else {
						log::warn!("  callee {:?} is not a function. (is this a problem?)", ea);
					}
				}
				IrInstKind::ICall {
					// TODO: once ICall has a Vec of destinations, we can determine if *any* are
					// external... well, if any are *internal* it'd fall under the recursive case
					cont: IrTarget::Internal(cont_bb),
					..
				} => {
					log::warn!("  TODO: ICall not yet handled. cont = bb{}", cont_bb);
				}
				_ => {}
			}
		}

		log::debug!("{:?}", crate::ir::IrFunctionWithNames(&ir, &arch));

		log::debug!("  callees to BBs = {:?}", callees_to_bbs);

		// SAFETY: phase 1 put reg usage on every function.
		ir.elim_dead_stores(reg_usage.of_fid(fid).unwrap());

		log::debug!("{:?}", crate::ir::IrFunctionWithNames(&ir, &arch));

		// any remaining return-use is a *true* return value from a callee and not just a clobber.
		for (callee_fid, irbbid) in callees_to_bbs.into_iter() {
			// TODO: if a function's reg usage has been analyzed before, its *arguments and
			// clobbers* cannot have changed, but its *return* regs may have. In that case... uhhhh
			// what? mark it for state change analysis?

			let mut ret_set = RegSet::new();

			for reg in ir.get_bb(irbbid).dummy_return_use_regs() {
				if ret_set.insert(reg.offset()) {
					log::trace!("  {:?} is a return value from {:?}",
						RegDbg(reg, Some(&arch)), callee_fid);
				}
			}

			// SAFETY: phase 1 put reg usage on every function.
			let mut usage = reg_usage.of_fid(callee_fid).unwrap();
			if usage.mark_returns(ret_set) {
				reg_usage.change(callee_fid, usage);
			}
		}
	}

	fn returns_mutually_recursive(&mut self, fids: &[FuncId]) {
		log::warn!("- TODO: mutually-recursive function returns not yet implemented. SCC:");

		for fid in fids {
			let ea = self.prog.get_func(*fid).ea();
			let name = self.prog.name_of_ea(ea);
			log::trace!("    {} @ {:?} ({:?})", name, ea, fid);
		}
	}
}