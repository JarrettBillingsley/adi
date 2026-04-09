
use crate::fxhash::{ FxHashMap as HashMap, FxHashMapEx };

use crate::program::{ EA, Program, FuncId, RegSet, FuncRegUsage };
use crate::program::analysis::callgraph::*;
use crate::program::analysis::to_ir::{ IRewriteCtx };
use crate::ir::{ RegDbg, IrInstKind, IrTarget, IrBBId, IrReg, IrFunctionWithNames };

// ------------------------------------------------------------------------------------------------
// Whole-program register usage analysis
// ------------------------------------------------------------------------------------------------

impl Program {
	pub(super) fn reg_usage_pass(&mut self) {
		log::info!("------------------------------------------------------------------------");
		log::info!("- begin register usage pass");

		let cg = self.build_call_graph();
		let sccs = cg.sccs();

		// do the analysis
		let fids_to_usage = RegUsagePass::new(self, &cg).analyze(&sccs).finish();

		// then apply results
		log::trace!("  return analysis finished. applying final results");

		for (fid, usage) in fids_to_usage.into_iter() {
			log::trace!("    setting {:?} usage to {:?}", fid, usage);

			// TODO: check if usage changed *here* and... enqueue state change analysis on them??
			*self.funcs.get_mut(fid).reg_usage_mut() = Some(usage);
		}

		log::info!("- end register usage pass");
		log::info!("------------------------------------------------------------------------");
	}
}

impl<'a> IRewriteCtx for RegUsagePass<'a> {
	fn reg_usage_of(&self, ea: EA) -> Option<FuncRegUsage> {
		if let Some(fid) = self.eas_to_fids.get(&ea) {
			if let Some(ru) = self.fids_to_usage.get(fid).copied() {
				log::trace!("    callee {:?} {:?} {:?}", ea, fid, ru);
				Some(ru)
			} else {
				log::trace!("    callee {:?} {:?} has no usage in the map", ea, fid);
				None
			}
		} else {
			log::trace!("    callee {:?} is not in the map", ea);
			None
		}
	}

	fn arch_ir_reg(&self, offset: u8) -> IrReg {
		self.prog.arch().arch_ir_reg(offset)
	}

	fn default_regs(&self) -> RegSet {
		self.prog.arch().arch_reg_set()
	}

	fn is_return_analysis_pass(&self) -> bool {
		self.is_return_analysis_pass
	}
}

struct RegUsagePass<'a> {
	prog:          &'a Program,
	cg:            &'a ProgramCallGraph,
	eas_to_fids:   HashMap<EA, FuncId>,
	fids_to_usage: HashMap<FuncId, FuncRegUsage>,
	is_return_analysis_pass: bool,
}

impl<'a> RegUsagePass<'a> {
	fn new(prog: &'a Program, cg: &'a ProgramCallGraph) -> Self {
		let all_regs = prog.arch().arch_reg_set();

		Self {
			prog,
			cg,
			eas_to_fids: prog.all_funcs()
				.map(|func| func.entrypoints().iter()
					.map(|bbid| (prog.bbidx.get(*bbid).ea(), func.id()))
				).flatten().collect(),
			fids_to_usage: prog.all_funcs()
					.map(|func| (func.id(), func.reg_usage()
					// start off with no arguments since we're determining them bottom-up, and
					// recursive functions' arguments will be mis-analyzed if the set is nonempty.
					//
					// clobbers start as all_regs, though, since uhhhhhhhhhhhh uhHHHHHHH
					.unwrap_or_else(|| FuncRegUsage::new(RegSet::EMPTY, all_regs)))
				).collect(),
			is_return_analysis_pass: false,
		}
	}

	fn analyze(mut self, sccs: &[Vec<FuncId>]) -> Self {
		self.analyze_clobbers_and_args(&sccs);
		self.analyze_returns(&sccs);
		self
	}

	fn finish(self) -> HashMap<FuncId, FuncRegUsage> {
		self.fids_to_usage
	}

	// --------------------------------------------------------------------------------------------

	fn usage_of_fid(&self, fid: FuncId) -> FuncRegUsage {
		self.fids_to_usage[&fid]
	}

	fn change_usage(&mut self, fid: FuncId, usage: FuncRegUsage) -> bool {
		if self.usage_of_fid(fid) != usage {
			log::trace!("  temporarily setting {:?} usage to \
				\n            {:?} <-- new usage\
				\n            {:?} <-- old usage", fid, usage, self.usage_of_fid(fid));
			self.fids_to_usage.insert(fid, usage);
			true
		} else {
			false
		}
	}

	fn change_clobbers(&mut self, fid: FuncId, clobber_set: RegSet) -> bool {
		self.change_usage(fid, self.usage_of_fid(fid).with_clobbers(clobber_set))
	}

	fn change_args(&mut self, fid: FuncId, arg_set: RegSet) -> bool {
		self.change_usage(fid, self.usage_of_fid(fid).with_args(arg_set))
	}

	fn change_returns(&mut self, fid: FuncId, ret_set: RegSet) -> bool {
		self.change_usage(fid, self.usage_of_fid(fid).with_returns(ret_set))
	}

	// --------------------------------------------------------------------------------------------
	// pass 1: bottom-up, determine clobber and argument sets for each function
	fn analyze_clobbers_and_args(&mut self, sccs: &[Vec<FuncId>]) {
		for scc in sccs.iter() {
			match scc[..] {
				[] => unreachable!("petgraph::algo::tarjan_scc gave a 0-size SCC???"),
				[fid] => {
					self.analyze_clobbers(fid, None);
					self.analyze_args(fid);
				}
				_ => {
					log::trace!("  mutually-recursive SCC: {:?}", scc);
					self.analyze_clobbers_mutrec(scc);
					self.analyze_args_mutrec(scc);
				}
			}
		}
	}

	// --------------------------------------------------------------------------------------------
	// clobber regs are the union of all callee changes* plus any reg with nonzero generation at
	// any exit point.
	//
	// *For recursive and mutually-recursive funcs, it's..... more subtle

	fn analyze_clobbers_mutrec(&mut self, scc: &[FuncId]) {
		let all_regs = self.prog.arch().arch_reg_set();

		// since we'll be replacing the clobber sets with dummies in the loop below, this vec holds
		// the actual clobber sets which will be applied after the loop.
		let mut actual_clobbers = vec![RegSet::EMPTY; scc.len()];

		// FIRST ROUND: determine "local" clobbers.
		log::trace!("  > BEGIN mutrec local clobbers");

		for (i, &fid) in scc.iter().enumerate() {
			// assume all *other* SCC's clobbers are empty, but assume *this* function's clobbers
			// are all_regs.
			//
			// note that this is not the same thing as simply "not adding callees' clobbers to this
			// function's clobber set," as is implemented by the in_scc check in analyze_clobbers.
			// this actually changes which `mov _, <return>` instructions are inserted during IR
			// building and therefore changes which registers are in use at the end of this
			// function.
			self.change_clobbers(fid, all_regs);
			for &other in scc.iter() {
				if other != fid {
					self.change_clobbers(other, RegSet::EMPTY);
				}
			}

			// run clobbers algo.
			self.analyze_clobbers(fid, Some(scc));

			// whatever clobbers are on the function now are its local clobbers; put it in
			// actual_clobbers.
			actual_clobbers[i] = self.usage_of_fid(fid).clobbers();
		}

		// now apply the clobbers
		for (&fid, clobber_set) in scc.iter().zip(actual_clobbers.into_iter()) {
			log::trace!("    {:?} local clobbers = {:?}", fid, clobber_set);
			self.change_clobbers(fid, clobber_set);
		}

		log::trace!("  >   END mutrec local clobbers");

		// SECOND ROUND: incorporate clobbers from callees in SCC
		for loop_iteration in 0 .. {
			log::trace!("  >> mutrec clobbers loop start {}", loop_iteration);
			let mut any_changed = false;

			for &fid in scc.iter() {
				// pass None as scc this time, so that other items in the SCC *are* merged with this
				// function's clobbers.
				any_changed |= self.analyze_clobbers(fid, None);
			}

			if !any_changed {
				break;
			} else if loop_iteration > scc.len() {
				panic!("hmmmmmm should have converged by now...");
			}
		}
	}

	fn analyze_clobbers(&mut self, fid: FuncId, scc: Option<&[FuncId]>) -> bool {
		log::debug!("> BEGIN clobbers  {} {:?}", self.prog.get_func(fid).ea(), fid);
		let arch = self.prog.arch();
		let all_regs = arch.arch_reg_set();
		let ir = self.prog.func_to_ir_ctx(fid, self);

		log::trace!("{:?}", IrFunctionWithNames(&ir, &arch));

		let mut clobber_set = RegSet::new();

		let in_scc = |id: FuncId| -> bool {
			match scc {
				// yeah this is O(n) but n is extremely likely to be tiny and mutrecs are rare to
				// begin with
				Some(scc) => scc.contains(&id),
				None      => id == fid,
			}
		};

		log::trace!("  > BEGIN building clobber set");
		log::trace!("    callees:");

		for callee in self.cg.callees_of(fid) {
			if !in_scc(callee) {
				let usage = self.usage_of_fid(callee);
				log::trace!("      callee {:?} adds {:?}", callee, usage.changes());
				clobber_set |= usage.changes();
			}

			// early out if all regs are clobbered
			if clobber_set == all_regs {
				break;
			}
		}

		log::trace!("    exitpoints: {:?}", ir.exitpoints());

		if clobber_set != all_regs {
			for irbbid in ir.exitpoints().iter().copied() {
				for reg in ir.get_bb(irbbid).clobber_regs() {
					if !reg.is_gen0() {
						log::trace!("      bb{} adds {:?}", irbbid, reg);
						if clobber_set.insert(reg.offset()) && clobber_set == all_regs {
							// early out if all regs are clobbered
							break;
						}
					}
				}
			}
		}

		log::trace!("  >   END building clobber set");

		log::trace!("    clobber_set = {:?}", clobber_set);
		log::debug!(">   END clobbers  {} {:?}", self.prog.get_func(fid).ea(), fid);
		self.change_clobbers(fid, clobber_set)
	}

	// --------------------------------------------------------------------------------------------
	// argument regs are zero-generation registers which are used at least once.

	fn analyze_args_mutrec(&mut self, scc: &[FuncId]) {
		// since we'll be replacing the clobber sets with dummies in the loop below, this vec holds
		// the actual clobber sets which will be applied after the loop.
		let backup_clobbers: Vec<RegSet> = scc.iter()
			.map(|&fid| self.usage_of_fid(fid).clobbers())
			.collect();
		let mut actual_args = vec![RegSet::EMPTY; scc.len()];

		// assume all all args and clobbers are empty at first.
		for &fid in scc.iter() {
			self.change_args(fid, RegSet::EMPTY);
			self.change_clobbers(fid, RegSet::EMPTY);
		}

		// FIRST ROUND: determine "local" args.
		log::trace!("  > BEGIN mutrec local args");

		for (i, &fid) in scc.iter().enumerate() {
			// run args algo.
			self.analyze_args(fid);

			// get the local args and empty the args back out.
			actual_args[i] = self.usage_of_fid(fid).args();
			self.change_args(fid, RegSet::EMPTY);
		}

		// restore backup clobbers and apply actual arguments
		for ((&fid, clobber_set), arg_set) in scc.iter()
			.zip(backup_clobbers.into_iter())
			.zip(actual_args.into_iter()) {
			self.change_args(fid, arg_set);
			self.change_clobbers(fid, clobber_set);
		}

		log::trace!("  >   END mutrec local args");

		// for loop_iteration in 0 .. {
		// 	log::trace!("  >> mutrec args loop start {}", loop_iteration);
		// 	let mut any_changed = false;

		// 	for &fid in scc.iter() {
		// 		any_changed |= self.analyze_args(fid);
		// 	}

		// 	if !any_changed {
		// 		break;
		// 	} else if loop_iteration > scc.len() {
		// 		panic!("hmmmmmm should have converged by now...");
		// 	}
		// }
	}

	fn analyze_args(&mut self, fid: FuncId) -> bool {
		log::debug!("> BEGIN arguments {} {:?}", self.prog.get_func(fid).ea(), fid);
		let arch = self.prog.arch();
		let all_regs = arch.arch_reg_set();
		let ir = self.prog.func_to_ir_ctx(fid, self);
		log::trace!("{:?}", IrFunctionWithNames(&ir, &arch));

		let defs = ir.find_defs_and_uses();

		let mut arg_set = all_regs;

		for reg in arch.arch_ir_regs() {
			let reg = reg.sub(0);
			match defs.get(&reg) {
				Some(usage) if usage.is_really_used() => {
					log::trace!("      {:?} is an argument to {:?} ({:?})",
						RegDbg(reg, Some(&arch)), fid, usage);
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
		log::debug!(">   END arguments {} {:?}", self.prog.get_func(fid).ea(), fid);
		self.change_args(fid, arg_set)
	}

	// --------------------------------------------------------------------------------------------
	// pass 2: top-down, determine return sets for each function
	fn analyze_returns(&mut self, sccs: &[Vec<FuncId>]) {
		self.is_return_analysis_pass = true;
		for scc in sccs.iter().rev() {
			match scc[..] {
				[] => unreachable!("petgraph::algo::tarjan_scc gave a 0-size SCC???"),
				[fid] => { self.analyze_returns_func(fid); }
				_     => {
					// 2 or more mutually-recursive functions.
					log::trace!("  mutually-recursive SCC: {:?}", scc);

					for i in 0 .. {
						log::trace!("  >> returns loop start {}", i);
						let mut any_changed = false;

						for fid in scc.iter() {
							any_changed |= self.analyze_returns_func(*fid);
						}

						if !any_changed {
							break;
						} else if i > scc.len() {
							panic!("hmmmmmm should have converged by now...");
						}
					}
				}
			}
		}
		self.is_return_analysis_pass = false;
	}

	fn analyze_returns_func(&mut self, fid: FuncId) -> bool {
		log::debug!("> BEGIN returns   {} {:?}", self.prog.get_func(fid).ea(), fid);
		let arch = self.prog.arch();
		let mut ir = self.prog.func_to_ir_ctx(fid, self);

		// 1. find calls with external destinations and internal continuations.

		// maps from callees' FuncIds to the IR BB which contains the `mov _, <return>` instructions
		// after calling it.
		let mut callees_to_bbs: HashMap<FuncId, IrBBId> = HashMap::new();

		for irbbid in 0 .. ir.num_bbs() {
			match ir.get_bb(irbbid).term_inst().kind() {
				IrInstKind::Call {
					dst: IrTarget::External(ea),
					cont: IrTarget::Internal(cont_bb),
					..
				} => {
					// self-calls will not be matched by this, since their dst will be Internal.
					if let Some(callee) = self.eas_to_fids.get(&ea) {
						callees_to_bbs.insert(*callee, cont_bb);
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

		log::trace!("    before DSE:\n{:?}", IrFunctionWithNames(&ir, &arch));

		ir.elim_dead_stores();

		log::trace!("    after DSE:\n{:?}", IrFunctionWithNames(&ir, &arch));

		let mut any_changed = false;

		// any remaining return-use is a *true* return value from a callee and not just a clobber.
		for (callee_fid, irbbid) in callees_to_bbs.into_iter() {
			// TODO: if a function's reg usage has been analyzed before, its *arguments and
			// clobbers* cannot have changed, but its *return* regs may have. In that case... uhhhh
			// what? mark it for state change analysis?

			let mut ret_set = RegSet::new();

			for reg in ir.get_bb(irbbid).dummy_return_use_regs() {
				if ret_set.insert(reg.offset()) {
					log::trace!("    {:?} is a return value from {:?}",
						RegDbg(reg, Some(&arch)), callee_fid);
				}
			}

			any_changed |= self.change_returns(callee_fid, ret_set);
		}

		log::debug!(">   END returns   {} {:?}", self.prog.get_func(fid).ea(), fid);
		any_changed
	}
}