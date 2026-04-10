
use crate::fxhash::{ FxHashMap as HashMap, FxHashMapEx, FxHashSet as HashSet, FxHashSetEx };

use crate::program::{ Program, BBTerm, FuncId, RegSet, FuncRegUsage };
use crate::memory::{ EA } ;
use crate::arch::{ IIrCompiler };
use crate::ir::{ IrFunction, IrBuilder, IrBasicBlock, IrBBId, IrCfg, IrInst, IrSrc,
	IrInstKind, IrTarget, IrReg };

// ------------------------------------------------------------------------------------------------
// Conversion of a function to IR
// ------------------------------------------------------------------------------------------------

impl Program {
	/// Compile a function to IR and return it. **This is (probably) a slow function!**
	///
	/// As of now it's the caller's responsibility to drop the returned `IrFunction` if the
	/// originating function is modified, or else the IR will be out of sync with it. This may
	/// change in the future (e.g. by having `IrFunction` hold a reference to the original
	/// function.)
	pub(super) fn func_to_ir(&self, fid: FuncId) -> IrFunction {
		self.func_to_ir_ctx(fid, self)
	}

	/// Same as above but uses a given context for looking up register usage.
	pub(super) fn func_to_ir_ctx(&self, fid: FuncId, ctx: &impl IRewriteCtx) -> IrFunction {
		// 1. compile BBs (and build a map from BBIds to IrBBIds)
		let compiler = self.plat.arch().new_ir_compiler();
		let func = self.funcs.get(fid);

		if func.is_multi_entry() {
			log::warn!("func_to_ir on multi-entry function {:?} @ {:?}", fid, func.ea());
			// self.func_dump_cfg(&self.func_analyze_cfg(func));
		}

		// log::debug!("func has {} bbs", func.num_bbs());
		// self.func_dump_cfg(&self.func_analyze_cfg(func));
		// for bb in func.all_bbs() {
		// 	let bb = self.bbidx.get(bb);
		// 	log::debug!("terminator for {:?} @ {:?} is {:?}", bb.id(), bb.ea(), bb.term());
		// }

		// the IR BBs, one for each of the original BBs
		let mut bbs = vec![];

		// extra IR BBs, since some original BBs are turned into *two* IR BBs
		let mut extra_bbs = vec![];
		let mut extra_irbbid = func.num_bbs();

		// extra edges which need to be added to the CFG as a result of extra BBs above, but whose
		// EAs cannot be turned into IrBBIds until the first loop is done
		let mut extra_edges: HashSet<(IrBBId, IrBBId)> = HashSet::new();

		// map from BB start EAs to IR basic block IDs. not all IrBBIds will end up in here, because
		// some real BBs get turned into 2 IR BBs, and so only the "first half" of those will be
		// in here; the second half is handled by the "extra_edges" above
		let mut eas_to_bbids: HashMap<EA, IrBBId> = HashMap::new();

		// BBs which have zero in-function successors
		let mut exitpoints: Vec<IrBBId> = vec![];
		// BBs which need to have `use` added before them
		let mut callpoints: Vec<IrBBId> = vec![];
		// BBs which need to have `mov _, <return>` added after them
		let mut returnpoints: Vec<IrBBId> = vec![];

		for (irbbid, bbid) in func.all_bbs().enumerate() {
			let bb = self.get_bb(bbid);
			let bbea = bb.ea();
			eas_to_bbids.insert(bbea, irbbid);

			let mut b = IrBuilder::new(extra_irbbid);

			// SAFETY: BasicBlock::new asserts that insts is non-empty
			let (last, rest) = bb.insts().split_last().unwrap();
			rest.iter().for_each(|inst| {
				b.set_inst(inst);
				compiler.build_ir(&mut b)
			});
			b.set_inst(last);
			compiler.build_ir_term(&mut b, &bb.term);

			match bb.term {
				BBTerm::FallThru { cont } |
				BBTerm::StateChange { cont, .. } => { b.branch(cont); }
				BBTerm::DeadEnd                  => { b.halt(); }
				_                                => {}
			}

			let rewrite_irbbid = match b.finish() {
				(insts, None) => {
					assert!(!insts.is_empty(),
						"no IR instructions emitted for BB {:?} at {:?}", bb.id(), bb.ea());

					irbb_terminator_sanity_check(bb.term(), &insts);
					bbs.push(IrBasicBlock::new(irbbid, bbid, bbea, insts));
					irbbid
				}
				(insts_before, Some(insts_after)) => {
					assert!(!insts_after.is_empty(),
						"no IR instructions emitted after split for BB {:?} at {:?}",
						bb.id(), bb.ea());

					// yes, using insts_after here - because the real BB's terminator is expecting
					// the Call at the end of insts_after
					irbb_terminator_sanity_check(bb.term(), &insts_after);

					bbs.push(IrBasicBlock::new(irbbid, bbid, bbea, insts_before));

					if matches!(bb.term, BBTerm::Call { .. }) {
						// log::debug!("  pushing extra edge from {} to {}", irbbid, extra_irbbid);
						extra_edges.insert((irbbid, extra_irbbid));
					} else {
						// log::debug!("  cond ret, not pushing an extra edge");
					}

					// finally push the second part of the code as an extra bb
					let after_ea = insts_after[0].ea();
					extra_bbs.push(IrBasicBlock::new(extra_irbbid, bbid, after_ea, insts_after));
					extra_irbbid += 1;
					extra_irbbid - 1
				}
			};

			// determine exitpoints and callpoints
			use BBTerm::*;
			match bb.term {
				DeadEnd | Halt => {}
				Return { .. } => {
					exitpoints.push(rewrite_irbbid);
				}
				Call { cont, .. } | IndirCall { cont, .. } => {
					// it doesn't matter whether this is a normal call or tailcall; we are passing
					// arguments to that function.
					callpoints.push(rewrite_irbbid);

					// if the continuation successor is in-function, we need to insert mov-returns
					// after it; otherwise, it's a tailcall, and this BB is an exitpoint, so we need
					// to insert clobbers. the same BB can have *both* clobbers *and* uses inserted,
					// but with different reg sets.
					if self.ea_is_bb_in_function(cont, bb.func()).is_some() {
						returnpoints.push(rewrite_irbbid);
					} else {
						exitpoints.push(rewrite_irbbid);
					}

					// // if cont is an in-function successor, it needs return-insertion
					// // TODO: just func, not bb.func()
					// if self.ea_is_bb_in_function(cont, bb.func()).is_some() {
					// 	callpoints.push(rewrite_irbbid);
					// 	returnpoints.push(rewrite_irbbid);
					// } else if !self.bb_any_successor_in_function(bbid) {
					// 	// if ALL successors are out-of-function, it's an exitpoint.
					// 	// yes, the same BB can be *both* a callpoint *and* an exitpoint!
					// 	exitpoints.push(rewrite_irbbid);
					// }
				}
				Jump { .. } | FallThru { .. } | StateChange { .. } |
				Cond { .. } | IndirJump { .. }  => {
					// if ANY successor is out-of-function, it's a tailcall.
					if !self.bb_all_successors_in_function(bbid) {
						callpoints.push(rewrite_irbbid);
						exitpoints.push(rewrite_irbbid);
					}
				}
			}
		}

		assert_eq!(extra_irbbid, bbs.len() + extra_bbs.len());
		bbs.append(&mut extra_bbs);

		// 2. fix up control flow targets. NOTE: this MUST be done before rewrites, or else that
		// step will not have the right info to choose which registers to use.
		fixup_ir_targets(&mut bbs, &eas_to_bbids);

		// 3. perform rewrites
		// arch_reg_set is the default ("pessimal") sets of argument and return value registers, to
		// be used for functions which have not had their register usage determined yet.
		IrRewriter::new(ctx, func.ea(), &mut bbs)
			.perform_rewrites(ctx, &exitpoints, callpoints, returnpoints);

		// 4. build the CFG from the IrBB terminators
		let cfg = build_ir_cfg(&bbs, extra_edges);
		let entrypoints = func.entrypoints().iter()
			.map(|&bbid| eas_to_bbids[&self.bbidx.get(bbid).ea()])
			.collect();

		// use petgraph::dot::{ Dot, Config as DotConfig };
		// println!("{:?}", Dot::with_config(&cfg, &[DotConfig::EdgeNoLabel]));

		// 5. create the IrFunction (which converts it to SSA)
		IrFunction::new(fid, bbs, cfg, entrypoints, exitpoints)
	}
}

fn fixup_ir_targets(bbs: &mut [IrBasicBlock], eas_to_bbids: &HashMap<EA, IrBBId>) {
	// log::debug!("eas_to_bbids: {:#?}", eas_to_bbids);

	let fixup = |target: &mut IrTarget| {
		if let IrTarget::External(ea) = target {
			if let Some(bbid) = eas_to_bbids.get(ea) {
				// log::debug!("  rewriting external {:?} to internal bb{}", ea, *bbid);
				*target = IrTarget::Internal(*bbid);
			} else {
				// log::debug!("  failed to rewrite external {:?}", ea);

			}
		}
	};

	for bb in bbs.iter_mut() {
		match bb.term_inst_mut().kind_mut() {
			IrInstKind::Branch  { dst, .. }       => { fixup(dst);              }
			IrInstKind::CBranch { dst, cont, .. } => { fixup(dst); fixup(cont); }
			IrInstKind::Call    { dst, cont, .. } => { fixup(dst); fixup(cont); }
			IrInstKind::ICall   { cont, .. }      => { fixup(cont);             }
			_ => {}
		}
	}
}

fn build_ir_cfg(bbs: &[IrBasicBlock], extra_edges: HashSet<(IrBBId, IrBBId)>) -> IrCfg {
	let mut cfg = IrCfg::new();
	cfg.add_node(0); // IrBBId of the function's head BB - 0 by definition

	for (src, dst) in extra_edges.into_iter() {
		cfg.add_edge(src, dst, ());
	}

	let mut edge = |src: IrBBId, dst: IrTarget| {
		if let IrTarget::Internal(dst) = dst {
			cfg.add_edge(src, dst, ());
		}
	};

	for bb in bbs.iter() {
		match bb.term_inst().kind() {
			IrInstKind::Branch  { dst, .. }       => { edge(bb.id, dst);                    }
			IrInstKind::CBranch { dst, cont, .. } => { edge(bb.id, dst); edge(bb.id, cont); }
			IrInstKind::Call    { dst, cont, .. } => { edge(bb.id, dst); edge(bb.id, cont); }
			IrInstKind::ICall   { cont, .. }      => { edge(bb.id, cont);                   }
			IrInstKind::IBranch { .. } |
			IrInstKind::Ret     { .. } |
			IrInstKind::Halt                      => {}
			_ => {
				log::error!("irbb{} does not end in a control flow instruction", bb.id);
				for inst in bb.insts.iter() {
					log::error!("  {:?}", inst);
				}
				panic!();
			}
		}
	}
	cfg
}

/// Does sanity checking on the terminating instruction of an IR BB to ensure the arch IR
/// compiler is implemented correctly (or at least, consistently with the instruction
/// categorization).
///
/// These are the rules. Really it's only checking "if the BBTerm is this, then the IrInstKind
/// must be that," but the first column is to clarify *how* those BBTerms were determined.
///
/// | if an instruction | then analysis used | and the IR compiler should have  |
/// | was categorized   | this terminator... | emitted this kind of instruction |
/// | as a...           |                    | as the IRBB's last one.          |
/// |-------------------|--------------------|----------------------------------|
/// | `InstructionKind` | `BBTerm`           | `IrInstKind`                     |
/// |-------------------|--------------------|----------------------------------|
/// | `Ret`             | `Return`           | `ret`                            |
/// | `Indir`           | `IndirJump`        | `ibranch`                        |
/// | `IndirCall`       | `IndirCall`        | `icall`                          |
/// | `Call(..)`        | `Call`             | `call`                           |
/// | `Uncond`          | `Jump`             | `branch`                         |
/// | `Cond`            | `Cond`             | `cbranch`                        |
/// | `Halt`            | `Halt`             | `halt`                           |
/// | _                 | `DeadEnd`          | `halt` (done by `func_to_ir`)    |
/// | _                 | `FallThru`         | `branch` (done by `func_to_ir`)  |
/// | _                 | `StateChange`      | `branch` (done by `func_to_ir`)  |
///
///
/// Panics if the above check for the appropriate terminating
/// instruction fails.
fn irbb_terminator_sanity_check(term: &BBTerm, insts: &[IrInst]) {
	// safe because caller asserts it's not empty
	let (inst, _) = insts.split_last().unwrap();

	use BBTerm::*;
	match term {
		Halt | DeadEnd => {
			assert!(matches!(inst.kind(), IrInstKind::Halt),
				"for `{:?}`, the terminating instruction should have \
				been `IrInstKind::Halt`, but found this instead: {:?}", term, inst.kind());
		}
		Return { .. } => {
			assert!(matches!(inst.kind(), IrInstKind::Ret { .. }),
				"for `BBTerm::Return`, the terminating instruction should have \
				been `IrInstKind::Ret`, but found this instead: {:?}", inst.kind());
		}
		IndirJump { .. } => {
			assert!(matches!(inst.kind(), IrInstKind::IBranch { .. }),
				"for `BBTerm::Jump`, the terminating instruction should have \
				been `IrInstKind::IBranch`, but found this instead: {:?}", inst.kind());
		}
		IndirCall { .. } => {
			assert!(matches!(inst.kind(), IrInstKind::ICall { .. }),
				"for `BBTerm::Indir`, the terminating instruction should have \
				been `IrInstKind::ICall`, but found this instead: {:?}", inst.kind());
		}
		Call { .. } => {
			assert!(matches!(inst.kind(), IrInstKind::Call { .. }),
				"for `BBTerm::Call {{ cond: false }}`, the terminating instruction should have \
				been `IrInstKind::Call`, but found this instead: {:?}", inst.kind());
		}
		FallThru { .. } |
		StateChange { .. } |
		Jump { .. } => {
			assert!(matches!(inst.kind(), IrInstKind::Branch { .. }),
				"for `{:?}`, the terminating instruction should have \
				been `IrInstKind::Branch`, but found this instead: {:?}", term, inst.kind());
		}
		Cond { .. } => {
			assert!(matches!(inst.kind(), IrInstKind::CBranch { .. }),
				"for `BBTerm::Cond`, the terminating instruction should have \
				been `IrInstKind::CBranch`, but found this instead: {:?}", inst.kind());
		}
	}
}

// ------------------------------------------------------------------------------------------------
// Trait for stuff IrRewriter needs
// ------------------------------------------------------------------------------------------------

pub(crate) trait IRewriteCtx {
	/// Returns
	/// - `None` if `ea` doesn't correspond to a function entrypoint or hasn't been analyzed
	/// - `Some(usage)` if it has
	fn reg_usage_of(&self, ea: EA) -> Option<FuncRegUsage>;

	/// Map from IR register offset to `IrReg`
	fn arch_ir_reg(&self, offset: u8) -> IrReg;

	/// The default set of regs for this arch
	fn default_regs(&self) -> RegSet;

	/// Is the return analysis pass running?
	fn is_return_analysis_pass(&self) -> bool;
}

impl IRewriteCtx for Program {
	fn reg_usage_of(&self, ea: EA) -> Option<FuncRegUsage> {
		if let Some(func) = self.func_that_contains(ea) {
			if let Some(ru) = func.reg_usage() {
				// log::trace!("    callee {:?} usage = {:?}", ea, ru);
				Some(*ru)
			} else {
				// log::trace!("    callee {:?} has no usage", ea);
				None
			}
		} else {
			// log::trace!("    callee {:?} is not a func", ea);
			None
		}
	}

	fn arch_ir_reg(&self, offset: u8) -> IrReg {
		self.arch().arch_ir_reg(offset)
	}

	fn default_regs(&self) -> RegSet {
		self.arch().arch_reg_set()
	}

	fn is_return_analysis_pass(&self) -> bool {
		false
	}
}

// ------------------------------------------------------------------------------------------------
// IrRewriter
// ------------------------------------------------------------------------------------------------

struct IrRewriter<'a> {
	bbs:          &'a mut Vec<IrBasicBlock>,
	new_bbs:      Vec<IrBasicBlock>,
	new_bbid:     IrBBId,
	all_regs:     RegSet,
	arg_regs:     RegSet,
	ret_regs:     RegSet,
	changed_regs: RegSet,
}

impl<'a> IrRewriter<'a> {
	fn new(ctx: &impl IRewriteCtx, ea: EA, bbs: &'a mut Vec<IrBasicBlock>) -> Self {
		let all_regs = ctx.default_regs();
		let reg_usage =
			ctx.reg_usage_of(ea)
			.unwrap_or_else(|| FuncRegUsage::new(all_regs, all_regs));

		Self {
			new_bbs:  vec![],
			new_bbid: bbs.len(),
			bbs,
			arg_regs:     *reg_usage.args(),
			ret_regs:     *reg_usage.rets(),
			changed_regs: reg_usage.changes(),
			all_regs,
		}
	}

	fn perform_rewrites(&mut self, ctx: &impl IRewriteCtx, exitpoints: &[IrBBId],
	callpoints: Vec<IrBBId>, returnpoints: Vec<IrBBId>) {
		// log::trace!("  IrRewriter::perform_rewrites \
		// 	exitpoints = {:?} callpoints = {:?} returnpoints = {:?}",
		// 	exitpoints, callpoints, returnpoints);

		for irbbid in callpoints.into_iter() {
			self.insert_callpoint_uses(ctx, irbbid);
		}

		for &irbbid in exitpoints.iter() {
			self.insert_exitpoint_clobbers(ctx, irbbid);
		}

		// do this last since it modifies the CFG
		for irbbid in returnpoints.into_iter() {
			self.insert_callpoint_return_movs(ctx, irbbid);
		}

		self.bbs.append(&mut self.new_bbs);
	}

	fn insert_callpoint_uses(&mut self, ctx: &impl IRewriteCtx, irbbid: IrBBId) {
		// log::debug!("dummy uses on irbb{}", irbbid);
		let irbb = &mut self.bbs[irbbid];
		let term_inst = *irbb.term_inst();

		// SAFETY: this match is valid because callpoints contains only `call/icall` instructions
		// which are guaranteed to have a target.
		// (fixup_ir_targets is the step before this one, so this match is good.)

		let regs = match term_inst.kind() {
			IrInstKind::Branch  { dst, .. } |
			IrInstKind::CBranch { dst, .. } |
			IrInstKind::Call    { dst, .. } => match dst {
				// recursive call! use our own arg regs.
				IrTarget::Internal(_)  => self.arg_regs,
				IrTarget::External(ea) => {
					// if the callee exists, and *its* argument registers have been analyzed, use
					// those; otherwise use the default ones.
					ctx.reg_usage_of(ea)
					.map(|ru| ru.args().clone())
					.unwrap_or(self.all_regs)
				}
			}

			IrInstKind::ICall { .. } |
			IrInstKind::IBranch { .. } => {
				// TODO: once we know the destinations of these, do the union of all their args
				self.all_regs
			}

			_ => unreachable!("what the hell is callpoints.push() pushing??"),
		};

		// TODO: abstract this out of here and insert_exitpoint_clobbers
		if !regs.is_empty() {
			let ea = term_inst.ea();
			irbb.insts.pop();

			for reg_offs in regs.iter() {
				irbb.insts.push(IrInst::use_(ea, ctx.arch_ir_reg(reg_offs)));
			}

			irbb.insts.push(term_inst);
		}
	}

	fn insert_exitpoint_clobbers(&mut self, ctx: &impl IRewriteCtx, irbbid: IrBBId) {
		// log::debug!("clobbers on irbb{}", irbbid);
		let irbb = &mut self.bbs[irbbid];
		let term_inst = *irbb.term_inst();

		let regs = if ctx.is_return_analysis_pass() {
			// during return analysis, if we insert clobbers here, it'll over-diagnose
			// returns...
			self.ret_regs
		} else {
			// because *all* modified registers (returns *and* clobbers) need to be marked
			// as used or else they'll be marked dead by DSE.
			self.changed_regs
		};

		if !regs.is_empty() {
			let ea = term_inst.ea();
			irbb.insts.pop();

			for reg_offs in regs.iter() {
				irbb.insts.push(IrInst::clobber(ea, ctx.arch_ir_reg(reg_offs)));
			}

			irbb.insts.push(term_inst);
		}
	}

	fn insert_callpoint_return_movs(&mut self, ctx: &impl IRewriteCtx, irbbid: IrBBId) {
		// log::debug!("return movs on irbb{}", irbbid);
		// first update the cfg.
		let (old_cont, callee_changed_regs) = self.change_cont(ctx, irbbid, self.new_bbid);

		// then build the new interstitial BB.
		let mut b = IrBuilder::new(0); // never using cbranch_and_split, so whatev
		let bb = &self.bbs[irbbid];
		let last_ea = bb.term_inst().ea();
		b.set_ea(last_ea);

		for reg_offs in callee_changed_regs.iter() {
			let reg = ctx.arch_ir_reg(reg_offs);
			b.mov(reg, IrSrc::Return(reg.size()));
		}

		b.branch(EA::unresolved(0));
		let mut insts = b.finish_one();
		// SAFETY: above b.branch ensures at least 1 inst in insts
		*insts.last_mut().unwrap().kind_mut() = IrInstKind::Branch { dst: old_cont, dstn: -1 };

		let new_bb = IrBasicBlock::new(self.new_bbid, bb.real_bbid, last_ea, insts);
		self.new_bbid += 1;
		self.new_bbs.push(new_bb);
	}

	/// change the continuation target of the terminating call instruction of `irbbid` to
	/// `new_cont`. returns the old continuation target and the return `RegSet` of the call target,
	/// or the default reg set if the target is not yet analyzed/is indirect.
	fn change_cont(&mut self, ctx: &impl IRewriteCtx, irbbid: IrBBId, new_cont: IrBBId)
	-> (IrTarget, RegSet) {
		let bb = &mut self.bbs[irbbid];

		match bb.term_inst_mut().kind_mut() {
			IrInstKind::Call { dst, cont, .. } => {
				let regs = match dst {
					// recursive call! use our own ret regs.
					// this *is* different compared to non-recursive calls, because if we used
					// the changed regs which haven't been computed yet, it would be misanalyzed
					// as changing *all* regs.
					//
					// but on subsequent reg usage passes, this will correctly insert return-uses
					// for the return registers.
					IrTarget::Internal(_)  => {
						// log::trace!("  callee is self, using {:?}", self.ret_regs);
						self.ret_regs
					}
					IrTarget::External(ea) => {
						ctx.reg_usage_of(*ea)
						.map(|ru| ru.changes())
						.unwrap_or(self.all_regs)
					}
				};

				let old_cont = *cont;
				*cont = IrTarget::Internal(new_cont);
				(old_cont, regs)
			}
			IrInstKind::ICall { cont, .. } => {
				let old_cont = *cont;
				*cont = IrTarget::Internal(new_cont);
				// can't know target, so have to return default regset
				(old_cont, self.all_regs)
			}
			_ => panic!("bb{} marked for return-insertion should have ended with `Call` or `ICall` \
				but ended with {:?} instead", irbbid, bb.term_inst().kind()),
		}
	}
}