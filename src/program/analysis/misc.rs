
use std::collections::{ HashMap };

use log::*;

use crate::program::{ Program, BBId, BBTerm, FuncId };
use crate::memory::{ MmuState, EA, SpanKind, VA };
use crate::arch::{ IArchitecture, IIrCompiler };
use crate::platform::{ IPlatform };
use crate::ir::{ IrFunction, IrBuilder, IrRewrite, IrBasicBlock, IrBBId, IrCfg, IrInst,
	IrInstKind };

// ------------------------------------------------------------------------------------------------
// Misc analysis helper functions
// ------------------------------------------------------------------------------------------------

impl Program {
	/// Split a basic block `old_bbid` at address `start`. All instructions from `start` onward
	/// become part of a new BB, and the old BB's terminator is set to fall through to the new BB.
	///
	/// Returns:
	/// - `Ok(Some(new_bbid))` if the split succeeded; `new_bbid` is the ID of the newly-split-off
	///   BB. **NOTE:** it is the *caller's* responsibility to add this to the function's BB list!
	/// - `Ok(None)` if `start` points to the beginning of the old BB. No splitting happened, but
	///   it's a harmless no-op.
	/// - `Err(())` if `start` points into the middle of an instruction.
	pub(super) fn split_bb(&mut self, old_bbid: BBId, start: EA, owner: Option<FuncId>)
	-> Result<Option<BBId>, ()> {
		let old_bb = self.bbidx.get(old_bbid);

		// if the start address is the beginning of the BB, there's nothing to do, but it's harmless
		// to call this function in this case.
		if start == old_bb.ea {
			return Ok(None);
		}

		// now we have to split the existing bb. first, let's make sure that `start` points to the
		// beginning of an instruction, because otherwise we'd be jumping to an invalid address.
		let idx = match old_bb.last_instr_before(start) {
			Some(idx) => idx,
			None => {
				warn!("splitting bb at {} failed", old_bb.ea);
				return Err(());
			}
		};

		// now we can split the existing BB...
		let new_bbid = self.split_bb_worker(old_bbid, idx, start);

		// ...fill in the owner...
		let span_kind = match owner {
			Some(fid) => {
				self.bbidx.get_mut(new_bbid).mark_complete(fid);
				SpanKind::Code(new_bbid)
			}
			None => SpanKind::AnaCode(new_bbid)
		};

		// ...and update the span map.
		self.segment_from_ea_mut(start).split_span(start, span_kind);

		Ok(Some(new_bbid))
	}

	// returns id of newly split-off BB.
	fn split_bb_worker(&mut self, old_bbid: BBId, inst_idx: usize, new_start: EA) -> BBId {
		let old = self.bbidx.get_mut(old_bbid);
		let term_ea = old.insts[inst_idx].ea();
		let state = old.mmu_state();
		let insts = old.insts.split_off(inst_idx + 1);

		assert!(old.ea < new_start);
		assert!(term_ea < new_start);

		let new_bbid = self.bbidx.new_bb(
			new_start,
			BBTerm::FallThru(new_start), // NOT WRONG, they get swapped below.
			insts,
			state
		);

		let (old, new) = self.bbidx.get2_mut(old_bbid, new_bbid);
		std::mem::swap(&mut old.term, &mut new.term);

		trace!("  split bb new id: {:?} ea: {}, term: {:?}", new_bbid, new.ea, new.term);
		new_bbid
	}

	/// Given an MMU state and a target VA, return either the valid EA for it; or an unresolved EA
	/// with the target VA as its offset.
	pub(super) fn resolve_target(&self, state: MmuState, target: VA) -> EA {
		match self.ea_for_va(state, target) {
			Some(l) => l,
			None    => EA::unresolved(target.0),
		}
	}

	/// Compile a function to IR and return it. **This is (probably) a slow function!**
	///
	/// As of now it's the caller's responsibility to drop the returned `IrFunction` if the
	/// originating function is modified, or else the IR will be out of sync with it. This may
	/// change in the future (e.g. by having `IrFunction` hold a reference to the original
	/// function.)
	pub(super) fn func_to_ir(&self, fid: FuncId) -> IrFunction {
		// 1. compile BBs (and build a map from BBIds to IrBBIds)
		let compiler = self.plat.arch().new_ir_compiler();
		let func = self.funcs.get(fid);

		// the IR BBs, one for each of the original BBs
		let mut bbs = vec![];

		// extra IR BBs, since some original BBs are turned into *two* IR BBs
		let mut extra_bbs = vec![];
		let mut extra_irbbid = func.num_bbs();

		// extra edges which need to be added to the CFG as a result of extra BBs above, but whose
		// EAs cannot be turned into IrBBIds until the first loop is done
		let mut extra_edges: HashMap<IrBBId, EA> = HashMap::new();

		// maps from the original BBId to its IrBBId
		let mut bbid_to_irbbid = HashMap::new();

		// rewrites that need to be done by IrFunction::new()
		let mut rewrites: Vec<(IrBBId, IrRewrite)> = vec![];

		// the IR CFG
		let mut cfg = IrCfg::new();
		cfg.add_node(0); // IrBBId of the function's head BB - 0 by definition

		for (irbbid, bbid) in func.all_bbs().enumerate() {
			let bb = self.get_bb(bbid);
			let mut b = IrBuilder::new();

			// SAFETY: BasicBlock::new asserts that insts is non-empty
			let (last, rest) = bb.insts().split_last().unwrap();
			rest.iter().for_each(|inst|
				compiler.build_ir(inst, None, &mut b));
			compiler.build_ir(last, bb.control_target(), &mut b);

			// TODO: uhhhhh if the terminator is NOT a control flow inst, the IR BB doesn't actually
			// end with a terminator. is that an issue? the IR CFG encodes this info already...

			let rewrite_irbbid = match b.finish() {
				(insts, None) => {
					assert!(!insts.is_empty(),
						"no IR instructions emitted for BB {:?} at {:?}", bb.id(), bb.ea());

					irbb_terminator_sanity_check(bb.term(), &insts);
					bbs.push(IrBasicBlock::new(irbbid, bbid, insts));
					bbid_to_irbbid.insert(bbid, irbbid);
					irbbid
				}
				(insts_before, Some(insts_after)) => {
					// cbranch_and_split guarantees insts_before is not empty
					assert!(!insts_after.is_empty(),
						"no IR instructions emitted after split for BB {:?} at {:?}",
						bb.id(), bb.ea());

					// yes, using insts_after here - because the real BB's terminator is expecting
					// the Call at the end of insts_after
					irbb_terminator_sanity_check(bb.term(), &insts_after);

					let (next_ea, is_call) = match bb.term {
						BBTerm::Call { ret, .. }            => (ret, true),
						BBTerm::Return { cont: Some(cont) } => (cont, false),
						_                                   => panic!("impastabowl"),
					};

					// push the first half as the "true" BB so other BBs will properly refer to it
					// when using bbid_to_irbbid in the loop after this one.
					//
					// in the loop below, this will also add an edge from the first half to the
					// next BB (the one at `next_ea`), which we do actually want.
					bbs.push(IrBasicBlock::new(irbbid, bbid, insts_before));
					bbid_to_irbbid.insert(bbid, irbbid);

					// add a new edge from the first half to the second
					cfg.add_edge(irbbid, extra_irbbid, ());

					if is_call {
						// and push an extra edge from the second to the next_ea EA
						extra_edges.insert(extra_irbbid, next_ea);
					}

					// finally push the second part of the code as an extra bb
					extra_bbs.push(IrBasicBlock::new(extra_irbbid, bbid, insts_after));
					extra_irbbid += 1;
					extra_irbbid - 1
				}
			};

			// determine what kind of use-insertion is needed, if any.
			use BBTerm::*;
			match bb.term {
				// never insert
				DeadEnd | Halt => {}
				// always insert, before final
				Return { .. } => {
					// before, ret regs
					rewrites.push((rewrite_irbbid, IrRewrite::Uses { before_last: true }));
				}
				_ => {
					// only insert uses if there is *at least one* out-of-function successor.
					if !self.bb_all_successors_in_function(bbid) {
						let before_last = match bb.term {
							Jump(..) | JumpTbl(..) | Cond{..} | Call{..} | IndirCall{..} => true,
							FallThru(..) | StateChange(..)                               => false,
							_ => unreachable!()
						};

						rewrites.push((rewrite_irbbid, IrRewrite::Uses { before_last }));
					}
				}
			}

			// determine if return-insertion is needed
			if let Call { ret, .. } | IndirCall { ret, .. } = bb.term {
				// if ret is an in-function successor, it needs return-insertion
				if self.ea_is_bb_in_function(ret, bb.func()).is_some() {
					rewrites.push((rewrite_irbbid, IrRewrite::Returns));
				}
			}
		}

		assert_eq!(extra_irbbid, bbs.len() + extra_bbs.len());

		// 2. finish making the CFG
		for bbid in func.all_bbs() {
			let irbbid = bbid_to_irbbid[&bbid];

			self.bb_successors_in_function(bbid, |succ| {
				cfg.add_edge(irbbid, bbid_to_irbbid[&succ], ());
			});
		}

		for (irbbid, ea) in extra_edges.into_iter() {
			if let Some(succ) = self.ea_is_bb_in_function(ea, fid) {
				cfg.add_edge(irbbid, bbid_to_irbbid[&succ], ());
			}
		}

		// 3. create the IrFunction
		bbs.append(&mut extra_bbs);
		IrFunction::new(&compiler, rewrites, fid, bbs, cfg)
	}
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
/// | `Ret`             | `Return`           | `Ret`                            |
/// | `Halt`            | `Halt`             | non-control-flow is allowed\*    |
/// | `Indir`           | `JumpTbl`          | `IBranch`                        |
/// | `IndirCall`       | `IndirCall`        | `ICall`                          |
/// | `Call(..)`        | `Call`             | `Call`                           |
/// | `Uncond`          | `Jump`             | `Branch`                         |
/// | `Cond`            | `Cond`             | `CBranch`                        |
/// | _                 | `DeadEnd`          | non-control-flow is allowed\*    |
/// | _                 | `FallThru`         | non-control-flow is allowed      |
/// | _                 | `StateChange`      | `Load` or `Store`                |
///
/// Other notes:
///
/// - Currently only loads or stores are checked for MMU state changes. This will probably change
///   in the future, once "state change functions" are implemented, in which case call instructions
///   will be checked as well; but beyond that, I doubt any more will be.
/// - For `BBTerm::Halt` and `BBTerm::DeadEnd`, currently any non-control flow instruction is
///   allowed, but that may change in the future (if `IrInstKind` gains some halt or dead-end
///   instructions).
///
/// Panics if the above check for the appropriate terminating
/// instruction fails.
fn irbb_terminator_sanity_check(term: &BBTerm, insts: &[IrInst]) {
	// safe because code above asserts it's not empty
	let (inst, _) = insts.split_last().unwrap();

	use BBTerm::*;
	match term {
		FallThru(..) | Halt | DeadEnd => {
			match inst.kind() {
				IrInstKind::Ret { .. }
				| IrInstKind::IBranch { .. }
				| IrInstKind::ICall { .. }
				| IrInstKind::Call { .. }
				| IrInstKind::Branch { .. }
				| IrInstKind::CBranch { .. } => {
					panic!("for `{:?}`, the terminating instruction should not have been a \
						control flow instruction, but found this: {:?}", term, inst.kind());
				}

				_ => {} // yay
			}
		}
		Return { .. } => {
			assert!(matches!(inst.kind(), IrInstKind::Ret { .. }),
				"for `BBTerm::Return`, the terminating instruction should have \
				been `IrInstKind::Ret`, but found this instead: {:?}", inst.kind());
		}
		JumpTbl(..) => {
			assert!(matches!(inst.kind(), IrInstKind::IBranch { .. }),
				"for `BBTerm::Jump`, the terminating instruction should have \
				been `IrInstKind::IBranch`, but found this instead: {:?}", inst.kind());
		}
		IndirCall {..} => {
			assert!(matches!(inst.kind(), IrInstKind::ICall { .. }),
				"for `BBTerm::Indir`, the terminating instruction should have \
				been `IrInstKind::ICall`, but found this instead: {:?}", inst.kind());
		}
		Call { .. } => {
			assert!(matches!(inst.kind(), IrInstKind::Call { .. }),
				"for `BBTerm::Call {{ cond: false }}`, the terminating instruction should have \
				been `IrInstKind::Call`, but found this instead: {:?}", inst.kind());
		}
		Jump(..) => {
			assert!(matches!(inst.kind(), IrInstKind::Branch { .. }),
				"for `BBTerm::Jump`, the terminating instruction should have \
				been `IrInstKind::Branch`, but found this instead: {:?}", inst.kind());
		}
		Cond {..} => {
			assert!(matches!(inst.kind(), IrInstKind::CBranch { .. }),
				"for `BBTerm::Cond`, the terminating instruction should have \
				been `IrInstKind::CBranch`, but found this instead: {:?}", inst.kind());
		}
		StateChange(..) => {
			assert!(matches!(inst.kind(), IrInstKind::Load { .. } | IrInstKind::Store { .. }),
				"for `BBTerm::StateChange`, the terminating instruction should have \
				been `IrInstKind::Load` or `IrInstKind::Store`, but found this instead: {:?}",
				inst.kind());
		}
	}
}