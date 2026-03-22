
use std::collections::{ HashMap };

use crate::program::{ Program, BBTerm, FuncId };
use crate::memory::{ EA } ;
use crate::arch::{ IArchitecture, IIrCompiler };
use crate::platform::{ IPlatform };
use crate::ir::{ IrFunction, IrBuilder, IrBasicBlock, IrBBId, IrCfg, IrInst, IrReg, IrSrc,
	IrInstKind, debug_dump_ir_cfg_and_bbs };

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
		// 1. compile BBs (and build a map from BBIds to IrBBIds)
		let compiler = self.plat.arch().new_ir_compiler();
		let func = self.funcs.get(fid);

		if func.is_multi_entry() {
			log::warn!("func_to_ir on multi-entry function");
			let cfg = self.func_analyze_cfg(func);
			self.func_dump_cfg(&cfg);
		}

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
			compiler.build_ir(last, Some(bb.term()), &mut b);

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
						BBTerm::Call { cont, .. }           => (cont, true),
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
						let before_last = bb.term.has_explicit_successors();
						rewrites.push((rewrite_irbbid, IrRewrite::Uses { before_last }));
					}
				}
			}

			// if cont is an in-function successor, it needs return-insertion
			if let Call { cont, .. } | IndirCall { cont, .. } = bb.term &&
			self.ea_is_bb_in_function(cont, bb.func()).is_some() {
				rewrites.push((rewrite_irbbid, IrRewrite::Returns));
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

		bbs.append(&mut extra_bbs);
		// 3. perform rewrites
		IrRewriter::new(&compiler, &mut bbs, &mut cfg).perform_rewrites(rewrites);

		// 4. create the IrFunction
		IrFunction::new(fid, bbs, cfg)
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
/// | `Ret`             | `Return`           | `ret`                            |
/// | `Indir`           | `IndirJump`        | `ibranch`                        |
/// | `IndirCall`       | `IndirCall`        | `icall`                          |
/// | `Call(..)`        | `Call`             | `call`                           |
/// | `Uncond`          | `Jump`             | `branch`                         |
/// | `Cond`            | `Cond`             | `cbranch`                        |
/// | `Halt`            | `Halt`             | non-control-flow is allowed\*    |
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
		FallThru { .. } | Halt | DeadEnd => {
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
		Jump { .. } => {
			assert!(matches!(inst.kind(), IrInstKind::Branch { .. }),
				"for `BBTerm::Jump`, the terminating instruction should have \
				been `IrInstKind::Branch`, but found this instead: {:?}", inst.kind());
		}
		Cond { .. } => {
			assert!(matches!(inst.kind(), IrInstKind::CBranch { .. }),
				"for `BBTerm::Cond`, the terminating instruction should have \
				been `IrInstKind::CBranch`, but found this instead: {:?}", inst.kind());
		}
		StateChange { .. } => {
			assert!(matches!(inst.kind(), IrInstKind::Load { .. } | IrInstKind::Store { .. }),
				"for `BBTerm::StateChange`, the terminating instruction should have \
				been `IrInstKind::Load` or `IrInstKind::Store`, but found this instead: {:?}",
				inst.kind());
		}
	}
}

// ------------------------------------------------------------------------------------------------
// IrRewrite
// ------------------------------------------------------------------------------------------------

#[derive(Debug, Copy, Clone)]
pub(crate) enum IrRewrite {
	Uses { before_last: bool },
	Returns,
}

// ------------------------------------------------------------------------------------------------
// IrRewriter
// ------------------------------------------------------------------------------------------------

struct IrRewriter<'a, C: IIrCompiler> {
	compiler: &'a C,
	bbs:      &'a mut Vec<IrBasicBlock>,
	cfg:      &'a mut IrCfg,
	new_bbs:  Vec<IrBasicBlock>,
	new_bbid: IrBBId,
}

impl<'a, C: IIrCompiler> IrRewriter<'a, C> {
	fn new(compiler: &'a C, bbs: &'a mut Vec<IrBasicBlock>, cfg: &'a mut IrCfg) -> Self {
		Self {
			new_bbs:  vec![],
			new_bbid: bbs.len(),
			compiler,
			bbs,
			cfg,
		}
	}

	fn perform_rewrites(&mut self, rewrites: Vec<(IrBBId, IrRewrite)>) {
		let arg_regs = self.compiler.arg_regs();
		let ret_regs = self.compiler.return_regs();

		// log::debug!("-------------BEFORE REWRITE----------------");
		// debug_dump_ir_cfg_and_bbs(self.cfg, self.bbs);

		// first pass: insert uses
		for (irbbid, rewrite) in rewrites.iter() {
			if let IrRewrite::Uses { before_last } = rewrite {
				insert_dummy_uses(&mut self.bbs[*irbbid], arg_regs, ret_regs, *before_last);
			}
		}

		// log::debug!("-------------AFTER USE-INSERTION----------------");
		// debug_dump_ir_cfg_and_bbs(self.cfg, self.bbs);

		// second pass: insert dummy BBs for return-uses after calls
		for (irbbid, rewrite) in rewrites.into_iter() {
			match rewrite {
				IrRewrite::Uses { .. } => {} // already handled
				IrRewrite::Returns => {
					self.rewrite_returns(irbbid, ret_regs);
				}
			}
		}

		self.bbs.append(&mut self.new_bbs);

		// log::debug!("-------------AFTER REWRITE----------------");
		// debug_dump_ir_cfg_and_bbs(self.cfg, self.bbs);
	}

	fn rewrite_returns(&mut self, irbbid: IrBBId, ret_regs: &[IrReg]) {
		let bb = &self.bbs[irbbid];

		// first update the cfg.
		// println!("{}: {:?}", bb.id, self.cfg.edges(bb.id).map(|(_, n, _)|n).collect::<Vec<_>>());

		let old_dest = self.get_old_dest(bb);

		log::debug!("  changing bb{}'s dest from bb{} to bb{}", bb.id, old_dest, self.new_bbid);

		assert!(self.cfg.remove_edge(bb.id, old_dest).is_some());
		self.cfg.add_edge(bb.id, self.new_bbid, ());
		self.cfg.add_edge(self.new_bbid, old_dest, ());

		let mut b = IrBuilder::new();
		// SAFETY: rewrite_call_or_ret ensures it has at least 1 inst.
		b.set_ea(bb.insts.last().unwrap().ea());

		for &reg in ret_regs.iter() {
			b.mov(reg, IrSrc::Return(reg.size()), -1, -1);
		}

		let real_bbid = bb.real_bbid;
		let new_bb = IrBasicBlock::new(self.new_bbid, real_bbid, b.finish_one());
		self.new_bbid += 1;
		self.new_bbs.push(new_bb);
	}

	fn get_old_dest(&self, bb: &IrBasicBlock) -> usize {
		let targets = self.cfg.edges(bb.id).map(|(_, n, _)|n).collect::<Vec<_>>();
		match targets[..] {
			[] => {
				log::error!("offending function:");
				debug_dump_ir_cfg_and_bbs(self.cfg, self.bbs);
				panic!("IrRewrite::Returns put on bb{} with no in-function successor. \
					See function above", bb.id);
			}
			[target]           => target, // ok cool beans
			[target1, target2] => {
				// this case can happen if a function is recursive, which is okay. but any
				// NON-recursive call would be an error.
				if target1 == 0 {
					target2
				} else if target2 == 0 {
					target1
				} else {
					log::error!("offending function:");
					debug_dump_ir_cfg_and_bbs(self.cfg, self.bbs);

					panic!(
						"IrRewrite::Returns put on bb{} @ {} where one of the call targets \
						({}, {}) is self-call but NOT a recursive call.\n\
						See function above. Why hasn't this function been split?",
						bb.id,
						bb.insts[0].ea(),
						target1, target2);
				}
			}
			_ => panic!("UHHHHHHHHHHHHHHHH TOO MANY EDGES"),
		}
	}
}

fn insert_dummy_uses(irbb: &mut IrBasicBlock, arg_regs: &[IrReg], ret_regs: &[IrReg],
before_last: bool) {
	// SAFETY: `func_to_ir` checks that every IR BB has at least 1 instruction.
	let (terminating_inst, _) = irbb.insts.split_last().unwrap();
	let terminating_inst = *terminating_inst;

	// the match is also valid because irbb_terminator_sanity_check ensured that any BB that
	// ends in IrInstKind::Ret really did come from a BB with BBTerm::Ret.
	let regs = match terminating_inst.kind() {
		IrInstKind::Ret { .. } => ret_regs,
		_                      => arg_regs,
	};

	if !regs.is_empty() {
		let ea = terminating_inst.ea();

		if before_last {
			irbb.insts.pop();
		}

		for &reg in regs.iter() {
			irbb.insts.push(IrInst::use_(ea, reg));
		}

		if before_last {
			irbb.insts.push(terminating_inst);
		}
	}
}