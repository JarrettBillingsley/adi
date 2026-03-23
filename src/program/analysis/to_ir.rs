
use std::collections::{ HashMap, HashSet };

use crate::program::{ Program, BBTerm, FuncId };
use crate::memory::{ EA } ;
use crate::arch::{ IArchitecture, IIrCompiler };
use crate::platform::{ IPlatform };
use crate::ir::{ IrFunction, IrBuilder, IrBasicBlock, IrBBId, IrCfg, IrInst, IrReg, IrSrc,
	IrInstKind, IrTarget };

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
			self.func_dump_cfg(&self.func_analyze_cfg(func));
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

		// rewrites that need to be done on the IR after it's been built
		let mut rewrites: Vec<(IrBBId, IrRewrite)> = vec![];

		for (irbbid, bbid) in func.all_bbs().enumerate() {
			let bb = self.get_bb(bbid);
			let bbea = bb.ea();
			eas_to_bbids.insert(bbea, irbbid);

			let mut b = IrBuilder::new(extra_irbbid);

			// SAFETY: BasicBlock::new asserts that insts is non-empty
			let (last, rest) = bb.insts().split_last().unwrap();
			rest.iter().for_each(|inst|
				compiler.build_ir(inst, &mut b));
			compiler.build_ir_term(last, &bb.term, &mut b);

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

			// determine what kind of use-insertion is needed, if any.
			use BBTerm::*;
			match bb.term {
				// never insert
				DeadEnd | Halt => {}
				// always insert, before final
				Return { .. } => {
					// before, ret regs
					rewrites.push((rewrite_irbbid, IrRewrite::Uses));
				}
				_ => {
					// only insert uses if there is *at least one* out-of-function successor.
					if !self.bb_all_successors_in_function(bbid) {
						rewrites.push((rewrite_irbbid, IrRewrite::Uses));
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
		bbs.append(&mut extra_bbs);

		// 2. fix up control flow targets
		fixup_ir_targets(&mut bbs, &eas_to_bbids);

		// 3. perform rewrites
		IrRewriter::new(&compiler, &mut bbs).perform_rewrites(rewrites);

		// 4. build the CFG from the IrBB terminators
		let cfg = build_ir_cfg(&bbs, extra_edges);
		let entrypoints = func.entrypoints().iter()
			.map(|&bbid| eas_to_bbids[&self.bbidx.get(bbid).ea()])
			.collect();

		// use petgraph::dot::{ Dot, Config as DotConfig };
		// println!("{:?}", Dot::with_config(&cfg, &[DotConfig::EdgeNoLabel]));

		// 5. create the IrFunction (which converts it to SSA)
		IrFunction::new(fid, bbs, cfg, entrypoints)
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
	// safe because code above asserts it's not empty
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
// IrRewrite
// ------------------------------------------------------------------------------------------------

#[derive(Debug, Copy, Clone)]
pub(crate) enum IrRewrite {
	Uses,
	Returns,
}

// ------------------------------------------------------------------------------------------------
// IrRewriter
// ------------------------------------------------------------------------------------------------

struct IrRewriter<'a, C: IIrCompiler> {
	compiler:     &'a C,
	bbs:          &'a mut Vec<IrBasicBlock>,
	new_bbs:      Vec<IrBasicBlock>,
	new_bbid:     IrBBId,
}

impl<'a, C: IIrCompiler> IrRewriter<'a, C> {
	fn new(compiler: &'a C, bbs: &'a mut Vec<IrBasicBlock>) -> Self {
		Self {
			new_bbs:  vec![],
			new_bbid: bbs.len(),
			compiler,
			bbs,
		}
	}

	fn perform_rewrites(&mut self, rewrites: Vec<(IrBBId, IrRewrite)>) {
		let arg_regs = self.compiler.arg_regs();
		let ret_regs = self.compiler.return_regs();

		// first pass: insert uses
		for (irbbid, rewrite) in rewrites.iter() {
			if let IrRewrite::Uses = rewrite {
				insert_dummy_uses(&mut self.bbs[*irbbid], arg_regs, ret_regs);
			}
		}

		// second pass: insert dummy BBs for return-uses after calls
		for (irbbid, rewrite) in rewrites.into_iter() {
			match rewrite {
				IrRewrite::Uses => {} // already handled
				IrRewrite::Returns => {
					self.rewrite_returns(irbbid, ret_regs);
				}
			}
		}

		self.bbs.append(&mut self.new_bbs);
	}

	fn rewrite_returns(&mut self, irbbid: IrBBId, ret_regs: &[IrReg]) {
		// log::debug!("returns on irbb{}", irbbid);
		// first update the cfg.
		let old_cont = self.change_cont(irbbid, self.new_bbid);

		// then build the new interstitial BB.
		let mut b = IrBuilder::new(0); // never using cbranch_and_split, so whatev
		let bb = &self.bbs[irbbid];
		let last_ea = bb.term_inst().ea();
		b.set_ea(last_ea);

		for &reg in ret_regs.iter() {
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

	fn change_cont(&mut self, irbbid: IrBBId, new_cont: IrBBId) -> IrTarget {
		let bb = &mut self.bbs[irbbid];

		match bb.term_inst_mut().kind_mut() {
			IrInstKind::Call { cont, .. } |
			IrInstKind::ICall { cont, .. } => {
				let old_cont = *cont;
				*cont = IrTarget::Internal(new_cont);
				old_cont
			}
			_ => panic!("IrRewrite::Returns put on bb{} which ended with something other than \
					`Call` or `ICall`.", irbbid),
		}
	}
}

fn insert_dummy_uses(irbb: &mut IrBasicBlock, arg_regs: &[IrReg], ret_regs: &[IrReg]) {
	// log::debug!("dummy uses on irbb{}", irbb.id);
	// SAFETY: `func_to_ir` checks that every IR BB has at least 1 instruction.
	let term_inst = *irbb.term_inst();

	// the match is also valid because irbb_terminator_sanity_check ensured that any BB that
	// ends in IrInstKind::Ret really did come from a BB with BBTerm::Ret.
	let regs = match term_inst.kind() {
		IrInstKind::Ret { .. } => ret_regs,
		_                      => arg_regs,
	};

	if !regs.is_empty() {
		let ea = term_inst.ea();

		irbb.insts.pop();

		for &reg in regs.iter() {
			irbb.insts.push(IrInst::use_(ea, reg));
		}

		irbb.insts.push(term_inst);
	}
}