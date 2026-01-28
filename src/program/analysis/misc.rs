
use crate::program::{ Program, BBId, BBTerm, FuncId };
use crate::memory::{ MmuState, EA, SpanKind, VA };

use std::collections::{ HashMap };

use crate::arch::{ IArchitecture, IIrCompiler };
use crate::platform::{ IPlatform };
use crate::ir::{ IrFunction, IrBuilder, IrBasicBlock, IrCfg };

// ------------------------------------------------------------------------------------------------
// Misc analysis helper functions
// ------------------------------------------------------------------------------------------------

impl Program {
	pub(super) fn check_split_bb(&mut self, old_id: BBId, start: EA, owner: Option<FuncId>)
	-> Option<BBId> {
		let old_bb = self.bbidx.get(old_id);

		if start != old_bb.ea {
			// ooh, now we have to split the existing bb.
			// first, let's make sure that `start` points to the beginning of an instruction,
			// because otherwise we'd be jumping to an invalid EA.
			let idx = match old_bb.last_instr_before(start) {
				Some(idx) => idx,
				None      => {
					log::warn!("splitting bb at {} failed", old_bb.ea);
					// todo!("have to flag referrer as being invalid somehow"),
					return None;
				}
			};

			// now we can split the existing BB...
			let new_bb = self.split_bb(old_id, idx, start);

			// ...and update the span map.
			let span_kind = if let Some(fid) = owner {
				// (oh and, fill in the owner)
				self.bbidx.get_mut(new_bb).mark_complete(fid);
				SpanKind::Code(new_bb)
			} else {
				SpanKind::AnaCode(new_bb)
			};

			self.segment_from_ea_mut(start).split_span(start, span_kind);

			Some(new_bb)
		} else {
			None
		}
	}

	// returns id of newly split-off BB
	pub(super) fn split_bb(&mut self, old_id: BBId, inst_idx: usize, new_start: EA) -> BBId {
		let old = self.bbidx.get_mut(old_id);
		let term_ea = old.insts[inst_idx].ea();
		let state = old.mmu_state();
		let insts = old.insts.split_off(inst_idx + 1);

		assert!(old.ea < new_start);
		assert!(term_ea < new_start);

		let new_id = self.bbidx.new_bb(
			new_start,
			BBTerm::FallThru(new_start), // NOT WRONG, they get swapped below.
			insts,
			state
		);

		let (old, new) = self.bbidx.get2_mut(old_id, new_id);
		std::mem::swap(&mut old.term, &mut new.term);

		log::trace!("split bb new id: {:?} ea: {}, term: {:?}", new_id, new.ea, new.term);
		new_id
	}

	pub(super) fn resolve_target(&self, state: MmuState, target: VA) -> EA {
		match self.ea_for_va(state, target) {
			Some(l) => l,
			None    => EA::invalid(target.0),
		}
	}

	pub(super) fn func_to_ir(&self, fid: FuncId) -> IrFunction {
		// 1. compile BBs (and build a map from BBIds to IrBBIds)
		let compiler = self.plat.arch().new_ir_compiler();
		let func = self.funcs.get(fid);
		let mut bbs = vec![];
		let mut bbid_to_irbbid = HashMap::new();

		for (irbbid, bbid) in func.all_bbs().enumerate() {
			let bb = self.get_bb(bbid);
			let mut b = IrBuilder::new();

			// unwrap is ok here because BasicBlock::new asserts that insts is non-empty
			let (last, rest) = bb.insts().split_last().unwrap();
			rest.iter().for_each(|inst|
				compiler.to_ir(inst, None, &mut b));
			compiler.to_ir(last, bb.control_target(), &mut b);

			// TODO: uhhhhh if the terminator is NOT a control flow inst, the IR BB doesn't actually
			// end with a terminator. is that an issue? the IR CFG encodes this info already...

			let insts = b.finish();
			bbs.push(IrBasicBlock::new(irbbid, bbid, insts));
			bbid_to_irbbid.insert(bbid, irbbid);
		}

		// 2. create the IrFunction
		// TODO: this code is very similar to `func_begin_analysis` but has the additional
		// complication of mapping from bbid -> irbbid so it's not a straightforward copy
		let mut cfg = IrCfg::new();

		cfg.add_node(bbid_to_irbbid[&func.head_id()]);

		for bbid in func.all_bbs() {
			let irbbid = bbid_to_irbbid[&bbid];

			self.bb_successors_in_function(bbid, |succ| {
				cfg.add_edge(irbbid, bbid_to_irbbid[&succ], ());
			});
		}

		IrFunction::new(&compiler, fid, bbs, cfg)
	}
}