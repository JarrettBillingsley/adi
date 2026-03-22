
use log::*;

use crate::program::{ Program, BBId, BBTerm, FuncId };
use crate::memory::{ MmuState, EA, SpanKind, VA };

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
			BBTerm::FallThru { cont: new_start }, // NOT WRONG, they get swapped below.
			insts,
			state
		);

		let (old, new) = self.bbidx.get2_mut(old_bbid, new_bbid);
		std::mem::swap(&mut old.term, &mut new.term);

		trace!("  split bb new id: {:?} ea: {}, term: {:?}", new_bbid, new.ea, new.term);
		new_bbid
	}

	/// Given an MMU state and a target VA, return either the valid EA for it, or an unresolved EA
	/// with the target VA as its offset.
	pub(super) fn resolve_target(&self, state: MmuState, target: VA) -> EA {
		match self.ea_for_va(state, target) {
			Some(l) => l,
			None    => EA::unresolved(target.0),
		}
	}
}