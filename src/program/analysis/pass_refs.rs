
use std::iter::{ IntoIterator };

use log::*;

use crate::program::{ InstructionKind, Program, FuncId, OpInfo, BBId };
use crate::memory::{ SpanKind, VA, EA, MmuState };

// ------------------------------------------------------------------------------------------------
// Refs pass
// ------------------------------------------------------------------------------------------------

impl Program {
	pub(super) fn func_refs_pass(&mut self, fid: FuncId) {
		let func = self.get_func(fid);

		trace!("------------------------------------------------------------------------");
		trace!("- begin function refs pass at {}", func.ea());

		// switch instruction
		let mut jumptables = Vec::<EA>::new();
		// call target, state when call was made
		let mut funcs      = Vec::<(EA, MmuState)>::new();
		// ID, instruction index, opn
		let mut varefs     = Vec::<(BBId, usize, usize)>::new();
		// src, dst
		let mut refs       = Vec::<(EA, EA)>::new();

		for bb in func.all_bbs() {
			let bb = self.bbidx.get(bb);

			let state = bb.mmu_state();
			for (instidx, inst) in bb.insts().iter().enumerate() {
				let src = inst.ea();

				for i in 0 .. inst.num_ops() {
					match inst.get_opinfo(i) {
						OpInfo::None => {
							let op = inst.get_op(i);

							if op.has_addr() {
								// AFAIK this can only happen for control flow instructions. the
								// loop over explicit_successors below SHOUUUULD handle them... but
								// in case some non-control instruction slipped in here, let's put
								// an assert here.
								assert!(inst.is_control());
							}
						}
						OpInfo::VARef { target, .. } => {
							trace!("VARef at {} to {}", src, target);
							// need to turn this into a Ref later
							varefs.push((bb.id(), instidx, i));
						}
						OpInfo::Ref { target, .. } => {
							// I could see this happening if this pass is re-run on a function. It's
							// harmless to push the ref anyway, since adding a ref to the refmap
							// that already exists is a no-op.
							refs.push((src, *target));
						}
					}
				}

				use InstructionKind::*;
				match inst.kind() {
					Indir => jumptables.push(inst.ea()),
					IndirCall => {
						// I don't thiiiiiiink there's anything that needs to be done here? if the
						// target could be statically determined, the const prop algo would have
						// done that and put an OpInfo::VARef on it... if not, there's nothing we
						// can do here cause we don't know what the target is.

						// maybe put a point of interest here?
					}
					Call | Cond | Uncond => {
						let target_ea = self.resolve_control_flow_target(
							inst.control_target().unwrap(), state, func.id(), &mut funcs);

						if target_ea.is_valid() {
							trace!("Call|Cond|Uncond at {} to {}", src, target_ea);
							refs.push((inst.ea(), target_ea));
						} else {
							trace!("Call|Cond|Uncond at {} to invalid EA {}", src, target_ea);
						}
					}
					_ => {}
				}
			}

			for &succ in bb.explicit_successors() {
				refs.push((bb.term_ea(), succ));
			}
		}

		for (bbid, instidx, opn) in varefs.into_iter() {
			let state = self.bbidx.get(bbid).mmu_state();
			let bb = self.bbidx.get(bbid);
			let inst = &bb.insts()[instidx];

			let replacement = match inst.get_opinfo(opn) {
				OpInfo::VARef { target, info } => {
					let target_ea = self.resolve_control_flow_target(
						*target, state, bb.func(), &mut funcs);

					// hm.
					assert!(target_ea.is_valid());
					refs.push((inst.ea(), target_ea));

					OpInfo::Ref {
						target: target_ea,
						info: *info,
					}
				}
				_ => unreachable!("something other than a VARef in varefs??? waaaaat")
			};

			*self.bbidx.get_mut(bbid)
				.insts_mut()[instidx]
				.get_opinfo_mut(opn) = replacement;
		}

		for (src, dst) in refs.into_iter() { self.add_ref(src, dst); }
		for t in jumptables.into_iter()    { self.enqueue_jump_table(t);  }
		for (f, s) in funcs.into_iter()    { self.enqueue_function(s, f); }
	}

	/// try to resolve a control flow target `va` using the given `state`. `func_id` is the function
	/// that owns this control flow instruction. if the target is resolved successfully and it
	/// refers to another function, that potential function is pushed onto `funcs`.
	///
	/// returns the resolved EA (which may or may not be valid...)
	fn resolve_control_flow_target(
		&self, va: VA, state: MmuState, func_id: FuncId, funcs: &mut Vec<(EA, MmuState)>) -> EA{
		let target_ea = self.resolve_target(state, va);

		if target_ea.is_valid() {
			use SpanKind::*;
			let should_push = match self.span_at_ea(target_ea).kind() {
				Unk => true,
				Code(other_bbid) => self.get_bb(other_bbid).func() != func_id,
				_ => false,
			};

			if should_push {
				funcs.push((target_ea, state));
			}
		}

		target_ea
	}
}
