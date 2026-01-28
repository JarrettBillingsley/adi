
use std::iter::{ IntoIterator };

use log::*;

use crate::program::{ InstructionKind, Program, FuncId };
use crate::memory::{ SpanKind };

// ------------------------------------------------------------------------------------------------
// Refs pass
// ------------------------------------------------------------------------------------------------

impl Program {
	pub(super) fn func_refs_pass(&mut self, fid: FuncId) {
		let func = self.get_func(fid);

		trace!("------------------------------------------------------------------------");
		trace!("- begin function refs pass at {}", func.ea());

		let mut jumptables = Vec::new();
		let mut funcs      = Vec::new();
		let mut refs       = Vec::new();

		for bb in func.all_bbs() {
			let bb = self.bbidx.get(bb);

			let state = bb.mmu_state();
			for inst in bb.insts() {
				let src = inst.ea();

				for i in 0 .. inst.num_ops() {
					let op = inst.get_op(i);

					// TODO: use OpInfo first here (look for VARefs); then fall
					// back on has_addr()
					// TODO: what if the operand already has an OpInfo::Ref? I could see
					// that happening if this pass is re-run on a function. I guess it'd be
					// harmless to push the ref anyway, since adding a ref to the refmap
					// that already exists is a no-op.
					if op.has_addr() {
						let dst = self.ea_from_va(state, op.addr());
						refs.push((src, dst));
					}
				}

				use InstructionKind::*;
				match inst.kind() {
					Indir => jumptables.push(inst.ea()),
					// TODO: IndirCall: is it a jumptable? not usually...
					Call | Cond | Uncond => {
						let target_ea = self.resolve_target(state, inst.control_target().unwrap());

						if target_ea.is_valid() {
							use SpanKind::*;
							let should_push = match self.span_at_ea(target_ea).kind() {
								Unk => true,
								Code(other_bbid) => self.get_bb(other_bbid).func() != func.id(),
								_ => false,
							};

							if should_push {
								funcs.push((target_ea, state));
							}
						}
					}
					_ => {}
				}
			}

			for &succ in bb.explicit_successors() {
				refs.push((bb.term_ea(), succ));
			}
		}

		for (src, dst) in refs.into_iter() { self.add_ref(src, dst); }
		for t in jumptables.into_iter()    { self.enqueue_jump_table(t);  }
		for (f, s) in funcs.into_iter()    { self.enqueue_function(s, f); }
	}
}
