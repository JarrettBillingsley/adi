
use std::collections::{ VecDeque };

use log::*;

use crate::arch::{ IArchitecture };
use crate::platform::{ IPlatform };
use crate::program::{ Instruction, InstructionKind, Program, BBId, BBTerm };
use crate::memory::{ MmuState, EA, ImageSliceable, SpanKind, VA, SegId };

// ------------------------------------------------------------------------------------------------
// First-pass function analysis
// ------------------------------------------------------------------------------------------------

impl Program {
	pub(super) fn new_func_pass(&mut self, ea: EA, state: MmuState) {
		trace!("------------------------------------------------------------------------");
		trace!("- begin function 1st pass at {} with state {:?}", ea, state);

		if !self.should_analyze_func(ea) {
			return;
		}

		// first pass is to build up the CFG. we're just looking for control flow
		// and finding the boundaries of this function.
		let mut bbs            = Vec::<BBId>::new();
		let mut potential_bbs  = VecDeque::<EA>::new();
		potential_bbs.push_back(ea);

		'outer: while let Some(start) = potential_bbs.pop_front() {
			trace!("evaluating potential bb at {}", start);

			// ok. should we analyze this?
			if !self.should_analyze_bb(&mut bbs, start) {
				continue 'outer;
			}

			// yes. mark the span as under analysis.
			self.span_begin_analysis(start);

			// now, we need the actual data.
			let (seg, span)  = self.seg_and_span_at_ea(start);
			let slice        = seg.image_slice(span).into_data();
			let va           = self.va_from_ea(state, start);

			// let's start disassembling instructions
			let dis          = self.plat().arch().new_disassembler();
			let mut end_ea   = start;
			let mut term     = None;
			let mut insts    = Vec::new();
			let mut iter     = dis.disas_all(slice, state, va, start);

			'instloop: for inst in &mut iter {
				// trace!("{:04X} {:?}", inst.va(), inst.bytes());
				end_ea = inst.next_ea();

				// the Old Way checked for bank changes here using self.mem.inst_state_change
				// on each instruction, and ending the BB if so. but now we're checking for
				// bank changes later, when we have the whole function and can do const prop
				// on it and stuff, in state_change_pass.

				use InstructionKind::*;
				match inst.kind() {
					Other => {
						insts.push(inst);
						continue 'instloop;
					}
					Ret => {
						// TODO: what about e.g. Z80 where you can have conditional returns?
						// should that end a BB?
						// well I think it should be like a conditional - yes, end this
						// BB, but push another BB of the next instruction.
						term = Some(BBTerm::Return);
					}
					Halt => {
						term = Some(BBTerm::Halt);
					}
					Indir => {
						// the target EAs are filled in later
						term = Some(BBTerm::JumpTbl(vec![]));
					}
					IndirCall => {
						let next = inst.next_ea();
						potential_bbs.push_back(next);

						// the destination EAs are filled in later
						term = Some(BBTerm::IndirCall { dst: vec![], ret: next });
					}
					Call => {
						let target_va = inst.control_target().expect("should have control target");
						let target_ea = self.va_to_ea_in_same_seg(seg.id(), state, target_va);

						let next = inst.next_ea();
						potential_bbs.push_back(next);

						// debug!("{:04X} t: {} next: {}", inst.va(), target_ea, next);

						term = Some(BBTerm::Call { dst: target_ea, ret: next });
					}
					Uncond | Cond => {
						let target_va = inst.control_target().expect("should have control target");
						let target_ea = self.va_to_ea_in_same_seg(seg.id(), state, target_va);

						// if it's into the same segment, it might be part of this function.
						// if not, it's probably a tailcall to another function.
						if seg.contains_ea(target_ea) {
							potential_bbs.push_back(target_ea);
						}

						if inst.kind() == Uncond {
							term = Some(BBTerm::Jump(target_ea));
						} else {
							let next = inst.next_ea();
							potential_bbs.push_back(next);

							// debug!("{:04X} t: {} next: {}", inst.va(), target_ea, next);

							term = Some(BBTerm::Cond { t: target_ea, f: next });
						}
					}
				}

				// this is down here for BORROWING REASONS
				insts.push(inst);
				break 'instloop;
			}

			if start == end_ea {
				// oop. no good.
				trace!("{} is NO GOOD", start);
				self.span_cancel_analysis(start);
			} else {
				if let Some(..) = iter.err() {
					assert!(end_ea == iter.err_ea());
					term = Some(BBTerm::DeadEnd);
				} else if term.is_none() {
					// we got through the whole slice with no errors, meaning
					// this is a fallthrough to the next bb.
					// if we got to the end of the *segment,* though, end_ea may be invalid.
					if end_ea.offs() >= seg.len() {
						term = Some(BBTerm::DeadEnd)
					} else {
						term = Some(BBTerm::FallThru(end_ea));
					}
				}

				let bbid = self.new_bb(start, term.unwrap(), insts, state);
				bbs.push(bbid);
				self.span_end_analysis(start, end_ea, SpanKind::AnaCode(bbid));
			}
		}

		assert!(potential_bbs.is_empty());

		// empty func can happen if the very first BB was NO GOOD and was canceled.
		if bbs.len() == 0 {
			trace!("NOPE that's not a good function.");
		} else {
			// now, make the function!!
			let fid = self.new_func(bbs);

			// finally, turn the crank by putting more work on the queue
			self.enqueue_state_change(fid);
		}
	}

	fn should_analyze_func(&mut self, ea: EA) -> bool {
		if let Some(bbid) = self.span_at_ea(ea).bb() {
			let bb = self.bbidx.get(bbid);
			let orig_func = self.get_func(bb.func());

			if ea == orig_func.ea() {
				// the beginning of an already-analyzed function.
				trace!("oh, I've seen this one already. NEVERMIIIND");
			} else {
				// the middle of an already-analyzed function.
				trace!("middle of an existing function...");
				self.enqueue_split_func(ea);
			}
			false
		} else if self.segment_from_ea(ea).is_fake() {
			trace!("skipping function at invalid EA: {:?}", ea);
			false
		} else {
			true
		}
	}

	fn should_analyze_bb(&mut self, bbs: &mut Vec<BBId>, start: EA) -> bool {
		// let's look at this EA to see what's here.
		// we want a fresh, undefined region of memory.

		match self.span_at_ea(start).kind() {
			SpanKind::Unk         => true, // yeeeeee that's what we want
			SpanKind::Code(..)    => false,   // fell through into another function.
			SpanKind::AnaCode(id) => {
				// oh, we've already analyzed this. we might have to split it though.
				match self.split_bb(id, start, None) {
					Ok(Some(bbid)) => bbs.push(bbid),
					Ok(None) => {} // s'fine
					Err(_) => {
						// TODO: mark referrer as being invalid somehow.
					}
				}

				false
			}

			SpanKind::Ana => panic!("span at {} is somehow being analyzed", start),
			SpanKind::Data(..) => todo!("uh oh. what do we do here? {}", start),
		}
	}

	fn new_bb(&mut self, ea: EA, term: BBTerm, insts: Vec<Instruction>, state: MmuState) -> BBId {
		log::trace!("new bb ea: {}, term: {:?}", ea, term);
		self.bbidx.new_bb(ea, term, insts, state)
	}

	fn va_to_ea_in_same_seg(&self, seg: SegId, state: MmuState, va: VA) -> EA {
		match self.ea_for_va(state, va) {
			Some(l) if l.seg() == seg => l,
			_                         => EA::invalid(va.0)
		}
	}
}
