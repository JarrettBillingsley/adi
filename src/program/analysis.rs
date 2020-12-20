
use std::collections::VecDeque;
use std::iter::IntoIterator;

use log::*;

use crate::arch::{ IArchitecture };
use crate::platform::{ IPlatform, InstTypeOf };
use crate::program::{ Program, IProgram, BasicBlock, BBTerm, BBId, FuncId, IntoBasicBlock };
use crate::memory::{ MmuState, Location, ImageSliceable, SpanKind, VA };
use crate::disasm::{
	IDisassembler,
	IInstruction,
	InstructionKind,
};

// ------------------------------------------------------------------------------------------------
// Sub-modules
// ------------------------------------------------------------------------------------------------

// ------------------------------------------------------------------------------------------------
// ProtoBB
// ------------------------------------------------------------------------------------------------

/// Most of a BasicBlock, except the ID. Used for initial exploration of a function
/// when we haven't committed the results to the Program yet.
#[derive(Debug)]
struct ProtoBB<TInstruction: IInstruction> {
	loc:      Location,
	term_loc: Location,
	term:     BBTerm,
	insts:    Vec<TInstruction>,

	/// Where it ends (not in a real BB, but more convenient for the analysis)
	end:      Location,
}

impl<T: IInstruction> ProtoBB<T> {
	fn last_instr_before(&self, loc: Location) -> Option<usize> {
		for (i, inst) in self.insts.iter().enumerate() {
			if inst.loc() < loc {
				let next = inst.next_loc();

				use std::cmp::Ordering::*;

				match next.cmp(&loc) {
					// uh oh. loc is in the middle of this instruction.
					Greater => return None,
					Equal   => return Some(i),
					Less    => {}
				}
			}
		}

		unreachable!("should never be able to get here")
	}
}

impl<T: IInstruction> IntoBasicBlock<T> for ProtoBB<T> {
	fn into_bb(self, id: BBId) -> BasicBlock<T> {
		BasicBlock::new(id, self.loc, self.term_loc, self.term, self.insts)
	}
}

/// Newtype wrapper for the index of a `ProtoBB` in a `ProtoFunc`.
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
struct PBBIdx(usize);

/// Used for initial exploration, like `ProtoBB`.
#[derive(Debug)]
struct ProtoFunc<TInstruction: IInstruction> {
	bbs: Vec<ProtoBB<TInstruction>>, // 0 is the head
}

impl<TInstruction: IInstruction> ProtoFunc<TInstruction> {
	fn new() -> Self {
		Self { bbs: Vec::new() }
	}
}

impl<T: IInstruction> ProtoFunc<T> {
	/// Adds a new basic block and returns its index.
	fn new_bb(&mut self,
		loc: Location,
		term_loc: Location,
		term: BBTerm,
		end: Location,
		insts: Vec<T>)
	-> PBBIdx {
		trace!("new bb loc: {}, term_loc: {}, end: {}, term: {:?}",
				loc, term_loc, end, term);

		self.push_bb(ProtoBB { loc, term_loc, term, end, insts })
	}

	fn push_bb(&mut self, bb: ProtoBB<T>) -> PBBIdx {
		let ret = self.bbs.len();
		self.bbs.push(bb);
		PBBIdx(ret)
	}

	fn split_bb(&mut self, idx: PBBIdx, inst_idx: usize, new_start: Location) -> PBBIdx {
		let old = &mut self.bbs[idx.0];
		let term_loc = old.insts[inst_idx].loc();

		assert!(old.loc < new_start);
		assert!(new_start < old.end);
		assert!(term_loc < new_start);

		let new_insts = old.insts.split_off(inst_idx + 1);

		let mut new = ProtoBB {
			loc:      new_start,
			term_loc: old.term_loc,
			term:     BBTerm::FallThru(new_start), // NOT WRONG, they get swapped below.
			insts:    new_insts,
			end:      old.end
		};

		std::mem::swap(&mut old.term, &mut new.term);
		old.term_loc = term_loc;
		old.end      = new_start;

		trace!("split bb loc: {}, term_loc: {}, end: {}, term: {:?}",
				new.loc, new.term_loc, new.end, new.term);

		self.push_bb(new)
	}

	/// Get the BB at the given index.
	fn get_bb(&self, idx: PBBIdx) -> &ProtoBB<T> {
		&self.bbs[idx.0]
	}

	/// Consuming iterator over the BBs (for promotion to a real function).
	fn into_iter(self) -> impl Iterator<Item = impl IntoBasicBlock<T>> {
		self.bbs.into_iter()
	}
}

// ------------------------------------------------------------------------------------------------
// Analyzer
// ------------------------------------------------------------------------------------------------

/// Things that can be put onto an `Analyzer`'s analysis queue.
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub(crate) enum AnalysisItem {
	/// An unexplored function.
	Func1stPass(Location, MmuState),
	/// A function that needs to be analyzed more deeply.
	Func2ndPass(FuncId),
	/// A jump table. The location points to the jump instruction.
	JumpTable(Location),
}

impl<Plat: IPlatform> Program<Plat> {
	fn should_analyze_func(&self, loc: Location) -> bool {
		if let Some(bbid) = self.span_at_loc(loc).bb() {
			let orig_func_head = self.get_func(bbid.func()).head_id();

			if bbid == orig_func_head {
				// the beginning of an already-analyzed function.
				trace!("oh, I've seen this one already. NEVERMIIIND");
			} else {
				// the middle of an already-analyzed function. TODO: maybe we should
				// split the function, maybe not. Consider diamond-shaped CFG where we
				// call a BB in the middle of one path. who owns the BBs at/after
				// the rejoining point?
				trace!("middle of an existing function... let's move on");
			}
			false
		} else {
			true
		}
	}

	fn should_analyze_bb(
		&mut self, func: &mut ProtoFunc<InstTypeOf<Plat>>, start: Location) -> bool {
		// let's look at this location to see what's here.
		// we want a fresh, undefined region of memory.

		match self.span_at_loc(start).kind() {
			SpanKind::Unk         => true, // yeeeeee that's what we want
			SpanKind::Code(..)    => false,   // fell through into another function.
			SpanKind::AnaCode(id) => {
				// oh, we've already analyzed this. but maybe we have some work to do...
				self.check_split_bb(func, PBBIdx(id), start);
				false
			}

			SpanKind::Ana => panic!("span at {} is somehow being analyzed", start),
			SpanKind::Data => todo!("uh oh. what do we do here? {}", start),
		}
	}

	fn check_split_bb(
		&mut self, func: &mut ProtoFunc<InstTypeOf<Plat>>, id: PBBIdx, start: Location) {
		let old_bb = func.get_bb(id);

		if start != old_bb.loc {
			// ooh, now we have to split the existing bb.
			// first, let's make sure that `start` points to the beginning of an instruction,
			// because otherwise we'd be jumping to an invalid location.
			let idx = match old_bb.last_instr_before(start) {
				Some(idx) => idx,
				None      => todo!("have to flag referrer as being invalid somehow"),
			};

			// now we can split the existing BB...
			let new_bb = func.split_bb(id, idx, start);

			// ...and update the span map.
			self.segment_from_loc_mut(start).split_span(start, SpanKind::AnaCode(new_bb.0));
		}
	}

	fn resolve_target(&self, state: MmuState, target: VA) -> Location {
		if let Some(l) = self.loc_for_va(state, target) { l } else {
			todo!("unresolvable control flow target");
		}
	}

	pub(crate) fn func_first_pass(&mut self, loc: Location, state: MmuState) {
		trace!("------------------------------------------------------------------------");
		trace!("- begin function 1st pass at {}", loc);

		if !self.should_analyze_func(loc) {
			return;
		}

		// first pass is to build up the CFG. we're just looking for control flow
		// and finding the boundaries of this function.
		let mut func           = ProtoFunc::new();
		let mut potential_bbs  = VecDeque::<Location>::new();
		potential_bbs.push_back(loc);

		'outer: while let Some(start) = potential_bbs.pop_front() {
			trace!("evaluating potential bb at {}", start);

			// ok. should we analyze this?
			if !self.should_analyze_bb(&mut func, start) {
				continue 'outer;
			}

			// yes. mark the span as under analysis.
			self.span_begin_analysis(start);

			// now, we need the actual data.
			let (seg, span)  = self.seg_and_span_at_loc(start);
			let slice        = seg.image_slice(span).into_data();
			let va           = self.va_from_loc(state, start);

			// let's start disassembling instructions
			let dis          = self.plat().arch().new_disassembler();
			let mut term_loc = start;
			let mut end_loc  = start;
			let mut term     = None;
			let mut insts    = Vec::new();
			let mut iter     = dis.disas_all(slice, state, va, start);

			'instloop: for inst in &mut iter {
				// trace!("{:04X} {:?}", inst.va(), inst.bytes());
				term_loc = end_loc;
				end_loc = inst.next_loc();
				let target_loc = inst.control_target().map(
					|t| self.resolve_target(inst.mmu_state(), t));

				use InstructionKind::*;
				match inst.kind() {
					Invalid => panic!("disas_all gave an invalid instruction"),
					Call | Other => {
						// call targets are processed in 2nd pass.
						insts.push(inst);
						continue 'instloop;
					}
					Ret => {
						// TODO: what about e.g. Z80 where you can have conditional returns?
						// should that end a BB?
						term = Some(BBTerm::Return);
					}
					Halt => {
						term = Some(BBTerm::Halt);
					}
					Indir => {
						term = Some(BBTerm::JumpTbl(vec![]));
					}
					Uncond | Cond => {
						// if it's into the same segment, it might be part of this function.
						// if not, it's probably a tailcall to another function.
						let target_loc = target_loc.expect("instruction should have control target");
						if seg.contains_loc(target_loc) {
							potential_bbs.push_back(target_loc);
						}

						if inst.is_cond() {
							let f = self.resolve_target(inst.mmu_state_after(), inst.next_addr());
							potential_bbs.push_back(f);

							// debug!("{:04X} t: {} f: {}", inst.va(), target_loc, f);

							term = Some(BBTerm::Cond { t: target_loc, f });
						} else {
							term = Some(BBTerm::Jump(target_loc));
						}
					}
				}

				insts.push(inst);
				break 'instloop;
			}

			if start == end_loc {
				// oop. no good.
				trace!("{} is NO GOOD", start);
				self.span_cancel_analysis(start);
			} else {
				if let Some(..) = iter.err() {
					assert!(end_loc == iter.err_loc());
					term = Some(BBTerm::DeadEnd);
				} else if term.is_none() {
					// we got through the whole slice with no errors, meaning
					// this is a fallthrough to the next bb.
					term = Some(BBTerm::FallThru(end_loc));
				}

				let bb_id = func.new_bb(start, term_loc, term.unwrap(), end_loc, insts);
				self.span_end_analysis(start, end_loc, SpanKind::AnaCode(bb_id.0));
			}
		}

		assert!(potential_bbs.is_empty());

		// now, turn the proto func into a real boy!!
		let fid = self.new_func(loc, func.into_iter());

		// finally, turn the crank by putting more work on the queue
		self.queue.push_back(AnalysisItem::Func2ndPass(fid));
	}

	pub(crate) fn func_second_pass(&mut self, fid: FuncId) {
		let func = self.get_func(fid);

		trace!("------------------------------------------------------------------------");
		trace!("- begin function 2nd pass at {}", func.start_loc());

		let mut jumptables = Vec::new();
		let mut funcs      = Vec::new();
		let mut refs       = Vec::new();

		for bb in func.all_bbs() {
			for inst in bb.insts() {
				let src = inst.loc();

				for i in 0 .. inst.num_ops() {
					let op = inst.get_op(i);

					if op.is_mem() {
						let dst = self.loc_from_va(inst.mmu_state(), op.addr());
						refs.push((src, dst));
					}
				}

				use InstructionKind::*;
				match inst.kind() {
					Indir => jumptables.push(inst.loc()),
					Call => {
						let target_loc = self.resolve_target(inst.mmu_state(),
							inst.control_target().unwrap());
						funcs.push((target_loc, inst.mmu_state_after()));
					}
					_ => {}
				}
			}

			for &succ in bb.explicit_successors() {
				refs.push((bb.term_loc(), succ));
			}
		}

		trace!("adding {} refs, {} jumptables, {} funcs", refs.len(), jumptables.len(), funcs.len());
		for (src, dst) in refs.into_iter() { self.add_ref(src, dst); }
		for t in jumptables.into_iter()    { self.enqueue_jump_table(t);  }
		for (f, s) in funcs.into_iter()    { self.enqueue_function(s, f); }
	}

	pub(crate) fn analyze_jump_table(&mut self, loc: Location) {
		trace!("there's a jumptable at {}", loc);
		// todo!()
	}
}