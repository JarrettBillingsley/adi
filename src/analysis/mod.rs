
use std::collections::VecDeque;
use std::iter::IntoIterator;

use derive_new::new;
use log::*;

use crate::program::{ Program, BasicBlock, BBTerm, BBId, FuncId, IntoBasicBlock };
use crate::memory::{ Location, ImageSliceable, SpanKind, VA };
use crate::disasm::{
	DisasResult,
	IDisassembler,
	IInstruction,
	InstructionKind,
	IOperand,
};

// ------------------------------------------------------------------------------------------------
// Sub-modules
// ------------------------------------------------------------------------------------------------

// ------------------------------------------------------------------------------------------------
// ProtoBB
// ------------------------------------------------------------------------------------------------

/// Most of a BasicBlock, except the ID. Used for initial exploration of a function
/// when we haven't committed the results to the Program yet.
#[derive(Debug, Clone)]
struct ProtoBB {
	/// Its globally-unique location.
	loc: Location,
	/// Where its terminator (last instruction) is located.
	term_loc: Location,
	/// How it ends, and what its successors are.
	term: BBTerm,

	/// Where it ends (not in a real BB, but more convenient for the analysis)
	end: Location,
}

impl IntoBasicBlock for ProtoBB {
	fn into_bb(self, id: BBId) -> BasicBlock {
		BasicBlock::new(id, self.loc, self.term_loc, self.term)
	}
}

/// Newtype wrapper for the index of a `ProtoBB` in a `ProtoFunc`.
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
struct PBBIdx(usize);

/// Used for initial exploration, like `ProtoBB`.
#[derive(Debug)]
#[derive(new)]
struct ProtoFunc {
	#[new(default)]
	bbs: Vec<ProtoBB>, // 0 is the head
}

impl ProtoFunc {
	/// Adds a new basic block and returns its index.
	fn new_bb(&mut self, loc: Location, term_loc: Location, term: BBTerm, end: Location) -> PBBIdx {
		trace!("new bb loc: {}, term_loc: {}, end: {}, term: {:?}",
				loc, term_loc, end, term);

		self.push_bb(ProtoBB { loc, term_loc, term, end })
	}

	fn push_bb(&mut self, bb: ProtoBB) -> PBBIdx {
		let ret = self.bbs.len();
		self.bbs.push(bb);
		PBBIdx(ret)
	}

	/// Splits an existing basic block `idx` in two at location `start`. The existing BB
	/// is shortened, and its terminator is set to fall through to the new one. **Important:
	/// the existing BB's terminator location is set to an invalid location and must be
	/// re-found after calling this method.**
	/// Returns the new BB's index.
	fn split_bb(&mut self, idx: PBBIdx, term_loc: Location, new_start: Location) -> PBBIdx {
		let old = &mut self.bbs[idx.0];

		assert!(old.loc < new_start);
		assert!(new_start < old.end);
		assert!(term_loc < new_start);

		let new = ProtoBB { loc: new_start, ..old.clone() };

		old.term_loc = term_loc;
		old.term     = BBTerm::FallThru(new_start);
		old.end      = new_start;


		trace!("split bb loc: {}, term_loc: {}, end: {}, term: {:?}",
				new.loc, new.term_loc, new.end, new.term);

		self.push_bb(new)
	}

	/// Get the BB at the given index.
	fn get_bb(&self, idx: PBBIdx) -> &ProtoBB {
		&self.bbs[idx.0]
	}

	/// Consuming iterator over the BBs (for promotion to a real function).
	fn into_iter(self) -> impl Iterator<Item = impl IntoBasicBlock> {
		self.bbs.into_iter()
	}
}

// ------------------------------------------------------------------------------------------------
// Analyzer
// ------------------------------------------------------------------------------------------------

/// Things that can be put onto an `Analyzer`'s analysis queue.
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
enum AnalysisItem {
	/// An unexplored function.
	Func1stPass(Location),
	/// A function that needs to be analyzed more deeply.
	Func2ndPass(FuncId),
	/// A jump table. The location points to the jump instruction.
	JumpTable(Location),
}

/// Analyzes code, discovers functions, builds control flow graphs, and defines
/// functions in a program.
#[derive(new)]
pub struct Analyzer<'prog, D: IDisassembler> {
	prog:  &'prog mut Program,
	dis:   D,
	#[new(default)]
	queue: VecDeque<AnalysisItem>,
}

impl<I, D> Analyzer<'_, D>
where
	I: IInstruction,
	D: IDisassembler<TInstruction = I>,
{
	/// Puts a location on the queue that should be the start of a function.
	pub fn enqueue_function(&mut self, loc: Location) {
		self.queue.push_back(AnalysisItem::Func1stPass(loc))
	}

	/// Puts a location on the queue that should be the jump instruction for a jump table.
	pub fn enqueue_jump_table(&mut self, loc: Location) {
		self.queue.push_back(AnalysisItem::JumpTable(loc))
	}

	/// Analyzes all items in the analysis queue. Analysis may generate more items to analyze,
	/// so this can do a lot of work in a single call.
	pub fn analyze_queue(&mut self) {
		loop {
			match self.queue.pop_front() {
				None => break,
				Some(AnalysisItem::Func1stPass(loc)) => self.func_first_pass(loc),
				Some(AnalysisItem::Func2ndPass(fid)) => self.func_second_pass(fid),
				Some(AnalysisItem::JumpTable(loc))   => self.analyze_jump_table(loc),
			}
		}
	}

	fn should_analyze_func(&self, loc: Location) -> bool {
		if let Some(bbid) = self.prog.span_at_loc(loc).bb() {
			let orig_func_head = self.prog.get_func(bbid.func()).head_id();

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

	fn should_analyze_bb(&mut self, func: &mut ProtoFunc, start: Location) -> bool {
		// let's look at this location to see what's here.
		// we want a fresh, undefined region of memory.

		match self.prog.span_at_loc(start).kind {
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

	fn last_instr_before(&self, loc: Location) -> DisasResult<I> {
		let (seg, span) = self.prog.seg_and_span_at_loc(loc);
		let slice       = seg.image_slice(span.start .. loc).into_data();
		let va          = seg.va_from_loc(span.start);
		self.dis.find_last_instr(slice, va, span.start)
	}

	fn check_split_bb(&mut self, func: &mut ProtoFunc, id: PBBIdx, start: Location) {
		let old_start = func.get_bb(id).loc;

		if start != old_start {
			// ooh, now we have to split the existing bb.
			// first, let's make sure that `start` points to the beginning of an instruction,
			// because otherwise we'd be jumping to an invalid location.
			let term_loc = match self.last_instr_before(start) {
				Ok(inst) => inst.loc(),
				Err(..)  => todo!("have to flag referrer as being invalid somehow"),
			};

			// now we can split the existing BB...
			let new_bb = func.split_bb(id, term_loc, start);

			// ...and update the span map.
			self.prog.segment_from_loc_mut(start).split_span(start, SpanKind::AnaCode(new_bb.0));
		}
	}

	fn resolve_target(&self, target: VA) -> Location {
		if let Some(l) = self.prog.loc_for_va(target) { l } else {
			todo!("unresolvable control flow target");
		}
	}

	fn func_first_pass(&mut self, loc: Location) {
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
			self.prog.span_begin_analysis(start);

			// now, we need the actual data.
			let (seg, span)  = self.prog.seg_and_span_at_loc(start);
			let slice        = seg.image_slice(span).into_data();
			let va           = seg.va_from_loc(start);

			// let's start disassembling instructions
			let mut term_loc = start;
			let mut end_loc  = start;
			let mut term     = None;
			let mut iter     = self.dis.disas_all(slice, va, start);

			'instloop: for inst in &mut iter {
				// trace!("{:04X} {:?}", inst.va(), inst.bytes());
				term_loc = end_loc;
				end_loc = inst.next_loc();
				let target_loc = inst.control_target().map(|t| self.resolve_target(t));

				use InstructionKind::*;
				match inst.kind() {
					Invalid => panic!("disas_all gave an invalid instruction"),
					Call | Other => continue 'instloop,
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
							let f = self.resolve_target(inst.next_addr());
							potential_bbs.push_back(f);

							// debug!("{:04X} t: {} f: {}", inst.va(), target_loc, f);

							term = Some(BBTerm::Cond { t: target_loc, f });
						} else {
							term = Some(BBTerm::Jump(target_loc));
						}
					}
				}

				break 'instloop;
			}

			if start == end_loc {
				// oop. no good.
				trace!("{} is NO GOOD", start);
				self.prog.span_cancel_analysis(start);
			} else {
				if let Some(..) = iter.err() {
					assert!(end_loc == iter.err_loc());
					term = Some(BBTerm::DeadEnd);
				} else if term.is_none() {
					// we got through the whole slice with no errors, meaning
					// this is a fallthrough to the next bb.
					term = Some(BBTerm::FallThru(end_loc));
				}

				let bb_id = func.new_bb(start, term_loc, term.unwrap(), end_loc);
				self.prog.span_end_analysis(start, end_loc, SpanKind::AnaCode(bb_id.0));
			}
		}

		assert!(potential_bbs.len() == 0);

		// now, turn the proto func into a real boy!!
		let fid = self.prog.new_func(loc, func.into_iter());

		// finally, turn the crank by putting more work on the queue
		self.queue.push_back(AnalysisItem::Func2ndPass(fid));
	}

	fn func_second_pass(&mut self, fid: FuncId) {
		let func = self.prog.get_func(fid);

		trace!("------------------------------------------------------------------------");
		trace!("- begin function 2nd pass at {}", func.start_loc());

		let mut jumptables = Vec::new();
		let mut funcs      = Vec::new();
		let mut refs       = Vec::new();

		for bb in func.all_bbs() {
			let start        = bb.loc;
			let (seg, span)  = self.prog.seg_and_span_at_loc(start);
			let slice        = seg.image_slice(span).into_data();
			let va           = seg.va_from_loc(start);
			let mut iter     = self.dis.disas_all(slice, va, start);

			for inst in &mut iter {
				let src = inst.loc();

				for i in 0 .. inst.num_ops() {
					let op = inst.get_op(i);

					if op.is_mem() {
						let dst = self.prog.loc_from_va(op.addr());
						refs.push((src, dst));
					}
				}

				use InstructionKind::*;
				match inst.kind() {
					Indir => jumptables.push(inst.loc()),
					Call => {
						let target_loc = self.resolve_target(inst.control_target().unwrap());
						funcs.push(target_loc);
					}
					_ => {}
				}
			}

			assert!(!iter.has_err(), "should be impossible");

			for &succ in bb.explicit_successors() {
				refs.push((bb.term_loc, succ));
			}
		}

		trace!("adding {} refs, {} jumptables, {} funcs", refs.len(), jumptables.len(), funcs.len());
		for (src, dst) in refs.into_iter() { self.prog.add_ref(src, dst); }
		for t in jumptables.into_iter()    { self.enqueue_jump_table(t);  }
		for f in funcs.into_iter()         { self.enqueue_function(f);    }
	}

	fn analyze_jump_table(&mut self, loc: Location) {
		trace!("there's a jumptable at {}", loc);
		// todo!()
	}
}