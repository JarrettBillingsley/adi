
use std::collections::VecDeque;
use std::iter::IntoIterator;

use derive_new::new;
use log::*;

use crate::program::{ Program, BasicBlock, BBTerm, BBId, IntoBasicBlock };
use crate::memory::{ Location, ImageSliceable, SpanKind };
use crate::disasm::{ DisasErrorKind, DisassemblerTrait, InstructionTrait, PrinterTrait };

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
	fn new_bb(&mut self, loc: Location, term_loc: Location, term: BBTerm, end: Location)
	-> PBBIdx {
		let ret = self.bbs.len();

		debug!("new bb loc: {}, term_loc: {}, end: {}, term: {:?}",
				loc, term_loc, end, term);

		self.bbs.push(ProtoBB { loc, term_loc, term, end });
		PBBIdx(ret)
	}

	/// Splits an existing basic block `idx` in two at location `start`. The existing BB
	/// is shortened, and its terminator is set to fall through to the new one. **Important:
	/// the existing BB's terminator location is set to an invalid location and must be
	/// re-found after calling this method.**
	/// Returns the new BB's index.
	fn split_bb(&mut self, idx: PBBIdx, start: Location) -> PBBIdx {
		let ret = self.bbs.len();

		assert!(self.bbs[idx.0].loc.offs < start.offs);
		assert!(start.offs < self.bbs[idx.0].end.offs);
		let new = ProtoBB { loc: start, ..self.bbs[idx.0].clone() };

		// TODO: I'm pretty sure I can find the last instruction before the terminator BEFORE
		// splitting so that this won't be necessary.
		self.bbs[idx.0].term_loc = Location::invalid();
		self.bbs[idx.0].term = BBTerm::FallThru(start);
		self.bbs[idx.0].end = start;

		debug!("split bb loc: {}, term_loc: {}, end: {}, term: {:?}",
				new.loc, new.term_loc, new.end, new.term);
		self.bbs.push(new);
		PBBIdx(ret)
	}

	/// Get the BB at the given index.
	fn get_bb(&self, idx: PBBIdx) -> &ProtoBB {
		&self.bbs[idx.0]
	}

	/// Set the terminator location of a BB that was split by `split_bb`.
	fn set_term_loc(&mut self, idx: PBBIdx, term_loc: Location) {
		assert!(self.bbs[idx.0].term_loc == Location::invalid());
		self.bbs[idx.0].term_loc = term_loc;
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
	/// A function.
	Func(Location),

	/// A jump table. The location points to the jump instruction.
	JumpTable(Location),
}

/// Analyzes code, discovers functions, builds control flow graphs, and defines
/// functions in a program.
#[derive(new)]
pub struct Analyzer<'prog, D: DisassemblerTrait, P: PrinterTrait> {
	prog:  &'prog mut Program,
	dis:   D,
	print: P,
	#[new(value = "VecDeque::new()")]
	queue: VecDeque<AnalysisItem>,
}

impl<I, D, P> Analyzer<'_, D, P>
where
	I: InstructionTrait,
	D: DisassemblerTrait<TInstruction = I>,
	P: PrinterTrait<TInstruction = I>,
{
	/// Puts a location on the queue that should be the start of a function.
	pub fn enqueue_function(&mut self, loc: Location) {
		self.queue.push_back(AnalysisItem::Func(loc))
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
				Some(AnalysisItem::Func(loc))      => self.analyze_func(loc),
				Some(AnalysisItem::JumpTable(loc)) => self.analyze_jump_table(loc),
			}
		}

		for (_, func) in self.prog.all_funcs() {
			debug!("---------------------------------------------------------------------------");
			debug!("{:#?}", func);
		}
	}

	fn refind_terminator(&self, func: &mut ProtoFunc, idx: PBBIdx, loc: Location) {
		let seg = self.prog.mem().segment_from_loc(loc);
		let span = seg.span_at_loc(loc);
		let slice = seg.image_slice(span.start .. span.end).into_data();
		let va = seg.va_from_loc(loc);

		match self.dis.find_last_instr(slice, va) {
			Ok(Some(inst)) => {
				let term_loc = self.prog.mem().loc_for_va(inst.va()).unwrap();
				func.set_term_loc(idx, term_loc);
			}

			// should not happen, since the split method will panic if you try to
			// make an empty span.
			Ok(None) => panic!("trying to find terminator of an empty bb???"),

			// should not happen, if we didn't do an invalid split.
			Err(e) =>   todo!("what does this mean??? {}", e),
		}
	}

	fn analyze_func(&mut self, loc: Location) {
		debug!("------------------------------------------------------------------------");
		debug!("- begin function analysis at {}", loc);

		let kind = self.prog.mem().segment_from_loc(loc).span_at_loc(loc).kind;
		if let SpanKind::Code(bbid) = kind {
			let orig_func_head = self.prog.get_func(bbid.func()).head_id();

			if bbid == orig_func_head {
				debug!("oh, I've seen this one already. NEVERMIIIND");
				return;
			} else {
				todo!("have to split function at {} and make predecessor(s) tailcalls", loc);
			}
		}

		// first pass is to build up the CFG. we're just looking for control flow
		// and finding the boundaries of this function.
		let mut func = ProtoFunc::new();
		let mut potential_bbs = VecDeque::<Location>::new();
		potential_bbs.push_back(loc);
		let mut new_jumptables = Vec::<Location>::new();
		let mut new_funcs = Vec::<Location>::new();

		while let Some(start) = potential_bbs.pop_front() {
			debug!("evaluating potential bb at {}", start);

			// ok. we have a potential BB to analyze.
			// let's look at this location to see what's here.
			// we want a fresh, undefined region of memory.
			let span = self.prog.mem().segment_from_loc(start).span_at_loc(start);

			match span.kind {
				SpanKind::Unk         => {} // yeeeeee that's what we want
				SpanKind::Ana         => panic!("span at {} is somehow being analyzed", start),
				SpanKind::Code(..)    => todo!("span at {} already analyzed; fallthru?", start),
				SpanKind::Data        => todo!("uh oh. what do we do here? {}", start),
				SpanKind::AnaCode(id) => {
					let id = PBBIdx(id);

					let old_start = func.get_bb(id).loc;

					// oh, we've already analyzed this.
					if start != old_start {
						// TODO: ensure that `start` actually points to the beginning of an instruction

						// ooh, now we have to split the existing bb.
						let new_bb = func.split_bb(id, start);

						// then update the span map...
						self.prog.mem_mut().segment_from_loc_mut(start).split_span(
							start, SpanKind::AnaCode(new_bb.0));

						// ...and now we have to re-find the terminator for the old one.
						self.refind_terminator(&mut func, id, old_start);
					}

					// done all we need here
					continue;
				}
			}

			// mark the span as under analysis.
			self.prog.mem_mut().segment_from_loc_mut(start).span_begin_analysis(start);

			// ...and refresh span, as the previous line invalidated it.
			let span = self.prog.mem().segment_from_loc(start).span_at_loc(start);

			// now, we need the actual data.
			let seg = self.prog.mem().segment_from_loc(start);
			let slice = seg.image_slice(span.start .. span.end).into_data();
			let va = seg.va_from_loc(start);

			// let's start disassembling instructions
			let mut iter = self.dis.disas_all(slice, va);
			let mut term_loc = va;
			let mut end = va;
			let mut term: Option<BBTerm> = None;

			for inst in &mut iter {
				// debug!("{:04X} {:?}", inst.va(), inst.bytes());
				let next_addr = inst.va() + inst.size();
				term_loc = end;
				end = next_addr;

				if !inst.is_control() {
					// if it's not control, skip it.
					continue;
				} else if inst.is_return() {
					term = Some(BBTerm::Return);
					break;
				} else if inst.is_halt() {
					term = Some(BBTerm::Halt);
					break;
				}

				// OK. it's a conditional, a jump, an indirect jump, or a call.
				// First, let's see if it's an indirect jump, since that doesn't have a
				// hard-coded target.

				if inst.is_indir_jump() {
					// oooh. a jumptable to analyze...
					term = Some(BBTerm::JumpTbl(vec![]));
					new_jumptables.push(self.prog.mem().loc_for_va(inst.va()).expect("huh?"));
					break;
				}

				// not indirect, so let's check its target address.
				let target = inst.control_target().expect("how jump/call/branch not have target??");
				let target_loc = self.prog.mem().loc_for_va(target);

				let target_loc = if let Some(l) = target_loc { l } else {
					todo!("unresolvable control flow target");
				};

				if inst.is_jump() {
					// if it's into the same segment, it might be part of this function.
					// if not, it's probably a tailcall to another function.
					if seg.contains_va(target) {
						debug!("pushing potential bb at {}", target_loc);
						potential_bbs.push_back(target_loc);
					}

					term = Some(BBTerm::Jump(target_loc));
					break; // end of this BB.
				} else if inst.is_conditional() {
					// if it's into the same segment, it might be part of this function.
					// if not, it's probably a tailcall to another function.
					if seg.contains_va(target) {
						debug!("pushing potential bb at {}", target_loc);
						potential_bbs.push_back(target_loc);
					}

					let f = if let Some(l) = self.prog.mem().loc_for_va(next_addr) { l } else {
						todo!("unresolvable control flow target");
					};

					potential_bbs.push_back(f);

					term = Some(BBTerm::Cond { t: target_loc, f });
					debug!("pushing potential bb at {}", f);
					break;
				} else {
					assert!(inst.is_call());

					// clearly gotta be another function, so.
					debug!("func call to {}", target_loc);
					new_funcs.push(target_loc);

				}
			}

			if let Some(err) = iter.err() {
				// matching here so if we add more error kinds, we'll get an inexhaustive match error
				match err.kind {
					DisasErrorKind::OutOfBytes { expected, got } => {
						assert!(end == iter.err_va());
						term = Some(BBTerm::DeadEnd);
						debug!("out of bytes (ex {} got {})", expected, got);
						debug!("dead ended term {} end {}", term_loc, end);
					}

					DisasErrorKind::UnknownInstruction => {
						assert!(end == iter.err_va());
						term = Some(BBTerm::DeadEnd);
						debug!("dead ended term {} end {}", term_loc, end);
					}
				}
			} else if term.is_none() {
				// we got through the whole slice with no errors, meaning
				// this is a fallthrough to the next bb.
				term = Some(BBTerm::FallThru(self.prog.mem().loc_for_va(end).unwrap()));
			}

			let term = term.unwrap();
			let term_loc = self.prog.mem().loc_for_va(term_loc).expect("huh?");
			let end = self.prog.mem().loc_for_va(end).expect("huh?");

			let bb_id = func.new_bb(start, term_loc, term, end);

			debug!("{} {}", span.start, span.end);

			self.prog.mem_mut().segment_from_loc_mut(start).span_end_analysis(
				start, end, SpanKind::AnaCode(bb_id.0));
		}

		// debug!("{:#?}", func);

		// now, turn the proto func into a real boy!!
		self.prog.new_func(loc, func.into_iter());

		// finally, turn the crank by putting more work on the queue
		for jt in new_jumptables {
			self.enqueue_jump_table(jt);
		}

		for f in new_funcs {
			// debug!("discovered function at {}", f);
			self.enqueue_function(f);
		}
	}

	fn analyze_jump_table(&mut self, _loc: Location) {
		todo!()
	}
}