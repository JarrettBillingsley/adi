
use std::collections::VecDeque;

use derive_new::new;

use crate::program::Program;
use crate::memory::{ Location, ImageSliceable };
use crate::disasm::{ DisassemblerTrait, InstructionTrait, PrinterTrait };

// ------------------------------------------------------------------------------------------------
// Sub-modules
// ------------------------------------------------------------------------------------------------

mod func;

pub use func::*;

// ------------------------------------------------------------------------------------------------
// ProtoBB
// ------------------------------------------------------------------------------------------------

/// Most of a BasicBlock, except the ID. Used for initial exploration of a function
/// when we haven't committed the results to the Program yet.
#[derive(Debug)]
struct ProtoBB {
	/// Its globally-unique location.
	loc: Location,
	/// Where its terminator (last instruction) is located.
	term_loc: Location,
	/// How it ends, and what its successors are.
	term: BBTerm,
}

/// Used for initial exploration, like above.
#[derive(Debug)]
#[derive(new)]
struct ProtoFunc {
	#[new(default)]
	bbs: Vec<ProtoBB>, // 0 is the head
}

// ------------------------------------------------------------------------------------------------
// Analyzer
// ------------------------------------------------------------------------------------------------

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
enum AnalysisItem {
	Func(Location),
	JumpTable(Location),
}

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
	pub fn enqueue_function(&mut self, loc: Location) {
		self.queue.push_back(AnalysisItem::Func(loc))
	}

	pub fn enqueue_jump_table(&mut self, loc: Location) {
		self.queue.push_back(AnalysisItem::JumpTable(loc))
	}

	pub fn analyze_queue(&mut self) {
		loop {
			match self.queue.pop_front() {
				None => break,
				Some(AnalysisItem::Func(loc))      => self.analyze_func(loc),
				Some(AnalysisItem::JumpTable(loc)) => self.analyze_jump_table(loc),
			}
		}
	}

	fn analyze_func(&mut self, loc: Location) {
		// OKAY. first, let's look at this location to see what's here.
		// we want a fresh undefined region of memory.
		let seg = self.prog.mem_mut().segment_from_loc_mut(loc);
		let span = seg.span_at_loc(loc);

		if !span.is_unknown() {
			// TODO: if the span is under analysis, something bad has happened
			// (should never be in the middle of analyzing something when we start analysis)
			println!("wahhhhh {} already visited", loc);
			return;
		}

		// now, we need the fuckin. DATA. the S L I C E
		let slice = seg.image_slice(span.start .. span.end).into_data();
		let va = seg.va_from_loc(loc);
		for inst in self.dis.disas_all(slice, va) {
			println!("{:04X} {:?}", inst.va(), inst.bytes());
			// println!("{}", self.print.fmt_instr(&inst, self.prog));
		}

		// let mut func = ProtoFunc::new();

	}

	fn analyze_jump_table(&mut self, _loc: Location) {
		todo!()
	}
}