
use std::collections::VecDeque;

use derive_new::new;

use crate::program::Program;
use crate::memory::Location;

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
pub struct Analyzer {
	#[new(value = "VecDeque::new()")]
	queue: VecDeque<AnalysisItem>,
}

impl Analyzer {
	pub fn enqueue_function(&mut self, loc: Location) {
		self.queue.push_back(AnalysisItem::Func(loc))
	}

	pub fn enqueue_jump_table(&mut self, loc: Location) {
		self.queue.push_back(AnalysisItem::JumpTable(loc))
	}

	pub fn analyze_queue(&mut self, prog: &mut Program) {
		loop {
			match self.queue.pop_front() {
				None => break,
				Some(AnalysisItem::Func(loc))      => self.analyze_func(loc, prog),
				Some(AnalysisItem::JumpTable(loc)) => self.analyze_jump_table(loc, prog),
			}
		}
	}

	fn analyze_func(&mut self, _loc: Location, _prog: &mut Program) {
		todo!()
	}

	fn analyze_jump_table(&mut self, _loc: Location, _prog: &mut Program) {
		todo!()
	}
}