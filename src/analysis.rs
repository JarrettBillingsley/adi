
use std::collections::VecDeque;

use derive_new::new;

use crate::program::Program;
use crate::memory::segment::Location;

// ------------------------------------------------------------------------------------------------
// Sub-modules
// ------------------------------------------------------------------------------------------------

pub mod types;

pub use types::*;

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