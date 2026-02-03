
use std::collections::{ VecDeque };

use crate::program::{ Program, FuncId };
use crate::memory::{ MmuState, EA };

// ------------------------------------------------------------------------------------------------
// Submodules
// ------------------------------------------------------------------------------------------------

pub mod misc;
pub mod pass_newfunc;
pub mod pass_statechange;
pub mod pass_refs;
pub mod pass_jumptable;
pub mod pass_splitfunc;

// ------------------------------------------------------------------------------------------------
// Analysis items
// ------------------------------------------------------------------------------------------------

mod item_types {
	use super::*;
	#[derive(Debug, PartialEq, Eq, Copy, Clone)]
	pub(super) struct NewFunc(pub EA, pub MmuState);

	#[derive(Debug, PartialEq, Eq, Copy, Clone)]
	pub(super) struct SplitFunc(pub EA);

	#[derive(Debug, PartialEq, Eq, Copy, Clone)]
	pub(super) struct StateChange(pub FuncId);

	#[derive(Debug, PartialEq, Eq, Copy, Clone)]
	pub(super) struct FuncRefs(pub FuncId);

	#[derive(Debug, PartialEq, Eq, Copy, Clone)]
	pub(super) struct JumpTable(pub EA);
}

/// Things that can be put onto the analysis queue.
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub(crate) enum AnalysisItem {
	/// Explore an unexplored function.
	NewFunc(EA, MmuState),
	/// Split an existing function (another function called/jumped to the given EA).
	SplitFunc(EA),
	/// Determine if/how this function changes MMU state and propagate state changes to its BBs.
	StateChange(FuncId),
	/// Resolve references outside the function.
	FuncRefs(FuncId),
	/// A jump table to be analyzed. The EA points to the jump instruction.
	JumpTable(EA),
}

// ------------------------------------------------------------------------------------------------
// Analyzer
// ------------------------------------------------------------------------------------------------

pub(crate) struct Analyzer {
	new_func_queue:     VecDeque<item_types::NewFunc>,
	split_func_queue:   VecDeque<item_types::SplitFunc>,
	state_change_queue: VecDeque<item_types::StateChange>,
	func_refs_queue:    VecDeque<item_types::FuncRefs>,
	jump_table_queue:   VecDeque<item_types::JumpTable>,
}

impl Analyzer {
	pub(crate) fn new() -> Self {
		Self {
			new_func_queue:     VecDeque::new(),
			split_func_queue:   VecDeque::new(),
			state_change_queue: VecDeque::new(),
			func_refs_queue:    VecDeque::new(),
			jump_table_queue:   VecDeque::new(),
		}
	}

	/// Puts an EA on the queue that should be the start of a function.
	pub fn enqueue_function(&mut self, state: MmuState, ea: EA) {
		self.new_func_queue.push_back(item_types::NewFunc(ea, state))
	}

	/// Puts an EA on the queue that should be the jump instruction for a jump table.
	pub fn enqueue_jump_table(&mut self, ea: EA) {
		self.jump_table_queue.push_back(item_types::JumpTable(ea))
	}

	/// Schedules a function to have its references analyzed.
	fn enqueue_func_refs(&mut self, fid: FuncId) {
		self.func_refs_queue.push_back(item_types::FuncRefs(fid));
	}

	/// Schedules a function to have its MMU state changes analyzed.
	fn enqueue_state_change(&mut self, fid: FuncId) {
		self.state_change_queue.push_back(item_types::StateChange(fid));
	}

	/// Schedules a function to attempt to be split at the given EA.
	fn enqueue_split_func(&mut self, ea: EA) {
		self.split_func_queue.push_back(item_types::SplitFunc(ea));
	}

	fn pop(&mut self) -> Option<AnalysisItem> {
		// TODO: make this prettier.
		use item_types::*;
		if let Some(NewFunc(ea, state)) = self.new_func_queue.pop_front() {
			Some(AnalysisItem::NewFunc(ea, state))
		} else if let Some(SplitFunc(ea)) = self.split_func_queue.pop_front() {
			Some(AnalysisItem::SplitFunc(ea))
		} else if let Some(StateChange(fid)) = self.state_change_queue.pop_front() {
			Some(AnalysisItem::StateChange(fid))
		} else if let Some(FuncRefs(fid)) = self.func_refs_queue.pop_front() {
			Some(AnalysisItem::FuncRefs(fid))
		} else if let Some(JumpTable(ea)) = self.jump_table_queue.pop_front() {
			Some(AnalysisItem::JumpTable(ea))
		} else {
			None
		}
	}
}

// ------------------------------------------------------------------------------------------------
// analyze_queue
// ------------------------------------------------------------------------------------------------

impl Program
{
	/// Analyzes all items in the analysis queue. Analysis may generate more items to analyze,
	/// so this can do a lot of work in a single call.
	pub fn analyze_queue(&mut self) {
		while let Some(item) = self.queue.pop() {
			use AnalysisItem::*;
			match item {
				NewFunc(ea, state) => self.new_func_pass(ea, state),
				StateChange(fid)   => self.state_change_pass(fid),
				FuncRefs(fid)      => self.func_refs_pass(fid),
				JumpTable(ea)      => self.jump_table_pass(ea),
				SplitFunc(ea)      => self.split_func_pass(ea),
			}
		}
	}
}