
use crate::program::{ Program, FuncId };
use crate::memory::{ MmuState, EA };
use crate::dataflow::{ WorkQueue };

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
	new_func_queue:     WorkQueue<(EA, MmuState)>,
	split_func_queue:   WorkQueue<EA>,
	state_change_queue: WorkQueue<FuncId>,
	func_refs_queue:    WorkQueue<FuncId>,
	jump_table_queue:   WorkQueue<EA>,
}

impl Analyzer {
	pub(crate) fn new() -> Self {
		Self {
			new_func_queue:     WorkQueue::new(10),
			split_func_queue:   WorkQueue::new(10),
			state_change_queue: WorkQueue::new(10),
			func_refs_queue:    WorkQueue::new(10),
			jump_table_queue:   WorkQueue::new(10),
		}
	}

	/// Puts an EA on the queue that should be the start of a function.
	pub fn enqueue_function(&mut self, state: MmuState, ea: EA) {
		self.new_func_queue.enqueue((ea, state))
	}

	/// Puts an EA on the queue that should be the jump instruction for a jump table.
	pub fn enqueue_jump_table(&mut self, ea: EA) {
		self.jump_table_queue.enqueue(ea)
	}

	/// Schedules a function to have its references analyzed.
	fn enqueue_func_refs(&mut self, fid: FuncId) {
		self.func_refs_queue.enqueue(fid);
	}

	/// Schedules a function to have its MMU state changes analyzed.
	fn enqueue_state_change(&mut self, fid: FuncId) {
		self.state_change_queue.enqueue(fid);
	}

	/// Schedules a function to attempt to be split at the given EA.
	fn enqueue_split_func(&mut self, ea: EA) {
		self.split_func_queue.enqueue(ea);
	}

	fn pop(&mut self) -> Option<AnalysisItem> {
		// TODO: make this prettier.
		if let Some((ea, state)) = self.new_func_queue.dequeue() {
			Some(AnalysisItem::NewFunc(ea, state))
		} else if let Some(ea) = self.split_func_queue.dequeue() {
			Some(AnalysisItem::SplitFunc(ea))
		} else if let Some(fid) = self.state_change_queue.dequeue() {
			Some(AnalysisItem::StateChange(fid))
		} else if let Some(fid) = self.func_refs_queue.dequeue() {
			Some(AnalysisItem::FuncRefs(fid))
		} else if let Some(ea) = self.jump_table_queue.dequeue() {
			Some(AnalysisItem::JumpTable(ea))
		} else {
			None
		}
	}
}

// ------------------------------------------------------------------------------------------------
// analyze_queue
// ------------------------------------------------------------------------------------------------

impl Program {
	/// Analyzes all items in the analysis queue. Analysis may generate more items to analyze,
	/// so this can do a lot of work in a single call.
	pub fn analyze_queue(&mut self) {
		while let Some(item) = self.queue.pop() {
			match item {
				AnalysisItem::NewFunc(ea, state) => self.new_func_pass(ea, state),
				AnalysisItem::StateChange(fid)   => self.state_change_pass(fid),
				AnalysisItem::FuncRefs(fid)      => self.func_refs_pass(fid),
				AnalysisItem::JumpTable(ea)      => self.jump_table_pass(ea),
				AnalysisItem::SplitFunc(ea)      => self.split_func_pass(ea),
			}
		}
	}
}