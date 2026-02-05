
use crate::program::{ Program, FuncId };
use crate::memory::{ MmuState, EA };
use crate::dataflow::{ WorkQueue };

// ------------------------------------------------------------------------------------------------
// Submodules
// ------------------------------------------------------------------------------------------------

pub mod misc;
pub mod pass_newfunc;
pub mod pass_splitfunc;
pub mod pass_statechange;
pub mod pass_refs;
pub mod pass_jumptable;

// ------------------------------------------------------------------------------------------------
// AnalysisQueue
// ------------------------------------------------------------------------------------------------

pub(crate) struct AnalysisQueue {
	new_func_queue:     WorkQueue<(EA, MmuState)>,
	split_func_queue:   WorkQueue<EA>,
	state_change_queue: WorkQueue<FuncId>,
	func_refs_queue:    WorkQueue<FuncId>,
	jump_table_queue:   WorkQueue<EA>,
}

impl AnalysisQueue {
	pub(crate) fn new() -> Self {
		Self {
			new_func_queue:     WorkQueue::new(50),
			split_func_queue:   WorkQueue::new(50),
			state_change_queue: WorkQueue::new(50),
			func_refs_queue:    WorkQueue::new(50),
			jump_table_queue:   WorkQueue::new(50),
		}
	}

	/// Puts an EA on the queue that should be the start of a function.
	pub(crate) fn enqueue_new_func(&mut self, state: MmuState, ea: EA) {
		self.new_func_queue.enqueue((ea, state))
	}

	/// Schedules a function to attempt to be split at the given EA.
	fn enqueue_split_func(&mut self, ea: EA) {
		self.split_func_queue.enqueue(ea);
	}

	/// Schedules a function to have its MMU state changes analyzed.
	fn enqueue_state_change(&mut self, fid: FuncId) {
		self.state_change_queue.enqueue(fid);
	}

	/// Schedules a function to have its references analyzed.
	fn enqueue_func_refs(&mut self, fid: FuncId) {
		self.func_refs_queue.enqueue(fid);
	}

	/// Puts an EA on the queue that should be the jump instruction for a jump table.
	pub(crate) fn enqueue_jump_table(&mut self, ea: EA) {
		self.jump_table_queue.enqueue(ea)
	}
}

// ------------------------------------------------------------------------------------------------
// analyze_queue
// ------------------------------------------------------------------------------------------------

impl Program {
	/// Analyzes all items in the analysis queue. Analysis may generate more items to analyze,
	/// so this can do a lot of work in a single call.
	pub fn analyze_queue(&mut self) {
		loop {
			// this if-let-else chain defines the pass priority. it's janky but it works.
			if let Some((ea, state)) = self.queue.new_func_queue.dequeue() {
				self.new_func_pass(ea, state);
				continue;
			} else if let Some(ea) = self.queue.split_func_queue.dequeue() {
				self.split_func_pass(ea);
				continue;
			} else if let Some(fid) = self.queue.state_change_queue.dequeue() {
				self.state_change_pass(fid);
				continue;
			} else if let Some(fid) = self.queue.func_refs_queue.dequeue() {
				self.func_refs_pass(fid);
				continue;
			} else if let Some(ea) = self.queue.jump_table_queue.dequeue() {
				self.jump_table_pass(ea);
				continue;
			}

			break;
		}
	}
}