
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
// Analyzer
// ------------------------------------------------------------------------------------------------

/// Things that can be put onto the analysis queue.
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub(crate) enum AnalysisItem {
	/// Explore an unexplored function.
	NewFunc(EA, MmuState),
	/// Determine if/how this function changes MMU state and propagate state changes to its BBs.
	StateChange(FuncId),
	/// Resolve references outside the function.
	FuncRefs(FuncId),
	/// A jump table to be analyzed. The EA points to the jump instruction.
	JumpTable(EA),
	/// Split an existing function (another function called/jumped to the given EA).
	SplitFunc(EA),
}

impl Program {
	/// Puts an EA on the queue that should be the start of a function.
	pub fn enqueue_function(&mut self, state: MmuState, ea: EA) {
		self.queue.push_back(AnalysisItem::NewFunc(ea, state))
	}

	/// Puts an EA on the queue that should be the jump instruction for a jump table.
	pub fn enqueue_jump_table(&mut self, ea: EA) {
		self.queue.push_back(AnalysisItem::JumpTable(ea))
	}

	/// Schedules a function to have its references analyzed.
	fn enqueue_func_refs(&mut self, fid: FuncId) {
		self.queue.push_back(AnalysisItem::FuncRefs(fid));
	}

	/// Schedules a function to have its MMU state changes analyzed.
	fn enqueue_state_change(&mut self, fid: FuncId) {
		self.queue.push_back(AnalysisItem::StateChange(fid));
	}

	/// Schedules a function to attempt to be split at the given EA.
	fn enqueue_split_func(&mut self, ea: EA) {
		self.queue.push_back(AnalysisItem::SplitFunc(ea));
	}

	/// Analyzes all items in the analysis queue. Analysis may generate more items to analyze,
	/// so this can do a lot of work in a single call.
	pub fn analyze_queue(&mut self) {
		while let Some(item) = self.queue.pop_front() {
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