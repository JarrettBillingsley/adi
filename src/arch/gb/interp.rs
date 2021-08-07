
use crate::arch::{ IInterpreter, ValueKind };
use crate::memory::{ Memory, EA, MmuState };
use crate::program::{ BasicBlock };

// ------------------------------------------------------------------------------------------------
// Interpreter
// ------------------------------------------------------------------------------------------------

pub struct GBInterpreter;

impl GBInterpreter {
	pub(super) fn new() -> Self {
		Self {}
	}
}

impl IInterpreter for GBInterpreter {
	fn reset(&mut self) {}

	fn interpret_bb(&mut self, _mem: &Memory, _bb: &BasicBlock, _state: Option<MmuState>)
	-> Option<EA> {
		None
	}

	fn last_mmu_state_change(&self) -> Option<(MmuState, ValueKind)> {
		None
	}
}
