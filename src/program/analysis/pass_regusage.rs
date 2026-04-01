
use crate::program::{ Program };

// ------------------------------------------------------------------------------------------------
// Whole-program register usage
// ------------------------------------------------------------------------------------------------

impl Program {
	pub(super) fn reg_usage_pass(&mut self) {
		log::info!("register usage");
		// TODO: if a function's reg usage has been analyzed before, its *arguments and clobbers*
		// cannot have changed, but its *return* regs may have.

		// TODO: redo state change analysis on every function which changed???
	}
}
