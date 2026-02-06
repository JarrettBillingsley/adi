
use log::*;

use crate::program::{ Program };
use crate::memory::{ EA };

// ------------------------------------------------------------------------------------------------
// Jump table analysis
// ------------------------------------------------------------------------------------------------

impl Program {
	pub(super) fn jump_table_pass(&mut self, ea: EA) {
		info!("WOWOWOW there's a jumptable at {}", ea);
		// todo!()
	}
}
