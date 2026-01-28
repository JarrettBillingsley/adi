
use super::*;

// ------------------------------------------------------------------------------------------------
// Dead store elimination
// ------------------------------------------------------------------------------------------------

// This runs after phi pruning, so anything def'ed by a phi is definitely used, so we don't
// check those, only vars def'ed by instructions.
pub(crate) fn elim_dead_stores(bbs: &mut [IrBasicBlock]) {
	let defs = find_defs_and_uses(bbs);

	for (reg, def) in defs.iter() {
		if !def.used() {
			// TODO: uhhhhhhh actually eliminate the dead stores lmao
			println!("{:?} is dead", reg);
		}
	}
}