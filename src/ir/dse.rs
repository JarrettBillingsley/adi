
use crate::program::{ FuncRegUsage };
use super::*;

// ------------------------------------------------------------------------------------------------
// Dead store elimination
// ------------------------------------------------------------------------------------------------

// This runs after phi pruning, so anything def'ed by a phi is definitely used, so we don't
// check those, only vars def'ed by instructions.
pub(super) fn elim_dead_stores(bbs: &mut [IrBasicBlock], _reg_usage: FuncRegUsage) {
	for (reg, def) in find_defs_and_uses(bbs).iter() {
		if let Some((irbbid, instn)) = def.loc() && def.is_unused() {
			log::trace!("{:?} is dead", reg);
			*bbs[irbbid].insts[instn].kind_mut() = IrInstKind::Nop;
		}
	}
}