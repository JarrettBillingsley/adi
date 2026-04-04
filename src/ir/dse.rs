
use crate::program::{ FuncRegUsage };
use super::*;

// ------------------------------------------------------------------------------------------------
// Dead store elimination
// ------------------------------------------------------------------------------------------------

pub(super) fn elim_dead_stores(bbs: &mut [IrBasicBlock], _reg_usage: FuncRegUsage) {
	for (reg, def) in find_defs_and_uses(bbs).iter() {
		if !def.is_really_used() &&
		let DefLocation::Inst { bbid, instn } = def.loc() {
			log::trace!("{:?} is dead", reg);
			*bbs[bbid].insts[instn].kind_mut() = IrInstKind::Nop;
		}
	}
}