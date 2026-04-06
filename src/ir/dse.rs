
use super::*;

// ------------------------------------------------------------------------------------------------
// Dead store elimination
// ------------------------------------------------------------------------------------------------

pub(super) fn elim_dead_stores(bbs: &mut [IrBasicBlock]) {
	for (reg, def) in find_defs_and_uses(bbs).iter() {
		if !def.is_really_used() {
			match def.loc() {
				DefLocation::Inst { bbid, instn } => {
					log::trace!("{:?} is dead", reg);
					*bbs[bbid].insts[instn].kind_mut() = IrInstKind::Nop;
				}
				DefLocation::Phi { bbid, phin } => {
					log::trace!("{:?} is dead, BUT IT'S DEF'D BY A PHI! bb{} phi{}",
						reg, bbid, phin);
				}
				DefLocation::Arg => {
					log::trace!("{:?} is dead, but it's an arg, so it'll get pruned", reg);
				}
			}
		}
	}
}