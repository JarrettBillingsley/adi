
use std::collections::{ HashMap };
use super::*;

// ------------------------------------------------------------------------------------------------
// DefInfo, DefMap
// ------------------------------------------------------------------------------------------------

/// Information about a single IR register def.
pub(crate) struct DefInfo {
	used: bool,
	bbid: IrBBId,
	inst: usize,
}

impl DefInfo {
	fn new(bbid: IrBBId, inst: usize) -> Self {
		Self { used: false, bbid, inst }
	}

	/// returns `true` if the register is used anywhere. If not, the IR instruction which assigns
	/// it can be safely removed.
	pub(crate) fn used(&self) -> bool {
		self.used
	}

	/// the ID of the IR BB where it is defined.
	pub(crate) fn bb(&self) -> IrBBId {
		self.bbid
	}

	/// the instruction index into the above IR BB where it is defined.
	pub(crate) fn inst(&self) -> usize {
		self.inst
	}

	fn mark_used(&mut self) {
		self.used = true;
	}
}

/// Return type for `find_defs_and_uses`. Maps from IR registers to their `DefInfo`. If a register
/// is not present in this map, it's because it's a generation 0 (i.e. a function argument), so
/// its definition is implicit.
pub(crate) type DefMap = HashMap<IrReg, DefInfo>;

// ------------------------------------------------------------------------------------------------
// Finding defs
// ------------------------------------------------------------------------------------------------

/// Find all defs and uses of those defs in the given list of `IrBasicBlock`. The returned map
/// can be used to do DSE (dead store elimination) or simply to look up where certain IR regs
/// are defined.
///
/// Linear time in the number of instructions.
pub(crate) fn find_defs_and_uses(bbs: &[IrBasicBlock]) -> DefMap {
	let mut defs = DefMap::new();

	// first find all defs
	for bb in bbs.iter() {
		for (i, inst) in bb.insts().enumerate() {
			if let Some(reg) = inst.dst_reg() {
				defs.insert(reg, DefInfo::new(bb.id, i));
			}
		}
	}

	// then find all uses of those defs
	for bb in bbs.iter() {
		for phi in bb.phis() {
			for arg in phi.args() {
				// _0 vars aren't gonna be in the defs map, hence this check.
				if let Some(def) = defs.get_mut(arg) {
					def.mark_used();
				}
			}
		}

		for inst in bb.insts() {
			inst.visit_uses(|reg| {
				if let Some(def) = defs.get_mut(&reg) {
					def.mark_used();
				}
			});
		}
	}

	defs
}