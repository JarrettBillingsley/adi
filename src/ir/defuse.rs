
use crate::fxhash::{ FxHashMap as HashMap, FxHashMapEx };

use super::*;

// ------------------------------------------------------------------------------------------------
// DefUseKind, DefLocation, DefInfo, DefMap
// ------------------------------------------------------------------------------------------------

/// Ways in which a register is used.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub(crate) enum DefUseKind {
	/// Not used anywhere.
	None,

	/// Used, but only by dummy `use` instructions.
	OnlyDummy,

	/// Used by at least one "real" instruction or phi function.
	Real,
}

/// Where a register was defined.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub(crate) enum DefLocation {
	/// It's an argument, so it has no definition location.
	Arg,
	/// Defined by the `phin`th phi function at the start of `bbid`.
	Phi  { bbid: IrBBId, phin:  usize },
	/// Defined by the `instn`th instruction of `bbid`.
	Inst { bbid: IrBBId, instn: usize },
}

/// Information about a single IR register def.
pub(crate) struct DefInfo {
	use_kind: DefUseKind,
	loc:      DefLocation,
}

impl DefInfo {
	fn new_inst(bbid: IrBBId, instn: usize) -> Self {
		Self {
			use_kind: DefUseKind::None,
			loc:      DefLocation::Inst { bbid, instn },
		}
	}

	fn new_arg() -> Self {
		Self {
			use_kind: DefUseKind::None,
			loc:      DefLocation::Arg,
		}
	}

	fn new_phi(bbid: IrBBId, phin: usize) -> Self {
		Self {
			use_kind: DefUseKind::None,
			loc:      DefLocation::Phi { bbid, phin },
		}
	}

	/// how the register is used - either for real, or only in dummy `use` instructions, or never
	/// used (in which case its def can be pruned).
	pub(crate) fn how_used(&self) -> DefUseKind {
		self.use_kind
	}

	/// the location where it was defined.
	pub(crate) fn loc(&self) -> DefLocation {
		self.loc
	}

	pub(crate) fn is_really_used(&self) -> bool {
		matches!(self.use_kind, DefUseKind::Real)
	}

	fn mark_used(&mut self, is_dummy: bool) {
		if is_dummy {
			if matches!(self.use_kind, DefUseKind::None) {
				self.use_kind = DefUseKind::OnlyDummy;
			}
			// otherwise, leave it as is
		} else {
			self.use_kind = DefUseKind::Real;
		}
	}
}

/// Return type for `find_defs_and_uses`. Maps from IR registers to their `DefInfo`.
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
		for (i, phi) in bb.phis().enumerate() {
			defs.insert(phi.dst_reg(), DefInfo::new_phi(bb.id, i));
		}

		for (i, inst) in bb.insts().enumerate() {
			if let Some(reg) = inst.dst_reg() {
				defs.insert(reg, DefInfo::new_inst(bb.id, i));
			}
		}
	}

	// then find all uses of those defs
	for bb in bbs.iter() {
		for phi in bb.phis() {
			for reg in phi.args() {
				if !defs.contains_key(&reg) {
					assert!(reg.is_gen0());
					defs.insert(*reg, DefInfo::new_arg());
				}

				log::trace!(" marking bb{} phi arg {:?} as used", bb.id, reg);
				// SAFETY: see above
				defs.get_mut(&reg).unwrap().mark_used(false);
			}
		}

		for inst in bb.insts() {
			inst.visit_uses(|reg| {
				if !defs.contains_key(&reg) {
					assert!(reg.is_gen0());
					defs.insert(reg, DefInfo::new_arg());
				}

				// SAFETY: see above
				defs.get_mut(&reg).unwrap().mark_used(inst.is_dummy_use());
			});
		}
	}

	defs
}