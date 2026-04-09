
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

	/// Used, but only by one or more kinds of "weird" uses.
	Weird { clobber: bool, use_: bool, phi: bool },

	/// Used by at least one "real" instruction.
	Real,
}

impl DefUseKind {
	fn clobber() -> Self {
		Self::Weird { clobber: true, use_: false, phi: false }
	}

	fn use_() -> Self {
		Self::Weird { clobber: false, use_: true, phi: false }
	}

	fn phi() -> Self {
		Self::Weird { clobber: false, use_: false, phi: true }
	}
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
#[derive(Debug)]
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
		// matches!(self.use_kind, DefUseKind::Real | DefUseKind::OnlyPhi | DefUseKind::DummyAndPhi)
		!matches!(self.use_kind, DefUseKind::None)
	}

	/// mark it as used by a `clobber` instruction.
	fn mark_clobbered(&mut self) {
		match &mut self.use_kind {
			DefUseKind::None                  => self.use_kind = DefUseKind::clobber(),
			DefUseKind::Weird { clobber, .. } => *clobber = true,
			DefUseKind::Real                  => {}
		}
	}

	/// mark it as used by a dummy `use` instruction.
	fn mark_dummy_used(&mut self) {
		match &mut self.use_kind {
			DefUseKind::None               => self.use_kind = DefUseKind::use_(),
			DefUseKind::Weird { use_, .. } => *use_ = true,
			DefUseKind::Real               => {}
		}
	}

	/// mark it as used by phi function.
	fn mark_phi_used(&mut self) {
		match &mut self.use_kind {
			DefUseKind::None              => self.use_kind = DefUseKind::phi(),
			DefUseKind::Weird { phi, .. } => *phi = true,
			DefUseKind::Real              => {}
		}
	}

	fn mark_really_used(&mut self) {
		self.use_kind = DefUseKind::Real;
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

				// SAFETY: see above
				defs.get_mut(&reg).unwrap().mark_phi_used();
			}
		}

		for inst in bb.insts() {
			inst.visit_uses(|reg| {
				if !defs.contains_key(&reg) {
					assert!(reg.is_gen0());
					defs.insert(reg, DefInfo::new_arg());
				}

				// SAFETY: see above
				let usage = defs.get_mut(&reg).unwrap();

				if inst.is_dummy_use() {
					usage.mark_dummy_used();
				} else if inst.is_clobber() {
					usage.mark_clobbered();
				} else {
					usage.mark_really_used();
				}
			});
		}
	}

	defs
}