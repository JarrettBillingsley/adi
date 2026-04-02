
use crate::fxhash::{ FxHashMap as HashMap, FxHashMapEx };

use super::*;

// ------------------------------------------------------------------------------------------------
// DefInfo, DefMap
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

/// Information about a single IR register def.
pub(crate) struct DefInfo {
	use_kind: DefUseKind,
	// None for _0 regs, and Some for all others. `(bbid, instruction index)`
	loc: Option<(IrBBId, usize)>,
}

impl DefInfo {
	fn new_gen0() -> Self {
		Self {
			use_kind: DefUseKind::None,
			loc:      None
		}
	}

	fn new(bbid: IrBBId, inst: usize) -> Self {
		Self {
			use_kind: DefUseKind::None,
			loc:      Some((bbid, inst)),
		}
	}

	/// how the register is used - either for real, or only in dummy `use` instructions, or never
	/// used (in which case its def can be pruned).
	pub(crate) fn how_used(&self) -> DefUseKind {
		self.use_kind
	}

	/// the location where it was defined as a tuple `(BB ID, instruction index)`, or `None` if it's
	/// a zero-generation reg.
	pub(crate) fn loc(&self) -> Option<(IrBBId, usize)> {
		self.loc
	}

	fn mark_dummy_used(&mut self) {
		if matches!(self.use_kind, DefUseKind::None) {
			self.use_kind = DefUseKind::OnlyDummy;
		}
		// otherwise, leave it as is
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
				if let Some(arg) = defs.get_mut(&arg) {
					arg.mark_really_used();
				}
			}
		}

		for inst in bb.insts() {
			let is_dummy = matches!(inst.kind(), IrInstKind::Use { .. });

			inst.visit_uses(|reg| {
				if !defs.contains_key(&reg) {
					assert!(reg.is_gen0());
					defs.insert(reg, DefInfo::new_gen0());
				}

				// SAFETY: see above
				let u = defs.get_mut(&reg).unwrap();

				if is_dummy {
					u.mark_dummy_used();
				} else {
					u.mark_really_used();
				}
			});
		}
	}

	defs
}