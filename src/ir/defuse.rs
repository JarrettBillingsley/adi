
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
	/// Used as an argument to the `phin`th phi function of `bbid`.
	///
	/// Note this is never in the map returned by find_defs_and_uses - they will all be either
	/// `None` or `Real`. This is used internally by the algorithm.
	Phi { bbid: IrBBId, phin: usize },
	/// Used by at least one "real" instruction.
	Real,
}

/// Where a register was defined.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub(crate) enum DefLocation {
	/// It's an argument, so it has no definition location.
	Arg,
	/// Defined by the `phin`th phi function of `bbid`.
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

	/// how the register is used.
	pub(crate) fn how_used(&self) -> DefUseKind {
		self.use_kind
	}

	/// the location where it was defined.
	pub(crate) fn loc(&self) -> DefLocation {
		self.loc
	}

	pub(crate) fn is_used(&self) -> bool {
		!matches!(self.use_kind, DefUseKind::None)
	}

	/// mark it as used by phi function.
	fn mark_phi_used(&mut self, bbid: IrBBId, phin: usize) {
		if self.use_kind == DefUseKind::None {
			self.use_kind = DefUseKind::Phi { bbid, phin };
		}
	}

	fn mark_used(&mut self) {
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

	let ensure_in_defs = |defs: &mut DefMap, reg: &IrReg| {
		if !defs.contains_key(&reg) {
			assert!(reg.is_gen0());
			defs.insert(*reg, DefInfo::new_arg());
		}
	};

	// then find all uses of those defs
	for bb in bbs.iter() {
		for (phin, phi) in bb.phis().enumerate() {
			for reg in phi.args() {
				ensure_in_defs(&mut defs, reg);
				// SAFETY: see above
				defs.get_mut(&reg).unwrap().mark_phi_used(bb.id, phin);
			}
		}

		for inst in bb.insts() {
			// despite looking like a "use," a clobber instruction doesn't really mark the use of
			// a register - it only records the current generation of the register before leaving
			// the function. visit_uses does visit that register to make the SSA algo work tho.
			if !inst.is_clobber() {
				inst.visit_uses(|reg| {
					ensure_in_defs(&mut defs, &reg);
					// SAFETY: see above
					defs.get_mut(&reg).unwrap().mark_used();
				});
			}
		}
	}

	// finally, for each register with DefUseKind::Phi, follow the chain of regs def'd by that phi
	// and any subsequent phis until we get to the actual usage - Real or None.
	let mut true_use_kinds: HashMap<IrReg, DefUseKind> = HashMap::new();

	for (&reg, info) in defs.iter() {
		if let DefUseKind::Phi { bbid, phin } = info.use_kind {
			let mut final_reg = bbs[bbid].get_phi(phin).dst_reg();
			let mut final_info = defs.get(&final_reg).unwrap();

			while let DefUseKind::Phi { bbid, phin } = final_info.use_kind {
				final_reg = bbs[bbid].get_phi(phin).dst_reg();
				final_info = defs.get(&final_reg).unwrap();
			}

			true_use_kinds.insert(reg, final_info.use_kind);
		}
	}

	// log::trace!("DEFUSE TRUE USE KINDS:\n{:#?}", true_use_kinds);

	for (reg, use_kind) in true_use_kinds.into_iter() {
		// SAFETY: it's gotta be in here.
		defs.get_mut(&reg).unwrap().use_kind = use_kind;
	}

	// log::trace!("DEFUSE RETURNING:\n{:#?}", defs);

	defs
}