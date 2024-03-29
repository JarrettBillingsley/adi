
use std::fmt::{ Debug, Display };

use parse_display::Display;
use enum_dispatch::enum_dispatch;

use crate::program::{ Instruction };
use crate::memory::{ VA, EA };
use crate::platform::{ GBMmu, NesMmu, ToyMmu };

/// Newtype for MMU configuration state. The interpretation of this type is up to each
/// implementor of `IMmu`.
#[derive(Debug, Default, PartialEq, Eq, Copy, Clone)]
pub struct MmuState(u128);

impl MmuState {
	pub fn from_usize(v: usize) -> Self {
		Self(v as u128)
	}

	pub fn to_usize(&self) -> usize {
		self.0 as usize
	}
}

#[enum_dispatch]
#[derive(Debug, Display)]
pub enum Mmu {
	#[display("{0}")]
	GBMmu,
	#[display("{0}")]
	NesMmu,
	#[display("{0}")]
	ToyMmu,
}

/// How an instruction can possibly change the state of an MMU.
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum StateChange {
	/// Definitely does not/cannot change the state.
	None,

	/// Might change it, but not enough info to know yet. For example, if `0x1000` is an address
	/// that changes MMU state, and an instruction writes to `0xFF0 + X` for some register X, it
	/// may change the state if `X == 16`.
	Maybe,

	/// Definitely changes it, but in a way that cannot be statically determined. For example,
	/// if `0x1000` is an address that changes MMU state, and an instruction writes into `0x1000`,
	/// but the value it writes was loaded from a global variable.
	Dynamic,

	/// Changes it to the given state, statically determinable.
	Static(MmuState),
}

impl StateChange {
	pub fn is_none(&self) -> bool { *self == StateChange::None }
	pub fn is_some(&self) -> bool { !self.is_none() }
}

/// Trait for MMUs (memory management units), which abstract the VA-to-EA mapping.
///
/// The "MMU" broadly means "the hardware that decides what physical device a given virtual
/// address refers to." This can be built into a system (hardwired), or the system might
/// have registers to do banking, or the cartridges might have hardware to do that.
#[enum_dispatch(Mmu)]
pub trait IMmu: Debug + Display + Sync + Send {
	/// Get the initial configuration state of this MMU.
	fn initial_state(&self) -> MmuState;

	/// Given a particular configuration state and VA, get the EA it refers to, if any.
	fn ea_for_va(&self, state: MmuState, va: VA) -> Option<EA>;

	/// Given a particular configuration state and EA, get the VA that corresponds to it, if any.
	fn va_for_ea(&self, state: MmuState, ea: EA) -> Option<VA>;

	/// Come up with an autogenerated name prefix for a given VA.
	fn name_prefix_for_va(&self, state: MmuState, va: VA) -> String;

	/// Given an instruction, tells how that instruction changes the state.
	fn inst_state_change(&self, state: MmuState, inst: &Instruction) -> StateChange;

	/// Given an old state, a value to write, and an address to write it, produce the new MMU
	/// state that would result from such a write.
	fn write(&self, old: MmuState, addr: VA, val: usize) -> MmuState;
}