
use std::fmt::{ Debug, Display };

use crate::memory::{ VA, Location };

/// Newtype for MMU configuration state. The interpretation of this type is up to each
/// implementor of `IMmu`.
#[derive(Debug, Default, PartialEq, Eq, Copy, Clone)]
pub struct MmuState(u128);

impl MmuState {
	pub fn to_usize(&self) -> usize {
		self.0 as usize
	}
}

/// Trait for MMUs (memory management units), which abstract the VA-to-Location mapping.
///
/// The "MMU" broadly means "the hardware that decides what physical device a given virtual
/// address refers to." This can be built into a system (hardwired), or the system might
/// have registers to do banking, or the cartridges might have hardware to do that.
pub trait IMmu: Debug + Display + Sync + Send {
	/// Get the initial configuration state of this MMU.
	fn initial_state(&self) -> MmuState;

	/// Given a particular configuration state and VA, get the Location it refers to, if any.
	fn loc_for_va(&self, state: MmuState, va: VA) -> Option<Location>;

	/// Given a particular configuration state and Location, get the VA that corresponds to it,
	/// if any.
	fn va_for_loc(&self, state: MmuState, loc: Location) -> Option<VA>;

	/// Come up with an autogenerated name prefix for a given VA.
	fn name_prefix_for_va(&self, state: MmuState, va: VA) -> String;
}