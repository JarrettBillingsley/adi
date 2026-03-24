use std::fmt::{ Debug, Display, Formatter, Result as FmtResult };

use std::ops::{ Add, AddAssign, Sub, SubAssign };
// use std::fmt::{ Debug, UpperHex, Formatter, Result as FmtResult };

use crate::{ Offs };
use crate::memory::{ SegId };

// ------------------------------------------------------------------------------------------------
// EA
// ------------------------------------------------------------------------------------------------

/// A unique location consisting of segment ID and an offset within that Segment. EAs can be
/// resolved or unresolved.
///
/// A **resolved EA** consists of a segment ID and an offset into that segment.
///
/// An **unresolved EA** has a segment ID of `SegId::unresolved()`, and its offset is a VA which
/// could not be mapped to a known EA. This happens sometimes - not everything can be determined
/// through static analysis.
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub struct EA(Offs);

impl Display for EA {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		write!(f, "{:04X}:{:08X}", self.seg().id, self.offs())
	}
}

const SEG_MASK: Offs  = 0xFFFF0000_00000000;
const OFFS_MASK: Offs = 0x0000FFFF_FFFFFFFF;
const SEG_SHIFT: usize = 48;

impl EA {
	/// Make a new EA from a segment ID and offset.
	///
	/// Panics if the offset is too big (more than 48 bits).
	pub fn new(seg: SegId, offs: Offs) -> Self {
		assert!((offs as Offs) & SEG_MASK == 0);
		Self(((seg.id as Offs) << SEG_SHIFT) | (offs as Offs))
	}

	/// Make a new unresolved EA with the given VA embedded in it.
	pub fn unresolved(offs: Offs) -> Self {
		Self::new(SegId::unresolved(), offs)
	}

	/// Is this EA unresolved?
	pub fn is_unresolved(&self) -> bool {
		self.seg().is_unresolved()
	}

	/// Is this EA resolved?
	pub fn is_resolved(&self) -> bool {
		!self.seg().is_unresolved()
	}

	/// The segment ID of this EA.
	#[inline]
	pub fn seg(&self) -> SegId { SegId::unchecked((self.0 >> SEG_SHIFT) as u16) }

	/// The offset of this EA.
	#[inline]
	pub fn offs(&self) -> Offs { self.0 & OFFS_MASK }

	/// Set the offset of this EA.
	#[inline]
	fn set_offs(&mut self, new_offs: Offs) {
		assert!(new_offs & SEG_MASK == 0);
		self.0 &= !OFFS_MASK;
		self.0 |= new_offs;
	}
}

impl Debug for EA {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		write!(f, "{}", self)
	}
}

impl Add<Offs> for EA {
	type Output = Self;
	fn add(self, other: Offs) -> Self {
		EA::new(self.seg(), self.offs() + other)
	}
}

impl AddAssign<Offs> for EA {
	fn add_assign(&mut self, other: Offs) {
		self.set_offs(self.offs() + other);
	}
}

impl Sub<Offs> for EA {
	type Output = Self;
	fn sub(self, other: Offs) -> Self {
		EA::new(self.seg(), self.offs() - other)
	}
}

impl SubAssign<Offs> for EA {
	fn sub_assign(&mut self, other: Offs) {
		self.set_offs(self.offs() - other);
	}
}