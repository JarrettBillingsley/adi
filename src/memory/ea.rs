use std::fmt::{ Debug, Display, Formatter, Result as FmtResult };

use std::ops::{ Add, AddAssign, Sub, SubAssign };
// use std::fmt::{ Debug, UpperHex, Formatter, Result as FmtResult };

use crate::memory::{ SegId };

// ------------------------------------------------------------------------------------------------
// EA
// ------------------------------------------------------------------------------------------------

/// A unique location consisting of Segment ID and an offset within that Segment. EAs can be valid
/// or invalid.
///
/// A **valid EA** consists of a segment ID and an offset into that segment.
///
/// An **invalid EA** has a segment ID of `SegId::invalid()`, and its offset may be a VA which could
/// not be mapped to a valid EA.
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct EA(u64);

impl Display for EA {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		write!(f, "{:04X}:{:08X}", self.seg().0, self.offs())
	}
}

const SEG_MASK: u64  = 0xFFFF0000_00000000;
const OFFS_MASK: u64 = 0x0000FFFF_FFFFFFFF;
const SEG_SHIFT: usize = 48;

impl EA {
	pub fn new(seg: SegId, offs: usize) -> Self {
		assert!((offs as u64) & SEG_MASK == 0);
		Self(((seg.0 as u64) << SEG_SHIFT) | (offs as u64))
	}

	pub fn invalid(offs: usize) -> Self {
		Self::new(SegId::invalid(), offs)
	}

	pub fn is_invalid(&self) -> bool {
		self.seg().is_invalid()
	}

	pub fn is_valid(&self) -> bool {
		!self.seg().is_invalid()
	}

	#[inline]
	pub fn seg(&self) -> SegId { SegId((self.0 >> SEG_SHIFT) as u16) }

	#[inline]
	pub fn offs(&self) -> usize { (self.0 & OFFS_MASK) as usize }

	#[inline]
	fn set_offs(&mut self, new_offs: usize) {
		assert!((new_offs as u64) & SEG_MASK == 0);
		self.0 &= !OFFS_MASK;
		self.0 |= new_offs as u64;
	}
}

impl Debug for EA {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		write!(f, "{}", self)
	}
}

impl Add<usize> for EA {
	type Output = Self;
	fn add(self, other: usize) -> Self {
		EA::new(self.seg(), self.offs() + other)
	}
}

impl AddAssign<usize> for EA {
	fn add_assign(&mut self, other: usize) {
		self.set_offs(self.offs() + other);
	}
}

impl Sub<usize> for EA {
	type Output = Self;
	fn sub(self, other: usize) -> Self {
		EA::new(self.seg(), self.offs() - other)
	}
}

impl SubAssign<usize> for EA {
	fn sub_assign(&mut self, other: usize) {
		self.set_offs(self.offs() - other);
	}
}