use derive_new::*;
use parse_display::*;

use std::ops::{ Add, AddAssign, Sub, SubAssign };
use std::fmt::{ Debug, UpperHex, Formatter, Result as FmtResult };

// ------------------------------------------------------------------------------------------------
// VA
// ------------------------------------------------------------------------------------------------

/// newtype for virtual addresses.
#[derive(Debug, Display, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct VA(pub usize);

impl UpperHex for VA {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		UpperHex::fmt(&self.0, f)
	}
}

impl Add<usize> for VA {
	type Output = Self;
	fn add(self, other: usize) -> Self {
		VA(self.0 + other)
	}
}

impl AddAssign<usize> for VA {
	fn add_assign(&mut self, other: usize) {
		self.0 += other;
	}
}

impl Add<Offset> for VA {
	type Output = Self;
	fn add(self, other: Offset) -> Self {
		VA(self.0 + other.0)
	}
}

impl Sub for VA {
	type Output = usize;
	fn sub(self, other: Self) -> usize {
		self.0 - other.0
	}
}

impl SubAssign<usize> for VA {
	fn sub_assign(&mut self, other: usize) {
		self.0 -= other;
	}
}

// ------------------------------------------------------------------------------------------------
// Offset
// ------------------------------------------------------------------------------------------------

/// newtype for offsets into segments.
#[derive(Debug, Display, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct Offset(pub usize);

impl UpperHex for Offset {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		UpperHex::fmt(&self.0, f)
	}
}

impl Add<usize> for Offset {
	type Output = Self;
	fn add(self, other: usize) -> Self {
		Offset(self.0 + other)
	}
}

impl Sub for Offset {
	type Output = usize;
	fn sub(self, other: Self) -> usize {
		self.0 - other.0
	}
}

// ------------------------------------------------------------------------------------------------
// SegId
// ------------------------------------------------------------------------------------------------

/// newtype for segment IDs. each segment gets a unique ID (index into an array).
#[derive(Debug, Display, Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct SegId(pub u16);

// ------------------------------------------------------------------------------------------------
// Location
// ------------------------------------------------------------------------------------------------

/// A unique location consisting of Segment ID and an offset within that Segment.
#[derive(Display, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
#[derive(new)]
#[display("{seg.0:04X}:{offs:08X}")]
pub struct Location {
	pub seg:  SegId,
	pub offs: Offset,
}

impl Debug for Location {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		write!(f, "{}", self)
	}
}

// ------------------------------------------------------------------------------------------------
// Endian
// ------------------------------------------------------------------------------------------------

/// Byte order.
#[derive(Debug, Display, PartialEq, Eq, Clone, Copy)]
pub enum Endian {
	#[display("little")] Little,
	#[display("big")]    Big,
	#[display("n/a")]    NA,
}