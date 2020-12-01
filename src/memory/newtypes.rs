use derive_new::*;
use parse_display::*;

use std::ops::{ Add, Sub };
use std::fmt::{ UpperHex, Formatter, Result as FmtResult };

// ------------------------------------------------------------------------------------------------
// VAddr
// ------------------------------------------------------------------------------------------------

/// newtype for virtual addresses.
#[derive(Debug, Display, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct VAddr(pub usize);

impl UpperHex for VAddr {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		UpperHex::fmt(&self.0, f)
	}
}

impl Add<usize> for VAddr {
	type Output = Self;
	fn add(self, other: usize) -> Self {
		VAddr(self.0 + other)
	}
}

impl Add<SegOffset> for VAddr {
	type Output = Self;
	fn add(self, other: SegOffset) -> Self {
		VAddr(self.0 + other.0)
	}
}

impl Sub for VAddr {
	type Output = usize;
	fn sub(self, other: Self) -> usize {
		self.0 - other.0
	}
}

// ------------------------------------------------------------------------------------------------
// PAddr
// ------------------------------------------------------------------------------------------------

/// newtype for physical addresses (i.e. offsets into an image).
#[derive(Debug, Display, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct PAddr(pub usize);

impl UpperHex for PAddr {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		UpperHex::fmt(&self.0, f)
	}
}


impl Add<usize> for PAddr {
	type Output = Self;
	fn add(self, other: usize) -> Self {
		PAddr(self.0 + other)
	}
}

impl Add<SegOffset> for PAddr {
	type Output = Self;
	fn add(self, other: SegOffset) -> Self {
		PAddr(self.0 + other.0)
	}
}

impl Sub for PAddr {
	type Output = usize;
	fn sub(self, other: Self) -> usize {
		self.0 - other.0
	}
}

// ------------------------------------------------------------------------------------------------
// SegOffset
// ------------------------------------------------------------------------------------------------

/// newtype for offsets into segments.
#[derive(Debug, Display, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct SegOffset(pub usize);

impl UpperHex for SegOffset {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		UpperHex::fmt(&self.0, f)
	}
}

impl Add<usize> for SegOffset {
	type Output = Self;
	fn add(self, other: usize) -> Self {
		SegOffset(self.0 + other)
	}
}

impl Sub for SegOffset {
	type Output = usize;
	fn sub(self, other: Self) -> usize {
		self.0 - other.0
	}
}

// ------------------------------------------------------------------------------------------------
// ImageRange
// ------------------------------------------------------------------------------------------------

/// A range of physical addresses within an image.
#[derive(Debug, Clone, Copy)]
pub struct ImageRange {
	pub pbase: PAddr,
	pub pend:  PAddr,
}

// ------------------------------------------------------------------------------------------------
// RomImage
// ------------------------------------------------------------------------------------------------

/// The contents of a ROM image file.
#[derive(new)]
pub struct RomImage<'a> {
	pub name: &'a str,
	pub data: &'a [u8],
}