use derive_new::*;
use parse_display::*;

use std::ops::{ Add, Sub };

// ------------------------------------------------------------------------------------------------
// Addresses and Offsets
// ------------------------------------------------------------------------------------------------

/// newtype for virtual addresses.
#[derive(Debug, Display, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct VAddr(pub usize);

/// newtype for physical addresses (i.e. offsets into an image).
#[derive(Debug, Display, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct PAddr(pub usize);

/// newtype for offsets into segments.
#[derive(Debug, Display, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
#[display("0x{0:08X}")]
pub struct SegOffset(pub usize);

// vaddr + usize = vaddr
impl Add<usize> for VAddr {
	type Output = Self;
	fn add(self, other: usize) -> Self {
		VAddr(self.0 + other)
	}
}

// vaddr + segoffset = vaddr
impl Add<SegOffset> for VAddr {
	type Output = Self;
	fn add(self, other: SegOffset) -> Self {
		VAddr(self.0 + other.0)
	}
}

// vaddr - vaddr = usize
impl Sub for VAddr {
	type Output = usize;
	fn sub(self, other: Self) -> usize {
		self.0 - other.0
	}
}

// paddr + usize = paddr
impl Add<usize> for PAddr {
	type Output = Self;
	fn add(self, other: usize) -> Self {
		PAddr(self.0 + other)
	}
}

// paddr + segoffset = paddr
impl Add<SegOffset> for PAddr {
	type Output = Self;
	fn add(self, other: SegOffset) -> Self {
		PAddr(self.0 + other.0)
	}
}

// paddr - paddr = usize
impl Sub for PAddr {
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