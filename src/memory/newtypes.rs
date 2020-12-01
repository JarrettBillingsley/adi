use derive_new::*;
use parse_display::*;

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

/// A range of physical addresses within an image.
#[derive(Debug, Clone, Copy)]
pub struct ImageRange {
	pub pbase: PAddr,
	pub pend:  PAddr,
}

/// The contents of a ROM image file.
#[derive(new)]
pub struct RomImage<'a> {
	pub name: &'a str,
	pub data: &'a [u8],
}