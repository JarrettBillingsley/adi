use parse_display::Display;
use derive_new::new;

use crate::memory::{ VA, SegId };

// ------------------------------------------------------------------------------------------------
// Memory map regions
// ------------------------------------------------------------------------------------------------

/// Describes part of a CPU's memory map - the name, address range, and so on.
#[derive(Debug, Display, Clone)]
#[derive(new)]
#[display("{name} [0x{base:08X} .. 0x{end:08X})")]
pub struct MemoryRegion {
	/// Human-readable name.
	pub name: String,
	/// Address of first byte.
	pub base: VA,
	/// Address of first byte *after* this region.
	pub end:  VA,
	/// What kind of thing is at these addresses.
	pub kind: MemoryRegionKind,
	/// The segment to which this is hardwired, if any.
	/// If this is `None`, then it's up to the MMU to decide what segment is mapped here.
	pub seg: Option<SegId>,
}

#[allow(clippy::len_without_is_empty)]
impl MemoryRegion {
	/// true if these two regions overlap one another.
	pub fn overlaps(&self, other: &MemoryRegion) -> bool {
		!(self.end <= other.base || other.end <= self.base)
	}

	/// gets the size in bytes.
	pub fn len(&self) -> usize { self.end - self.base }

	/// true this region's kind is bankable.
	pub fn is_bankable(&self) -> bool { self.seg.is_none() }
}

/// What you access when you use an address in a region's range.
#[derive(Debug, Display, PartialEq, Eq, Clone, Copy)]
pub enum MemoryRegionKind {
	/// RAM.
	Ram,
	/// ROM.
	Rom,
	/// Non-volatile RAM.
	NvRam,
	/// A mirror of the previous region of memory.
	Mirror,
	/// Memory-mapped IO ports.
	Mmio,
}