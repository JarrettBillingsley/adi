use parse_display::Display;
use derive_new::new;

use crate::memory::VA;

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
	/// Whether this is provided by the hardware or by a cartridge etc.
	pub hw:   bool,
	/// What kind of thing is at these addresses.
	pub kind: MemoryRegionKind,
	/// Whether or not this region can be banked (swapped out) by a memory manager.
	pub bankable: bool,

	/// How big this region is, in bytes.
	#[new(value = "end - base")]
	pub size: usize,
}

#[allow(clippy::len_without_is_empty)]
impl MemoryRegion {
	/// true if these two regions overlap one another.
	pub fn overlaps(&self, other: &MemoryRegion) -> bool {
		!(self.end <= other.base || other.end <= self.base)
	}

	/// gets the size in bytes.
	pub fn len(&self) -> usize { self.size }

	/// true this region's kind is bankable.
	pub fn is_bankable(&self) -> bool { self.bankable }
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