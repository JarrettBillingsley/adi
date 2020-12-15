use parse_display::Display;
use derive_new::new;

use crate::memory::{ VA, SegId };

// ------------------------------------------------------------------------------------------------
// Memory map regions
// ------------------------------------------------------------------------------------------------

/// Describes part of a virtual memory map.
#[derive(Debug, Display, Clone)]
#[derive(new)]
#[display("{name} [0x{base:08X} .. 0x{end:08X})")]
pub struct MemoryRegion {
	name: String,
	base: VA,
	end:  VA,
	kind: MemoryRegionKind,
	seg: Option<SegId>,
}

#[allow(clippy::len_without_is_empty)]
impl MemoryRegion {
	/// The human-readable name.
	#[inline] pub fn name(&self) -> &String { &self.name }
	/// The virtual address of this region's first byte.
	#[inline] pub fn base(&self) -> VA { self.base }
	/// The virtual address of first byte *after* this region.
	#[inline] pub fn end(&self) -> VA { self.end }
	/// The size in bytes.
	#[inline] pub fn len(&self) -> usize { self.end - self.base }
	/// What kind of thing is at these addresses.
	#[inline] pub fn kind(&self) -> MemoryRegionKind { self.kind }
	/// The ID of the segment to which this is hardwired, if any.
	/// If this is `None`, then it's up to the MMU to decide what segment is mapped here.
	#[inline] pub fn seg(&self) -> Option<SegId> { self.seg }
	/// True this region is bankable.
	#[inline] pub fn is_bankable(&self) -> bool { self.seg.is_none() }
	/// True if `self` and `other` overlap one another.
	#[inline] pub fn overlaps(&self, other: &MemoryRegion) -> bool {
		!(self.end <= other.base || other.end <= self.base)
	}
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