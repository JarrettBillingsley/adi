use std::fmt::{ Display, Formatter, Result as FmtResult };
use std::collections::{
	btree_map::Values as BTreeValues,
};

use super::types::*;
use super::spans::*;

// ------------------------------------------------------------------------------------------------
// Segment
// ------------------------------------------------------------------------------------------------

/// A single segment. Can be an image segment (data comes from a ROM image) or a fake
/// segment (there is no data, e.g. RAM, but it's useful to put spans there).
pub struct Segment<'a> {
	pub id:    SegId,
	pub name:  &'a str,
	pub vbase: VAddr,
	pub vend:  VAddr,
	pub size:  usize,
	pub spans: SpanMap,
	pub image: Option<ImageRange>,
}

impl<'a> Display for Segment<'a> {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		match self.image {
			Some(image) =>
				write!(f, "{} (image) VA [{:08X}..{:08X}) PA: [{:08X}..{:08X})",
					self.name, self.vbase, self.vend, image.pbase, image.pend),
			None =>
				write!(f, "{} (fake) VA [{:08X}..{:08X})", self.name, self.vbase, self.vend),
		}
	}
}

#[allow(clippy::len_without_is_empty)]
impl<'a> Segment<'a> {
	/// Creates a new Segment that covers a given virtual address range, optionally mapped to
	/// part of a ROM image.
	pub fn new(id: SegId, name: &'a str, vbase: VAddr, vend: VAddr, pbase: Option<PAddr>) -> Self {
		let size = vend - vbase;
		let image = pbase.map(|pbase| ImageRange { pbase, pend: pbase + size });

		Self {
			id,
			name,
			vbase,
			vend,
			size,
			spans: SpanMap::new(size),
			image,
		}
	}

	// ---------------------------------------------------------------------------------------------
	// Queries

	/// Length in bytes.
	pub fn len(&self) -> usize {
		self.size
	}

	/// True if this is a "fake" segment (has no physical image mapping).
	pub fn is_fake(&self) -> bool {
		self.image.is_none()
	}

	/// Gets the range of physical addresses this segment is mapped to.
	pub fn get_image_range(&self) -> ImageRange {
		self.image.unwrap()
	}

	/// Given a ROM image, get the slice of that image that this segment covers,
	/// or None if this is a fake segment.
	pub fn get_image_slice(&self, image: &'a RomImage) -> Option<&'a [u8]> {
		match self.image {
			Some(range) => Some(&image.data[range.pbase.0 .. range.pend.0]),
			None => None,
		}
	}

	/// Whether the given offset is valid for this segment.
	pub fn contains_offset(&self, offs: SegOffset) -> bool {
		(.. SegOffset(self.size)).contains(&offs)
	}

	/// Whether this segment contains a given VA.
	pub fn contains_va(&self, addr: VAddr) -> bool {
		(self.vbase .. self.vend).contains(&addr)
	}

	/// Whether this segment and another segment overlap in the virtual address space.
	pub fn overlaps_va(&self, other: &Segment) -> bool {
		!(self.vend <= other.vbase || other.vend <= self.vbase)
	}

	/// Whether this segment contains a given PA.
	pub fn contains_pa(&self, addr: PAddr) -> bool {
		match self.image {
			Some(i) => (i.pbase .. i.pend).contains(&addr),
			None    => false,
		}
	}

	/// Whether this segment and another segment overlap in the physical address space.
	/// (That really should never happen, I don't think...)
	pub fn overlaps_pa(&self, other: &Segment) -> bool {
		let i = self.get_image_range();
		let o = other.get_image_range();
		!(i.pend <= o.pbase || o.pend <= i.pbase)
	}

	// ---------------------------------------------------------------------------------------------
	// Conversions between segment offsets, virtual addresses, physical addresses, and names
	// (there are so many for "convenience" I guess)
	// (remains to be seen how many of these are actually used in practice)

	/// Given a VA, convert it to an offset into this segment.
	pub fn offset_from_va(&self, va: VAddr) -> SegOffset {
		assert!(self.contains_va(va));
		SegOffset(va - self.vbase)
	}

	/// Given a PA, convert it to an offset into this segment.
	pub fn offset_from_pa(&self, pa: PAddr) -> SegOffset {
		assert!(self.contains_pa(pa));
		let pbase = self.get_image_range().pbase;
		SegOffset(pa - pbase)
	}

	/// Given an offset into this segment, get the VA.
	pub fn va_from_offset(&self, offs: SegOffset) -> VAddr {
		assert!(self.contains_offset(offs));
		self.vbase + offs
	}

	/// Given a PA in this segment, get the VA.
	pub fn va_from_pa(&self, pa: PAddr) -> VAddr {
		assert!(self.contains_pa(pa));
		let pbase = self.get_image_range().pbase;
		self.vbase + (pa - pbase)
	}

	/// Given an offset into this segment, get the PA.
	pub fn pa_from_offset(&self, offs: SegOffset) -> PAddr {
		assert!(self.contains_offset(offs));
		let pbase = self.get_image_range().pbase;
		pbase + offs
	}

	/// Given a VA in this segment, get the PA.
	pub fn pa_from_va(&self, va: VAddr) -> PAddr {
		assert!(self.contains_va(va));
		let pbase = self.get_image_range().pbase;
		pbase + (va - self.vbase)
	}

	// ---------------------------------------------------------------------------------------------
	// Span management (spanagement?)

	/// Redefines a range of addresses (by segment offsets) to be a new span.
	pub fn redefine_span_offsets(&mut self,
		start: SegOffset, end: SegOffset, kind: SpanKind) {
		self.spans.redefine(start, end, kind)
	}

	/// Redefines a range of addresses (by VAs) to be a new span.
	pub fn redefine_span_vas(&mut self,
		start: VAddr, end: VAddr, kind: SpanKind) {
		self.spans.redefine(self.offset_from_va(start), self.offset_from_va(end), kind)
	}

	/// Get the span which contains the given offset.
	pub fn span_from_offset(&self, offs: SegOffset) -> &Span {
		self.spans.span_at(offs)
	}

	/// Get the span which contains the given VA.
	pub fn span_from_va(&self, va: VAddr) -> &Span {
		self.spans.span_at(self.offset_from_va(va))
	}

	/// Get the span which contains the given PA.
	pub fn span_from_pa(&self, pa: PAddr) -> &Span {
		self.spans.span_at(self.offset_from_pa(pa))
	}

	/// Iterator over all spans in this segment, in order.
	pub fn all_spans(&self) -> BTreeValues<'_, SegOffset, Span> {
		self.spans.iter()
	}
}
