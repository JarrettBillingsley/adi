use std::fmt::{ Display, Formatter, Result as FmtResult };

use super::newtypes::*;
use super::spans::*;

// ------------------------------------------------------------------------------------------------
// Segment
// ------------------------------------------------------------------------------------------------

/// A single segment. Can be an image segment (data comes from a ROM image) or a fake
/// segment (there is no data, e.g. RAM, but it's useful to put spans there).
pub struct Segment<'a> {
	pub name:  &'a str,
	pub vbase: VAddr,
	pub vend:  VAddr,
	pub size:  usize,
	pub spans: SpanMap<'a>,
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

impl<'a> Segment<'a> {
	/// Creates a new Segment that covers a given virtual address range, optionally mapped to
	/// part of a ROM image.
	pub fn new(name: &'a str, vbase: VAddr, vend: VAddr, pbase: Option<PAddr>) -> Self {
		let size = vend - vbase;
		let image = pbase.map(|pbase| ImageRange { pbase, pend: pbase + size });

		Self {
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

	pub fn len(&self) -> usize {
		self.size
	}

	pub fn is_fake(&self) -> bool {
		self.image.is_none()
	}

	pub fn get_image_range(&self) -> ImageRange {
		self.image.unwrap()
	}

	pub fn contains_offset(&self, offs: SegOffset) -> bool {
		(.. SegOffset(self.size)).contains(&offs)
	}

	pub fn contains_va(&self, addr: VAddr) -> bool {
		(self.vbase .. self.vend).contains(&addr)
	}

	pub fn overlaps_va(&self, other: &Segment) -> bool {
		!(self.vend <= other.vbase || other.vend <= self.vbase)
	}

	pub fn contains_pa(&self, addr: PAddr) -> bool {
		match self.image {
			Some(i) => (i.pbase .. i.pend).contains(&addr),
			None    => false,
		}
	}

	pub fn overlaps_pa(&self, other: &Segment) -> bool {
		let i = self.get_image_range();
		let o = other.get_image_range();
		!(i.pend <= o.pbase || o.pend <= i.pbase)
	}

	// ---------------------------------------------------------------------------------------------
	// Conversions between segment offsets, virtual addresses, physical addresses, and names
	// (there are so many for "convenience" I guess)
	// (remains to be seen how many of these are actually used in practice)

	pub fn offset_from_va(&self, va: VAddr) -> SegOffset {
		assert!(self.contains_va(va));
		SegOffset(va - self.vbase)
	}

	pub fn offset_from_pa(&self, pa: PAddr) -> SegOffset {
		assert!(self.contains_pa(pa));
		let pbase = self.get_image_range().pbase;
		SegOffset(pa - pbase)
	}

	pub fn va_from_offset(&self, offs: SegOffset) -> VAddr {
		assert!(self.contains_offset(offs));
		self.vbase + offs
	}

	pub fn va_from_pa(&self, pa: PAddr) -> VAddr {
		assert!(self.contains_pa(pa));
		let pbase = self.get_image_range().pbase;
		self.vbase + (pa - pbase)
	}

	pub fn pa_from_offset(&self, offs: SegOffset) -> PAddr {
		assert!(self.contains_offset(offs));
		let pbase = self.get_image_range().pbase;
		pbase + offs
	}

	pub fn pa_from_va(&self, va: VAddr) -> PAddr {
		assert!(self.contains_va(va));
		let pbase = self.get_image_range().pbase;
		pbase + (va - self.vbase)
	}

	// ---------------------------------------------------------------------------------------------
	// Span management (spanagement?)

	// TODO:
	// def makeNewSpan(self, loc, endOffs, type, owner):
	// 	self.spans.redefine(loc.offs, endOffs, type, owner)

	pub fn span_from_offset(&self, offs: SegOffset) -> &Span {
		self.spans.span_at(offs)
	}

	pub fn span_from_va(&self, va: VAddr) -> &Span {
		self.spans.span_at(self.offset_from_va(va))
	}

	pub fn span_from_pa(&self, pa: PAddr) -> &Span {
		self.spans.span_at(self.offset_from_pa(pa))
	}
}
