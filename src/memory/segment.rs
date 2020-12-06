use std::fmt::{ Display, Formatter, Result as FmtResult };

use super::types::*;
use super::spans::*;

// ------------------------------------------------------------------------------------------------
// Segment
// ------------------------------------------------------------------------------------------------

/// A single segment. Can be an image segment (data comes from a ROM image) or a fake
/// segment (there is no data, e.g. RAM, but it's useful to put spans there).
pub struct Segment {
	pub id:    SegId,
	pub name:  String,
	pub vbase: VA,
	pub vend:  VA,
	pub size:  usize,
	pub spans: SpanMap,
	pub image: Option<ImageRange>,
}

impl Display for Segment {
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
impl Segment {
	/// Creates a new Segment that covers a given virtual address range, optionally mapped to
	/// part of a ROM image.
	pub fn new(id: SegId, name: &str, vbase: VA, vend: VA, pbase: Option<PA>) -> Self {
		let size = vend - vbase;
		let image = pbase.map(|pbase| ImageRange { pbase, pend: pbase + size });

		Self {
			id,
			name: name.into(),
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
	pub(crate) fn get_image_slice<'img>(&self, image: &'img RomImage) -> Option<&'img [u8]> {
		self.get_image_slice_offs(image, Offset(0))
	}

	/// Given a ROM image, get the slice of that image starting at `va` until the end
	/// of the segment, or None if this is a fake segment.
	pub(crate) fn get_image_slice_va<'img>(&self, image: &'img RomImage, va: VA)
	-> Option<&'img [u8]> {
		self.get_image_slice_offs(image, self.offset_from_va(va))
	}

	/// Given a ROM image, get the slice of that image starting at `offs` until the end
	/// of the segment, or None if this is a fake segment.
	pub(crate) fn get_image_slice_offs<'img>(&self, image: &'img RomImage, offs: Offset)
	-> Option<&'img [u8]> {
		match self.image {
			Some(range) => Some(&image.data[range.pbase.0 + offs.0 .. range.pend.0]),
			None => None,
		}
	}

	/// Whether the given offset is valid for this segment.
	pub fn contains_offset(&self, offs: Offset) -> bool {
		offs.0 < self.size
	}

	/// Whether this segment contains a given VA.
	pub fn contains_va(&self, addr: VA) -> bool {
		self.vbase <= addr && addr < self.vend
	}

	/// Whether this segment and another segment overlap in the virtual address space.
	pub fn overlaps_va(&self, other: &Segment) -> bool {
		!(self.vend <= other.vbase || other.vend <= self.vbase)
	}

	/// Whether this segment contains a given PA.
	pub fn contains_pa(&self, addr: PA) -> bool {
		match self.image {
			Some(i) => i.pbase <= addr && addr < i.pend,
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
	pub fn offset_from_va(&self, va: VA) -> Offset {
		assert!(self.contains_va(va));
		Offset(va - self.vbase)
	}

	/// Given a PA, convert it to an offset into this segment.
	pub fn offset_from_pa(&self, pa: PA) -> Offset {
		assert!(self.contains_pa(pa));
		let pbase = self.get_image_range().pbase;
		Offset(pa - pbase)
	}

	/// Given an offset into this segment, get the VA.
	pub fn va_from_offset(&self, offs: Offset) -> VA {
		assert!(self.contains_offset(offs));
		self.vbase + offs
	}

	/// Given a PA in this segment, get the VA.
	pub fn va_from_pa(&self, pa: PA) -> VA {
		assert!(self.contains_pa(pa));
		let pbase = self.get_image_range().pbase;
		self.vbase + (pa - pbase)
	}

	/// Given an offset into this segment, get the PA.
	pub fn pa_from_offset(&self, offs: Offset) -> PA {
		assert!(self.contains_offset(offs));
		let pbase = self.get_image_range().pbase;
		pbase + offs
	}

	/// Given a VA in this segment, get the PA.
	pub fn pa_from_va(&self, va: VA) -> PA {
		assert!(self.contains_va(va));
		let pbase = self.get_image_range().pbase;
		pbase + (va - self.vbase)
	}

	// ---------------------------------------------------------------------------------------------
	// Span management (spanagement?)

	/// Get the span which contains the given offset.
	pub fn span_from_offset(&self, offs: Offset) -> Span {
		self.spans.span_at(offs)
	}

	/// Get the span which contains the given VA.
	pub fn span_from_va(&self, va: VA) -> Span {
		self.spans.span_at(self.offset_from_va(va))
	}

	/// Get the span which contains the given PA.
	pub fn span_from_pa(&self, pa: PA) -> Span {
		self.spans.span_at(self.offset_from_pa(pa))
	}

	/// Iterator over all spans in this segment, in order.
	pub fn all_spans(&self) -> impl Iterator<Item = Span> + '_ {
		self.spans.iter()
	}
}
