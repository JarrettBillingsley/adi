use std::fmt::{ Display, Formatter, Result as FmtResult };
use std::ops::{ Range, RangeBounds, Bound };

use super::image::*;
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
	pub image: Option<Image>,
}

impl Display for Segment {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		match &self.image {
			Some(image) => {
				let orig = image.orig_range();

				write!(f, "{} (image '{}') VA [{:08X}..{:08X}) PA: [{:08X}..{:08X})",
					self.name, image.name(), self.vbase, self.vend, orig.start.0, orig.end.0)
			}
			None =>
				write!(f, "{} (fake) VA [{:08X}..{:08X})", self.name, self.vbase, self.vend),
		}
	}
}

#[allow(clippy::len_without_is_empty)]
impl Segment {
	/// Creates a new Segment that covers a given virtual address range, optionally mapped to
	/// part of a ROM image.
	pub fn new(id: SegId, name: &str, vbase: VA, vend: VA, image: Option<Image>) -> Self {
		let size = vend - vbase;

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
	/// Panics if this is a fake segment.
	pub fn image_range(&self) -> Range<Offset> {
		self.image.as_ref().expect("fake segment!").orig_range()
	}

	/// Convenience method to get a slice of the whole image, since
	/// `image_slice` is overloaded and `image_slice(..)` is ambiguous.
	pub fn image_slice_all(&self) -> ImageSlice {
		self.image_slice(Offset(0)..)
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

	// ---------------------------------------------------------------------------------------------
	// Conversions between segment offsets, virtual addresses, physical addresses, and names
	// (there are so many for "convenience" I guess)
	// (remains to be seen how many of these are actually used in practice)

	/// Given a VA, convert it to an offset into this segment.
	pub fn offset_from_va(&self, va: VA) -> Offset {
		assert!(self.contains_va(va));
		Offset(va - self.vbase)
	}

	/// Given an offset into this segment, get the VA.
	pub fn va_from_offset(&self, offs: Offset) -> VA {
		assert!(self.contains_offset(offs));
		self.vbase + offs
	}

	/// Given VA bounds, convert them into offset bounds.
	pub fn offset_bounds_from_va_bounds(&self, bounds: impl RangeBounds<VA>)
	-> impl RangeBounds<Offset> {
		use Bound::*;

		let start = match bounds.start_bound() {
			Included(&s) => Included(self.offset_from_va(s)),
			Excluded(&s) => Excluded(self.offset_from_va(s)),
			Unbounded    => Unbounded,
		};

		let end = match bounds.end_bound() {
			Included(&e) => Included(self.offset_from_va(e)),
			Excluded(&e) => Excluded(self.offset_from_va(e)),
			Unbounded    => Unbounded,
		};

		(start, end)
	}

	/// Given offset bounds, convert them into VA bounds.
	pub fn va_bounds_from_offset_bounds(&self, bounds: impl RangeBounds<Offset>)
	-> impl RangeBounds<VA> {
		use Bound::*;

		let start = match bounds.start_bound() {
			Included(&s) => Included(self.va_from_offset(s)),
			Excluded(&s) => Excluded(self.va_from_offset(s)),
			Unbounded    => Unbounded,
		};

		let end = match bounds.end_bound() {
			Included(&e) => Included(self.va_from_offset(e)),
			Excluded(&e) => Excluded(self.va_from_offset(e)),
			Unbounded    => Unbounded,
		};

		(start, end)
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

	/// Iterator over all spans in this segment, in order.
	pub fn all_spans(&self) -> impl Iterator<Item = Span> + '_ {
		self.spans.iter()
	}
}

impl ImageSliceable<VA> for Segment {
	/// Get a read-only slice of this image's data.
	fn image_slice(&self, range: impl RangeBounds<VA>) -> ImageSlice {
		self.image_slice(self.offset_bounds_from_va_bounds(range))
	}
}

impl ImageSliceable<Offset> for Segment {
	/// Get a read-only slice of this image's data.
	fn image_slice(&self, range: impl RangeBounds<Offset>) -> ImageSlice {
		self.image.as_ref().expect("trying to slice a fake segment").image_slice(range)
	}
}