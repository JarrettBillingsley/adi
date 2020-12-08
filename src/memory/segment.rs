use std::fmt::{ Display, Formatter, Result as FmtResult };
use std::ops::{ Range, RangeBounds, Bound };
use std::fmt::{ Debug, };

use derive_new::new;
use parse_display::Display;

use crate::memory::{ Image, ImageSlice, ImageRead, ImageSliceable, SpanMap, Span, SpanKind, VA };

// ------------------------------------------------------------------------------------------------
// SegId
// ------------------------------------------------------------------------------------------------

/// newtype for segment IDs. each segment gets a unique ID (index into an array).
#[derive(Debug, Display, Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct SegId(pub u16);

// ------------------------------------------------------------------------------------------------
// Location
// ------------------------------------------------------------------------------------------------

/// A unique location consisting of Segment ID and an offset within that Segment.
#[derive(Display, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
#[derive(new)]
#[display("{seg.0:04X}:{offs:08X}")]
pub struct Location {
	pub seg:  SegId,
	pub offs: usize,
}

impl Location {
	pub const fn invalid() -> Self {
		Self { seg: SegId(u16::MAX), offs: usize::MAX }
	}
}

impl Debug for Location {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		write!(f, "{}", self)
	}
}

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
	spans: SpanMap,
	pub image: Option<Image>,
}

impl Display for Segment {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		match &self.image {
			Some(image) => {
				let orig = image.orig_range();

				write!(f, "{} (image '{}') VA [{:08X}..{:08X}) PA: [{:08X}..{:08X})",
					self.name, image.name(), self.vbase, self.vend, orig.start, orig.end)
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
			spans: SpanMap::new(id, size),
			image,
		}
	}

	// ---------------------------------------------------------------------------------------------
	// Queries

	/// Length in bytes.
	pub fn len(&self) -> usize {
		self.size
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
	// Image

	/// True if this is a "fake" segment (has no physical image mapping).
	pub fn is_fake(&self) -> bool {
		self.image.is_none()
	}

	/// Gets the range of physical addresses this segment is mapped to.
	/// Panics if this is a fake segment.
	pub fn image_range(&self) -> Range<usize> {
		self.image.as_ref().expect("fake segment!").orig_range()
	}

	/// Convenience method to get a slice of the whole image, since
	/// `image_slice` is overloaded so `image_slice(..)` is ambiguous.
	pub fn image_slice_all(&self) -> ImageSlice {
		self.image_slice(0..)
	}

	// ---------------------------------------------------------------------------------------------
	// Conversions between locations and virtual addresses

	/// Given a VA, convert it to a Location. Panics if this segment doesn't contain the VA.
	pub fn loc_from_va(&self, va: VA) -> Location {
		assert!(self.contains_va(va));
		Location { seg: self.id, offs: va - self.vbase }
	}

	/// Given a Location, get the VA. Panics if the Location doesn't refer to this segment.
	pub fn va_from_loc(&self, loc: Location) -> VA {
		assert!(loc.seg == self.id);
		self.va_from_offset(loc.offs)
	}

	// ---------------------------------------------------------------------------------------------
	// Span management (spanagement?)

	/// Get the span which contains the given location.
	pub fn span_at_loc(&self, loc: Location) -> Span {
		assert!(loc.seg == self.id);
		self.spans.span_at(loc.offs)
	}

	/// Get the span which contains the given VA.
	pub fn span_at_va(&self, va: VA) -> Span {
		self.spans.span_at(self.offset_from_va(va))
	}

	/// Iterator over all spans in this segment, in order.
	pub fn all_spans(&self) -> impl Iterator<Item = Span> + '_ {
		self.spans.iter()
	}

	pub fn span_begin_analysis(&mut self, loc: Location) {
		assert!(loc.seg == self.id);
		// may not be at the beginning of a span, so have to use define
		let end = self.spans.span_at(loc.offs).end;
		self.spans.define(loc.offs, end.offs - loc.offs, SpanKind::Ana);
	}

	pub fn span_end_analysis(&mut self, loc: Location, end: Location, kind: SpanKind) {
		assert!(loc.seg == self.id);
		assert!(self.spans.span_at(loc.offs).kind == SpanKind::Ana);
		self.spans.undefine(loc.offs);
		self.spans.define(loc.offs, end.offs - loc.offs, kind);
	}

	/// Split the span that owns `loc` into two parts; the second part will be given `kind`.
	/// Panics if the existing span is Unknown, or if the length of either part will be 0.
	pub fn split_span(&mut self, loc: Location, kind: SpanKind) {
		assert!(loc.seg == self.id);
		let existing = self.spans.span_at(loc.offs);

		assert!(existing.start.offs < loc.offs);
		assert!(loc.offs < existing.end.offs);

		let first_len = loc.offs - existing.start.offs;
		let second_len = existing.end.offs - loc.offs;

		self.spans.truncate(existing.start.offs, first_len);
		self.spans.define(loc.offs, second_len, kind);
	}

	pub fn redefine_span(&mut self, start: Location, kind: SpanKind) {
		assert!(start.seg == self.id);
		self.spans.redefine(start.offs, kind);
	}

	// ---------------------------------------------------------------------------------------------
	// PRIVATE

	// Get the span which contains the given offset.
	fn span_from_offset(&self, offs: usize) -> Span {
		self.spans.span_at(offs)
	}

	// Given a VA, convert it to an offset into this segment.
	fn offset_from_va(&self, va: VA) -> usize {
		assert!(self.contains_va(va));
		va - self.vbase
	}

	// Given an offset into this segment, get the VA.
	fn va_from_offset(&self, offs: usize) -> VA {
		assert!(offs < self.size);
		self.vbase + offs
	}

	// Given VA bounds, convert them into offset bounds.
	fn offset_bounds_from_va_bounds(&self, bounds: impl RangeBounds<VA>)
	-> impl RangeBounds<usize> {
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

	// Given VA bounds, convert them into offset bounds.
	fn offset_bounds_from_loc_bounds(&self, bounds: impl RangeBounds<Location>)
	-> impl RangeBounds<usize> {
		use Bound::*;

		let start = match bounds.start_bound() {
			Included(&Location { seg, offs }) => { assert!(seg == self.id); Included(offs) }
			Excluded(&Location { seg, offs }) => { assert!(seg == self.id); Excluded(offs) }
			Unbounded                         => Unbounded,
		};

		let end = match bounds.end_bound() {
			Included(&Location { seg, offs }) => { assert!(seg == self.id); Included(offs) }
			Excluded(&Location { seg, offs }) => { assert!(seg == self.id); Excluded(offs) }
			Unbounded                         => Unbounded,
		};

		(start, end)
	}

	// Given offset bounds, convert them into VA bounds.
	fn va_bounds_from_offset_bounds(&self, bounds: impl RangeBounds<usize>)
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
}

impl ImageSliceable<VA> for Segment {
	/// Get a read-only slice of this image's data.
	fn image_slice(&self, range: impl RangeBounds<VA>) -> ImageSlice {
		self.image_slice(self.offset_bounds_from_va_bounds(range))
	}
}

impl ImageSliceable<usize> for Segment {
	/// Get a read-only slice of this image's data.
	fn image_slice(&self, range: impl RangeBounds<usize>) -> ImageSlice {
		self.image.as_ref().expect("trying to slice a fake segment").image_slice(range)
	}
}

impl ImageSliceable<Location> for Segment {
	/// Get a read-only slice of this image's data.
	fn image_slice(&self, range: impl RangeBounds<Location>) -> ImageSlice {
		self.image_slice(self.offset_bounds_from_loc_bounds(range))
	}
}

impl ImageRead<VA> for Segment {
	fn read_u8    (&self, idx: VA) -> u8  { self.read_u8(self.offset_from_va(idx))     }
	fn read_le_u16(&self, idx: VA) -> u16 { self.read_le_u16(self.offset_from_va(idx)) }
	fn read_be_u16(&self, idx: VA) -> u16 { self.read_be_u16(self.offset_from_va(idx)) }
	fn read_le_u32(&self, idx: VA) -> u32 { self.read_le_u32(self.offset_from_va(idx)) }
	fn read_be_u32(&self, idx: VA) -> u32 { self.read_be_u32(self.offset_from_va(idx)) }
}

impl ImageRead<Location> for Segment {
	fn read_u8(&self, idx: Location) -> u8      {
		assert!(idx.seg == self.id);
		self.read_u8(idx.offs)
	}
	fn read_le_u16(&self, idx: Location) -> u16 {
		assert!(idx.seg == self.id);
		self.read_le_u16(idx.offs)
	}
	fn read_be_u16(&self, idx: Location) -> u16 {
		assert!(idx.seg == self.id);
		self.read_be_u16(idx.offs)
	}
	fn read_le_u32(&self, idx: Location) -> u32 {
		assert!(idx.seg == self.id);
		self.read_le_u32(idx.offs)
	}
	fn read_be_u32(&self, idx: Location) -> u32 {
		assert!(idx.seg == self.id);
		self.read_be_u32(idx.offs)
	}
}

impl ImageRead<usize> for Segment {
	fn read_u8    (&self, idx: usize) -> u8  { self.image_slice_all().read_u8(idx)     }
	fn read_le_u16(&self, idx: usize) -> u16 { self.image_slice_all().read_le_u16(idx) }
	fn read_be_u16(&self, idx: usize) -> u16 { self.image_slice_all().read_be_u16(idx) }
	fn read_le_u32(&self, idx: usize) -> u32 { self.image_slice_all().read_le_u32(idx) }
	fn read_be_u32(&self, idx: usize) -> u32 { self.image_slice_all().read_be_u32(idx) }
}