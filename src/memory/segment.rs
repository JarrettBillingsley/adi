use std::fmt::{ Display, Formatter, Result as FmtResult };
use std::ops::{ Range, RangeBounds, Bound, Add, AddAssign, Sub, SubAssign };
use std::fmt::{ Debug, };

use parse_display::Display;

use crate::memory::{ Image, ImageSlice, ImageRead, ImageSliceable, SpanMap, Span, SpanKind };
use crate::program::{ DataId };

// ------------------------------------------------------------------------------------------------
// SegId
// ------------------------------------------------------------------------------------------------

/// newtype for segment IDs. each segment gets a unique ID (index into an array).
#[derive(Debug, Display, Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct SegId(pub u16);

impl SegId {
	pub fn invalid() -> Self {
		SegId(u16::MAX)
	}

	pub fn is_invalid(&self) -> bool {
		self.0 == u16::MAX
	}
}

// ------------------------------------------------------------------------------------------------
// EA
// ------------------------------------------------------------------------------------------------

/// A unique location consisting of Segment ID and an offset within that Segment.
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct EA(u64);

impl Display for EA {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		write!(f, "{:04X}:{:08X}", self.seg().0, self.offs())
	}
}

const SEG_MASK: u64  = 0xFFFF0000_00000000;
const OFFS_MASK: u64 = 0x0000FFFF_FFFFFFFF;
const SEG_SHIFT: usize = 48;

impl EA {
	pub fn new(seg: SegId, offs: usize) -> Self {
		assert!((offs as u64) & SEG_MASK == 0);
		Self(((seg.0 as u64) << SEG_SHIFT) | (offs as u64))
	}

	pub fn invalid(offs: usize) -> Self {
		Self::new(SegId::invalid(), offs)
	}

	pub fn is_invalid(&self) -> bool {
		self.seg().is_invalid()
	}

	#[inline]
	pub fn seg(&self) -> SegId { SegId((self.0 >> SEG_SHIFT) as u16) }

	#[inline]
	pub fn offs(&self) -> usize { (self.0 & OFFS_MASK) as usize }

	#[inline]
	fn set_offs(&mut self, new_offs: usize) {
		assert!((new_offs as u64) & SEG_MASK == 0);
		self.0 &= !OFFS_MASK;
		self.0 |= new_offs as u64;
	}
}

impl Debug for EA {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		write!(f, "{}", self)
	}
}

impl Add<usize> for EA {
	type Output = Self;
	fn add(self, other: usize) -> Self {
		EA::new(self.seg(), self.offs() + other)
	}
}

impl AddAssign<usize> for EA {
	fn add_assign(&mut self, other: usize) {
		self.set_offs(self.offs() + other);
	}
}

impl Sub<usize> for EA {
	type Output = Self;
	fn sub(self, other: usize) -> Self {
		EA::new(self.seg(), self.offs() - other)
	}
}

impl SubAssign<usize> for EA {
	fn sub_assign(&mut self, other: usize) {
		self.set_offs(self.offs() - other);
	}
}

// ------------------------------------------------------------------------------------------------
// Segment
// ------------------------------------------------------------------------------------------------

/// A single segment. Can be an image segment (data comes from a ROM image) or a fake
/// segment (there is no data, e.g. RAM, but it's useful to put spans there).
pub struct Segment {
	id:    SegId,
	name:  String,
	size:  usize,
	spans: SpanMap,
	image: Option<Image>,
}

impl Display for Segment {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		match &self.image {
			Some(image) => {
				let orig = image.orig_range();

				write!(f, "{} (image '{}') PA: [{:08X}..{:08X})",
					self.name, image.name(), orig.start, orig.end)
			}
			None =>
				write!(f, "{} (fake)", self.name),
		}
	}
}

#[allow(clippy::len_without_is_empty)]
impl Segment {
	/// Creates a new Segment that covers a given virtual address range, optionally mapped to
	/// part of a ROM image.
	pub fn new(id: SegId, name: &str, size: usize, image: Option<Image>) -> Self {
		if let Some(ref image) = image { assert_eq!(size, image.len()); }

		Self {
			id,
			name: name.into(),
			size,
			spans: SpanMap::new(id, size),
			image,
		}
	}

	// ---------------------------------------------------------------------------------------------
	// Queries

	/// Unique ID.
	#[inline] pub fn id   (&self) -> SegId          { self.id }
	/// Human-readable name.
	#[inline] pub fn name (&self) -> &String        { &self.name }
	/// Image which this is mapped to, if any.
	#[inline] pub fn image(&self) -> &Option<Image> { &self.image }
	/// Length in bytes.
	#[inline] pub fn len(&self) -> usize            { self.size }

	/// Whether this segment contains a given EA.
	pub fn contains_ea(&self, ea: EA) -> bool {
		if ea.seg() == self.id {
			assert!(ea.offs() < self.size);
			true
		} else {
			false
		}
	}

	// ---------------------------------------------------------------------------------------------
	// Image

	/// True if this is a "fake" segment (has no physical image mapping).
	pub fn is_fake(&self) -> bool {
		self.image.is_none()
	}

	/// The opposite of above.
	pub fn is_real(&self) -> bool {
		self.image.is_some()
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
	// Span management (spanagement?)

	/// Get the span which contains the given EA.
	pub fn span_at_ea(&self, ea: EA) -> Span {
		assert!(ea.seg() == self.id);
		self.spans.span_at(ea.offs())
	}

	/// Iterator over all spans in this segment, in order.
	pub fn all_spans(&self) -> impl Iterator<Item = Span> + '_ {
		self.spans.iter()
	}

	pub(crate) fn span_make_data(&mut self, ea: EA, size: usize, id: DataId) {
		let span = self.span_at_ea(ea);

		assert!(span.is_unknown(), "defining a data item at non-empty EA {}", ea);
		assert!(span.len() >= size,
			"defining a data item too big for its span (item is {} bytes, have {})", size, span.len());

		self.spans.define(ea.offs(), size, SpanKind::Data(id));
	}

	pub(crate) fn span_begin_analysis(&mut self, ea: EA) {
		assert!(ea.seg() == self.id);
		// may not be at the beginning of a span, so have to use define
		let end = self.spans.span_at(ea.offs()).end();
		self.spans.define(ea.offs(), end.offs() - ea.offs(), SpanKind::Ana);
	}

	pub(crate) fn span_cancel_analysis(&mut self, ea: EA) {
		assert!(ea.seg() == self.id);
		// may not be at the beginning of a span, so have to use define
		self.spans.undefine(ea.offs());
	}

	pub(crate) fn span_end_analysis(&mut self, start: EA, end: EA, kind: SpanKind) {
		assert!(start.seg() == self.id);
		assert!(self.spans.span_at(start.offs()).kind() == SpanKind::Ana);
		self.spans.undefine(start.offs());
		self.spans.define(start.offs(), end.offs() - start.offs(), kind);
	}

	/// Split the span that owns `ea` into two parts; the second part will be given `kind`.
	/// Panics if the existing span is Unknown, or if the length of either part will be 0.
	pub fn split_span(&mut self, ea: EA, kind: SpanKind) {
		assert!(ea.seg() == self.id);
		let existing = self.spans.span_at(ea.offs());

		assert!(existing.start().offs() < ea.offs());
		assert!(ea.offs() < existing.end().offs());

		let first_len = ea.offs() - existing.start().offs();
		let second_len = existing.end().offs() - ea.offs();

		self.spans.truncate(existing.start().offs(), first_len);
		self.spans.define(ea.offs(), second_len, kind);
	}

	pub fn redefine_span(&mut self, start: EA, kind: SpanKind) {
		assert!(start.seg() == self.id);
		self.spans.redefine(start.offs(), kind);
	}

	#[cfg(any(test, debug_assertions))]
	pub fn dump_spans(&self) {
		self.spans.dump_spans();
	}

	// ---------------------------------------------------------------------------------------------
	// PRIVATE

	// Get the span which contains the given offset.
	fn span_from_offset(&self, offs: usize) -> Span {
		self.spans.span_at(offs)
	}

	// Given EA bounds, convert them into offset bounds.
	fn offset_bounds_from_ea_bounds(&self, bounds: impl RangeBounds<EA>)
	-> impl RangeBounds<usize> {
		use Bound::*;

		let start = match bounds.start_bound() {
			Included(ea) => { assert!(ea.seg() == self.id); Included(ea.offs()) }
			Excluded(ea) => { assert!(ea.seg() == self.id); Excluded(ea.offs()) }
			Unbounded                         => Unbounded,
		};

		let end = match bounds.end_bound() {
			Included(ea) => { assert!(ea.seg() == self.id); Included(ea.offs()) }
			Excluded(ea) => { assert!(ea.seg() == self.id); Excluded(ea.offs()) }
			Unbounded                         => Unbounded,
		};

		(start, end)
	}
}

impl ImageSliceable<usize> for Segment {
	/// Get a read-only slice of this image's data.
	fn image_slice(&self, range: impl RangeBounds<usize>) -> ImageSlice {
		self.image.as_ref().expect("trying to slice a fake segment").image_slice(range)
	}
}

impl ImageSliceable<EA> for Segment {
	/// Get a read-only slice of this image's data.
	fn image_slice(&self, range: impl RangeBounds<EA>) -> ImageSlice {
		self.image_slice(self.offset_bounds_from_ea_bounds(range))
	}
}

impl ImageRead<EA> for Segment {
	fn read_u8(&self, idx: EA) -> u8      {
		assert!(idx.seg() == self.id);
		self.read_u8(idx.offs())
	}
	fn read_le_u16(&self, idx: EA) -> u16 {
		assert!(idx.seg() == self.id);
		self.read_le_u16(idx.offs())
	}
	fn read_be_u16(&self, idx: EA) -> u16 {
		assert!(idx.seg() == self.id);
		self.read_be_u16(idx.offs())
	}
	fn read_le_u32(&self, idx: EA) -> u32 {
		assert!(idx.seg() == self.id);
		self.read_le_u32(idx.offs())
	}
	fn read_be_u32(&self, idx: EA) -> u32 {
		assert!(idx.seg() == self.id);
		self.read_be_u32(idx.offs())
	}
	fn read_le_u64(&self, idx: EA) -> u64 {
		assert!(idx.seg() == self.id);
		self.read_le_u64(idx.offs())
	}
	fn read_be_u64(&self, idx: EA) -> u64 {
		assert!(idx.seg() == self.id);
		self.read_be_u64(idx.offs())
	}
}

impl ImageRead<usize> for Segment {
	fn read_u8    (&self, idx: usize) -> u8  { self.image_slice_all().read_u8(idx)     }
	fn read_le_u16(&self, idx: usize) -> u16 { self.image_slice_all().read_le_u16(idx) }
	fn read_be_u16(&self, idx: usize) -> u16 { self.image_slice_all().read_be_u16(idx) }
	fn read_le_u32(&self, idx: usize) -> u32 { self.image_slice_all().read_le_u32(idx) }
	fn read_be_u32(&self, idx: usize) -> u32 { self.image_slice_all().read_be_u32(idx) }
	fn read_le_u64(&self, idx: usize) -> u64 { self.image_slice_all().read_le_u64(idx) }
	fn read_be_u64(&self, idx: usize) -> u64 { self.image_slice_all().read_be_u64(idx) }
}