use std::fmt::{ Display, Formatter, Result as FmtResult };
use std::ops::{ Range, RangeBounds, Bound };
use std::fmt::{ Debug, };

use parse_display::Display;

use crate::{ Size, Offs };
use crate::memory::{ Image, ImageSlice, ImageRead, ImageSliceable, SpanMap, Span, SpanKind, EA,
VA, SpanMapListener };
use crate::program::{ DataId };

// ------------------------------------------------------------------------------------------------
// SegId
// ------------------------------------------------------------------------------------------------

/// Newtype for segment IDs. Each segment gets a unique ID.
#[derive(Debug, Display, Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct SegId {
	pub(crate) id: u16,
}

/// Public constructor for `SegId`. Panics if given an `id > SegId::LAST_USER`.
#[allow(non_snake_case)]
pub fn SegId(id: u16) -> SegId {
	if id > SegId::LAST_USER {
		panic!("SegId was given {}; only segments <= {} are valid", id, SegId::LAST_USER);
	}

	SegId { id }
}

impl SegId {
	/// Segment ID for "unresolved" addresses (see [`EA::unresolved`]). Can be constructed with
	/// [`SegId::unresolved`] and tested for with [`SegId::is_unresolved`].
	pub const UNRESOLVED: u16 = u16::MAX;
	/// Segment ID for struct definitions.
	pub const STRUCTS:    u16 = u16::MAX - 1;
	/// Segment ID for enum definitions.
	pub const ENUMS:      u16 = u16::MAX - 2;
	/// Segment ID for bitfield definitions.
	pub const BITFIELDS:  u16 = u16::MAX - 3;
	/// Last segment ID allowed for user-defined segments. IDs greater than this are reserved for
	/// internal use (like the ones above).
	pub const LAST_USER:  u16 = u16::MAX - 16;

	/// Internal unchecked constructor which allows any ID.
	pub(crate) fn unchecked(id: u16) -> SegId { SegId { id } }
	/// Construct an unresolved `SegId`.
	pub fn unresolved() -> Self { SegId { id: Self::UNRESOLVED } }
	/// Construct a `SegId` that refers to the structs segment.
	pub fn structs()    -> Self { SegId { id: Self::STRUCTS    } }
	/// Construct a `SegId` that refers to the enums segment.
	pub fn enums()      -> Self { SegId { id: Self::ENUMS      } }
	/// Construct a `SegId` that refers to the bitfields segment.
	pub fn bitfields()  -> Self { SegId { id: Self::BITFIELDS  } }

	/// Test if a `SegId` is unresolved.
	pub fn is_unresolved(&self) -> bool { self.id == Self::UNRESOLVED }
	/// Test if a `SegId` refers to the structs segment.
	pub fn is_structs(&self)    -> bool { self.id == Self::STRUCTS    }
	/// Test if a `SegId` refers to the enums segment.
	pub fn is_enums(&self)      -> bool { self.id == Self::ENUMS      }
	/// Test if a `SegId` refers to the bitfields segment.
	pub fn is_bitfields(&self)  -> bool { self.id == Self::BITFIELDS  }
	/// Test if a `SegId` is a user segment.
	pub fn is_user(&self)       -> bool { self.id <= Self::LAST_USER  }
}

// ------------------------------------------------------------------------------------------------
// Segment
// ------------------------------------------------------------------------------------------------

/// A single segment. Can be an image segment (data comes from a ROM image) or a fake
/// segment (there is no data, e.g. RAM, but it's useful to put spans there).
pub struct Segment {
	id:      SegId,
	name:    String,
	size:    Size,
	spans:   SpanMap,
	image:   Option<Image>,
	base_va: Option<VA>,
}

impl Display for Segment {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		write!(f, "id: 0x{:04X}", self.id.id)?;

		match self.base_va {
			Some(base) => write!(f, " Base VA: 0x{:08X}", base)?,
			None       => write!(f, " Base VA: (unknown) ")?,
		}

		write!(f, " {:12}", self.name)?;

		match &self.image {
			Some(image) => {
				let orig = image.orig_range();

				write!(f, " (image '{}') PA: [{:08X}..{:08X})",
					image.name(), orig.start, orig.end)?;
			}
			None =>
				write!(f, " (fake)")?,
		}

		Ok(())
	}
}

#[allow(clippy::len_without_is_empty)]
impl Segment {
	/// Creates a new Segment that covers a given virtual address range, optionally mapped to
	/// part of a ROM image.
	pub fn new(id: SegId, name: &str, size: Size, image: Option<Image>) -> Self {
		Self::new_with_va(id, name, size, image, None)
	}

	/// Same as above, but also initializes the base VA.
	pub fn new_with_va(id: SegId, name: &str, size: Size, image: Option<Image>,
	base_va: Option<VA>) -> Self {
		if let Some(ref image) = image { assert_eq!(size, image.len()); }

		Self {
			id,
			name: name.into(),
			size,
			spans: SpanMap::new(id, size),
			image,
			base_va,
		}
	}

	// ---------------------------------------------------------------------------------------------
	// Queries

	/// Unique ID.
	#[inline] pub fn id   (&self) -> SegId          { self.id }
	/// Human-readable name.
	#[inline] pub fn name (&self) -> &String        { &self.name }
	/// Image which this is mapped to, if any.
	#[inline] pub fn image(&self) -> Option<&Image> { self.image.as_ref() }
	/// Length in bytes.
	#[inline] pub fn len(&self) -> Size             { self.size }

	/// Whether this segment contains a given EA.
	pub fn contains_ea(&self, ea: EA) -> bool {
		if ea.seg() == self.id {
			assert!(ea.offs() < self.size);
			true
		} else {
			false
		}
	}

	/// Gets the VA which corresponds to this EA if a base VA has been set, or `None` if not.
	///
	/// Panics if the given EA does not fall within this segment.
	pub fn va_for_ea(&self, ea: EA) -> Option<VA> {
		assert!(self.contains_ea(ea));
		self.base_va.map(|base_va| VA(ea.offs() + base_va.0))
	}

	/// Same as above, but infallible.
	pub fn va_from_ea(&self, ea: EA) -> VA {
		self.va_for_ea(ea).unwrap()
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
	pub fn image_range(&self) -> Range<Offs> {
		self.image.as_ref().expect("fake segment!").orig_range()
	}

	/// Convenience method to get a slice of the whole image, since
	/// `image_slice` is overloaded so `image_slice(..)` is ambiguous.
	pub fn image_slice_all(&'_ self) -> ImageSlice<'_> {
		self.image_slice(0u64..)
	}

	// ---------------------------------------------------------------------------------------------
	// Base VA

	/// Gets this segment's base VA, or `None` if none has been set.
	pub fn base_va(&self) -> Option<VA> {
		self.base_va
	}

	/// Set this segment's base VA. Does nothing if the base VA was already set to the same VA.
	///
	/// Panics if its VA has already been set to a different VA.
	pub fn set_base_va(&mut self, va: VA) {
		if let Some(cur_va) = self.base_va {
			// it's OK to set it to the same VA; changing it is a Problem.
			assert!(cur_va == va,
				"trying to change {:?}'s VA from {:X} to {:X}", self.id, cur_va, va);
		} else {
			self.base_va = Some(va);
		}
	}

	// ---------------------------------------------------------------------------------------------
	// Span management (spanagement?)

	/// How many spans there are in this segment.
	pub fn num_spans(&self) -> Size {
		self.spans.len() as Size
	}

	/// The offset of the last span.
	pub fn last_span_offset(&self) -> Offs {
		self.spans.last_span_offset()
	}

	/// Attach or detach a [`SpanMapListener`] to this segment's `SpanMap`. Passing `None` will
	/// remove any listener currently attached.
	pub fn attach_listener(&mut self, new_listener: Option<Box<dyn SpanMapListener + Send>>) {
		self.spans.attach_listener(new_listener);
	}

	/// Get the span which contains the given EA.
	pub fn span_at_ea(&self, ea: EA) -> Span {
		assert!(ea.seg() == self.id);
		self.spans.span_at(ea.offs())
	}

	/// Given an address in the segment, gets the span which comes after the containing span,
	/// or None if the containing span is the last one in the segment.
	///
	/// # Panics
	///
	/// - if `ea` is after the last address.
	pub fn span_after_ea(&self, ea: EA) -> Option<Span> {
		assert!(ea.seg() == self.id);
		self.spans.span_after(ea.offs())
	}

	/// Given an address in the segment, gets the span which comes before the containing span,
	/// or None if the containing span is the first one in the segment.
	///
	/// # Panics
	///
	/// - if `ea` is after the last address.
	pub fn span_before_ea(&self, ea: EA) -> Option<Span> {
		assert!(ea.seg() == self.id);
		self.spans.span_before(ea.offs())
	}

	/// Iterator over all spans in this segment, in order.
	pub fn all_spans(&self) -> impl Iterator<Item = Span> + '_ {
		self.spans.iter()
	}

	pub(crate) fn span_make_data(&mut self, ea: EA, size: Size, id: DataId) {
		let span = self.span_at_ea(ea);

		assert!(span.is_unknown(), "defining a data item at non-empty EA {}", ea);
		assert!(span.len() >= size,
			"defining a data item too big for its span (item is {} bytes, have {})",
			size, span.len());

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
	fn span_from_offset(&self, offs: Offs) -> Span {
		self.spans.span_at(offs)
	}

	// Given EA bounds, convert them into offset bounds.
	fn offset_bounds_from_ea_bounds(&self, bounds: impl RangeBounds<EA>)
	-> impl RangeBounds<Offs> {
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

impl ImageSliceable<Offs> for Segment {
	/// Get a read-only slice of this image's data.
	fn image_slice(&'_ self, range: impl RangeBounds<Offs>) -> ImageSlice<'_> {
		self.image.as_ref().expect("trying to slice a fake segment").image_slice(range)
	}
}

impl ImageSliceable<EA> for Segment {
	/// Get a read-only slice of this image's data.
	fn image_slice(&'_ self, range: impl RangeBounds<EA>) -> ImageSlice<'_> {
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

impl ImageRead<Offs> for Segment {
	fn read_u8    (&self, idx: Offs) -> u8  { self.image_slice_all().read_u8(idx)     }
	fn read_le_u16(&self, idx: Offs) -> u16 { self.image_slice_all().read_le_u16(idx) }
	fn read_be_u16(&self, idx: Offs) -> u16 { self.image_slice_all().read_be_u16(idx) }
	fn read_le_u32(&self, idx: Offs) -> u32 { self.image_slice_all().read_le_u32(idx) }
	fn read_be_u32(&self, idx: Offs) -> u32 { self.image_slice_all().read_be_u32(idx) }
	fn read_le_u64(&self, idx: Offs) -> u64 { self.image_slice_all().read_le_u64(idx) }
	fn read_be_u64(&self, idx: Offs) -> u64 { self.image_slice_all().read_be_u64(idx) }
}