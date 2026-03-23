use std::ops::{ Range, RangeBounds, Bound, Index };
use std::convert::TryInto;

use crate::{ Size, Offs, to_usize };
use crate::memory::{ Endian };

// ------------------------------------------------------------------------------------------------
// ImageRead
// ------------------------------------------------------------------------------------------------

pub trait ImageRead<Index> {
	fn read_u8(&self, index: Index) -> u8;
	fn read_le_u16(&self, index: Index) -> u16;
	fn read_be_u16(&self, index: Index) -> u16;
	fn read_le_u32(&self, index: Index) -> u32;
	fn read_be_u32(&self, index: Index) -> u32;
	fn read_le_u64(&self, index: Index) -> u64;
	fn read_be_u64(&self, index: Index) -> u64;

	fn read_u16(&self, index: Index, endian: Endian) -> u16 {
		match endian {
			Endian::Little => self.read_le_u16(index),
			Endian::Big    => self.read_be_u16(index),
			Endian::NA     => panic!("invalid endianness"),
		}
	}

	fn read_u32(&self, index: Index, endian: Endian) -> u32 {
		match endian {
			Endian::Little => self.read_le_u32(index),
			Endian::Big    => self.read_be_u32(index),
			Endian::NA     => panic!("invalid endianness"),
		}
	}

	fn read_u64(&self, index: Index, endian: Endian) -> u64 {
		match endian {
			Endian::Little => self.read_le_u64(index),
			Endian::Big    => self.read_be_u64(index),
			Endian::NA     => panic!("invalid endianness"),
		}
	}
}

// ------------------------------------------------------------------------------------------------
// ImageSliceable
// ------------------------------------------------------------------------------------------------

/// Trait for getting `ImageSlice`s from things.
pub trait ImageSliceable<Index> {
	fn image_slice(&'_ self, bounds: impl RangeBounds<Index>) -> ImageSlice<'_>;
}

// ------------------------------------------------------------------------------------------------
// ImageSlice
// ------------------------------------------------------------------------------------------------

/// A read-only slice of an `Image`'s data.
pub struct ImageSlice<'img> {
	data: &'img [u8],
}

impl<'img> ImageSlice<'img> {
	pub fn data(&self) -> &'img [u8] {
		self.data
	}

	pub fn into_data(self) -> &'img [u8] {
		self.data
	}

	// ---------------------------------------------------------------------------------------------
	// private

	fn check_range(&self, range: Range<Offs>) -> (Offs, Offs) {
		let (start, end) = (range.start, range.end);
		assert!(end > start, "no zero-size slices");
		assert!(end <= self.data.len() as Offs, "range exceeds data length");
		(start, end)
	}
}

impl Index<Offs> for ImageSlice<'_> {
	type Output = u8;
	fn index(&self, idx: Offs) -> &Self::Output {
		&self.data[to_usize(idx)]
	}
}

impl ImageRead<Offs> for ImageSlice<'_> {
	fn read_u8(&self, idx: Offs) -> u8 {
		self.data[to_usize(idx)]
	}

	fn read_le_u16(&self, idx: Offs) -> u16 {
		let data = &self.data[to_usize(idx) .. to_usize(idx + 2)];
		u16::from_le_bytes(data.try_into().unwrap())
	}

	fn read_be_u16(&self, idx: Offs) -> u16 {
		let data = &self.data[to_usize(idx) .. to_usize(idx + 2)];
		u16::from_be_bytes(data.try_into().unwrap())
	}

	fn read_le_u32(&self, idx: Offs) -> u32 {
		let data = &self.data[to_usize(idx) .. to_usize(idx + 4)];
		u32::from_le_bytes(data.try_into().unwrap())
	}

	fn read_be_u32(&self, idx: Offs) -> u32 {
		let data = &self.data[to_usize(idx) .. to_usize(idx + 4)];
		u32::from_be_bytes(data.try_into().unwrap())
	}

	fn read_le_u64(&self, idx: Offs) -> Offs {
		let data = &self.data[to_usize(idx) .. to_usize(idx + 8)];
		u64::from_le_bytes(data.try_into().unwrap())
	}

	fn read_be_u64(&self, idx: Offs) -> Offs {
		let data = &self.data[to_usize(idx) .. to_usize(idx + 8)];
		u64::from_be_bytes(data.try_into().unwrap())
	}
}

impl ImageSliceable<Offs> for ImageSlice<'_> {
	/// Get a read-only slice of this image's data.
	fn image_slice(&'_ self, range: impl RangeBounds<Offs>) -> ImageSlice<'_> {
		let start = match range.start_bound() {
			Bound::Included(&s) => s,
			Bound::Excluded(&s) => s + 1,
			Bound::Unbounded    => 0,
		};

		let end = match range.end_bound() {
			Bound::Included(&e) => e + 1,
			Bound::Excluded(&e) => e,
			Bound::Unbounded    => self.data.len() as Offs,
		};

		let (start, end) = self.check_range(start .. end);
		ImageSlice { data: &self.data[to_usize(start) .. to_usize(end)] }
	}
}

// ------------------------------------------------------------------------------------------------
// Image
// ------------------------------------------------------------------------------------------------

/// A read-only image of a ROM or executable, or a slice thereof.
pub struct Image {
	name:      String,
	data:      Vec<u8>,
	orig_offs: Offs,
}

#[allow(clippy::len_without_is_empty)]
impl Image {
	/// Ctor.
	pub fn new(name: &str, data: &[u8]) -> Self {
		Self {
			name:      name.into(),
			data:      data.into(),
			orig_offs: 0,
		}
	}

	/// Convenience ctor to load the data directly from a file.
	pub fn new_from_file<P: AsRef<std::path::Path>>(filename: P) -> std::io::Result<Self> {
		let data = std::fs::read(&filename)?;
		Ok(Self::new(&filename.as_ref().to_string_lossy().into_owned(), &data))
	}

	/// Create a *new* image whose data is a copy of a range of this one's.
	pub fn new_from_range(&self, range: Range<Offs>) -> Image {
		let (start, end) = self.check_range(range);

		Image {
			name:      self.name.clone(),
			data:      self.data[to_usize(start) .. to_usize(end)].into(),
			orig_offs: self.orig_offs + start,
		}
	}

	/// Get the image's name (file name, usually).
	#[inline] pub fn name(&self) -> &str {
		&self.name
	}

	/// Gets the image's length in bytes.
	#[inline] pub fn len(&self) -> Size {
		self.data.len() as Size
	}

	/// Gets a view of the image's data.
	#[inline] pub fn data(&self) -> &[u8] {
		&self.data
	}

	/// Get the range of the original file from which this was created.
	#[inline] pub fn orig_range(&self) -> Range<Offs> {
		self.orig_offs .. self.orig_offs + self.data.len() as Offs
	}

	// ---------------------------------------------------------------------------------------------
	// private

	fn check_range(&self, range: Range<Offs>) -> (Offs, Offs) {
		let (start, end) = (range.start, range.end);
		assert!(end > start, "no zero-size slices");
		assert!(end <= self.data.len() as Offs, "range exceeds data length");
		(start, end)
	}
}

impl ImageSliceable<Offs> for Image {
	/// Get a read-only slice of this image's data.
	fn image_slice(&'_ self, range: impl RangeBounds<Offs>) -> ImageSlice<'_> {
		let start = match range.start_bound() {
			Bound::Included(&s) => s,
			Bound::Excluded(&s) => s + 1,
			Bound::Unbounded    => 0,
		};

		let end = match range.end_bound() {
			Bound::Included(&e) => e + 1,
			Bound::Excluded(&e) => e,
			Bound::Unbounded    => self.data.len() as Offs,
		};

		let (start, end) = self.check_range(start .. end);
		ImageSlice { data: &self.data[to_usize(start) .. to_usize(end)] }
	}
}