use std::ops::{ Range, RangeBounds, Bound, Index };
use std::convert::TryInto;

// ------------------------------------------------------------------------------------------------
// ImageRead
// ------------------------------------------------------------------------------------------------

pub trait ImageRead<Index> {
	fn read_u8(&self, index: Index) -> u8;
	fn read_le_u16(&self, index: Index) -> u16;
	fn read_be_u16(&self, index: Index) -> u16;
	fn read_le_u32(&self, index: Index) -> u32;
	fn read_be_u32(&self, index: Index) -> u32;
}

// ------------------------------------------------------------------------------------------------
// ImageSlice
// ------------------------------------------------------------------------------------------------

/// A read-only slice of an `Image`'s data.
pub struct ImageSlice<'img> {
	data: &'img [u8],
}

impl<'img> ImageSlice<'img> {
	// TODO: more interface
	pub fn data(&self) -> &'img [u8] {
		self.data
	}

	pub fn into_data(self) -> &'img [u8] {
		self.data
	}
}

impl Index<usize> for ImageSlice<'_> {
	type Output = u8;
	fn index(&self, idx: usize) -> &Self::Output {
		&self.data[idx]
	}
}

impl ImageRead<usize> for ImageSlice<'_> {
	fn read_u8(&self, idx: usize) -> u8 {
		self.data[idx]
	}

	fn read_le_u16(&self, idx: usize) -> u16 {
		let data = &self.data[idx .. idx + 2];
		u16::from_le_bytes(data.try_into().unwrap())
	}

	fn read_be_u16(&self, idx: usize) -> u16 {
		let data = &self.data[idx .. idx + 2];
		u16::from_be_bytes(data.try_into().unwrap())
	}

	fn read_le_u32(&self, idx: usize) -> u32 {
		let data = &self.data[idx .. idx + 4];
		u32::from_le_bytes(data.try_into().unwrap())
	}

	fn read_be_u32(&self, idx: usize) -> u32 {
		let data = &self.data[idx .. idx + 4];
		u32::from_be_bytes(data.try_into().unwrap())
	}
}

// ------------------------------------------------------------------------------------------------
// ImageSliceable
// ------------------------------------------------------------------------------------------------

/// Trait for getting `ImageSlice`s from things.
pub trait ImageSliceable<Index> {
	fn image_slice(&self, bounds: impl RangeBounds<Index>) -> ImageSlice;
}

// ------------------------------------------------------------------------------------------------
// Image
// ------------------------------------------------------------------------------------------------

/// A read-only image of a ROM or executable, or a slice thereof.
pub struct Image {
	name:      String,
	data:      Vec<u8>,
	orig_offs: usize,
}

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
	pub fn new_from_file(filename: &str) -> std::io::Result<Self> {
		let data = std::fs::read(filename)?;
		Ok(Self::new(filename, &data))
	}

	/// Create a *new* image whose data is a copy of a range of this one's.
	pub fn new_from_range(&self, range: Range<usize>) -> Image {
		let (start, end) = self.check_range(range);

		Image {
			name:      self.name.clone(),
			data:      self.data[start .. end].into(),
			orig_offs: self.orig_offs + start,
		}
	}

	/// Get the image's name (file name, usually).
	pub fn name(&self) -> &str {
		&self.name
	}

	/// Get the range of the original file from which this was created.
	pub fn orig_range(&self) -> Range<usize> {
		self.orig_offs .. self.orig_offs + self.data.len()
	}

	// ---------------------------------------------------------------------------------------------
	// private

	fn check_range(&self, range: Range<usize>) -> (usize, usize) {
		let (start, end) = (range.start, range.end);
		assert!(end > start, "no zero-size slices");
		assert!(end <= self.data.len(), "range exceeds data length");
		(start, end)
	}
}

impl ImageSliceable<usize> for Image {
	/// Get a read-only slice of this image's data.
	fn image_slice(&self, range: impl RangeBounds<usize>) -> ImageSlice {
		let start = match range.start_bound() {
			Bound::Included(&s) => s,
			Bound::Excluded(&s) => s + 1,
			Bound::Unbounded    => 0,
		};

		let end = match range.end_bound() {
			Bound::Included(&e) => e + 1,
			Bound::Excluded(&e) => e,
			Bound::Unbounded    => self.data.len(),
		};

		let (start, end) = self.check_range(start .. end);
		ImageSlice { data: &self.data[start .. end] }
	}
}
