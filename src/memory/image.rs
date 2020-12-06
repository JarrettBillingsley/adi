use std::ops::{ Range, RangeBounds, Bound };

use super::types::*;

// ------------------------------------------------------------------------------------------------
// Image
// ------------------------------------------------------------------------------------------------

/// A read-only image of a ROM or executable, or a slice thereof.
pub struct Image {
	name: String,
	data: Vec<u8>,
	orig_offs: Offset,
}

/// A read-only slice of an `Image`'s data.
pub struct ImageSlice<'img> {
	data: &'img [u8],
}

impl ImageSlice<'_> {
	// TODO: more interface
	pub fn data(&self) -> &[u8] {
		self.data
	}
}

/// Trait for getting `ImageSlice`s from things.
pub trait ImageSliceable<Index> {
	fn image_slice(&self, bounds: impl RangeBounds<Index>) -> ImageSlice;
}

impl Image {
	/// Ctor.
	pub fn new(name: &str, data: &[u8]) -> Self {
		Self {
			name:      name.into(),
			data:      data.into(),
			orig_offs: Offset(0),
		}
	}

	/// Convenience ctor to load the data directly from a file.
	pub fn new_from_file(filename: &str) -> std::io::Result<Self> {
		let data = std::fs::read(filename)?;
		Ok(Self::new(filename, &data))
	}

	/// Create a *new* image whose data is a copy of a range of this one's.
	pub fn new_from_range(&self, range: Range<Offset>) -> Image {
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
	pub fn orig_range(&self) -> Range<Offset> {
		self.orig_offs .. self.orig_offs + self.data.len()
	}

	// ---------------------------------------------------------------------------------------------
	// private

	fn check_range(&self, range: Range<Offset>) -> (usize, usize) {
		let (start, end) = (range.start.0, range.end.0);
		assert!(end > start, "no zero-size slices");
		assert!(end <= self.data.len(), "range exceeds data length");
		(start, end)
	}
}

impl ImageSliceable<Offset> for Image {
	/// Get a read-only slice of this image's data.
	fn image_slice(&self, range: impl RangeBounds<Offset>) -> ImageSlice {
		let start = match range.start_bound() {
			Bound::Included(&s) => s,
			Bound::Excluded(&s) => s + 1,
			Bound::Unbounded    => Offset(0),
		};

		let end = match range.end_bound() {
			Bound::Included(&e) => e + 1,
			Bound::Excluded(&e) => e,
			Bound::Unbounded    => Offset(self.data.len()),
		};

		let (start, end) = self.check_range(start .. end);
		ImageSlice { data: &self.data[start .. end] }
	}
}
