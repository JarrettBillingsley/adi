use std::fmt::{ Debug, Formatter, Result as FmtResult };

use super::segment::*;
use super::spans::*;
use super::newtypes::*;

// ------------------------------------------------------------------------------------------------
// Location
// ------------------------------------------------------------------------------------------------

/// A unique location consisting of a reference to a Segment and an offset within that Segment.
#[derive(Clone, Copy)]
pub struct Location<'a> {
	pub seg: &'a Segment<'a>,
	pub offs: SegOffset,
}

impl<'a> Debug for Location<'a> {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		// TODO: is it possible to get the name for this with only the segment reference?
		write!(f, "{}:{:04X}", self.seg.name, self.offs)
	}
}

impl<'a> Location<'a> {
	pub fn new(seg: &'a Segment, offs: SegOffset) -> Self {
		assert!(seg.contains_offset(offs));
		Self { seg, offs }
	}

	pub fn to_va(&self) -> VAddr {
		self.seg.vbase + self.offs
	}

	pub fn get_span(&self) -> &Span {
		self.seg.span_from_offset(self.offs)
	}
}

// ------------------------------------------------------------------------------------------------
// References
// ------------------------------------------------------------------------------------------------

pub struct Reference<'a> {
	pub src: Location<'a>,
	pub dst: Location<'a>,
}

impl<'a> Reference<'a> {
	pub fn new(src: Location<'a>, dst: Location<'a>) -> Self {
		Self { src, dst }
	}

	// pub fn get_name(&self) -> String {
	// 	self.dst.get_name()
	// }

	// pub fn get_full_name(&self) -> String {
	// 	self.dst.get_full_name()
	// }

	// def __hash__(self):      return hash(self.src) ^ hash(self.dst)
	// def __eq__(self, other): return self.src == other.src and self.dst == other.dst
}