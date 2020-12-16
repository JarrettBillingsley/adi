use std::collections::HashMap;
use std::fmt::{ Display, Formatter, Result as FmtResult };

use parse_display::Display;
use delegate::delegate;

// ------------------------------------------------------------------------------------------------
// Sub-modules
// ------------------------------------------------------------------------------------------------

mod image;
mod mmu;
mod segment;
mod spans;
mod va;

#[cfg(test)]
mod tests;

pub use image::*;
pub use mmu::*;
pub use segment::*;
pub use spans::*;
pub use va::*;

// ------------------------------------------------------------------------------------------------
// Endian
// ------------------------------------------------------------------------------------------------

/// Byte order.
#[derive(Debug, Display, PartialEq, Eq, Clone, Copy)]
pub enum Endian {
	#[display("little")] Little,
	#[display("big")]    Big,
	#[display("n/a")]    NA,
}

// ------------------------------------------------------------------------------------------------
// SegCollection
// ------------------------------------------------------------------------------------------------

pub struct SegCollection {
	/// The actual segments.
	segs:         Vec<Segment>,
	/// Monotonically increasing ID. IDs are never reused
	next_seg_id:  SegId,
	/// Maps from segment names to indices into `segs`.
	seg_name_map: HashMap<String, usize>,
	/// Maps from segment IDs to indices into `segs`.
	seg_id_map:   HashMap<SegId, usize>,
}

impl SegCollection {
	/// Makes a new empty collection.
	pub fn new() -> Self {
		Self {
			segs: Vec::new(),
			next_seg_id: SegId(0),
			seg_name_map: HashMap::new(),
			seg_id_map: HashMap::new(),
		}
	}

	/// Adds a new segment. Returns its id.
	///
	/// # Panics
	///
	/// - if `name` is already the name of an existing segment.
	pub fn add_segment(&mut self, name: &str, vbase: VA, vend: VA, image: Option<Image>) -> SegId {
		let idx = self.segs.len();

		let existing = self.seg_name_map.insert(name.into(), idx);
		assert!(existing.is_none(), "duplicate segment name {}", name);

		let id = self.next_seg_id;
		self.next_seg_id = SegId(self.next_seg_id.0 + 1);
		self.seg_id_map.insert(id, idx);

		self.segs.push(Segment::new(id, name, vbase, vend, image));

		id
	}

	/// Given a segment name, get the Segment named that (if any).
	pub fn segment_for_name(&self, name: &str) -> Option<&Segment> {
		let idx = self.seg_name_map.get(name)?;
		Some(&self.segs[*idx])
	}

	/// Same as above but mutable.
	pub fn segment_for_name_mut(&mut self, name: &str) -> Option<&mut Segment> {
		let segs = &mut self.segs;
		let idx = self.seg_name_map.get_mut(name)?;
		Some(&mut segs[*idx])
	}

	/// Given a location, get the Segment which contains it.
	pub fn segment_from_loc(&self, loc: Location) -> &Segment {
		&self.segs[*self.seg_id_map.get(&loc.seg).unwrap()]
	}

	/// Same as above but mutable.
	pub fn segment_from_loc_mut(&mut self, loc: Location) -> &mut Segment {
		&mut self.segs[*self.seg_id_map.get(&loc.seg).unwrap()]
	}

	/// Given a segment ID, get the Segment which it refers to.
	pub fn segment_from_id(&self, id: SegId) -> &Segment {
		&self.segs[*self.seg_id_map.get(&id).unwrap()]
	}

	/// Same as above but mutable.
	pub fn segment_from_id_mut(&mut self, id: SegId) -> &mut Segment {
		&mut self.segs[*self.seg_id_map.get(&id).unwrap()]
	}

	/// Iterator over all segments.
	pub fn iter(&self) -> impl Iterator<Item = &Segment> {
		self.segs.iter()
	}
}

// ------------------------------------------------------------------------------------------------
// IMemory
// ------------------------------------------------------------------------------------------------

pub trait IMemory: Display {
	// ---------------------------------------------------------------------------------------------
	// Getters

	/// Endianness.
	fn endianness(&self) -> Endian;
	/// How many bits an address is.
	fn bits(&self) -> usize;
	/// How many digits in a formatted address.
	fn digits(&self) -> usize;
	/// The length of the address space.
	fn len(&self) -> usize;

	// ---------------------------------------------------------------------------------------------
	// Segments

	/// Given a segment name, get the Segment named that (if any).
	fn segment_for_name(&self, name: &str) -> Option<&Segment>;
	/// Same as above but mutable.
	fn segment_for_name_mut(&mut self, name: &str) -> Option<&mut Segment>;
	/// Given a location, get the Segment which contains it.
	fn segment_from_loc(&self, loc: Location) -> &Segment;
	/// Same as above but mutable.
	fn segment_from_loc_mut(&mut self, loc: Location) -> &mut Segment;
	/// Given a segment ID, get the Segment which it refers to.
	fn segment_from_id(&self, id: SegId) -> &Segment;
	/// Same as above but mutable.
	fn segment_from_id_mut(&mut self, id: SegId) -> &mut Segment;
	/// Given a VA, get the Segment which contains it (if any).
	fn segment_for_va(&self, va: VA) -> Option<&Segment>;
	/// Same as above but mutable.
	fn segment_for_va_mut(&mut self, va: VA) -> Option<&mut Segment>;

	// ---------------------------------------------------------------------------------------------
	// Address translation

	/// Tries to find a unique location for the given VA.
	/// If there is no mapping, or if the region is bankable, returns None.
	fn loc_for_va(&self, va: VA) -> Option<Location>;
	/// Same as above, but infallible.
	fn loc_from_va(&self, va: VA) -> Location;
	/// Formats a number as a hexadecimal number with the appropriate number of digits
	/// for the size of the address space.
	fn fmt_addr(&self, addr: usize) -> String;
	/// Come up with an autogenerated name prefix for a given VA.
	fn name_prefix_for_va(&self, va: VA) -> String;
}

// ------------------------------------------------------------------------------------------------
// Memory
// ------------------------------------------------------------------------------------------------

/// This is the data structure on which everything else is built.
/// Ties together a memory map and a segment collection.
pub struct Memory<TMmu: IMmu> {
	bits:       usize,
	digits:     usize,
	endianness: Endian,
	segs:       SegCollection,
	mmu:        TMmu,
}

impl<TMmu: IMmu> Memory<TMmu> {
	pub fn new(bits: usize, endianness: Endian, segs: SegCollection, mmu: TMmu) -> Self {
		Self {
			bits,
			digits: ((bits + 3) & !3) >> 2, // round up to next multiple of 4, divide by 4
			endianness,
			segs,
			mmu
		}
	}

	fn segid_for_va(&self, va: VA) -> Option<SegId> {
		self.mmu.segid_for_va(self.mmu.initial_state(), va)
	}
}

impl<TMmu: IMmu> IMemory for Memory<TMmu> {
	// ---------------------------------------------------------------------------------------------
	// Getters

	fn endianness(&self) -> Endian { self.endianness }
	fn bits(&self) -> usize { self.bits }
	fn digits(&self) -> usize { self.digits }
	fn len(&self) -> usize { 2_usize.pow(self.bits as u32) }

	// ---------------------------------------------------------------------------------------------
	// Segments

	delegate! {
		to self.segs {
			fn segment_for_name(&self, name: &str) -> Option<&Segment>;
			fn segment_for_name_mut(&mut self, name: &str) -> Option<&mut Segment>;
			fn segment_from_loc(&self, loc: Location) -> &Segment;
			fn segment_from_loc_mut(&mut self, loc: Location) -> &mut Segment;
			fn segment_from_id(&self, id: SegId) -> &Segment;
			fn segment_from_id_mut(&mut self, id: SegId) -> &mut Segment;
		}
	}

	fn segment_for_va(&self, va: VA) -> Option<&Segment> {
		let seg_id = self.segid_for_va(va)?;
		Some(self.segs.segment_from_id(seg_id))
	}

	fn segment_for_va_mut(&mut self, va: VA) -> Option<&mut Segment> {
		let seg_id = self.segid_for_va(va)?;
		Some(self.segs.segment_from_id_mut(seg_id))
	}

	// ---------------------------------------------------------------------------------------------
	// Address translation

	fn loc_for_va(&self, va: VA) -> Option<Location> {
		let seg = self.segs.segment_from_id(self.segid_for_va(va)?);
		Some(seg.loc_from_va(va))
	}

	fn loc_from_va(&self, va: VA) -> Location {
		self.loc_for_va(va).unwrap()
	}

	fn fmt_addr(&self, addr: usize) -> String {
		format!("{:0width$X}", addr, width = self.digits)
	}

	fn name_prefix_for_va(&self, va: VA) -> String {
		self.mmu.name_prefix_for_va(self.mmu.initial_state(), va)
	}
}

impl<TMmu: IMmu> Display for Memory<TMmu> {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		writeln!(f, "Memory: 0x{:X} bytes, {}-endian", self.len(), self.endianness)?;
		writeln!(f, "MMU: {}", self.mmu)?;
		writeln!(f, "\nSegments:")?;

		for seg in self.segs.iter() {
			writeln!(f, "    {}", seg)?;
		}

		Ok(())
	}
}