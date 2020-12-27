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

#[allow(clippy::new_without_default)]
impl SegCollection {
	/// Makes a new empty collection.
	pub fn new() -> Self {
		Self {
			segs: vec![Segment::new(SegId::invalid(), "[UNRESOLVED]", usize::MAX, None)],
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
	pub fn add_segment(&mut self, name: &str, size: usize, image: Option<Image>) -> SegId {
		let idx = self.segs.len();

		let existing = self.seg_name_map.insert(name.into(), idx);
		assert!(existing.is_none(), "duplicate segment name {}", name);

		let id = self.next_seg_id;
		self.next_seg_id = SegId(self.next_seg_id.0 + 1);
		self.seg_id_map.insert(id, idx);

		self.segs.push(Segment::new(id, name, size, image));

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
// Memory
// ------------------------------------------------------------------------------------------------

/// This is the data structure on which everything else is built.
/// Ties together a memory map and a segment collection.
pub struct Memory {
	bits:       usize,
	digits:     usize,
	endianness: Endian,
	segs:       SegCollection,
	mmu:        Mmu,
}

#[allow(clippy::len_without_is_empty)]
impl Memory {
	pub fn new(bits: usize, endianness: Endian, segs: SegCollection, mmu: Mmu) -> Self {
		Self {
			bits,
			digits: ((bits + 3) & !3) >> 2, // round up to next multiple of 4, divide by 4
			endianness,
			segs,
			mmu
		}
	}

	// ---------------------------------------------------------------------------------------------
	// Getters

	/// Endianness.
	pub fn endianness(&self) -> Endian { self.endianness }
	/// How many bits an address is.
	pub fn bits(&self) -> usize { self.bits }
	/// How many digits in a formatted address.
	pub fn digits(&self) -> usize { self.digits }
	/// The length of the address space.
	pub fn len(&self) -> usize { 2_usize.pow(self.bits as u32) }
	/// The initial state of the MMU.
	pub fn initial_mmu_state(&self) -> MmuState { self.mmu.initial_state() }

	// ---------------------------------------------------------------------------------------------
	// Segments

	delegate! {
		to self.segs {
			/// Given a segment name, get the Segment named that (if any).
			pub fn segment_for_name(&self, name: &str) -> Option<&Segment>;
			/// Same as above but mutable.
			pub fn segment_for_name_mut(&mut self, name: &str) -> Option<&mut Segment>;
			/// Given a location, get the Segment which contains it.
			pub fn segment_from_loc(&self, loc: Location) -> &Segment;
			/// Same as above but mutable.
			pub fn segment_from_loc_mut(&mut self, loc: Location) -> &mut Segment;
			/// Given a segment ID, get the Segment which it refers to.
			pub fn segment_from_id(&self, id: SegId) -> &Segment;
			/// Same as above but mutable.
			pub fn segment_from_id_mut(&mut self, id: SegId) -> &mut Segment;
		}
	}

	/// Given a VA, get the Segment which contains it (if any).
	pub fn segment_for_va(&self, state: MmuState, va: VA) -> Option<&Segment> {
		let loc = self.loc_for_va(state, va)?;
		Some(self.segs.segment_from_loc(loc))
	}

	/// Same as above but mutable.
	pub fn segment_for_va_mut(&mut self, state: MmuState, va: VA) -> Option<&mut Segment> {
		let loc = self.loc_for_va(state, va)?;
		Some(self.segs.segment_from_loc_mut(loc))
	}

	// ---------------------------------------------------------------------------------------------
	// Address translation

	/// Tries to find a unique location for the given VA.
	/// If there is no mapping, or if the region is bankable, returns None.
	pub fn loc_for_va(&self, state: MmuState, va: VA) -> Option<Location> {
		self.mmu.loc_for_va(state, va)
	}

	/// Same as above, but infallible.
	pub fn loc_from_va(&self, state: MmuState, va: VA) -> Location {
		self.loc_for_va(state, va).unwrap_or_else(|| Location::invalid(va.0))
	}

	/// Gets the VA which corresponds to this location, if any.
	pub fn va_for_loc(&self, state: MmuState, loc: Location) -> Option<VA> {
		self.mmu.va_for_loc(state, loc)
	}

	/// Same as above, but infallible.
	pub fn va_from_loc(&self, state: MmuState, loc: Location) -> VA {
		self.va_for_loc(state, loc).unwrap()
	}

	/// Formats a number as a hexadecimal number with the appropriate number of digits
	/// for the size of the address space.
	pub fn fmt_addr(&self, addr: usize) -> String {
		format!("{:0width$X}", addr, width = self.digits)
	}

	/// Come up with an autogenerated name prefix for a given VA.
	pub fn name_prefix_for_va(&self, state: MmuState, va: VA) -> String {
		self.mmu.name_prefix_for_va(state, va)
	}
}

impl Display for Memory {
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