use std::collections::HashMap;
use std::fmt::{ Display, Formatter, Result as FmtResult };

use parse_display::Display;
use delegate::delegate;

// ------------------------------------------------------------------------------------------------
// Sub-modules
// ------------------------------------------------------------------------------------------------

mod image;
mod map;
mod mmu;
mod region;
mod segment;
mod spans;
mod va;

#[cfg(test)]
mod tests;

pub use image::*;
pub use map::*;
pub use mmu::*;
pub use region::*;
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
// Memory
// ------------------------------------------------------------------------------------------------

/// This is the data structure on which everything else is built.
/// Ties together a memory map and a segment collection.
pub struct Memory {
	endianness: Endian,
	segs:       SegCollection,
	mem_map:    MemoryMap,
}

impl Memory {
	pub fn new(endianness: Endian, segs: SegCollection, mem_map: MemoryMap) -> Self {
		Self { endianness, segs, mem_map }
	}

	// ---------------------------------------------------------------------------------------------
	// Getters

	/// Get the memory address space map.
	#[inline] pub fn map(&self) -> &MemoryMap {
		&self.mem_map
	}

	/// Gets endianness.
	#[inline] pub fn endianness(&self) -> Endian {
		self.endianness
	}

	// ---------------------------------------------------------------------------------------------
	// Regions

	/// Given a range of two VAs, do they cross over the boundary between two regions?
	pub fn range_crosses_regions(&self, start: VA, end: VA) -> bool {
		assert!(end > start);

		match (self.mem_map.region_for_va(start), self.mem_map.region_for_va(end)) {
			(Some(s), Some(e)) => !std::ptr::eq(s, e),
			(None, None)       => false,
			_                  => true,
		}
	}

	delegate! {
		to self.mem_map {
			/// Given a virtual address, get the memory region which contains it, if any.
			pub fn region_for_va(&self, va: VA) -> Option<&MemoryRegion>;
			/// Given a name, gets the memory region with that name, if any.
			pub fn region_for_name(&self, name: &str) -> Option<&MemoryRegion>;
		}
	}

	// ---------------------------------------------------------------------------------------------
	// Segments

	delegate! {
		to self.segs {
			/// Adds a new segment.
			///
			/// # Panics
			///
			/// - if `name` is already the name of an existing segment.
			pub fn add_segment(&mut self, name: &str, vbase: VA, vend: VA, image: Option<Image>);
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
	pub fn segment_for_va(&self, va: VA) -> Option<&Segment> {
		let seg_id = self.segid_for_va(va)?;
		Some(self.segs.segment_from_id(seg_id))
	}

	/// Same as above but mutable.
	pub fn segment_for_va_mut(&mut self, va: VA) -> Option<&mut Segment> {
		let seg_id = self.segid_for_va(va)?;
		Some(self.segs.segment_from_id_mut(seg_id))
	}

	// ---------------------------------------------------------------------------------------------
	// Address translation

	/// Tries to find a unique location for the given VA.
	/// If there is no mapping, or if the region is bankable, returns None.
	pub fn loc_for_va(&self, va: VA) -> Option<Location> {
		let seg = self.segs.segment_from_id(self.segid_for_va(va)?);
		Some(seg.loc_from_va(va))
	}

	/// Same as above, but infallible.
	pub fn loc_from_va(&self, va: VA) -> Location {
		self.loc_for_va(va).unwrap()
	}

	/// Formats a number as a hexadecimal number with the appropriate number of digits
	/// for the size of the address space.
	pub fn fmt_addr(&self, addr: usize) -> String {
		format!("{:0width$X}", addr, width = self.mem_map.digits())
	}

	// ---------------------------------------------------------------------------------------------
	// Private

	fn segid_for_va(&self, va: VA) -> Option<SegId> {
		self.segid_for_region(self.mem_map.region_for_va(va)?)
	}

	fn segid_for_region(&self, region: &MemoryRegion) -> Option<SegId> {
		if region.seg().is_some() {
			region.seg()
		} else {
			// TODO: this is where MMU comes in
			None
		}
	}

	fn segment_for_region(&self, region: &MemoryRegion) -> Option<&Segment> {
		let seg_id = self.segid_for_region(region)?;
		Some(self.segs.segment_from_id(seg_id))
	}
}

impl Display for Memory {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		writeln!(f, "Memory: 0x{:X} bytes, {}-endian", self.mem_map.len(), self.endianness)?;
		writeln!(f, "\nSegments:")?;

		for seg in self.segs.iter() {
			writeln!(f, "    {}", seg)?;
		}

		writeln!(f, "\nMemory map:")?;

		for region in self.mem_map.all_regions() {
			write!(f, "{:>40}", region.to_string())?;

			match self.segment_for_region(region) {
				Some(seg) => writeln!(f, " => {}", seg)?,
				None      => writeln!(f, " (unmapped)")?,
			}
		}

		Ok(())
	}
}