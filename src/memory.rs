use std::collections::HashMap;
use std::fmt::{ Display, Formatter, Result as FmtResult };

use parse_display::Display;
use delegate::delegate;

// ------------------------------------------------------------------------------------------------
// Sub-modules
// ------------------------------------------------------------------------------------------------

mod ea;
mod image;
mod mmu;
mod segment;
mod spans;
mod va;

#[cfg(test)]
mod tests;

pub use ea::*;
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
// MemAccess
// ------------------------------------------------------------------------------------------------

const R_BIT: u8 = 1;
const W_BIT: u8 = 2;
const O_BIT: u8 = 4;
const T_BIT: u8 = 8;

/// How a memory operand is accessed. This kind of looks/works like bitflags, but for reasons
/// of ergonomics, `bitflags` is not used. (`bitflags` prevents `use`-ing the names within.)
#[repr(u8)]
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum MemAccess {
	/// A read (load).
	R = R_BIT,
	/// A write (store).
	W = W_BIT,
	/// Read and write.
	RW = R_BIT | W_BIT,
	/// Getting the address without accessing the data at it. (e.g. `lea`, `la`)
	Offset = O_BIT,
	/// Read and offset.
	RO = R_BIT | O_BIT,
	/// Write and offset.
	WO = W_BIT | O_BIT,
	/// Read, write, and offset.
	RWO = R_BIT | W_BIT | O_BIT,
	/// Used as the target of a jump or branch.
	Target = T_BIT,
	/// Read and target.
	RT = R_BIT | T_BIT,
	/// Write and target.
	WT = W_BIT | T_BIT,
	/// Read, write, and target.
	RWT = R_BIT | W_BIT | T_BIT,
	/// Offset and target.
	OT = O_BIT | T_BIT,
	/// Read, offset, and target.
	ROT = R_BIT | O_BIT | T_BIT,
	/// Write, offset, and target.
	WOT = W_BIT | O_BIT | T_BIT,
	/// Read, write, offset, and target.
	RWOT = R_BIT | W_BIT | O_BIT | T_BIT,
}

impl MemAccess {
	/// Does this read memory?
	pub fn reads_mem(&self) -> bool {
		((*self as u8) & R_BIT) != 0
	}

	/// Does this write memory?
	pub fn writes_mem(&self) -> bool {
		((*self as u8) & W_BIT) != 0
	}
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
		let mut ret = Self {
			segs: vec![
				Segment::new_with_va(SegId::unresolved(), "[UNRESOLVED]", usize::MAX, None,
					Some(VA(0)))
			],
			next_seg_id: SegId(0),
			seg_name_map: HashMap::new(),
			seg_id_map: HashMap::new(),
		};

		ret.seg_id_map.insert(SegId::unresolved(), 0);

		ret
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

	/// Same as above, but also initializes its base VA.
	///
	/// # Panics
	///
	/// - if `name` is already the name of an existing segment.
	pub fn add_segment_with_va(&mut self, name: &str, size: usize, image: Option<Image>,
	base_va: VA) -> SegId {
		let ret = self.add_segment(name, size, image);
		self.segment_from_id_mut(ret).set_base_va(base_va);
		ret
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

	/// Given an EA, get the Segment which contains it.
	pub fn segment_from_ea(&self, ea: EA) -> &Segment {
		&self.segs[*self.seg_id_map.get(&ea.seg()).unwrap()]
	}

	/// Same as above but mutable.
	pub fn segment_from_ea_mut(&mut self, ea: EA) -> &mut Segment {
		&mut self.segs[*self.seg_id_map.get(&ea.seg()).unwrap()]
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

	/// Iterator over all segments that map to an image.
	pub fn image_segs_iter(&self) -> impl Iterator<Item = &Segment> {
		self.segs.iter().filter(|s| s.is_real())
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

	// ---------------------------------------------------------------------------------------------
	// MMU

	delegate! {
		to self.mmu {
			/// The initial state of the MMU.
			#[call(initial_state)]
			pub fn initial_mmu_state(&self) -> MmuState;

			/// How would the given memory access change the state?
			pub fn state_change(&self, state: MmuState, va: VA, val: Option<u64>, load: bool)
			-> StateChange;
		}
	}

	// ---------------------------------------------------------------------------------------------
	// Segments

	delegate! {
		to self.segs {
			/// Given a segment name, get the Segment named that (if any).
			pub fn segment_for_name(&self, name: &str) -> Option<&Segment>;
			/// Same as above but mutable.
			pub fn segment_for_name_mut(&mut self, name: &str) -> Option<&mut Segment>;
			/// Given an EA, get the Segment which contains it.
			pub fn segment_from_ea(&self, ea: EA) -> &Segment;
			/// Same as above but mutable.
			pub fn segment_from_ea_mut(&mut self, ea: EA) -> &mut Segment;
			/// Given a segment ID, get the Segment which it refers to.
			pub fn segment_from_id(&self, id: SegId) -> &Segment;
			/// Same as above but mutable.
			pub fn segment_from_id_mut(&mut self, id: SegId) -> &mut Segment;
			/// Iterator over all segments.
			#[call(iter)]
			pub fn segs_iter(&self) -> impl Iterator<Item = &Segment>;
			/// Iterator over all segments that map to an image.
			pub fn image_segs_iter(&self) -> impl Iterator<Item = &Segment>;
		}
	}

	/// Given a VA, get the Segment which contains it (if any).
	pub fn segment_for_va(&self, state: MmuState, va: VA) -> Option<&Segment> {
		let ea = self.ea_for_va(state, va)?;
		Some(self.segs.segment_from_ea(ea))
	}

	/// Same as above but mutable.
	pub fn segment_for_va_mut(&mut self, state: MmuState, va: VA) -> Option<&mut Segment> {
		let ea = self.ea_for_va(state, va)?;
		Some(self.segs.segment_from_ea_mut(ea))
	}

	// ---------------------------------------------------------------------------------------------
	// Address translation

	/// Tries to find a unique EA for the given VA.
	/// If there is no mapping, or if the region is bankable, returns None.
	pub fn ea_for_va(&self, state: MmuState, va: VA) -> Option<EA> {
		self.mmu.ea_for_va(state, va)
	}

	/// Same as above, but infallible.
	pub fn ea_from_va(&self, state: MmuState, va: VA) -> EA {
		self.ea_for_va(state, va).unwrap_or_else(|| EA::unresolved(va.0))
	}

	/// Gets the VA which corresponds to this EA, if any.
	pub fn va_for_ea(&self, state: MmuState, ea: EA) -> Option<VA> {
		if ea.is_unresolved() {
			Some(VA(ea.offs()))
		} else {
			self.mmu.va_for_ea(state, ea)
		}
	}

	/// Same as above, but infallible.
	pub fn va_from_ea(&self, state: MmuState, ea: EA) -> VA {
		self.va_for_ea(state, ea).unwrap()
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