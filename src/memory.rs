
use std::fmt::{ Display, Formatter, Result as FmtResult };

use parse_display::Display;
use delegate::delegate;

use crate::fxhash::{ FxHashMap as HashMap, FxHashMapEx };

use crate::{ Offs, Size };

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

	/// Is this an offset?
	pub fn is_offset(&self) -> bool {
		((*self as u8) & O_BIT) != 0
	}

	/// Is this a control flow target?
	pub fn is_target(&self) -> bool {
		((*self as u8) & T_BIT) != 0
	}

	/// Returns a new MemAccess that is the union of `self` and `other` (that is, with all bits
	/// in either of the sources turned on).
	pub fn union(&self, other: MemAccess) -> MemAccess {
		let combined = (*self as u8) | (other as u8);
		assert!(combined <= (MemAccess::RWOT as u8));
		unsafe {
			std::mem::transmute(combined)
		}
	}
}

impl Display for MemAccess {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		use MemAccess::*;
		match self {
			// technically these could be handled by the default case but let's common-case
			// fast-path them cause "mixed" mem accesses are likely to be rare
			R      => write!(f, "READ"),
			W      => write!(f, "WRITE"),
			Offset => write!(f, "OFFSET"),
			Target => write!(f, "TARGET"),
			_ => {
				// 2 or more bits set
				let mut first = true;

				if self.reads_mem() {
					write!(f, "READ")?;
					first = false;
				}

				if self.writes_mem() {
					if !first { write!(f, "+")?; }
					write!(f, "WRITE")?;
					first = false;
				}

				if self.is_offset() {
					if !first { write!(f, "+")?; }
					write!(f, "OFFSET")?;
					first = false;
				}

				if self.is_target() {
					if !first { write!(f, "+")?; }
					write!(f, "TARGET")?;
				}

				Ok(())
			}
		}
	}
}

// ------------------------------------------------------------------------------------------------
// SegCollection
// ------------------------------------------------------------------------------------------------

pub struct SegCollection {
	/// The actual segments.
	segs:         Vec<Segment>,
	/// Monotonically increasing ID. IDs are never reused.
	next_seg_id:  SegId,
	/// Maps from segment names to indices into `segs`.
	seg_name_map: HashMap<String, usize>,
	/// Maps from segment IDs to indices into `segs`.
	seg_id_map:   HashMap<SegId, usize>,
}

#[allow(clippy::new_without_default)]
impl SegCollection {
	const UNRESOLVED: &'static str = "[UNRESOLVED]";
	const STRUCTS:    &'static str = "[STRUCTS]";
	const ENUMS:      &'static str = "[ENUMS]";
	const BITFIELDS:  &'static str = "[BITFIELDS]";

	fn check_not_reserved_name(name: &str) {
		match name {
			Self::UNRESOLVED | Self::STRUCTS | Self::ENUMS | Self::BITFIELDS =>
				panic!("'{}' is a reserved name.", name),
			_ => {}
		}
	}

	/// Makes a new empty collection.
	pub fn new() -> Self {
		let dummy = Some(VA(0));
		let segs = vec![
			Segment::new_with_va(SegId::unresolved(), Self::UNRESOLVED, Size::MAX, None, dummy),
			Segment::new_with_va(SegId::structs(),    Self::STRUCTS,    Size::MAX, None, dummy),
			Segment::new_with_va(SegId::enums(),      Self::ENUMS,      Size::MAX, None, dummy),
			Segment::new_with_va(SegId::bitfields(),  Self::BITFIELDS,  Size::MAX, None, dummy),
		];

		let seg_id_map = segs.iter().enumerate()
			.map(|(i, seg)| (seg.id(), i))
			.collect();

		Self {
			segs,
			next_seg_id:  SegId(0),
			seg_name_map: HashMap::new(),
			seg_id_map,
		}
	}

	fn generate_id(&mut self) -> SegId {
		let mut id = self.next_seg_id;

		// want to skip over any used IDs.
		while self.seg_id_map.contains_key(&id) {
			if id.id + 1 > SegId::LAST_USER {
				panic!("Ran out of segment IDs!!!");
			}

			id = SegId(id.id + 1);
		}

		// this may be invalid, but it won't give an error until the *next* time we generate an ID.
		self.next_seg_id = SegId::unchecked(id.id + 1);
		id
	}

	/// Adds a new segment with the given ID.
	///
	/// # Panics
	///
	/// - if `id` is already in use.
	/// - if `name` is the name of an existing segment.
	/// - if `name` is a reserved (internal) segment name.
	pub fn add_with_id(&mut self, id: SegId, name: &str, size: Size, image: Option<Image>)
	-> SegId {
		Self::check_not_reserved_name(name);
		if self.seg_id_map.contains_key(&id) {
			panic!("id {:?} is already in use.", id);
		}

		let idx = self.segs.len();
		let existing = self.seg_name_map.insert(name.into(), idx);
		assert!(existing.is_none(), "segment name {} is already in use.", name);
		self.seg_id_map.insert(id, idx);
		self.segs.push(Segment::new(id, name, size, image));
		id
	}

	/// Same as above, but automatically generates an ID for it and returns that.
	///
	/// # Panics
	///
	/// - if there are no more valid segment IDs left.
	/// - if `name` is the name of an existing segment.
	/// - if `name` is a reserved (internal) segment name.
	pub fn add(&mut self, name: &str, size: Size, image: Option<Image>) -> SegId {
		Self::check_not_reserved_name(name);

		let id = self.generate_id();
		self.add_with_id(id, name, size, image);
		id
	}

	/// Same as above, but also initializes its base VA.
	///
	/// # Panics
	///
	/// - if `name` is the name of an existing segment.
	/// - if `name` is a reserved (internal) segment name.
	/// - if there are no more valid segment IDs left.
	pub fn add_with_va(&mut self, name: &str, size: Size, image: Option<Image>, base_va: VA)
	-> SegId {
		let ret = self.add(name, size, image);
		self.segment_from_id_mut(ret).set_base_va(base_va);
		ret
	}

	/// Same as above, but with a given ID.
	///
	/// # Panics
	///
	/// - if `name` is the name of an existing segment.
	/// - if `name` is a reserved (internal) segment name.
	/// - if there are no more valid segment IDs left.
	pub fn add_with_id_va(&mut self, id: SegId, name: &str, size: Size, image: Option<Image>,
	base_va: VA) -> SegId {
		self.add_with_id(id, name, size, image);
		self.segment_from_id_mut(id).set_base_va(base_va);
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
	pub fn len(&self) -> Size { 2_u64.pow(self.bits as u32) }

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
		match ea.seg().id {
			SegId::STRUCTS |
			SegId::ENUMS |
			SegId::BITFIELDS |
			SegId::UNRESOLVED      => Some(VA(ea.offs())),
			0 ..= SegId::LAST_USER => self.mmu.va_for_ea(state, ea),
			_                      => unimplemented!("was a new reserved segment added?"),
		}
	}

	/// Same as above, but infallible.
	pub fn va_from_ea(&self, state: MmuState, ea: EA) -> VA {
		self.va_for_ea(state, ea).unwrap()
	}

	/// Formats a number as a hexadecimal number with the appropriate number of digits
	/// for the size of the address space.
	pub fn fmt_addr(&self, addr: Offs) -> String {
		format!("{:0width$X}", addr, width = self.digits)
	}

	/// Come up with an autogenerated name prefix for a given VA.
	pub fn name_prefix_for_va(&self, state: MmuState, va: VA) -> String {
		self.mmu.name_prefix_for_va(state, va)
	}
}

impl Display for Memory {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		use std::collections::{ HashSet };
		let images: HashSet<_> = self.segs.iter()
			.filter_map(Segment::image)
			.map(Image::name)
			.collect();

		writeln!(f, "Memory: 0x{:X} bytes, {}-endian", self.len(), self.endianness)?;

		if images.len() == 1 {
			writeln!(f, "Image: {:?}", images.iter().next().unwrap())?;
		} else {
			writeln!(f, "Images: {:?}", images)?;
		}

		writeln!(f, "MMU: {}", self.mmu)?;
		writeln!(f, "\nSegments:")?;

		for seg in self.segs.iter() {
			writeln!(f, "    {}", seg)?;
		}

		Ok(())
	}
}