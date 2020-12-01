use std::collections::HashMap;
use std::convert::TryInto;
use std::fmt::{ Display, Formatter, Result as FmtResult };
use std::collections::{
	btree_map::Iter as BTreeIter,
	btree_map::Range as BTreeRange,
	hash_map::Iter as HashIter,
};
use std::ops::RangeBounds;

mod config;
mod map;
mod names;
mod types;
mod region;
mod segment;
mod spans;

#[cfg(test)]
mod tests;

pub use config::*;
pub use map::*;
pub use names::*;
pub use types::*;
pub use region::*;
pub use segment::*;
pub use spans::*;

// ------------------------------------------------------------------------------------------------
// MemoryBuilder
// ------------------------------------------------------------------------------------------------

/// Builder for the Memory object.
pub struct MemoryBuilder<'a> {
	endian:  Endian,
	image:   RomImage<'a>,
	segs:    Vec<Segment<'a>>,
	mem_map: MemoryMap<'a>,
	config:  MemoryConfig<'a>,
	seg_name_map: HashMap<&'a str, usize>,
}

impl<'a> MemoryBuilder<'a> {
	pub fn new(
		endian: Endian,
		image: RomImage<'a>,
		mem_map: MemoryMap<'a>,
		config: MemoryConfig<'a>
	) -> Self {
		Self {
			endian,
			image,
			segs: vec![],
			mem_map,
			config,
			seg_name_map: HashMap::new(),
		}
	}

	/// Add a segment. Name cannot be a duplicate of an existing one.
	pub fn segment(&mut self, name: &'a str, vbase: VAddr, vend: VAddr, pbase: Option<PAddr>) ->
		&mut Self {
		let id = self.segs.len();
		assert!(self.seg_name_map.insert(name, id).is_none(), "duplicate segment name {}", name);
		self.segs.push(Segment::new(SegId(id.try_into().unwrap()), name, vbase, vend, pbase));
		self
	}

	/// Construct the Memory object.
	pub fn build(self) -> Memory<'a> {
		// check the config.
		// it's OK for memory regions to be unmapped (e.g. for mirror areas, optional areas)
		for (region_name, seg_name) in self.config.iter() {
			let region = self.mem_map.region_for_name(&region_name).unwrap();
			let seg = &self.segs[*self.seg_name_map.get(seg_name).unwrap()];
			// if it's bankable, it must be real.
			assert!(!(region.is_bankable() && seg.is_fake()));
		}

		Memory {
			endian:       self.endian,
			image:        self.image,
			segs:         self.segs,
			mem_map:      self.mem_map,
			config:       self.config,
			seg_name_map: self.seg_name_map,
			names:        NameMap::new(),
		}
	}
}

// ------------------------------------------------------------------------------------------------
// Memory
// ------------------------------------------------------------------------------------------------

/// This is the data structure on which everything else is built.
/// Ties together an image, memory map, memory config, segments, and names.
pub struct Memory<'a> {
	endian:       Endian,
	image:        RomImage<'a>,
	segs:         Vec<Segment<'a>>,
	mem_map:      MemoryMap<'a>,
	config:       MemoryConfig<'a>,

	seg_name_map: HashMap<&'a str, usize>,
	// TODO: bankable regions config (stored here, or just passed into methods as needed?)
	names:        NameMap<'a>,
}

impl<'a> Memory<'a> {
	// ---------------------------------------------------------------------------------------------
	// Memory map and Segments

	/// How many bytes are in the memory map (virtual address map).
	pub fn len(&self) -> usize {
		self.mem_map.len()
	}

	/// Given a range of two VAs, do they cross over the boundary between two regions?
	pub fn range_crosses_regions(&self, start: VAddr, end: VAddr) -> bool {
		assert!(end > start);

		match (self.mem_map.region_for_va(start), self.mem_map.region_for_va(end)) {
			(Some(s), Some(e)) => !std::ptr::eq(s, e),
			(None, None)       => false,
			_                  => true,
		}
	}

	/// Given a region name, get the Segment mapped to it (if any).
	pub fn segment_for_region(&self, region_name: &str) -> Option<&Segment> {
		self.config.segment_for_region(region_name)
		.and_then(|seg_name| self.segment_for_name(seg_name))
	}

	/// Given a VA, get the Segment which contains it (if any).
	pub fn segment_for_va(&self, va: VAddr) -> Option<&Segment> {
		self.mem_map.region_for_va(va)
		.and_then(|region| self.config.segment_for_region(region.name))
		.and_then(|name|   self.segment_for_name(name))
	}

	/// Same as above but mutable.
	pub fn segment_for_va_mut(&'a mut self, va: VAddr) -> Option<&mut Segment> {
		let name = self.mem_map.region_for_va(va)
			.and_then(|region| self.config.segment_for_region(region.name));

		// Rust refuses to let me do a similar thing to above and I can't figure out why.
		match name {
			Some(name) => match self.seg_name_map.get(&name) {
				Some(&idx) => Some(&mut self.segs[idx]),
				None       => None,
			},
			None => None,
		}
	}

	/// Given a segment name, get the Segment named that (if any).
	pub fn segment_for_name(&self, name: &str) -> Option<&Segment> {
		self.seg_name_map.get(&name).map(|&idx| &self.segs[idx])
	}

	/// Same as above but mutable.
	pub fn segment_for_name_mut(&'a mut self, name: &str) -> Option<&mut Segment> {
		// Rust refuses to let me do a similar thing to above and I can't figure out why.
		match self.seg_name_map.get(&name) {
			Some(&idx) => Some(&mut self.segs[idx]),
			None       => None,
		}
	}

	/// Given a location, get the Segment which contains it.
	pub fn segment_from_loc(&self, loc: Location) -> &Segment {
		&self.segs[loc.seg.0 as usize]
	}

	/// Same as above but mutable.
	pub fn segment_from_loc_mut(&'a mut self, loc: Location) -> &mut Segment {
		&mut self.segs[loc.seg.0 as usize]
	}

	// TODO: adding/removing/redefining segments

	// ---------------------------------------------------------------------------------------------
	// Address translation

	/// Tries to find a unique location for the given VA.
	/// If there is no mapping, or if the region is bankable, returns None.
	pub fn va_to_loc(&self, va: VAddr) -> Option<Location> {
		self.mem_map.region_for_va(va)
		.and_then(|region| {
			if region.is_bankable() {
				None
			} else {
				self.segment_for_region(region.name)
				.and_then(|seg| {
					let offs = seg.offset_from_va(va);
					Some(Location::new(seg.id, offs))
				})
			}
		})
	}

	// ---------------------------------------------------------------------------------------------
	// Names

	/// Assigns a name to a given Location. Renames it if it already has one.
	pub fn add_name(&mut self, name: &'a str, loc: Location) {
		self.names.add(name, loc);
	}

	/// Assigns a name to a given VA. Panics if the VA doesn't map to a unique Location.
	pub fn add_name_va(&mut self, name: &'a str, va: VAddr) {
		let loc = self.va_to_loc(va).unwrap();
		self.add_name(name, loc);
	}

	/// Removes a name. Panics if the name doesn't exist.
	pub fn remove_name(&mut self, name: &'a str) {
		self.names.remove_name(name)
	}

	/// Removes the name from a location. Panics if there is no name.
	pub fn remove_name_from_loc(&mut self, loc: Location) {
		self.names.remove_loc(loc);
	}

	/// Whether this name exists.
	pub fn has_name(&self, name: &str) -> bool {
		self.names.has_name(name)
	}

	/// Whether this location has a name.
	pub fn has_name_for_loc(&self, loc: Location) -> bool {
		self.names.has_loc(loc)
	}

	/// Gets the name for a location. Panics if it has none.
	pub fn name_from_loc(&self, loc: Location) -> &str {
		self.names.name_for_loc(loc).unwrap()
	}

	/// Gets the location for a name. Panics if the name doesn't exist.
	pub fn loc_from_name(&self, name: &'a str) -> Location {
		self.names.loc_for_name(name).unwrap()
	}

	/// All (name, Location) pairs in arbitrary order.
	pub fn all_names(&self) -> HashIter<'a, &str, Location> {
		self.names.names()
	}

	/// All (Location, name) pairs in Location order.
	pub fn all_names_by_loc(&self) -> BTreeIter<'a, Location, &str> {
		self.names.locations()
	}

	/// All (Location, name) pairs in a given range of Locations, in Location order.
	pub fn names_in_range(&self, range: impl RangeBounds<Location>)
	-> BTreeRange<'a, Location, &str> {
		self.names.names_in_range(range)
	}

	/// Gets the name of a given VA if one exists, or generates one if not.
	pub fn name_of_va(&self, va: VAddr) -> String {
		if let Some(loc) = self.va_to_loc(va) {
			self.name_of_loc(loc)
		// no mapped segment?? uhhhh....... try region name?
		} else if let Some(region) = self.mem_map.region_for_va(va) {
			// name it "REGIONNAME_loc_0C30"
			self.generate_name(region.name, va)
		} else {
			// DUNNO!
			self.generate_name("UNK", va)
		}
	}

	/// Gets the name of a given Location if one exists, or generates one if not.
	pub fn name_of_loc(&self, loc: Location) -> String {
		// see if there's already a name here.
		if let Some(name) = self.names.name_for_loc(loc) {
			name.into()
		} else {
			// what span is here?
			let seg = &self.segs[loc.seg.0 as usize];
			let va = seg.va_from_offset(loc.offs);
			let start = seg.span_from_offset(loc.offs).start;

			match self.names.name_for_loc(Location::new(loc.seg, start)) {
				Some(name) =>
					// there's already a name, so name it like "main_loc_0C30"
					self.generate_name(name, va),
				None =>
					// no name, so name it "SEGNAME_loc_0C30"
					self.generate_name(seg.name, va),
			}
		}
	}

	fn generate_name(&self, base: &str, va: VAddr) -> String {
		format!("{}_{}_{}", base, AUTOGEN_NAME_PREFIX, self.mem_map.fmt_addr(va.0))
	}

	// ---------------------------------------------------------------------------------------------
	// Image

	// TODO: get slices of image, read bytes etc.
	// TODO: spanagement? or just leave that to the segment (segment_for_name_mut)
}

impl<'a> Display for Memory<'a> {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		writeln!(f, "Image: {} (0x{:X} bytes)", self.image.name, self.image.data.len())?;
		writeln!(f, "Memory: 0x{:X} bytes, {}-endian", self.mem_map.len(), self.endian)?;
		writeln!(f, "\nSegments:")?;

		for seg in self.segs.iter() {
			writeln!(f, "    {}", seg)?;
		}

		writeln!(f, "\nMemory map:")?;

		for region in self.mem_map.all_regions() {
			write!(f, "{:>40}", region.to_string())?;

			match self.segment_for_region(region.name) {
				Some(seg) => writeln!(f, " => {}", seg)?,
				None      => writeln!(f, " (unmapped)")?,
			}
		}

		Ok(())
	}
}