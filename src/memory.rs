use std::collections::HashMap;
use std::fmt::{ Display, Formatter, Result as FmtResult };
use std::collections::{
	btree_map::Iter as BTreeIter,
	btree_map::Range as BTreeRange,
	hash_map::Iter as HashIter,
};
use std::ops::RangeBounds;

mod config;
mod locref;
mod map;
mod names;
mod newtypes;
mod region;
mod segment;
mod spans;

#[cfg(test)]
mod tests;

pub use config::*;
pub use locref::*;
pub use map::*;
pub use names::*;
pub use newtypes::*;
pub use region::*;
pub use segment::*;
pub use spans::*;

// ------------------------------------------------------------------------------------------------
// Memory
// ------------------------------------------------------------------------------------------------

/// This is the data structure on which everything else is built.
/// Ties together an image, memory map, memory config, segments, and names.
pub struct Memory<'a> {
	image:        RomImage<'a>,
	mem_map:      MemoryMap<'a>,
	segs:         &'a mut [Segment<'a>],
	seg_name_map: HashMap<&'a str, usize>,
	config:       MemoryConfig<'a>,
	// TODO: bankable regions config (stored here, or just passed into methods as needed?)
	names:        NameMap<'a>,
}

impl<'a> Memory<'a> {
	pub fn new(
		image: RomImage<'a>,
		segs: &'a mut [Segment<'a>],
		mem_map: MemoryMap<'a>,
		config: MemoryConfig<'a>
	) -> Self {
		let mut seg_name_map = HashMap::new();

		for (i, s) in segs.iter().enumerate() {
			assert!(seg_name_map.insert(s.name, i).is_none());
		}

		// check the config.
		// it's OK for memory regions to be unmapped (e.g. for mirror areas, optional areas)
		for (region_name, seg_name) in config.iter() {
			let region = mem_map.region_for_name(&region_name).unwrap();
			let seg = &segs[*seg_name_map.get(seg_name).unwrap()];
			// if it's bankable, it must be real.
			assert!(!(region.is_bankable() && seg.is_fake()));
		}

		Self { image, mem_map, segs, seg_name_map, config, names: NameMap::new() }
	}

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

	// ---------------------------------------------------------------------------------------------
	// Names

	pub fn add_name(&mut self, name: &'a str, va: VAddr) {
		self.names.add(name, va);
	}

	pub fn remove_name(&mut self, name: &'a str) {
		self.names.remove_name(name)
	}

	pub fn remove_name_from_va(&mut self, va: VAddr) {
		self.names.remove_va(va);
	}

	pub fn has_name(&self, name: &str) -> bool {
		self.names.has_name(name)
	}

	pub fn has_name_for_va(&self, va: VAddr) -> bool {
		self.names.has_va(va)
	}

	pub fn name_from_va(&self, va: VAddr) -> &str {
		self.names.name_for_va(va).unwrap()
	}

	pub fn va_from_name(&self, name: &'a str) -> VAddr {
		self.names.va_for_name(name).unwrap()
	}

	pub fn name_of_va(&self, va: VAddr) -> String {
		// see if there's already a name here.
		if let Some(name) = self.names.name_for_va(va) {
			return name.into();
		}

		// nope. have to make our own.
		// if there's a mapped segment, see what span is there.
		if let Some(seg) = self.segment_for_va(va) {
			// since there's a segment, let's see if the span has a name.
			let start = seg.va_from_offset(seg.span_from_va(va).start);

			match self.names.name_for_va(start) {
				Some(name) =>
					// there's already a name, so name it like "main_loc_0C30"
					return format!("{}_loc_{:04X}", name, va),
				None =>
					// no name, so name it "SEGNAME_loc_0C30"
					return format!("{}_loc_{:04X}", seg.name, va),
			}
		}

		// no mapped segment?? uhhhh....... try region name?
		if let Some(region) = self.mem_map.region_for_va(va) {
			// name it "REGIONNAME_loc_0C30"
			return format!("{}_loc_{:04X}", region.name, va);
		} else {
			// how in the hell
			return format!("UNK_loc_{:04X}", va);
		}
	}

	/// All (name, VA) pairs in arbitrary order.
	pub fn all_names(&self) -> HashIter<'a, &str, VAddr> {
		self.names.names()
	}

	/// All (VA, name) pairs in VA order.
	pub fn all_names_by_va(&self) -> BTreeIter<'a, VAddr, &str> {
		self.names.vas()
	}

	/// All (VA, name) pairs in a given range of VAs, in VA order.
	pub fn names_in_range(&self, range: impl RangeBounds<VAddr>) -> BTreeRange<'a, VAddr, &str> {
		self.names.names_in_range(range)
	}

	// ---------------------------------------------------------------------------------------------
	// Image

	// TODO: get slices of image, read bytes etc. ENDIANNESS??
}

impl<'a> Display for Memory<'a> {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		writeln!(f, "Image: {} (0x{:X} bytes)", self.image.name, self.image.data.len())?;
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