use std::collections::HashMap;
use std::convert::TryInto;
use std::fmt::{ Display, Formatter, Result as FmtResult };

// ------------------------------------------------------------------------------------------------
// Sub-modules
// ------------------------------------------------------------------------------------------------

pub mod config;
pub mod map;
pub mod types;
pub mod region;
pub mod segment;
pub mod spans;

#[cfg(test)]
mod tests;

pub use config::*;
pub use map::*;
pub use types::*;
pub use region::*;
pub use segment::*;
pub use spans::*;

// ------------------------------------------------------------------------------------------------
// MemoryBuilder
// ------------------------------------------------------------------------------------------------

/// Builder for the Memory object.
pub struct MemoryBuilder<'a> {
	endianness:   Endian,
	segs:         Vec<Segment<'a>>,
	mem_map:      MemoryMap<'a>,
	config:       MemoryConfig<'a>,
	seg_name_map: HashMap<&'a str, usize>,
}

/// Builder for Memory objects. This exists mainly to check that the MemoryConfig
/// is valid before construction.
impl<'a> MemoryBuilder<'a> {
	pub fn new(
		endianness: Endian,
		mem_map:    MemoryMap<'a>,
		config:     MemoryConfig<'a>
	) -> Self {
		Self {
			endianness,
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
			endianness:   self.endianness,
			segs:         self.segs,
			mem_map:      self.mem_map,
			config:       self.config,
			seg_name_map: self.seg_name_map,
		}
	}
}

// ------------------------------------------------------------------------------------------------
// Memory
// ------------------------------------------------------------------------------------------------

/// This is the data structure on which everything else is built.
/// Ties together a memory map, memory config, and segments.
pub struct Memory<'a> {
	endianness:   Endian,
	mem_map:      MemoryMap<'a>,
	config:       MemoryConfig<'a>,
	segs:         Vec<Segment<'a>>,
	seg_name_map: HashMap<&'a str, usize>,
	// TODO: bankable regions config (stored here, or just passed into methods as needed?)
}

impl<'a> Memory<'a> {
	// ---------------------------------------------------------------------------------------------
	// Getters

	/// Get the memory address space map.
	pub fn map(&'a self) -> &'a MemoryMap<'a> {
		&self.mem_map
	}

	/// Get the memory configuration.
	pub fn config(&'a self) -> &'a MemoryConfig<'a> {
		&self.config
	}

	/// Gets endianness.
	pub fn endianness(&self) -> Endian {
		self.endianness
	}

	// ---------------------------------------------------------------------------------------------
	// Regions

	/// Given a range of two VAs, do they cross over the boundary between two regions?
	pub fn range_crosses_regions(&self, start: VAddr, end: VAddr) -> bool {
		assert!(end > start);

		match (self.mem_map.region_for_va(start), self.mem_map.region_for_va(end)) {
			(Some(s), Some(e)) => !std::ptr::eq(s, e),
			(None, None)       => false,
			_                  => true,
		}
	}

	/// Given a virtual address, get the memory region which contains it, if any.
	pub fn region_for_va(&self, va: VAddr) -> Option<&MemoryRegion> {
		self.mem_map.region_for_va(va)
	}

	/// Given a name, gets the memory region with that name, if any.
	pub fn region_for_name(&self, name: &str) -> Option<&MemoryRegion> {
		self.mem_map.region_for_name(name)
	}

	// ---------------------------------------------------------------------------------------------
	// Segments

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

	// TODO: adding/removing/redefining/iterating segments

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

	/// Formats a number as a hexadecimal number with the appropriate number of digits
	/// for the size of the address space.
	pub fn fmt_addr(&self, addr: usize) -> String {
		format!("{:0width$X}", addr, width = self.mem_map.digits)
	}
}

impl<'a> Display for Memory<'a> {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		writeln!(f, "Memory: 0x{:X} bytes, {}-endian", self.mem_map.len(), self.endianness)?;
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