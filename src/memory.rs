use std::collections::HashMap;
use std::fmt::{ Display, Formatter, Result as FmtResult };

use derive_new::new;
use parse_display::Display;

// ------------------------------------------------------------------------------------------------
// Sub-modules
// ------------------------------------------------------------------------------------------------

mod config;
mod image;
mod map;
mod region;
mod segment;
mod spans;
mod va;

#[cfg(test)]
mod tests;

pub use config::*;
pub use image::*;
pub use map::*;
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
// Memory
// ------------------------------------------------------------------------------------------------

/// This is the data structure on which everything else is built.
/// Ties together a memory map, memory config, and segments.
#[derive(new)]
pub struct Memory {
	endianness:   Endian,
	mem_map:      MemoryMap,
	config:       MemoryConfig,

	#[new(default)]
	segs:         Vec<Segment>,
	#[new(default)]
	seg_name_map: HashMap<String, usize>,
	#[new(value = "SegId(0)")]
	next_seg_id:  SegId,
	#[new(default)]
	seg_id_map:   HashMap<SegId, usize>,
	// TODO: bankable regions config (stored here, or just passed into methods as needed?)
}

impl Memory {
	// ---------------------------------------------------------------------------------------------
	// Getters

	/// Get the memory address space map.
	pub fn map(&self) -> &MemoryMap {
		&self.mem_map
	}

	/// Get the memory configuration.
	pub fn config(&self) -> &MemoryConfig {
		&self.config
	}

	/// Gets endianness.
	pub fn endianness(&self) -> Endian {
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

	/// Given a virtual address, get the memory region which contains it, if any.
	pub fn region_for_va(&self, va: VA) -> Option<&MemoryRegion> {
		self.mem_map.region_for_va(va)
	}

	/// Given a name, gets the memory region with that name, if any.
	pub fn region_for_name(&self, name: &str) -> Option<&MemoryRegion> {
		self.mem_map.region_for_name(name)
	}

	// ---------------------------------------------------------------------------------------------
	// Segments

	/// Adds a new segment.
	///
	/// # Panics
	///
	/// - if `name` is already the name of an existing segment.
	/// - if the segment is mapped to a bankable region, but `image` is `None`.
	pub fn add_segment(&mut self, name: &str, vbase: VA, vend: VA, image: Option<Image>) {
		let existing = self.seg_name_map.insert(name.into(), self.segs.len());
		assert!(existing.is_none(), "duplicate segment name {}", name);

		let fake = image.is_none();
		for (region_name, seg_name) in self.config.iter() {
			if seg_name == name {
				let region = self.mem_map.region_for_name(&region_name).unwrap();
				// if it's bankable, it must be real.
				assert!(!(region.is_bankable() && fake));
			}
		}

		let id = self.next_seg_id;
		self.next_seg_id = SegId(self.next_seg_id.0 + 1);
		self.seg_id_map.insert(id, self.segs.len());
		self.segs.push(Segment::new(id, name, vbase, vend, image));
	}

	/// Given a region name, get the Segment mapped to it (if any).
	pub fn segment_for_region(&self, region_name: &str) -> Option<&Segment> {
		self.config.segment_for_region(region_name)
			.and_then(|seg_name| self.segment_for_name(&seg_name))
	}

	/// Given a VA, get the Segment which contains it (if any).
	pub fn segment_for_va(&self, va: VA) -> Option<&Segment> {
		self.mem_map.region_for_va(va)
			.and_then(|region| self.config.segment_for_region(&region.name))
			.and_then(|name|   self.segment_for_name(name))
	}

	/// Same as above but mutable.
	pub fn segment_for_va_mut(&mut self, va: VA) -> Option<&mut Segment> {
		let idx = self.mem_map.region_for_va(va)
			.and_then(|region| self.config.segment_for_region(&region.name))
			.and_then(|name|   self.seg_name_map.get(name));

		// Rust refuses to let me do a similar thing to above and I can't figure out why.
		match idx {
			Some(&idx) => Some(&mut self.segs[idx]),
			None       => None,
		}
	}

	/// Given a segment name, get the Segment named that (if any).
	pub fn segment_for_name(&self, name: &str) -> Option<&Segment> {
		self.seg_name_map.get(name).map(|&idx| &self.segs[idx])
	}

	/// Same as above but mutable.
	pub fn segment_for_name_mut(&mut self, name: &str) -> Option<&mut Segment> {
		let segs = &mut self.segs;
		self.seg_name_map.get_mut(name).map(move |&mut idx| &mut segs[idx])
	}

	/// Given a location, get the Segment which contains it.
	pub fn segment_from_loc(&self, loc: Location) -> &Segment {
		&self.segs[*self.seg_id_map.get(&loc.seg).unwrap()]
	}

	/// Same as above but mutable.
	pub fn segment_from_loc_mut(&mut self, loc: Location) -> &mut Segment {
		&mut self.segs[*self.seg_id_map.get(&loc.seg).unwrap()]
	}

	// TODO: removing/redefining/iterating segments

	// ---------------------------------------------------------------------------------------------
	// Address translation

	/// Tries to find a unique location for the given VA.
	/// If there is no mapping, or if the region is bankable, returns None.
	pub fn va_to_loc(&self, va: VA) -> Option<Location> {
		self.mem_map.region_for_va(va)
		.and_then(|region| {
			if region.is_bankable() {
				None
			} else {
				self.segment_for_region(&region.name)
				.map(|seg| {
					let offs = seg.offset_from_va(va);
					Location::new(seg.id, offs)
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

			match self.segment_for_region(&region.name) {
				Some(seg) => writeln!(f, " => {}", seg)?,
				None      => writeln!(f, " (unmapped)")?,
			}
		}

		Ok(())
	}
}