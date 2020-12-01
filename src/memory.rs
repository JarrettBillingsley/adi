use std::collections::HashMap;
use std::fmt::{ Display, Formatter, Result as FmtResult };

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

pub struct Memory<'a> {
	image:        RomImage<'a>,
	mem_map:      MemoryMap<'a>,
	segs:         &'a mut [Segment<'a>],
	seg_name_map: HashMap<&'a str, usize>,
	config:       MemoryConfig<'a>,
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
	// Queries

	pub fn len(&self) -> usize {
		self.mem_map.len()
	}

	pub fn range_crosses_regions(&self, start: VAddr, end: VAddr) -> bool {
		assert!(end.0 > start.0);

		match (self.mem_map.region_for_va(start), self.mem_map.region_for_va(end)) {
			(Some(s), Some(e)) => !std::ptr::eq(s, e),
			(None, None)       => false,
			_                  => true,
		}
	}

	// ---------------------------------------------------------------------------------------------
	// Segments

	pub fn segment_for_va(&self, va: VAddr) -> Option<&Segment> {
		self.mem_map.region_for_va(va)
		.and_then(|region| self.config.segment_for_region(region.name))
		.and_then(|name|   self.segment_for_name(name))
	}

	pub fn segment_for_va_mut(&'a mut self, va: VAddr) -> Option<&mut Segment> {
		let name = match self.mem_map.region_for_va(va) {
			Some(region) => self.config.segment_for_region(region.name),
			None => None,
		};

		match name {
			Some(name) => match self.seg_name_map.get(&name) {
				Some(&idx) => Some(&mut self.segs[idx]),
				None       => None,
			},
			None => None,
		}
	}

	pub fn segment_for_name(&self, name: &str) -> Option<&Segment> {
		self.seg_name_map.get(&name).map(|&idx| &self.segs[idx])
	}

	pub fn segment_for_name_mut(&'a mut self, name: &str) -> Option<&mut Segment> {
		// Rust refuses to let me do a similar thing to above and I can't figure out why.
		match self.seg_name_map.get(&name) {
			Some(&idx) => Some(&mut self.segs[idx]),
			None       => None,
		}
	}

	pub fn segment_for_region(&self, region_name: &str) -> Option<&Segment> {
		self.config.segment_for_region(region_name)
		.and_then(|seg_name| self.segment_for_name(seg_name))
	}

	// ---------------------------------------------------------------------------------------------
	// Names

	pub fn add_name_va(&mut self, name: &'a str, va: VAddr) {
		self.names.add(name, va);
	}

	pub fn remove_name_name(&mut self, name: &'a str) {
		self.names.remove_name(name)
	}

	pub fn remove_name_va(&mut self, va: VAddr) {
		self.names.remove_va(va);
	}

	pub fn has_name_name(&self, name: &str) -> bool {
		self.names.has_name(name)
	}

	pub fn has_name_va(&self, va: VAddr) -> bool {
		self.names.has_va(va)
	}

	pub fn name_from_va(&self, va: VAddr) -> &str {
		self.names.name_for_va(va).unwrap()
	}

	pub fn va_from_name(&self, name: &'a str) -> VAddr {
		self.names.va_for_name(name).unwrap()
	}

	pub fn name_of_va(&self, va: VAddr) -> String {
		if let Some(name) = self.names.name_for_va(va) {
			name.into()
		} else {
			let base_name = self.names.name_for_va(va);

			let name = match self.segment_for_va(va) {
				Some(seg) => seg.name,
				None      => "UNK",
			};

			match base_name {
				None            => format!("{}_{:04X}", name, va.0),
				Some(base_name) => format!("{}_{}_{:04X}", name, base_name, va.0),
			}
		}
	}
}

impl<'a> Display for Memory<'a> {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		writeln!(f, "Image: {} (0x{:X} bytes)", self.image.name, self.image.data.len())?;
		writeln!(f, "Segments:")?;

		writeln!(f, "Memory map:")?;

		for region in self.mem_map.all_regions() {
			write!(f, "{:>40}", region.to_string())?;

			match self.segment_for_region(region.name) {
				Some(seg) => writeln!(f, " mapped to segment {}", seg)?,
				None      => writeln!(f, " (unmapped)")?,
			}
		}

		Ok(())
	}
}