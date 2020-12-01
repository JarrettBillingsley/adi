use std::collections::{
	BTreeMap,
	HashMap,
};

use super::newtypes::*;
use super::region::*;

// ------------------------------------------------------------------------------------------------
// Memory map
// ------------------------------------------------------------------------------------------------

/// Describes a CPU's entire memory map, and consists of multiple `MemoryRegion`s.
///
/// Once created, the memory map cannot change (can't add/remove regions).
#[derive(Debug)]
pub struct MemoryMap<'a> {
	/// how many bits an address is.
	bits:     usize,
	/// all the memory regions in the memory map.
	regions:  &'a [MemoryRegion<'a>],
	/// the size of the memory map, and the first invalid address.
	end:      usize,
	/// maps from virtual addresses to an index into `regions`.
	addr_map: BTreeMap<VAddr, usize>,  // from VAs to `regions` index
	/// maps from names into `regions`.
	name_map: HashMap<&'a str, usize>, // from names to `regions` index
}

impl<'a> MemoryMap<'a> {
	/// given a number of bits in the address and a list of regions, constructs a
	/// new MemoryMap. does sanity checks to ensure regions don't overlap or have
	/// the same name, and also builds the `addr_map` and `name_map` maps for quick
	/// lookups.
	pub fn new(bits: usize, regions: &'a [MemoryRegion]) -> Self {
		// sanity checks.
		for i in 0 .. regions.len() {
			let r = &regions[i];
			let rest = &regions[i + 1 ..];

			if let Some(other) = rest.iter().find(|other| r.overlaps(other)) {
				panic!("overlapping regions ({} and {})", r, other);
			}

			if let Some(other) = rest.iter().find(|other| r.name == other.name) {
				panic!("same name regions ({} and {})", r, other);
			}
		}

		let mut addr_map = BTreeMap::new();
		let mut name_map = HashMap::new();

		// fill in the maps.
		for (i, r) in regions.iter().enumerate() {
			addr_map.insert(VAddr(r.base), i);
			name_map.insert(r.name, i);
		}

		Self {
			bits,
			regions,

			end: 2_usize.pow(bits as u32),
			addr_map,
			name_map,
		}
	}

	/// The length of the address space.
	pub fn len(&self) -> usize { self.end }

	/// Given a virtual address, get the memory region which contains it, if any.
	pub fn region_for_va(&self, va: VAddr) -> Option<&MemoryRegion> {
		assert!(va.0 < self.end);

		// find the last entry whose start <= va
		match self.addr_map.range(..= va).next_back() {
			// if va < the region's end, we found it
			Some((_, &idx)) if va.0 < self.regions[idx].end => {
				Some(&self.regions[idx])
			}

			_ => None
		}
	}

	/// Given a name, gets the memory region with that name, if any.
	pub fn region_for_name(&self, name: &str) -> Option<&MemoryRegion> {
		self.name_map.get(&name).map(|&idx| &self.regions[idx])
	}

	/// Iterator over all memory regions.
	pub fn all_regions(&'a self) -> impl Iterator<Item = &'a MemoryRegion<'a>> {
		let func = move |&idx| &self.regions[idx];
		self.addr_map.values().map(func)
	}

	// TODO: iterators for bankable regions, ROM regions, etc?
}