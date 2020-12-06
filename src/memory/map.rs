use std::collections::{
	BTreeMap,
	HashMap,
};

use super::types::*;
use super::region::*;

// ------------------------------------------------------------------------------------------------
// Memory map
// ------------------------------------------------------------------------------------------------

/// Describes a CPU's entire memory map, and consists of multiple `MemoryRegion`s.
///
/// Once created, the memory map cannot change (can't add/remove regions).
#[derive(Debug)]
pub struct MemoryMap {
	/// how many bits an address is.
	pub bits:   usize,
	/// how many digits in a formatted address.
	pub digits: usize,
	/// all the memory regions in the memory map.
	regions:    Vec<MemoryRegion>,
	/// the first invalid address, and the size of the virtual address space.
	pub end:    VA,
	/// maps from virtual addresses to an index into `regions`.
	addr_map:   BTreeMap<VA, usize>,  // from VAs to `regions` index
	/// maps from names into `regions`.
	name_map:   HashMap<String, usize>, // from names to `regions` index
}

#[allow(clippy::len_without_is_empty)]
impl MemoryMap {
	/// given a number of bits in the address and a list of regions, constructs a
	/// new MemoryMap. does sanity checks to ensure regions don't overlap or have
	/// the same name, and also builds the `addr_map` and `name_map` maps for quick
	/// lookups.
	pub fn new(bits: usize, regions: &[MemoryRegion]) -> Self {
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
			addr_map.insert(r.base, i);
			name_map.insert(r.name.clone(), i);
		}

		Self {
			bits,
			digits: ((bits + 3) & !3) >> 2,
			regions: regions.into(),

			end: VA(2_usize.pow(bits as u32)),
			addr_map,
			name_map,
		}
	}

	/// The length of the address space.
	pub fn len(&self) -> usize { self.end.0 }

	/// Given a virtual address, get the memory region which contains it, if any.
	pub fn region_for_va(&self, va: VA) -> Option<&MemoryRegion> {
		assert!(va < self.end);

		// find the last entry whose start <= va
		match self.addr_map.range(..= va).next_back() {
			// if va < the region's end, we found it
			Some((_, &idx)) if va < self.regions[idx].end => {
				Some(&self.regions[idx])
			}

			_ => None
		}
	}

	/// Given a name, gets the memory region with that name, if any.
	pub fn region_for_name(&self, name: &str) -> Option<&MemoryRegion> {
		self.name_map.get(name).map(|&idx| &self.regions[idx])
	}

	/// Iterator over all memory regions.
	pub fn all_regions(&self) -> impl Iterator<Item = &MemoryRegion> {
		let func = move |&idx| &self.regions[idx];
		self.addr_map.values().map(func)
	}

	// TODO: iterators for bankable regions, ROM regions, etc?
}