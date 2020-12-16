use std::collections::{
	BTreeMap,
	HashMap,
};

use crate::memory::{ VA, MemoryRegion, IMmu, IMmuState, SegId };

// ------------------------------------------------------------------------------------------------
// Memory map
// ------------------------------------------------------------------------------------------------

#[allow(clippy::len_without_is_empty)]
pub trait IMemoryMap {
	/// how many bits an address is.
	fn bits(&self) -> usize;
	/// how many digits in a formatted address.
	fn digits(&self) -> usize;
	/// the first invalid address, and the size of the virtual address space.
	fn end(&self) -> VA;
	/// The length of the address space.
	fn len(&self) -> usize;
	/// Given a virtual address, get the memory region which contains it, if any.
	fn region_for_va(&self, va: VA) -> Option<&MemoryRegion>;
	/// Given a name, gets the memory region with that name, if any.
	fn region_for_name(&self, name: &str) -> Option<&MemoryRegion>;
	/// Iterator over all memory regions.
	fn all_regions(&self) -> Box<dyn Iterator<Item = &MemoryRegion> + '_>;
	/// Given a range of two VAs, do they cross over the boundary between two regions?
	fn range_crosses_regions(&self, start: VA, end: VA) -> bool;
	/// Given a VA, get the ID of the segment that (currently) contains it, or None if none does.
	fn segid_for_va(&self, va: VA) -> Option<SegId>;
}

/// Describes a CPU's entire memory map, and consists of multiple `MemoryRegion`s.
///
/// Once created, the memory map cannot change (can't add/remove regions).
#[derive(Debug)]
pub struct MemoryMap<TMmu: IMmu> {
	bits:     usize,
	digits:   usize,
	end:      VA,
	mmu:      TMmu,
	/// all the memory regions in the memory map, in address order.
	regions:  Vec<MemoryRegion>,
	/// maps from virtual addresses to an index into `regions`.
	addr_map: BTreeMap<VA, usize>,
	/// maps from names into `regions`.
	name_map: HashMap<String, usize>,
}

impl<TMmu: IMmu> MemoryMap<TMmu> {
	/// given a number of bits in the address and a list of regions, constructs a
	/// new MemoryMap. does sanity checks to ensure regions don't overlap or have
	/// the same name, and also builds the `addr_map` and `name_map` maps for quick
	/// lookups.
	pub fn new(bits: usize, regions: &[MemoryRegion], mmu: TMmu) -> Self {
		// sort them regions
		let mut regions: Vec<_> = regions.into();
		regions.sort_by(|a, b| a.base().cmp(&b.base()));

		// sanity checks.
		for i in 0 .. regions.len() {
			let r = &regions[i];
			let rest = &regions[i + 1 ..];

			if let Some(other) = rest.iter().find(|other| r.overlaps(other)) {
				panic!("overlapping regions ({} and {})", r, other);
			}

			if let Some(other) = rest.iter().find(|other| r.name() == other.name()) {
				panic!("same name regions ({} and {})", r, other);
			}
		}

		let mut addr_map = BTreeMap::new();
		let mut name_map = HashMap::new();

		// fill in the maps.
		for (i, r) in regions.iter().enumerate() {
			addr_map.insert(r.base(), i);
			name_map.insert(r.name().clone(), i);
		}

		Self {
			bits,
			digits: ((bits + 3) & !3) >> 2, // round up to next multiple of 4, divide by 4
			end: VA(2_usize.pow(bits as u32)),
			mmu,
			regions,
			addr_map,
			name_map,
		}
	}
}

impl<TMmu: IMmu> IMemoryMap for MemoryMap<TMmu> {
	/// how many bits an address is.
	fn bits(&self) -> usize { self.bits }

	/// how many digits in a formatted address.
	fn digits(&self) -> usize { self.digits }

	/// the first invalid address, and the size of the virtual address space.
	fn end(&self) -> VA { self.end }

	/// The length of the address space.
	fn len(&self) -> usize { self.end.0 }

	/// Given a virtual address, get the memory region which contains it, if any.
	fn region_for_va(&self, va: VA) -> Option<&MemoryRegion> {
		assert!(va < self.end);

		// find the last entry whose start <= va
		match self.addr_map.range(..= va).next_back() {
			// if va < the region's end, we found it
			Some((_, &idx)) if va < self.regions[idx].end() => {
				Some(&self.regions[idx])
			}

			_ => None
		}
	}

	/// Given a name, gets the memory region with that name, if any.
	fn region_for_name(&self, name: &str) -> Option<&MemoryRegion> {
		let idx = self.name_map.get(name)?;
		Some(&self.regions[*idx])
	}

	/// Iterator over all memory regions.
	fn all_regions(&self) -> Box<dyn Iterator<Item = &MemoryRegion> + '_> {
		let func = move |&idx| &self.regions[idx];
		Box::new(self.addr_map.values().map(func))
	}

	/// Given a range of two VAs, do they cross over the boundary between two regions?
	fn range_crosses_regions(&self, start: VA, end: VA) -> bool {
		assert!(end > start);

		match (self.region_for_va(start), self.region_for_va(end)) {
			(Some(s), Some(e)) => !std::ptr::eq(s, e),
			(None, None)       => false,
			_                  => true,
		}
	}

	fn segid_for_va(&self, va: VA) -> Option<SegId> {
		if let Some(region) = self.region_for_va(va) {
			if region.seg().is_some() {
				return region.seg();
			}
		}

		// MMU's turn
		let state = self.mmu.initial_state();
		self.mmu.segid_for_va(state, va)
	}
}