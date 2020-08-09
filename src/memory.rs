use parse_display::*;
use derive_new::*;
use std::collections::{ BTreeMap, HashMap };
use std::iter::IntoIterator;
use std::iter::FromIterator;

/// newtype for virtual addresses.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct VAddr(pub usize);

// ------------------------------------------------------------------------------------------------
// Memory map regions
// ------------------------------------------------------------------------------------------------

/// Describes part of a CPU's memory map - the name, address range, and so on.
#[derive(Debug, Display)]
#[derive(new)]
#[display("{name} [0x{base:08X} .. 0x{end:08X})")]
pub struct MemoryRegion<'a> {
	/// Human-readable name.
	pub name: &'a str,
	/// Address of first byte.
	pub base: usize,
	/// Address of first byte *after* this region.
	pub end:  usize,
	/// Whether this is provided by the hardware or by a cartridge etc.
	pub hw:   bool,
	/// What kind of thing is at these addresses.
	pub kind: MemoryRegionKind,

	/// How big this region is, in bytes.
	#[new(value = "end - base")]
	pub size: usize,
}

impl<'a> MemoryRegion<'a> {
	/// true if these two regions overlap one another.
	pub fn overlaps(&self, other: &MemoryRegion) -> bool {
		!(self.end <= other.base || other.end <= self.base)
	}

	/// gets the size in bytes.
	pub fn len(&self) -> usize { self.size }

	/// true this region's kind is bankable.
	pub fn is_bankable(&self) -> bool { self.kind.is_bankable() }
}

/// What you access when you use an address in a region's range.
#[derive(Debug, Display, PartialEq, Eq, Clone, Copy)]
pub enum MemoryRegionKind {
	/// RAM.
	Ram,
	/// Bankable RAM.
	RamBank,
	/// ROM.
	Rom,
	/// Bankable ROM.
	RomBank,
	/// Non-volatile RAM.
	NvRam,
	/// Bankable NVRAM.
	NvRamBank,
	/// A mirror of the previous region of memory.
	Mirror,
	/// Memory-mapped IO ports.
	Mmio,
}

impl MemoryRegionKind {
	/// true if the region of memory is bankable (i.e. its contents can be swapped out).
	pub fn is_bankable(&self) -> bool {
		use MemoryRegionKind::*;
		matches!(self, RamBank | RomBank | NvRamBank)
	}
}

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
	pub fn get(&self, va: VAddr) -> Option<&MemoryRegion> {
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

	// TODO: bankable_regions iter?
	// TODO: all_regions iter?
}

// ------------------------------------------------------------------------------------------------
// Memory configuration
// ------------------------------------------------------------------------------------------------

/// A collection of assignments of segments to areas in the memory map.
///
/// This just maps from region names to segment names. No hard references to objects.
///
/// You can create MemoryConfigs from any iterable that gives (name, name) pairs.
/// For example:
///
/// ```
/// let config = MemoryConfig::from_iter(&[
///     ("a", "b"),
///     ("c", "d"),
/// ]);
/// ```
#[derive(Debug, PartialEq, Eq)]
pub struct MemoryConfig<'a> {
	assignments: HashMap<&'a str, &'a str>,
}

type MemoryConfigIterItem<'a> = &'a (&'a str, &'a str);

impl<'a> FromIterator<MemoryConfigIterItem<'a>> for MemoryConfig<'a> {
	fn from_iter<I: IntoIterator<Item=MemoryConfigIterItem<'a>>>(iter: I) -> Self {
		MemoryConfig::new(iter.into_iter().copied().collect())
	}
}

impl<'a> MemoryConfig<'a> {
	/// ctor
	fn new(assignments: HashMap<&'a str, &'a str>) -> Self {
		Self {
			assignments
		}
	}

	/// Gets the segment mapped to the given region (if any).
	pub fn get_segment(&self, region_name: &str) -> Option<&str> {
		self.assignments.get(region_name).copied()
	}

	/// Iterates over the mappings.
	pub fn iter(&self) -> std::collections::hash_map::Iter<'a, &str, &str> {
		self.assignments.iter()
	}

	/// Construct a new `MemoryConfig` using this one as a base, and the items in `iter`
	/// will be added to the mappings (and will replace any existing mappings).
	pub fn derive<I: IntoIterator<Item=MemoryConfigIterItem<'a>>>(&self, iter: I) -> Self {
		let mut ret = Self::new(self.assignments.clone());
		ret.assignments.extend(iter.into_iter().copied());
		ret
	}
}