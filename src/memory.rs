use parse_display::*;
use derive_new::*;
use std::collections::{
	BTreeMap,
	HashMap,
	hash_map::Iter as HashIter,
	// btree_map::Iter as BTreeIter,
	btree_map::Values as BTreeValues,
};
use std::fmt::{ Debug, Display, Formatter, Result as FmtResult };
use std::iter::IntoIterator;
use std::iter::FromIterator;

/// newtype for virtual addresses.
#[derive(Debug, Display, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct VAddr(pub usize);

/// newtype for physical addresses (i.e. offsets into an image).
#[derive(Debug, Display, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct PAddr(pub usize);

/// newtype for offsets into segments.
#[derive(Debug, Display, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
#[display("0x{0:08X}")]
pub struct SegOffset(pub usize);

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

	/// A shorter name for `region_for_va`.
	pub fn get(&self, va: VAddr) -> Option<&MemoryRegion> {
		self.region_for_va(va)
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
/// use adi::memory::MemoryConfig;
/// use std::iter::FromIterator;
/// let config = MemoryConfig::from_iter(&[
///     ("a", "b"),
///     ("c", "d"),
/// ]);
/// assert_eq!(config.segment_for_region("a"), Some("b"));
/// assert_eq!(config.segment_for_region("x"), None);
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
	pub fn segment_for_region(&self, region_name: &str) -> Option<&str> {
		self.assignments.get(region_name).copied()
	}

	/// Iterates over the mappings (region name, segment name).
	pub fn iter(&self) -> HashIter<'a, &str, &str> {
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

// ------------------------------------------------------------------------------------------------
// Span
// ------------------------------------------------------------------------------------------------

// TODO: what IS this?
pub trait SpanOwner: Debug + Display {}

/// Describes a "slice" of a Segment. The start and end positions are given as offsets into the
/// segment, to avoid confusion when dealing with virtual and physical addresses.
///
/// Its "owner" is some kind of object which "manages" this span. For example, code spans can be
/// managed by BasicBlock objects. (data spans might have some kind of Array or Variable object?)
#[derive(Debug, Display)]
#[derive(new)]
#[display("{kind} [0x{start.0:08X} .. 0x{end.0:08X})")]
pub struct Span<'a> {
	/// address of first byte.
	start: SegOffset,
	/// address of first byte after span.
	end: SegOffset,
	/// what kind of span it is.
	kind: SpanKind,
	/// the owner, if any.
	owner: Option<&'a dyn SpanOwner>,
}

/// What kind of thing the span covers.
#[derive(Debug, Display, PartialEq, Eq, Clone, Copy)]
pub enum SpanKind {
	/// Unknown (not yet analyzed)
	Unk,
	/// Code (that is, a basic block of a function)
	Code,
	/// Data (anything that isn't code)
	Data,
}

// ------------------------------------------------------------------------------------------------
// SpanMap
// ------------------------------------------------------------------------------------------------

/// Representation of the "map" of a segment's spans. Has a fixed size (which matches the segment's
/// size) and the entire size is covered by spans (no "empty" spots).
///
/// Looking up spans by address is efficient (logarithmic time). Looking up spans in other ways
/// requires the use of an index.
pub struct SpanMap<'a> {
	// There's some duplication between the index into the map and Span::start.
	// But I don't think it's possible to use BTreeSet::range using a SegOffset as
	// the range bounds when the value is a Span. So, map it is.
	spans: BTreeMap<SegOffset, Span<'a>>,
	size: usize,

	// TODO:
	// self.functionsByLoc = {}
	// self.functionsByName = {}
}

impl<'a> SpanMap<'a> {
	/// Creates a new `SpanMap` with a single unknown span that covers the entire segment.
	pub fn new(size: usize) -> Self {
		let mut spans = BTreeMap::new();
		spans.insert(SegOffset(0), Span::new(SegOffset(0), SegOffset(size), SpanKind::Unk, None));
		Self {
			spans,
			size,
		}
	}

	/// Given an offset into the segment, gets the Span which contains it.
	pub fn span_at(&self, offs: SegOffset) -> &Span {
		assert!(offs.0 <= self.size); // TODO: should this be inclusive or exclusive...?
		self.spans.range(..= offs).next_back().expect("how even").1
	}

	/// Given an offset into the segment, gets the Span which comes after the containing Span,
	/// or None if the containing Span is the last one in the segment.
	pub fn span_after(&self, offs: SegOffset) -> Option<&Span> {
		assert!(offs.0 <= self.size); // TODO: should this be inclusive or exclusive...?

		if self.spans.contains_key(&offs) {
			self.spans.range(SegOffset(offs.0 + 1) ..)
		} else {
			self.spans.range(offs ..)
		}.next().map(|(_, a)| a)
	}

	/// Iterator over all spans (SegOffset, Span).
	pub fn iter(&self) -> BTreeValues<'a, SegOffset, Span> {
		self.spans.values()
	}

	/// Redefines a range of memory as being of a certain kind with a given owner.
	pub fn redefine(&mut self,
		// TODO: SegRange for a pair of offsets (more Rustic)
		start: SegOffset, end: SegOffset, kind: SpanKind, owner: Option<&'a dyn SpanOwner>) {

		assert!(start.0 < end.0);
		assert!(end.0 <= self.size);

		// get the span inside which start falls
		let (first_start, first_end) = self.range_from_offset(start);

		// SHORTCUT: if we're just rewriting a span, don't bother splitting things up.
		if start == first_start && end == first_end {
			self.rewrite(first_start, kind, owner);
		} else {
			let (_, last_end) = self.range_from_offset(end);
			let mut middle = self.spans.split_off(&first_start);
			let mut rest   = middle.split_off(&last_end);

			// at this point:
			// self.spans holds all the unaffected spans before start.
			// rest holds all the unaffected spans after end.
			// any changes will happen in middle.

			assert!(middle.len() >= 1);

			let mut middle_iter = middle.range(..);
			let first = middle_iter.next().unwrap().1;
			let last  = if middle.len() > 1 { middle_iter.next_back().unwrap().1 } else { first };

			// TODO: update indexes
			if start != first_start {
				// gotta split the first span into [first_start..start)
				self.spans.insert(first_start, Span::new(first.start, start, first.kind, first.owner));
			}

			self.spans.insert(start, Span::new(start, end, kind, owner));

			if end != last_end {
				// now to split the last span into [end..last_end)
				self.spans.insert(end, Span::new(end, last.end, last.kind, last.owner));
			}

			// finally, glob on the remainder
			self.spans.append(&mut rest);
		}

		#[cfg(debug_assertions)]
		self.check_invariants();
	}

	fn rewrite(&mut self, start: SegOffset, kind: SpanKind, owner: Option<&'a dyn SpanOwner>) {
		println!("rewriting 0x{:08X}!", start.0);
		let span = self.spans.get_mut(&start).unwrap();
		span.kind = kind;
		span.owner = owner;
		// TODO: update indexes
	}

	fn range_from_offset(&self, offs: SegOffset) -> (SegOffset, SegOffset) {
		let Span { start, end, .. } = *self.span_at(offs);
		(start, end)
	}

	#[cfg(debug_assertions)]
	fn check_invariants(&self) {
		// INVARIANT: span[i].end == span[i + 1].start
		let spans = self.spans.values().collect::<Vec<_>>();

		for i in 0 .. spans.len() - 1 {
			assert_eq!(spans[i].end, spans[i + 1].start);
		}

		// INVARIANT: span[n - 1].end == self.size
		assert_eq!(spans[spans.len() - 1].end.0, self.size);
	}

	#[cfg(any(test, debug_assertions))]
	#[allow(dead_code)]
	fn dump_spans(&self) {
		println!("-----------------");
		for (_, s) in &self.spans {
			println!("{}", s);
		}
	}
}

// ------------------------------------------------------------------------------------------------
// LabelMap
// ------------------------------------------------------------------------------------------------

/// A bidirectional mapping between labels (names) and segment offsets.
pub struct LabelMap<'a> {
	names_to_offsets: HashMap<&'a str, SegOffset>,
	offsets_to_names: BTreeMap<SegOffset, &'a str>,
}

impl<'a> LabelMap<'a> {
	/// Makes a new empty map.
	pub fn new() -> Self {
		Self {
			names_to_offsets: HashMap::new(),
			offsets_to_names: BTreeMap::new(),
		}
	}

	/// Assigns a name to a given offset. The offset must not already have a name.
	pub fn add(&mut self, name: &'a str, offset: SegOffset) {
		assert!(!self.names_to_offsets.contains_key(&name));
		self.names_to_offsets.insert(name, offset);
		self.offsets_to_names.insert(offset, name);
	}

	/// Removes a mapping by name.
	pub fn remove_name(&mut self, name: &'a str) {
		let offs = *self.names_to_offsets.get(&name).unwrap();
		self.names_to_offsets.remove(name);
		self.offsets_to_names.remove(&offs);
	}

	/// Removes a mapping by offset.
	pub fn remove_offset(&mut self, offs: SegOffset) {
		let name = *self.offsets_to_names.get(&offs).unwrap();
		self.names_to_offsets.remove(name);
		self.offsets_to_names.remove(&offs);
	}

	/// Gets the offset for a label name, if one of that name exists.
	pub fn offset_for_name(&self, name: &'a str) -> Option<SegOffset> {
		self.names_to_offsets.get(name).copied()
	}

	/// Gets the label name for an offset, if there is one.
	pub fn name_for_offset(&self, offs: SegOffset) -> Option<&'a str> {
		self.offsets_to_names.get(&offs).copied()
	}

	/// Whether or not the given label name exists.
	pub fn has_name(&self, name: &'a str) -> bool {
		self.names_to_offsets.contains_key(name)
	}

	/// Whether or not there is a label name for the given offset.
	pub fn has_offset(&self, offs: SegOffset) -> bool {
		self.offsets_to_names.contains_key(&offs)
	}
}

// ------------------------------------------------------------------------------------------------
// Segment
// ------------------------------------------------------------------------------------------------

/// Generates a string of the form `{name}_{va}` if `base_name` is `None`, or
/// `{name}_{base_name}_{va}` if `base_name` is `Some`.
fn generate_name(name: &str, va: VAddr, base_name: Option<&str>) -> String {
	match base_name {
		None            => format!("{}_{:04X}", name, va.0),
		Some(base_name) => format!("{}_{}_{:04X}", name, base_name, va.0),
	}
}

/// A range of physical addresses within an image.
#[derive(Debug, Clone, Copy)]
pub struct ImageRange {
	pbase: PAddr,
	pend:  PAddr,
}

/// A single segment. Can be an image segment (data comes from a ROM image) or a fake
/// segment (there is no data, e.g. RAM, but it's useful to put names and spans there).
pub struct Segment<'a> {
	name:   &'a str,
	vbase:  VAddr,
	vend:   VAddr,
	size:   usize,
	spans:  SpanMap<'a>,
	labels: LabelMap<'a>,
	image:  Option<ImageRange>,
}

impl<'a> Display for Segment<'a> {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		match self.image {
			Some(image) =>
				write!(f, "{} (image) VA [{:08X}..{:08X}) PA: [{:08X}..{:08X})",
					self.name, self.vbase.0, self.vend.0, image.pbase.0, image.pend.0),
			None =>
				write!(f, "{} (fake) VA [{:08X}..{:08X})", self.name, self.vbase.0, self.vend.0),
		}
	}
}

impl<'a> Segment<'a> {
	/// Creates a new Segment that covers a given virtual address range, optionally mapped to
	/// part of a ROM image.
	pub fn new(name: &'a str, vbase: VAddr, vend: VAddr, pbase: Option<PAddr>) -> Self {
		let size = vend.0 - vbase.0;
		let image = pbase.map(|pbase| ImageRange { pbase, pend: PAddr(pbase.0 + size) });

		Self {
			name,
			vbase,
			vend,
			size,
			spans: SpanMap::new(size),
			labels: LabelMap::new(),
			image,
		}
	}

	// ---------------------------------------------------------------------------------------------
	// Queries

	pub fn len(&self) -> usize {
		self.size
	}

	pub fn is_fake(&self) -> bool {
		self.image.is_none()
	}

	pub fn get_image_range(&self) -> ImageRange {
		self.image.unwrap()
	}

	pub fn contains_offset(&self, offs: SegOffset) -> bool {
		(0 .. self.size).contains(&offs.0)
	}

	pub fn contains_va(&self, addr: VAddr) -> bool {
		(self.vbase .. self.vend).contains(&addr)
	}

	pub fn overlaps_va(&self, other: &Segment) -> bool {
		!(self.vend <= other.vbase || other.vend <= self.vbase)
	}

	pub fn contains_pa(&self, addr: PAddr) -> bool {
		let i = self.get_image_range();
		(i.pbase .. i.pend).contains(&addr)
	}

	pub fn overlaps_pa(&self, other: &Segment) -> bool {
		let i = self.get_image_range();
		let o = other.get_image_range();
		!(i.pend <= o.pbase || o.pend <= i.pbase)
	}

	// ---------------------------------------------------------------------------------------------
	// Conversions between segment offsets, virtual addresses, physical addresses, and names
	// (there are so many for "convenience" I guess)
	// (remains to be seen how many of these are actually used in practice)

	pub fn offset_from_name(&self, name: &str) -> SegOffset {
		self.labels.offset_for_name(name).unwrap()
	}

	pub fn offset_from_va(&self, va: VAddr) -> SegOffset {
		assert!(self.contains_va(va));
		SegOffset(va.0 - self.vbase.0)
	}

	pub fn offset_from_pa(&self, pa: PAddr) -> SegOffset {
		assert!(self.contains_pa(pa));
		let pbase = self.get_image_range().pbase;
		SegOffset(pa.0 - pbase.0)
	}

	pub fn name_from_offset(&self, offs: SegOffset) -> &str {
		self.labels.name_for_offset(offs).unwrap()
	}

	pub fn name_from_va(&self, va: VAddr) -> &str {
		self.labels.name_for_offset(self.offset_from_va(va)).unwrap()
	}

	pub fn name_from_pa(&self, pa: PAddr) -> &str {
		self.name_from_offset(self.offset_from_pa(pa))
	}

	pub fn va_from_name(&self, name: &str) -> VAddr {
		self.va_from_offset(self.labels.offset_for_name(name).unwrap())
	}

	pub fn va_from_offset(&self, offs: SegOffset) -> VAddr {
		assert!(self.contains_offset(offs));
		VAddr(self.vbase.0 + offs.0)
	}

	pub fn va_from_pa(&self, pa: PAddr) -> VAddr {
		assert!(self.contains_pa(pa));
		let pbase = self.get_image_range().pbase;
		VAddr((pa.0 - pbase.0) + self.vbase.0)
	}

	pub fn pa_from_offset(&self, offs: SegOffset) -> PAddr {
		assert!(self.contains_offset(offs));
		let pbase = self.get_image_range().pbase;
		PAddr(pbase.0 + offs.0)
	}

	pub fn pa_from_va(&self, va: VAddr) -> PAddr {
		assert!(self.contains_va(va));
		let pbase = self.get_image_range().pbase;
		PAddr((va.0 - self.vbase.0) + pbase.0)
	}

	pub fn pa_from_name(&self, name: &str) -> PAddr {
		self.pa_from_offset(self.labels.offset_for_name(name).unwrap())
	}

	// ---------------------------------------------------------------------------------------------
	// Label management

	pub fn add_label_offset(&mut self, name: &'a str, offs: SegOffset) {
		assert!(self.contains_offset(offs));
		self.labels.add(name, offs);
	}

	pub fn add_label_va(&mut self, name: &'a str, va: VAddr) {
		self.add_label_offset(name, self.offset_from_va(va));
	}

	pub fn remove_label_name(&mut self, name: &'a str) {
		self.labels.remove_name(name)
	}

	pub fn remove_label_offset(&mut self, offs: SegOffset) {
		self.labels.remove_offset(offs);
	}

	pub fn remove_label_va(&mut self, va: VAddr) {
		self.labels.remove_offset(self.offset_from_va(va));
	}

	pub fn has_label_name(&self, name: &str) -> bool {
		self.labels.has_name(name)
	}

	pub fn has_label_offset(&self, offs: SegOffset) -> bool {
		self.labels.has_offset(offs)
	}

	pub fn has_label_va(&self, va: VAddr) -> bool {
		self.labels.has_offset(self.offset_from_va(va))
	}

	pub fn name_of_va(&self, va: VAddr) -> String {
		self.name_of_offset(self.offset_from_va(va))
	}

	pub fn name_of_offset(&self, offs: SegOffset) -> String {
		assert!(self.contains_offset(offs));

		if let Some(name) = self.labels.name_for_offset(offs) {
			name.into()
		} else {
			let span_start = self.spans.span_at(offs).start;
			let base_name = self.labels.name_for_offset(span_start);
			generate_name(self.name, self.va_from_offset(offs), base_name)
		}
	}

	// ---------------------------------------------------------------------------------------------
	// Span management (spanagement?)

	// def makeNewSpan(self, loc, endOffs, type, owner):
	// 	self.spans.redefine(loc.offs, endOffs, type, owner)

	pub fn span_from_name(&self, name: &str) -> &Span {
		self.spans.span_at(self.offset_from_name(name))
	}

	pub fn span_from_offset(&self, offs: SegOffset) -> &Span {
		self.spans.span_at(offs)
	}

	pub fn span_from_va(&self, va: VAddr) -> &Span {
		self.spans.span_at(self.offset_from_va(va))
	}

	pub fn span_from_pa(&self, pa: PAddr) -> &Span {
		self.spans.span_at(self.offset_from_pa(pa))
	}
}

// ------------------------------------------------------------------------------------------------
// Locations and References
// ------------------------------------------------------------------------------------------------

pub struct Location<'a> {
	pub seg: &'a Segment<'a>,
	pub offs: SegOffset,
}

impl<'a> Display for Location<'a> {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		write!(f, "{}:{}", self.seg.name, self.get_name())
	}
}

impl<'a> Location<'a> {
	pub fn new(seg: &'a Segment, offs: SegOffset) -> Self {
		assert!(seg.contains_offset(offs));
		Self { seg, offs }
	}

	pub fn get_name(&self) -> String {
		self.seg.name_of_offset(self.offs)
	}

	pub fn get_full_name(&self) -> String {
		self.to_string()
	}

	pub fn get_span(&self) -> &Span {
		self.seg.span_from_offset(self.offs)
	}

	// pub fn __hash__(&self) -> {
	// 	hash((self.seg, self.offs))
	// }
	// pub fn __eq__(&self, other) -> {
	// 	self.seg is other.seg and self.offs == other.offs
	// }
}

pub struct Reference<'a> {
	pub src: Location<'a>,
	pub dst: Location<'a>,
}

impl<'a> Reference<'a> {
	pub fn new(src: Location<'a>, dst: Location<'a>) -> Self {
		Self { src, dst }
	}

	pub fn get_name(&self) -> String {
		self.dst.get_name()
	}

	pub fn get_full_name(&self) -> String {
		self.dst.get_full_name()
	}

	// def __hash__(self):      return hash(self.src) ^ hash(self.dst)
	// def __eq__(self, other): return self.src == other.src and self.dst == other.dst
}

// ------------------------------------------------------------------------------------------------
// Memory
// ------------------------------------------------------------------------------------------------

#[derive(new)]
pub struct RomImage<'a> {
	name: &'a str,
	data: &'a [u8],
}

pub type SegmentMap<'a> = HashMap<&'a str, usize>;

pub struct Memory<'a> {
	image:        RomImage<'a>,
	mem_map:      MemoryMap<'a>,
	segs:         &'a mut [Segment<'a>],
	seg_name_map: SegmentMap<'a>,
	config:       MemoryConfig<'a>,
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

		Self { image, mem_map, segs, seg_name_map, config }
	}

	// ---------------------------------------------------------------------------------------------
	// Queries

	pub fn len(&self) -> usize {
		self.mem_map.len()
	}

	pub fn range_crosses_regions(&self, start: VAddr, end: VAddr) -> bool {
		assert!(end.0 > start.0);

		match (self.mem_map.get(start), self.mem_map.get(end)) {
			(Some(s), Some(e)) => !std::ptr::eq(s, e),
			(None, None)       => false,
			_                  => true,
		}
	}

	// ---------------------------------------------------------------------------------------------
	// Segments

	pub fn segment_for_va(&self, va: VAddr) -> Option<&Segment> {
		self.mem_map.get(va)
		.and_then(|region| self.config.segment_for_region(region.name))
		.and_then(|name|   self.segment_for_name(name))
	}

	pub fn segment_for_va_mut(&'a mut self, va: VAddr) -> Option<&mut Segment> {
		let name = match self.mem_map.get(va) {
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

	pub fn name_of_va(&self, va: VAddr) -> String {
		match self.segment_for_va(va) {
			Some(seg) => seg.name_of_va(va),
			None      => generate_name("UNK", va, None),
		}
	}

	pub fn name_of_loc(&'a self, loc: Location<'a>) -> String {
		// doing it this way ensures that we really do have this segment.
		self.segment_for_name(loc.seg.name)
			.expect("segment not in segment map")
			.name_of_offset(loc.offs)
	}

	pub fn add_label(&'a mut self, name: &'a str, loc: Location<'a>) {
		self.segment_for_name_mut(loc.seg.name)
			.expect("segment not in segment map")
			.add_label_offset(name, loc.offs);
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

// ------------------------------------------------------------------------------------------------
// Tests
// ------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn span_map() {
		let mut m = SpanMap::new(2usize.pow(16));
		m.dump_spans();
		m.redefine(SegOffset(0x1000), SegOffset(0x2000), SpanKind::Code, None);
		m.redefine(SegOffset(0x2000), SegOffset(0x10000), SpanKind::Data, None);
		m.redefine(SegOffset(0x800), SegOffset(0x1800), SpanKind::Unk, None);
		m.dump_spans();
		m.redefine(SegOffset(0), SegOffset(0x800), SpanKind::Code, None);
		m.dump_spans();

		println!("span map iter:");
		for s in m.iter() {
			println!("{}", s.start);
		}

		m.redefine(SegOffset(0), SegOffset(0x10000), SpanKind::Unk, None);
		m.dump_spans();
	}
}