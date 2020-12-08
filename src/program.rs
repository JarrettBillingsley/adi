
use std::collections::{
	btree_map::Iter as BTreeIter,
	btree_map::Range as BTreeRange,
	hash_map::Iter as HashIter,
};
use std::ops::{ Bound, RangeBounds };

use derive_new::new;

use crate::memory::{ Memory, Location, VA, SpanKind };
use crate::disasm::NameLookupTrait;

// ------------------------------------------------------------------------------------------------
// Sub-modules
// ------------------------------------------------------------------------------------------------

mod func;
mod namemap;
mod refmap;

pub use func::*;
pub use namemap::*;
pub use refmap::*;

// ------------------------------------------------------------------------------------------------
// Program
// ------------------------------------------------------------------------------------------------

/// A Program contains a Memory object and indexes of names, references, functions, and variables.
#[derive(new)]
pub struct Program {
	mem:   Memory,

	#[new(default)]
	names: NameMap,

	#[new(default)]
	refs:  RefMap,

	#[new(default)]
	funcs: FuncIndex,
}

impl Program {
	/// Gets the Memory object associated with this Program.
	pub fn mem(&self) -> &Memory {
		&self.mem
	}

	/// Gets the Memory object associated with this Program.
	pub fn mem_mut(&mut self) -> &mut Memory {
		&mut self.mem
	}

	// TODO: FuncIndex should proooobably not be publicly exposed?
	pub fn funcs(&self) -> &FuncIndex {
		&self.funcs
	}

	pub fn funcs_mut(&mut self) -> &mut FuncIndex {
		&mut self.funcs
	}

	pub fn new_func(&mut self, loc: Location, bbs: impl Iterator<Item = impl IntoBasicBlock>)
	-> FuncId {
		let fid = self.funcs.new_func();
		let new_func = self.funcs.get_mut(fid);
		let seg = self.mem.segment_from_loc_mut(loc);

		for bb in bbs {
			let bbid = new_func.next_id();
			let bb = bb.into_bb(bbid);
			let bb_loc = bb.loc;
			new_func.add_bb(bb);
			seg.redefine_span(bb_loc, SpanKind::Code(bbid));
		}

		fid
	}

	// ---------------------------------------------------------------------------------------------
	// Names

	/// Assigns a name to a given Location. Renames it if it already has one.
	pub fn add_name(&mut self, name: &str, loc: Location) {
		self.names.add(name, loc);
	}

	/// Assigns a name to a given VA. Panics if the VA doesn't map to a unique Location.
	pub fn add_name_va(&mut self, name: &str, va: VA) {
		let loc = self.mem.loc_for_va(va).unwrap();
		self.add_name(name, loc);
	}

	/// Removes a name. Panics if the name doesn't exist.
	pub fn remove_name(&mut self, name: &str) {
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
	pub fn loc_from_name(&self, name: &str) -> Location {
		self.names.loc_for_name(name).unwrap()
	}

	/// All (name, Location) pairs in arbitrary order.
	pub fn all_names(&self) -> HashIter<'_, String, Location> {
		self.names.names()
	}

	/// All (Location, name) pairs in Location order.
	pub fn all_names_by_loc(&self) -> BTreeIter<'_, Location, String> {
		self.names.locations()
	}

	/// All (Location, name) pairs in a given range of Locations, in Location order.
	pub fn names_in_range(&self, range: impl RangeBounds<Location>)
	-> BTreeRange<'_, Location, String> {
		self.names.names_in_range(range)
	}

	/// All (Location, name) pairs in a given range of VAs, in Location order.
	pub fn names_in_va_range(&self, range: impl RangeBounds<VA>)
	-> BTreeRange<'_, Location, String> {
		let range = va_range_to_loc_range(range, |va| self.mem.loc_for_va(va).unwrap());
		self.names_in_range(range)
	}

	/// Gets the name of a given VA if one exists, or generates one if not.
	pub fn name_of_va(&self, va: VA) -> String {
		if let Some(loc) = self.mem.loc_for_va(va) {
			self.name_of_loc(loc)
		// no mapped segment?? uhhhh....... try region name?
		} else if let Some(region) = self.mem.map().region_for_va(va) {
			// name it "REGIONNAME_loc_0C30"
			self.generate_name(&region.name, va)
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
			let seg = self.mem.segment_from_loc(loc);
			let va = seg.va_from_loc(loc);
			let start = seg.span_at_loc(loc).start;

			match self.names.name_for_loc(start) {
				Some(name) =>
					// there's already a name, so name it like "main_loc_0C30"
					self.generate_name(name, va),
				None =>
					// no name, so name it "SEGNAME_loc_0C30"
					self.generate_name(&seg.name, va),
			}
		}
	}

	fn generate_name(&self, base: &str, va: VA) -> String {
		format!("{}_{}_{}", base, AUTOGEN_NAME_PREFIX, self.mem.fmt_addr(va.0))
	}
}

impl NameLookupTrait for Program {
	fn lookup(&self, addr: VA) -> Option<String> {
		Some(self.name_of_va(addr))
	}
}

fn va_range_to_loc_range(range: impl RangeBounds<VA>, f: impl Fn(VA) -> Location)
-> impl RangeBounds<Location> {
	// this is the right way to convert RangeBounds but it feels so wrong.
	let start = match range.start_bound() {
		Bound::Included(va) => Bound::Included(f(*va)),
		Bound::Excluded(va) => Bound::Excluded(f(*va)),
		Bound::Unbounded    => Bound::Unbounded,
	};

	let end = match range.end_bound() {
		Bound::Included(va) => Bound::Included(f(*va)),
		Bound::Excluded(va) => Bound::Excluded(f(*va)),
		Bound::Unbounded    => Bound::Unbounded,
	};

	(start, end)
}