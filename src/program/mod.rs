
use std::collections::{
	btree_map::Iter as BTreeIter,
	btree_map::Range as BTreeRange,
	hash_map::Iter as HashIter,
};
use std::ops::{ Bound, RangeBounds };
use std::fmt::{ Display, Formatter, Result as FmtResult };

use derive_new::new;
use delegate::delegate;

use crate::memory::{ Memory, Location, VA, SegId, Span, SpanKind, Segment };
use crate::disasm::INameLookup;

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

impl Display for Program {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		write!(f, "{}", self.mem)
	}
}

impl Program {
	// ---------------------------------------------------------------------------------------------
	// Memory

	delegate! {
		to self.mem {
			/// Given a VA, get the Segment which contains it (if any).
			pub fn segment_for_va(&self, va: VA) -> Option<&Segment>;
			/// Same as above but mutable.
			pub fn segment_for_va_mut(&mut self, va: VA) -> Option<&mut Segment>;
			/// Given a segment name, get the Segment named that (if any).
			pub fn segment_for_name(&self, name: &str) -> Option<&Segment>;
			/// Same as above but mutable.
			pub fn segment_for_name_mut(&mut self, name: &str) -> Option<&mut Segment>;
			/// Given a location, get the Segment which contains it.
			pub fn segment_from_loc(&self, loc: Location) -> &Segment;
			/// Same as above but mutable.
			pub fn segment_from_loc_mut(&mut self, loc: Location) -> &mut Segment;
			/// Given a segment ID, get the Segment which it refers to.
			pub fn segment_from_id(&self, id: SegId) -> &Segment;
			/// Same as above but mutable.
			pub fn segment_from_id_mut(&mut self, id: SegId) -> &mut Segment;
			/// Tries to find a unique location for the given VA.
			/// If there is no mapping, or if the region is bankable, returns None.
			pub fn loc_for_va(&self, va: VA) -> Option<Location>;
			/// Same as above, but infallible.
			pub fn loc_from_va(&self, va: VA) -> Location;
			/// Formats a number as a hexadecimal number with the appropriate number of digits
			/// for the size of the address space.
			pub fn fmt_addr(&self, addr: usize) -> String;
		}
	}

	/// Get the span at a given location.
	pub fn span_at_loc(&self, loc: Location) -> Span {
		self.segment_from_loc(loc).span_at_loc(loc)
	}

	/// Get the owning segment and span of a given location.
	pub fn seg_and_span_at_loc(&self, loc: Location) -> (&Segment, Span) {
		let seg = self.segment_from_loc(loc);
		(seg, seg.span_at_loc(loc))
	}

	/// Same as above, but mutable.
	pub fn seg_and_span_at_loc_mut(&mut self, loc: Location) -> (&mut Segment, Span) {
		let seg = self.segment_from_loc_mut(loc);
		let span = seg.span_at_loc(loc);
		(seg, span)
	}

	pub(crate) fn span_begin_analysis(&mut self, loc: Location) {
		self.segment_from_loc_mut(loc).span_begin_analysis(loc)
	}

	pub(crate) fn span_cancel_analysis(&mut self, loc: Location) {
		self.segment_from_loc_mut(loc).span_cancel_analysis(loc)
	}

	pub(crate) fn span_end_analysis(&mut self, loc: Location, end: Location, kind: SpanKind) {
		self.segment_from_loc_mut(loc).span_end_analysis(loc, end, kind)
	}

	// ---------------------------------------------------------------------------------------------
	// Functions

	delegate! {
		to self.funcs {
			/// Get the function object with the given ID.
			#[call(get)]
			pub fn get_func(&self, id: FuncId) -> &Function;
			/// Same as above, but mutable.
			#[call(get_mut)]
			pub fn get_func_mut(&mut self, id: FuncId) -> &mut Function;
			/// Iterator over all functions in the program, in arbitrary order.
			#[call(iter)]
			pub fn all_funcs(&self) -> impl Iterator<Item = (FuncId, &Function)>;
			/// Same as above, but mutable.
			#[call(iter_mut)]
			pub fn all_funcs_mut(&mut self) -> impl Iterator<Item = (FuncId, &mut Function)>;
		}
	}

	/// Gets the ID of the function which starts at the given location, if one exists.
	pub fn func_defined_at(&self, loc: Location) -> Option<FuncId> {
		let func_id = self.func_that_contains(loc)?;
		if self.funcs.get(func_id).start_loc() == loc {
			Some(func_id)
		} else {
			None
		}
	}

	/// Gets the ID of the function that contains the given location, or None if none does.
	pub fn func_that_contains(&self, loc: Location) -> Option<FuncId> {
		Some(self.span_at_loc(loc).bb()?.func())
	}

	/// Creates a new function at the given location, with basic blocks given by the iterator.
	/// Returns the new function's globally unique ID.
	pub fn new_func(&mut self, loc: Location, bbs: impl Iterator<Item = impl IntoBasicBlock>)
	-> FuncId {
		assert!(self.func_defined_at(loc).is_none(), "redefining a function at {}", loc);

		let fid = self.funcs.new_func();
		let new_func = self.funcs.get_mut(fid);
		let seg = self.mem.segment_from_loc_mut(loc);

		for bb in bbs {
			let bbid = new_func.next_id();
			let bb = bb.into_bb(bbid);
			let bb_loc = bb.loc();
			new_func.add_bb(bb);
			seg.redefine_span(bb_loc, SpanKind::Code(bbid));
		}

		fid
	}

	// ---------------------------------------------------------------------------------------------
	// Names

	/// Assigns a name to a given VA. Panics if the VA doesn't map to a unique Location.
	pub fn add_name_va(&mut self, name: &str, va: VA) {
		let loc = self.mem.loc_for_va(va).unwrap();
		self.add_name(name, loc);
	}

	delegate! {
		to self.names {
			/// Assigns a name to a given Location. Renames it if it already has one.
			#[call(add)]
			pub fn add_name(&mut self, name: &str, loc: Location);
			/// Removes a name. Panics if the name doesn't exist.
			pub fn remove_name(&mut self, name: &str);
			/// Removes the name from a location. Panics if there is no name.
			#[call(remove_loc)]
			pub fn remove_name_from_loc(&mut self, loc: Location);
			/// Whether this name exists.
			pub fn has_name(&self, name: &str) -> bool;
			/// Whether this location has a name.
			#[call(has_loc)]
			pub fn has_name_for_loc(&self, loc: Location) -> bool;
			/// All (name, Location) pairs in arbitrary order.
			#[call(names)]
			pub fn all_names(&self) -> HashIter<'_, String, Location>;
			/// All (Location, name) pairs in Location order.
			#[call(locations)]
			pub fn all_names_by_loc(&self) -> BTreeIter<'_, Location, String>;
			/// All (Location, name) pairs in a given range of Locations, in Location order.
			#[call(names_in_range)]
			pub fn names_in_range(&self, range: impl RangeBounds<Location>)
			-> BTreeRange<'_, Location, String>;
		}
	}

	/// Gets the name for a location. Panics if it has none.
	pub fn name_from_loc(&self, loc: Location) -> &str {
		self.names.name_for_loc(loc).unwrap()
	}

	/// Gets the location for a name. Panics if the name doesn't exist.
	pub fn loc_from_name(&self, name: &str) -> Location {
		self.names.loc_for_name(name).unwrap()
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
		} else if let Some(region) = self.mem.region_for_va(va) {
			// name it "REGIONNAME_loc_0C30"
			self.generate_name(&region.name(), va)
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
			let start = seg.span_at_loc(loc).start();

			match self.names.name_for_loc(start) {
				Some(name) =>
					// there's already a name, so name it like "main_loc_0C30"
					self.generate_name(name, va),
				None =>
					// no name, so name it "SEGNAME_loc_0C30"
					self.generate_name(&seg.name(), va),
			}
		}
	}

	// ---------------------------------------------------------------------------------------------
	// References

	delegate! {
		to self.refs {
			/// Add a reference from `src` to `dst`.
			#[call(add)]
			pub fn add_ref(&mut self, src: Location, dst: Location);
			/// Remove a reference.
			#[call(remove)]
			pub fn remove_ref(&mut self, src: Location, dst: Location);
			/// Remove all outrefs from the given location.
			pub fn remove_all_outrefs(&mut self, src: Location);
			/// Remove all inrefs to the given location.
			pub fn remove_all_inrefs(&mut self, dst: Location);
			/// Get all inrefs to a given location, or None if there aren't any.
			pub fn get_inrefs(&self, dst: Location) -> Option<&RefSet>;
			/// Get all outrefs from a given location, or None if there aren't any.
			pub fn get_outrefs(&self, src: Location) -> Option<&RefSet>;
			/// Iterator over all outrefs in the entire map.
			pub fn all_outrefs(&self) -> BTreeIter<'_, Location, RefSet>;
		}
	}
	// ---------------------------------------------------------------------------------------------
	// Private

	fn generate_name(&self, base: &str, va: VA) -> String {
		format!("{}_{}_{}", base, AUTOGEN_NAME_PREFIX, self.mem.fmt_addr(va.0))
	}
}

impl INameLookup for Program {
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