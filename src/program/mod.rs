
use std::collections::{
	btree_map::Iter as BTreeIter,
	btree_map::Range as BTreeRange,
	hash_map::Iter as HashIter,
	VecDeque,
};
use std::ops::{ Bound, RangeBounds };
use std::fmt::{ Display, Formatter, Result as FmtResult };

use delegate::delegate;

use crate::analysis::{ AnalysisItem };
use crate::memory::{
	Memory, IMemory, MmuState, Location, VA, SegId, Span, SpanKind, Segment };
use crate::disasm::{ INameLookup };
use crate::platform::{ IPlatform, MmuTypeOf, InstTypeOf };

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

/// The public interface to a `Program` object.
pub trait IProgram: Display {
	fn enqueue_function(&mut self, state: MmuState, loc: Location);
	fn enqueue_jump_table(&mut self, loc: Location);
	fn analyze_queue(&mut self);
	fn initial_mmu_state(&self) -> MmuState;
	fn segment_for_va(&self, state: MmuState, va: VA) -> Option<&Segment>;
	fn segment_for_va_mut(&mut self, state: MmuState, va: VA) -> Option<&mut Segment>;
	fn segment_for_name(&self, name: &str) -> Option<&Segment>;
	fn segment_for_name_mut(&mut self, name: &str) -> Option<&mut Segment>;
	fn segment_from_loc(&self, loc: Location) -> &Segment;
	fn segment_from_loc_mut(&mut self, loc: Location) -> &mut Segment;
	fn segment_from_id(&self, id: SegId) -> &Segment;
	fn segment_from_id_mut(&mut self, id: SegId) -> &mut Segment;
	fn loc_for_va(&self, state: MmuState, va: VA) -> Option<Location>;
	fn loc_from_va(&self, state: MmuState, va: VA) -> Location;
	fn va_for_loc(&self, state: MmuState, loc: Location) -> Option<VA>;
	fn va_from_loc(&self, state: MmuState, loc: Location) -> VA;
	fn fmt_addr(&self, addr: usize) -> String;
	fn span_at_loc(&self, loc: Location) -> Span;
	fn seg_and_span_at_loc(&self, loc: Location) -> (&Segment, Span);
	fn seg_and_span_at_loc_mut(&mut self, loc: Location) -> (&mut Segment, Span);
	// fn get_func(&self, id: FuncId) -> &Function<I>;
	// fn get_func_mut(&mut self, id: FuncId) -> &mut Function<I>;
	// fn all_funcs(&self) -> impl Iterator<Item = (FuncId, &Function<I>)>;
	// fn all_funcs_mut(&mut self) -> impl Iterator<Item = (FuncId, &mut Function<I>)>;
	fn func_defined_at(&self, loc: Location) -> Option<FuncId>;
	fn func_that_contains(&self, loc: Location) -> Option<FuncId>;
	fn add_name_va(&mut self, name: &str, state: MmuState, va: VA);
	fn add_name(&mut self, name: &str, loc: Location);
	fn remove_name(&mut self, name: &str);
	fn remove_name_from_loc(&mut self, loc: Location);
	fn has_name(&self, name: &str) -> bool;
	fn has_name_for_loc(&self, loc: Location) -> bool;
	fn all_names(&self) -> HashIter<'_, String, Location>;
	fn all_names_by_loc(&self) -> BTreeIter<'_, Location, String>;
	fn name_from_loc(&self, loc: Location) -> &str;
	fn loc_from_name(&self, name: &str) -> Location;
	// fn names_in_range(&self, range: impl RangeBounds<Location>)
	// -> BTreeRange<'_, Location, String>;
	// fn names_in_va_range(&self, state: MmuState, range: impl RangeBounds<VA>)
	// -> BTreeRange<'_, Location, String>;
	fn name_of_va(&self, state: MmuState, va: VA) -> String;
	fn add_ref(&mut self, src: Location, dst: Location);
	fn remove_ref(&mut self, src: Location, dst: Location);
	fn remove_all_outrefs(&mut self, src: Location);
	fn remove_all_inrefs(&mut self, dst: Location);
	fn get_inrefs(&self, dst: Location) -> Option<&RefSet>;
	fn get_outrefs(&self, src: Location) -> Option<&RefSet>;
	fn all_outrefs(&self) -> BTreeIter<'_, Location, RefSet>;
}

/// A Program contains a Memory object and indexes of names, references, functions, and variables.
pub struct Program<Plat: IPlatform> {
	mem:   Memory<MmuTypeOf<Plat>>,
	plat:  Plat,
	names: NameMap,
	refs:  RefMap,
	funcs: FuncIndex<InstTypeOf<Plat>>,
	pub(crate) queue: VecDeque<AnalysisItem>,
}

impl<Plat: IPlatform> Display for Program<Plat> {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		writeln!(f, "Platform: {}", self.plat)?;
		write!(f, "{}", self.mem)
	}
}

impl<Plat: IPlatform> Program<Plat> {
	pub fn new(mem: Memory<MmuTypeOf<Plat>>, plat: Plat) -> Self {
		Self {
			mem,
			plat,
			names: NameMap::new(),
			refs:  RefMap::new(),
			funcs: FuncIndex::new(),
			queue: VecDeque::new(),
		}
	}

	pub(crate) fn plat(&self) -> &Plat {
		&self.plat
	}

	delegate! {
		to self.funcs {
			/// Get the function object with the given ID.
			#[call(get)]
			pub fn get_func(&self, id: FuncId) -> &Function<InstTypeOf<Plat>>;
			/// Same as above, but mutable.
			#[call(get_mut)]
			pub fn get_func_mut(&mut self, id: FuncId) -> &mut Function<InstTypeOf<Plat>>;
			/// Iterator over all functions in the program, in arbitrary order.
			#[call(iter)]
			pub fn all_funcs(&self) -> impl Iterator<Item = (FuncId, &Function<InstTypeOf<Plat>>)>;
			/// Same as above, but mutable.
			#[call(iter_mut)]
			pub fn all_funcs_mut(&mut self)
				-> impl Iterator<Item = (FuncId, &mut Function<InstTypeOf<Plat>>)>;
		}
	}

	/// Creates a new function at the given location, with basic blocks given by the iterator.
	/// Returns the new function's globally unique ID.
	pub(crate) fn new_func(&mut self, loc: Location,
		bbs: impl Iterator<Item = impl IntoBasicBlock<InstTypeOf<Plat>>>) -> FuncId {
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

	delegate! {
		to self.names {
			/// All (Location, name) pairs in a given range of Locations, in Location order.
			#[call(names_in_range)]
			pub fn names_in_range(&self, range: impl RangeBounds<Location>)
			-> BTreeRange<'_, Location, String>;
		}
	}

	/// All (Location, name) pairs in a given range of VAs, in Location order.
	pub fn names_in_va_range(&self, state: MmuState, range: impl RangeBounds<VA>)
	-> BTreeRange<'_, Location, String> {
		let range = va_range_to_loc_range(range, |va| self.mem.loc_for_va(state, va).unwrap());
		self.names_in_range(range)
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
	// Private

	fn generate_name(&self, base: &str, va: VA) -> String {
		format!("{}_{}_{}", base, AUTOGEN_NAME_PREFIX, self.mem.fmt_addr(va.0))
	}
}

impl<Plat: IPlatform> IProgram for Program<Plat> {
	// ---------------------------------------------------------------------------------------------
	// Analysis

	/// Puts a location on the queue that should be the start of a function.
	fn enqueue_function(&mut self, state: MmuState, loc: Location) {
		self.queue.push_back(AnalysisItem::Func1stPass(loc, state))
	}

	/// Puts a location on the queue that should be the jump instruction for a jump table.
	fn enqueue_jump_table(&mut self, loc: Location) {
		self.queue.push_back(AnalysisItem::JumpTable(loc))
	}

	/// Analyzes all items in the analysis queue. Analysis may generate more items to analyze,
	/// so this can do a lot of work in a single call.
	fn analyze_queue(&mut self) {
		loop {
			match self.queue.pop_front() {
				None => break,
				Some(AnalysisItem::Func1stPass(loc, state)) => self.func_first_pass(loc, state),
				Some(AnalysisItem::Func2ndPass(fid))        => self.func_second_pass(fid),
				Some(AnalysisItem::JumpTable(loc))          => self.analyze_jump_table(loc),
			}
		}
	}

	// ---------------------------------------------------------------------------------------------
	// Memory

	delegate! {
		to self.mem {
			/// The initial state of the MMU.
			fn initial_mmu_state(&self) -> MmuState;
			/// Given a VA, get the Segment which contains it (if any).
			fn segment_for_va(&self, state: MmuState, va: VA) -> Option<&Segment>;
			/// Same as above but mutable.
			fn segment_for_va_mut(&mut self, state: MmuState, va: VA) -> Option<&mut Segment>;
			/// Given a segment name, get the Segment named that (if any).
			fn segment_for_name(&self, name: &str) -> Option<&Segment>;
			/// Same as above but mutable.
			fn segment_for_name_mut(&mut self, name: &str) -> Option<&mut Segment>;
			/// Given a location, get the Segment which contains it.
			fn segment_from_loc(&self, loc: Location) -> &Segment;
			/// Same as above but mutable.
			fn segment_from_loc_mut(&mut self, loc: Location) -> &mut Segment;
			/// Given a segment ID, get the Segment which it refers to.
			fn segment_from_id(&self, id: SegId) -> &Segment;
			/// Same as above but mutable.
			fn segment_from_id_mut(&mut self, id: SegId) -> &mut Segment;
			/// Tries to find a unique location for the given VA.
			/// If there is no mapping, or if the region is bankable, returns None.
			fn loc_for_va(&self, state: MmuState, va: VA) -> Option<Location>;
			/// Same as above, but infallible.
			fn loc_from_va(&self, state: MmuState, va: VA) -> Location;
			/// Gets the VA which corresponds to this location, if any.
			fn va_for_loc(&self, state: MmuState, loc: Location) -> Option<VA>;
			/// Same as above, but infallible.
			fn va_from_loc(&self, state: MmuState, loc: Location) -> VA;
			/// Formats a number as a hexadecimal number with the appropriate number of digits
			/// for the size of the address space.
			fn fmt_addr(&self, addr: usize) -> String;
		}
	}

	/// Get the span at a given location.
	fn span_at_loc(&self, loc: Location) -> Span {
		self.segment_from_loc(loc).span_at_loc(loc)
	}

	/// Get the owning segment and span of a given location.
	fn seg_and_span_at_loc(&self, loc: Location) -> (&Segment, Span) {
		let seg = self.segment_from_loc(loc);
		(seg, seg.span_at_loc(loc))
	}

	/// Same as above, but mutable.
	fn seg_and_span_at_loc_mut(&mut self, loc: Location) -> (&mut Segment, Span) {
		let seg = self.segment_from_loc_mut(loc);
		let span = seg.span_at_loc(loc);
		(seg, span)
	}

	// ---------------------------------------------------------------------------------------------
	// Functions



	/// Gets the ID of the function which starts at the given location, if one exists.
	fn func_defined_at(&self, loc: Location) -> Option<FuncId> {
		let func_id = self.func_that_contains(loc)?;
		if self.funcs.get(func_id).start_loc() == loc {
			Some(func_id)
		} else {
			None
		}
	}

	/// Gets the ID of the function that contains the given location, or None if none does.
	fn func_that_contains(&self, loc: Location) -> Option<FuncId> {
		Some(self.span_at_loc(loc).bb()?.func())
	}



	// ---------------------------------------------------------------------------------------------
	// Names

	/// Assigns a name to a given VA. Panics if the VA doesn't map to a unique Location.
	fn add_name_va(&mut self, name: &str, state: MmuState, va: VA) {
		let loc = self.mem.loc_for_va(state, va).unwrap();
		self.add_name(name, loc);
	}

	delegate! {
		to self.names {
			/// Assigns a name to a given Location. Renames it if it already has one.
			#[call(add)]
			fn add_name(&mut self, name: &str, loc: Location);
			/// Removes a name. Panics if the name doesn't exist.
			fn remove_name(&mut self, name: &str);
			/// Removes the name from a location. Panics if there is no name.
			#[call(remove_loc)]
			fn remove_name_from_loc(&mut self, loc: Location);
			/// Whether this name exists.
			fn has_name(&self, name: &str) -> bool;
			/// Whether this location has a name.
			#[call(has_loc)]
			fn has_name_for_loc(&self, loc: Location) -> bool;
			/// All (name, Location) pairs in arbitrary order.
			#[call(names)]
			fn all_names(&self) -> HashIter<'_, String, Location>;
			/// All (Location, name) pairs in Location order.
			#[call(locations)]
			fn all_names_by_loc(&self) -> BTreeIter<'_, Location, String>;
		}
	}

	/// Gets the name for a location. Panics if it has none.
	fn name_from_loc(&self, loc: Location) -> &str {
		self.names.name_for_loc(loc).unwrap()
	}

	/// Gets the location for a name. Panics if the name doesn't exist.
	fn loc_from_name(&self, name: &str) -> Location {
		self.names.loc_for_name(name).unwrap()
	}

	/// Gets the name of a given VA if one exists, or generates one if not.
	fn name_of_va(&self, state: MmuState, va: VA) -> String {
		if let Some(_loc) = self.mem.loc_for_va(state, va) {
			"TODO".into() // self.name_of_loc(loc)
		} else {
			self.generate_name(&self.mem.name_prefix_for_va(state, va), va)
		}
	}

	// TODO
/*	/// Gets the name of a given Location if one exists, or generates one if not.
	fn name_of_loc(&self, loc: Location) -> String {
		// see if there's already a name here.
		if let Some(name) = self.names.name_for_loc(loc) {
			name.into()
		} else {
			// what span is here?
			let seg = self.mem.segment_from_loc(loc);
			let va = self.va_from_loc(loc);
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
	}*/

	// ---------------------------------------------------------------------------------------------
	// References

	delegate! {
		to self.refs {
			/// Add a reference from `src` to `dst`.
			#[call(add)]
			fn add_ref(&mut self, src: Location, dst: Location);
			/// Remove a reference.
			#[call(remove)]
			fn remove_ref(&mut self, src: Location, dst: Location);
			/// Remove all outrefs from the given location.
			fn remove_all_outrefs(&mut self, src: Location);
			/// Remove all inrefs to the given location.
			fn remove_all_inrefs(&mut self, dst: Location);
			/// Get all inrefs to a given location, or None if there aren't any.
			fn get_inrefs(&self, dst: Location) -> Option<&RefSet>;
			/// Get all outrefs from a given location, or None if there aren't any.
			fn get_outrefs(&self, src: Location) -> Option<&RefSet>;
			/// Iterator over all outrefs in the entire map.
			fn all_outrefs(&self) -> BTreeIter<'_, Location, RefSet>;
		}
	}
}

impl<Plat: IPlatform> INameLookup for Program<Plat> {
	fn lookup(&self, state: MmuState, addr: VA) -> Option<String> {
		Some(self.name_of_va(state, addr))
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