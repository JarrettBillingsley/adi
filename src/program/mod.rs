
use std::collections::{
	btree_map::Iter as BTreeIter,
	btree_map::Range as BTreeRange,
	hash_map::Iter as HashIter,
	HashMap,
	VecDeque,
};
use std::ops::{ Bound, RangeBounds };
use std::fmt::{ Display, Formatter, Result as FmtResult };

use smallvec::{ SmallVec };
use delegate::delegate;

use crate::arch::{ INameLookup, IPrinter, Printer, IArchitecture };
use crate::memory::{ Memory, MmuState, StateChange, Location, VA, SegId, Span, SpanKind, Segment };
use crate::platform::{ Platform, IPlatform };

// ------------------------------------------------------------------------------------------------
// Sub-modules
// ------------------------------------------------------------------------------------------------

mod analysis;
mod bb;
mod data;
mod func;
mod inst;
mod namemap;
mod refmap;

use analysis::*;
pub use bb::*;
pub use data::*;
pub use func::*;
pub use inst::*;
pub use namemap::*;
pub use refmap::*;

// ------------------------------------------------------------------------------------------------
// Program
// ------------------------------------------------------------------------------------------------

/// A Program contains a Memory object and indexes of names, references, functions, and variables.
pub struct Program {
	mem:   Memory,
	plat:  Platform,
	names: NameMap,
	refs:  RefMap,
	funcs: FuncIndex,
	data:  DataIndex,
	bbidx: BBIndex,
	pub(crate) queue: VecDeque<AnalysisItem>,
	print: Printer,
}

impl Display for Program {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		writeln!(f, "Platform: {}", self.plat)?;
		write!(f, "{}", self.mem)
	}
}

impl Program {
	pub fn new(mem: Memory, plat: Platform) -> Self {
		Self {
			mem,
			print: plat.arch().new_printer(),
			plat,
			names: NameMap::new(),
			refs:  RefMap::new(),
			funcs: FuncIndex::new(),
			data:  DataIndex::new(),
			bbidx: BBIndex::new(),
			queue: VecDeque::new(),
		}
	}

	pub fn plat(&self) -> &Platform {
		&self.plat
	}

	// ---------------------------------------------------------------------------------------------
	// Memory

	delegate! {
		to self.mem {
			/// The initial state of the MMU.
			pub fn initial_mmu_state(&self) -> MmuState;
			/// How this instruction changes the MMU state.
			pub fn inst_state_change(&self, state: MmuState, i: &Instruction) -> StateChange;
			/// Given a VA, get the Segment which contains it (if any).
			pub fn segment_for_va(&self, state: MmuState, va: VA) -> Option<&Segment>;
			/// Same as above but mutable.
			pub fn segment_for_va_mut(&mut self, state: MmuState, va: VA) -> Option<&mut Segment>;
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
			pub fn loc_for_va(&self, state: MmuState, va: VA) -> Option<Location>;
			/// Same as above, but infallible.
			pub fn loc_from_va(&self, state: MmuState, va: VA) -> Location;
			/// Gets the VA which corresponds to this location, if any.
			pub fn va_for_loc(&self, state: MmuState, loc: Location) -> Option<VA>;
			/// Same as above, but infallible.
			pub fn va_from_loc(&self, state: MmuState, loc: Location) -> VA;
			/// Formats a number as a hexadecimal number with the appropriate number of digits
			/// for the size of the address space.
			pub fn fmt_addr(&self, addr: usize) -> String;
		}
	}

	pub fn mmu_state_at(&self, loc: Location) -> Option<MmuState> {
		let span = self.span_at_loc(loc);

		match span.kind() {
			SpanKind::Code(bbid) => {
				let bb = self.bbidx.get(bbid);
				Some(bb.mmu_state())
			}
			_ => None,
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
		}
	}

	delegate! {
		to self.bbidx {
			/// Get the basic block with the given ID.
			#[call(get)]
			pub fn get_bb(&self, id: BBId) -> &BasicBlock;
			/// Same as above, but mutable.
			#[call(get_mut)]
			pub fn get_bb_mut(&mut self, id: BBId) -> &mut BasicBlock;
		}
	}

	pub fn interp(&self, func: FuncId, iters: usize) {
		use crate::arch::IInterpreter;
		let mut interpreter = self.plat.arch().new_interpreter();
		let func = self.funcs.get(func);
		let head = func.head_id();

		let mut bb = self.bbidx.get(head);
		for _ in 0 .. iters {
			match interpreter.interpret_bb(&self.mem, bb, None) {
				Some(loc) => {
					if let Some(next_bb) = self.span_at_loc(loc).bb() {
						bb = self.bbidx.get(next_bb);
						continue;
					} else {
						println!("invalid next location: {}", loc);
					}
				}

				None => {}
			}

			break;
		}

		println!("done.");
	}

	/// Iterator over all functions in the program, in arbitrary order.
	pub fn all_funcs(&self) -> impl Iterator<Item = &Function> + '_ {
		self.funcs.all_funcs().map(|(_, func)| func)
	}

	/// Gets the ID of the function which starts at the given location, if one exists.
	pub fn func_defined_at(&self, loc: Location) -> Option<&Function> {
		let func = self.func_that_contains(loc)?;

		if func.start_loc() == loc {
			Some(func)
		} else {
			None
		}
	}

	/// Gets the ID of the function that contains the given location, or None if none does.
	pub fn func_that_contains(&self, loc: Location) -> Option<&Function> {
		let bbid = self.span_at_loc(loc).bb()?;
		let func_id = self.bbidx.get(bbid).func();
		Some(self.funcs.get(func_id))
	}

	/// Calculate the predecessors of all BBs in this function. **This is an expensive
	/// operation!**
	pub fn func_bb_predecessors(&self, func: &Function) -> HashMap<BBId, SmallVec<[BBId; 4]>> {
		let mut ret = func.bbs.iter().map(|&bbid|
			(bbid, SmallVec::new())
		).collect::<HashMap<_, _>>();

		let fid = func.id();

		for pred in func.bbs.iter() {
			let pred = self.bbidx.get(*pred);
			for loc in pred.successors() {
				match self.span_at_loc(*loc).bb() {
					Some(succ_id) => {
						let succ = self.bbidx.get(succ_id);

						if succ.func() == fid {
							ret.get_mut(&succ_id)
								.expect("it's gotta be in there")
								.push(pred.id());
						}
					}
					_ => {
						// TODO: is there anything we need to do here??
					}
				}
			}
		}

		ret
	}

	pub fn inst_fmt_mnemonic(&self, i: &Instruction) -> String {
		self.print.fmt_mnemonic(i)
	}

	pub fn inst_fmt_operands(&self, state: MmuState, i: &Instruction) -> String {
		self.print.fmt_operands(i, state, self)
	}

	/// Creates a new function at the given location, with basic blocks given by the iterator.
	/// Returns the new function's globally unique ID.
	pub(crate) fn new_func(&mut self, bbs: Vec<BBId>) -> FuncId {
		let loc = self.bbidx.get(bbs[0]).loc;
		assert!(self.func_defined_at(loc).is_none(), "redefining a function at {}", loc);

		let fid      = self.funcs.new_func(loc, bbs);
		let new_func = self.funcs.get_mut(fid);
		let seg      = self.mem.segment_from_loc_mut(loc);

		for bb in &new_func.bbs {
			let bb = self.bbidx.get_mut(*bb);
			bb.mark_complete(fid);
			let bb_loc = bb.loc();
			seg.redefine_span(bb_loc, SpanKind::Code(bb.id()));
		}

		assert_ne!(new_func.num_bbs(), 0);

		fid
	}

	// ---------------------------------------------------------------------------------------------
	// Data

	delegate! {
		to self.data {
			/// Get the data item with the given ID.
			#[call(get)]
			pub fn get_data(&self, id: DataId) -> &DataItem;
			/// Same as above, but mutable.
			#[call(get_mut)]
			pub fn get_data_mut(&mut self, id: DataId) -> &mut DataItem;
		}
	}

	/// Iterator over all data items in the program, in arbitrary order.
	pub fn all_data_items(&self) -> impl Iterator<Item = &DataItem> + '_ {
		self.data.all_items().map(|(_, item)| item)
	}

	/// Gets the ID of the data item which starts at the given location, if one exists.
	pub fn data_defined_at(&self, loc: Location) -> Option<&DataItem> {
		let item = self.data_that_contains(loc)?;

		if item.loc() == loc {
			Some(item)
		} else {
			None
		}
	}

	/// Gets the ID of the data item that contains the given location, or None if none does.
	pub fn data_that_contains(&self, loc: Location) -> Option<&DataItem> {
		let data_id = self.span_at_loc(loc).data()?;
		Some(self.data.get(data_id))
	}

	/// Creates a new data item at the given location. Returns its ID.
	pub fn new_data(&mut self, name: Option<&str>, loc: Location, ty: Type, size: usize)
	-> DataId {
		let did = self.data.new_item(name.map(|s| s.into()), loc, ty, size);
		let seg = self.segment_from_loc_mut(loc);
		seg.span_make_data(loc, size, did);
		did
	}

	// ---------------------------------------------------------------------------------------------
	// Names

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

	/// Assigns a name to a given VA. Panics if the VA doesn't map to a unique Location.
	pub fn add_name_va(&mut self, name: &str, state: MmuState, va: VA) {
		let loc = self.mem.loc_for_va(state, va).unwrap();
		self.add_name(name, loc);
	}

	/// Gets the name for a location. Panics if it has none.
	pub fn name_from_loc(&self, loc: Location) -> &str {
		self.names.name_for_loc(loc).unwrap()
	}

	/// Gets the location for a name. Panics if the name doesn't exist.
	pub fn loc_from_name(&self, name: &str) -> Location {
		self.names.loc_for_name(name).unwrap()
	}

	/// Gets the name of a given VA if one exists, or generates one if not.
	pub fn name_of_va(&self, state: MmuState, va: VA) -> String {
		if let Some(loc) = self.mem.loc_for_va(state, va) {
			self.name_of_loc(loc)
		} else {
			self.generate_name(&self.mem.name_prefix_for_va(state, va), va)
		}
	}

	/// Gets the name of a given Location if one exists, or generates one if not.
	pub fn name_of_loc(&self, loc: Location) -> String {
		// see if there's already a name here.
		if let Some(name) = self.names.name_for_loc(loc) {
			name.into()
		} else if loc.is_invalid() {
			self.generate_name("UNRESOLVED", VA(loc.offs))
		} else {
			// what span is here?
			let seg = self.mem.segment_from_loc(loc);

			// TODO: this feels....... wrong and right at the same time.
			// if we have a valid Location, then we have already resolved it to a real segment,
			// meaning that segment has been mapped by the MMU at least once, meaning the MMU
			// must know at what VA it appears.
			let va = if let Some(state) = self.mmu_state_at(loc) {
				self.va_from_loc(state, loc)
				// but this second condition specifically...
				// does va_for_loc even *need* the MMU state? none of the MMU impls so far
				// use it, and because of the "one VA per bank" assumption, a location can
				// only ever correspond to one VA.
			} else if let Some(va) = self.va_for_loc(self.initial_mmu_state(), loc) {
				va
			} else {
				VA(loc.offs) // can this ever happen?
			};

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