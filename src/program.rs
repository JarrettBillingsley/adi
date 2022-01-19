
use std::collections::{
	btree_map::Iter as BTreeIter,
	btree_map::Range as BTreeRange,
	hash_map::Iter as HashIter,
	HashMap,
	HashSet,
	VecDeque,
};
use std::ops::{ Bound, RangeBounds };
use std::fmt::{ Display, Formatter, Result as FmtResult };

use smallvec::{ SmallVec };
use delegate::delegate;

use crate::arch::{ INameLookup, IPrinter, Printer, PrinterCtx, IPrintOutput, FmtWritePrintOutput,
	IArchitecture };
use crate::memory::{ Memory, MmuState, StateChange, EA, VA, SegId, Span, SpanKind, Segment,
	Endian };
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
			/// Endianness.
			pub fn endianness(&self) -> Endian;
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
			/// Given an EA, get the Segment which contains it.
			pub fn segment_from_ea(&self, ea: EA) -> &Segment;
			/// Same as above but mutable.
			pub fn segment_from_ea_mut(&mut self, ea: EA) -> &mut Segment;
			/// Given a segment ID, get the Segment which it refers to.
			pub fn segment_from_id(&self, id: SegId) -> &Segment;
			/// Same as above but mutable.
			pub fn segment_from_id_mut(&mut self, id: SegId) -> &mut Segment;
			/// Tries to find a unique EA for the given VA.
			/// If there is no mapping, or if the region is bankable, returns None.
			pub fn ea_for_va(&self, state: MmuState, va: VA) -> Option<EA>;
			/// Same as above, but infallible.
			pub fn ea_from_va(&self, state: MmuState, va: VA) -> EA;
			/// Gets the VA which corresponds to this EA, if any.
			pub fn va_for_ea(&self, state: MmuState, ea: EA) -> Option<VA>;
			/// Same as above, but infallible.
			pub fn va_from_ea(&self, state: MmuState, ea: EA) -> VA;
			/// Formats a number as a hexadecimal number with the appropriate number of digits
			/// for the size of the address space.
			pub fn fmt_addr(&self, addr: usize) -> String;
		}
	}

	/// Iterator over all segments.
	pub fn all_segs(&self) -> impl Iterator<Item = SegId> + '_ {
		self.mem.segs_iter().map(|s| s.id())
	}

	/// Iterator over all segments that map to an image.
	pub fn all_image_segs(&self) -> impl Iterator<Item = SegId> + '_ {
		self.mem.image_segs_iter().map(|s| s.id())
	}

	pub fn mmu_state_at(&self, ea: EA) -> Option<MmuState> {
		let span = self.span_at_ea(ea);

		match span.kind() {
			SpanKind::Code(bbid) => {
				let bb = self.bbidx.get(bbid);
				Some(bb.mmu_state())
			}
			_ => None,
		}
	}

	/// Get the span at a given EA.
	pub fn span_at_ea(&self, ea: EA) -> Span {
		self.segment_from_ea(ea).span_at_ea(ea)
	}

	/// Get the owning segment and span of a given EA.
	pub fn seg_and_span_at_ea(&self, ea: EA) -> (&Segment, Span) {
		let seg = self.segment_from_ea(ea);
		(seg, seg.span_at_ea(ea))
	}

	/// Same as above, but mutable.
	pub fn seg_and_span_at_ea_mut(&mut self, ea: EA) -> (&mut Segment, Span) {
		let seg = self.segment_from_ea_mut(ea);
		let span = seg.span_at_ea(ea);
		(seg, span)
	}

	pub(crate) fn span_begin_analysis(&mut self, ea: EA) {
		self.segment_from_ea_mut(ea).span_begin_analysis(ea)
	}

	pub(crate) fn span_cancel_analysis(&mut self, ea: EA) {
		self.segment_from_ea_mut(ea).span_cancel_analysis(ea)
	}

	pub(crate) fn span_end_analysis(&mut self, ea: EA, end: EA, kind: SpanKind) {
		self.segment_from_ea_mut(ea).span_end_analysis(ea, end, kind)
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

	/// Iterator over all functions in the program, in arbitrary order.
	pub fn all_funcs(&self) -> impl Iterator<Item = &Function> + '_ {
		self.funcs.all_funcs().map(|(_, func)| func)
	}

	/// Gets the ID of the function which starts at the given EA, if one exists.
	pub fn func_defined_at(&self, ea: EA) -> Option<&Function> {
		let func = self.func_that_contains(ea)?;

		if func.ea() == ea {
			Some(func)
		} else {
			None
		}
	}

	/// Gets the ID of the function that contains the given EA, or None if none does.
	pub fn func_that_contains(&self, ea: EA) -> Option<&Function> {
		let bbid = self.span_at_ea(ea).bb()?;
		let func_id = self.bbidx.get(bbid).func();
		Some(self.funcs.get(func_id))
	}

	/// Formats the given instruction's mnemonic into a string.
	pub fn inst_get_mnemonic(&self, i: &Instruction) -> String {
		self.print.get_mnemonic(i)
	}

	/// "Raw" interface for printing instructions. Takes the `output` object whose methods will
	/// be called to output the instruction.
	pub fn inst_print(&self, i: &Instruction, state: MmuState, output: &mut dyn IPrintOutput)
	-> FmtResult {
		let mut ctx = PrinterCtx::new(i, state, self, output);
		self.print.print_instr(&mut ctx)
	}

	/// Formats the given instruction into a string. No styling is included.
	pub fn inst_to_string(&self, i: &Instruction, state: MmuState) -> String {
		let mut ret    = String::new();
		let mut output = FmtWritePrintOutput(&mut ret);
		let mut ctx    = PrinterCtx::new(i, state, self, &mut output);
		self.print.print_instr(&mut ctx).expect("should never fail");
		ret
	}

	/// Formats the given instruction's operands into a string. No styling is included.
	pub fn inst_operands_to_string(&self, i: &Instruction, state: MmuState) -> String {
		let mut ret    = String::new();
		let mut output = FmtWritePrintOutput(&mut ret);
		let mut ctx    = PrinterCtx::new(i, state, self, &mut output);
		self.print.print_operands(&mut ctx).expect("should never fail");
		ret
	}

	/// Creates a new function at the given EA, with basic blocks given by the iterator.
	/// Returns the new function's globally unique ID.
	pub(crate) fn new_func(&mut self, bbs: Vec<BBId>) -> FuncId {
		let ea = self.bbidx.get(bbs[0]).ea;
		assert!(self.func_defined_at(ea).is_none(), "redefining a function at {}", ea);

		let fid      = self.funcs.new_func(ea, bbs);
		let new_func = self.funcs.get_mut(fid);
		let seg      = self.mem.segment_from_ea_mut(ea);

		for bb in &new_func.bbs {
			let bb = self.bbidx.get_mut(*bb);
			bb.mark_complete(fid);
			let bb_ea = bb.ea();
			seg.redefine_span(bb_ea, SpanKind::Code(bb.id()));
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

	/// Gets the ID of the data item which starts at the given EA, if one exists.
	pub fn data_defined_at(&self, ea: EA) -> Option<&DataItem> {
		let item = self.data_that_contains(ea)?;

		if item.ea() == ea {
			Some(item)
		} else {
			None
		}
	}

	/// Gets the ID of the data item that contains the given EA, or None if none does.
	pub fn data_that_contains(&self, ea: EA) -> Option<&DataItem> {
		let data_id = self.span_at_ea(ea).data()?;
		Some(self.data.get(data_id))
	}

	/// Creates a new data item at the given EA. Returns its ID.
	pub fn new_data(&mut self, name: Option<&str>, ea: EA, ty: Type, size: usize)
	-> DataId {
		let did = self.data.new_item(name.map(|s| s.into()), ea, ty, size);
		let seg = self.segment_from_ea_mut(ea);
		seg.span_make_data(ea, size, did);

		if let Some(name) = name {
			self.add_name(name, ea);
		}

		did
	}

	// ---------------------------------------------------------------------------------------------
	// Names

	delegate! {
		to self.names {
			/// Assigns a name to a given EA. Renames it if it already has one.
			#[call(add)]
			pub fn add_name(&mut self, name: &str, ea: EA);
			/// Removes a name. Panics if the name doesn't exist.
			pub fn remove_name(&mut self, name: &str);
			/// Removes the name from an EA. Panics if there is no name.
			#[call(remove_ea)]
			pub fn remove_name_from_ea(&mut self, ea: EA);
			/// Gets the EA for a name, if one of that name exists.
			pub fn ea_for_name(&self, name: &str) -> Option<EA>;
			/// Gets the name for an EA, if there is one.
			pub fn name_for_ea(&self, ea: EA) -> Option<&String>;
			/// Whether this name exists.
			pub fn has_name(&self, name: &str) -> bool;
			/// Whether this EA has a name.
			#[call(has_ea)]
			pub fn has_name_for_ea(&self, ea: EA) -> bool;
			/// All (name, EA) pairs in arbitrary order.
			#[call(names)]
			pub fn all_names(&self) -> HashIter<'_, String, EA>;
			/// All (EA, name) pairs in EA order.
			#[call(eas)]
			pub fn all_names_by_ea(&self) -> BTreeIter<'_, EA, String>;
			/// All (EA, name) pairs in a given range of EAs, in EA order.
			#[call(names_in_range)]
			pub fn names_in_range(&self, range: impl RangeBounds<EA>)
			-> BTreeRange<'_, EA, String>;
		}
	}

	/// Assigns a name to a given VA. Panics if the VA doesn't map to a unique EA.
	pub fn add_name_va(&mut self, name: &str, state: MmuState, va: VA) {
		let ea = self.mem.ea_for_va(state, va).unwrap();
		self.add_name(name, ea);
	}

	/// Gets the name for an EA. Panics if it has none.
	pub fn name_from_ea(&self, ea: EA) -> &str {
		self.names.name_for_ea(ea).unwrap()
	}

	/// Gets the EA for a name. Panics if the name doesn't exist.
	pub fn ea_from_name(&self, name: &str) -> EA {
		self.names.ea_for_name(name).unwrap()
	}

	/// Gets the name of a given VA if one exists, or generates one if not.
	pub fn name_of_va(&self, state: MmuState, va: VA) -> String {
		if let Some(ea) = self.mem.ea_for_va(state, va) {
			self.name_of_ea(ea)
		} else {
			self.generate_name(&self.mem.name_prefix_for_va(state, va), va)
		}
	}

	/// Gets the name of a given EA if one exists, or generates one if not.
	pub fn name_of_ea(&self, ea: EA) -> String {
		// TODO: uhhhh Functions and DataItems have their own name fields. how does
		// that interact with this? (should they even have name fields?)

		// see if there's already a name here.
		if let Some(name) = self.names.name_for_ea(ea) {
			name.into()
		} else if ea.is_invalid() {
			self.generate_name("UNRESOLVED", VA(ea.offs()))
		} else {
			// what span is here?
			let seg = self.mem.segment_from_ea(ea);

			// TODO: this feels....... wrong and right at the same time.
			// if we have a valid EA, then we have already resolved it to a real segment,
			// meaning that segment has been mapped by the MMU at least once, meaning the MMU
			// must know at what VA it appears.
			let va = if let Some(state) = self.mmu_state_at(ea) {
				self.va_from_ea(state, ea)
				// but this second condition specifically...
				// does va_for_ea even *need* the MMU state? none of the MMU impls so far
				// use it, and because of the "one VA per bank" assumption, an EA can
				// only ever correspond to one VA.
			} else if let Some(va) = self.va_for_ea(self.initial_mmu_state(), ea) {
				va
			} else {
				VA(ea.offs()) // can this ever happen?
			};

			let start = seg.span_at_ea(ea).start();

			match self.names.name_for_ea(start) {
				Some(name) =>
					// there's already a name, so name it like "main_loc_0C30"
					self.generate_name(name, va),
				None =>
					// no name, so name it "SEGNAME_loc_0C30"
					self.generate_name(&seg.name(), va),
			}
		}
	}

	/// All (EA, name) pairs in a given range of VAs, in EA order.
	pub fn names_in_va_range(&self, state: MmuState, range: impl RangeBounds<VA>)
	-> BTreeRange<'_, EA, String> {
		let range = va_range_to_ea_range(range, |va| self.mem.ea_for_va(state, va).unwrap());
		self.names_in_range(range)
	}

	// ---------------------------------------------------------------------------------------------
	// References

	delegate! {
		to self.refs {
			/// Add a reference from `src` to `dst`.
			#[call(add)]
			pub fn add_ref(&mut self, src: EA, dst: EA);
			/// Remove a reference.
			#[call(remove)]
			pub fn remove_ref(&mut self, src: EA, dst: EA);
			/// Remove all outrefs from the given EA.
			pub fn remove_all_outrefs(&mut self, src: EA);
			/// Remove all inrefs to the given EA.
			pub fn remove_all_inrefs(&mut self, dst: EA);
			/// Get all inrefs to a given EA, or None if there aren't any.
			pub fn get_inrefs(&self, dst: EA) -> Option<&RefSet>;
			/// Get all outrefs from a given EA, or None if there aren't any.
			pub fn get_outrefs(&self, src: EA) -> Option<&RefSet>;
			/// Iterator over all outrefs in the entire map.
			pub fn all_outrefs(&self) -> BTreeIter<'_, EA, RefSet>;
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

fn va_range_to_ea_range(range: impl RangeBounds<VA>, f: impl Fn(VA) -> EA)
-> impl RangeBounds<EA> {
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

// -------------------------------------------------------------------------------------------------
// CFG Algorithms

use lazycell::LazyCell;

pub type CfgGraph        = petgraph::graphmap::DiGraphMap<BBId, ()>;
pub type CfgDominators   = petgraph::algo::dominators::Dominators<BBId>;
pub type CfgPredecessors = HashMap<BBId, SmallVec<[BBId; 4]>>;

/// Type to hold onto function CFG analysis data structures to avoid having to recompute them
/// during longer analyses. Holds a reference to the function to prevent it from being modified
/// during the analysis.
pub struct FuncAnalysis<'f> {
	func:  &'f Function,
	cfg:   CfgGraph,
	doms:  LazyCell<CfgDominators>,
	preds: LazyCell<CfgPredecessors>,
}

impl<'f> FuncAnalysis<'f> {
	fn new(func: &'f Function, cfg: CfgGraph) -> Self {
		Self {
			func,
			cfg,
			doms:  LazyCell::new(),
			preds: LazyCell::new(),
		}
	}
}

impl Program {
	/// Begin the analysis of a function. The returned object is meant to be passed to the other
	/// analysis methods and is used to cache their results (as some methods use the results of
	/// others to operate properly).
	pub fn func_begin_analysis<'f>(&self, func: &'f Function) -> FuncAnalysis<'f> {
		let num_bbs = func.num_bbs();
		let mut cfg = CfgGraph::with_capacity(num_bbs, num_bbs);

		// for CFGs with only one node, there are 0 edges, so the loop below will not add
		// the head node.
		cfg.add_node(func.head_id());

		for bbid in func.all_bbs() {
			self.bb_successors_in_function(bbid, |succ_id| {
				cfg.add_edge(bbid, succ_id, ());
			});
		}

		// should be true... the only way it couldn't be true is if there were a BB that had no
		// successors or predecessors in the function, which seeeeeeeems impossible. FOR NOW.
		// this is going to be the setup for a brick joke, isn't it?
		assert!(cfg.node_count() == num_bbs);

		FuncAnalysis::new(func, cfg)
	}

	/// Get or calculate the predecessors of all BBs in this function.
	pub fn func_bb_predecessors<'f>(&self, ana: &'f FuncAnalysis) -> &'f CfgPredecessors {
		use petgraph::visit::{ DfsPostOrder, Walker };

		if !ana.preds.filled() {
			let mut preds = CfgPredecessors::new();

			// "borrowed" from petgraph::algo::dominators::simple_fast_post_order :P
			for pred in DfsPostOrder::new(&ana.cfg, ana.func.head_id()).iter(&ana.cfg) {
				for succ in ana.cfg.neighbors(pred) {
					preds.entry(succ).or_insert_with(SmallVec::new).push(pred);
				}
			}

			ana.preds.fill(preds).unwrap();
		}

		ana.preds.borrow().unwrap()
	}

	/// Get or calculate the dominators of all BBs in this function.
	///
	/// Panics if the function is multi-entry.
	pub fn func_bb_dominators<'f>(&self, ana: &'f FuncAnalysis) -> &'f CfgDominators {
		if !ana.doms.filled() {
			assert!(!ana.func.is_multi_entry());

			let doms = petgraph::algo::dominators::simple_fast(&ana.cfg, ana.func.head_id());

			ana.doms.fill(doms).unwrap();
		}

		ana.doms.borrow().unwrap()
	}

	/// Dump the function's CFG as a DOT diagram description to the console. DEBUGGING!
	pub fn func_dump_cfg<'f>(&self, ana: &'f FuncAnalysis) {
		use petgraph::dot::{ Dot, Config };
		println!("--------------------------------------------------------------");
		println!("function {}", self.name_of_ea(ana.func.ea()));
		println!("{:?}", Dot::with_config(&ana.cfg, &[Config::EdgeNoLabel]));
	}

	/// Calculates the set of all BBs reachable from the `start` bb in this function. The returned
	/// set includes `start`. The result of this analysis is not cached.
	pub fn func_reachable_bbs<'f>(&self, ana: &'f FuncAnalysis, start: BBId) -> HashSet<BBId> {
		use petgraph::visit::{ DfsPostOrder, Walker };

		let mut reachable = HashSet::new();

		for bb in DfsPostOrder::new(&ana.cfg, start).iter(&ana.cfg) {
			reachable.insert(bb);
		}

		reachable
	}

	/// Given a BB, iterate over only the successors which appear within the same function.
	/// This does not return an iterator for Borrowing Reasons.
	pub fn bb_successors_in_function(&self, bbid: BBId, mut visit: impl FnMut(BBId)) {
		let bb = self.bbidx.get(bbid);
		let fid = bb.func();

		for &ea in bb.successors() {
			if let Some(succ_id) = self.span_at_ea(ea).bb() {
				if self.bbidx.get(succ_id).func() == fid {
					visit(succ_id);
				}
			}
		}
	}
}