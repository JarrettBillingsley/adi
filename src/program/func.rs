
use std::fmt::{ Debug, Formatter, Result as FmtResult };

use generational_arena::{ Arena, Index };

use crate::memory::{ Location, MmuState };
use crate::program::{ Instruction, BasicBlock, BBId, BBTerm };

// ------------------------------------------------------------------------------------------------
// Function
// ------------------------------------------------------------------------------------------------

/// Newtype which uniquely identifies a `Function`.
#[derive(PartialEq, Eq, Copy, Clone, Hash)]
pub struct FuncId(pub Index);

impl Debug for FuncId {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		let (index, generation) = self.0.into_raw_parts();
		write!(f, "FuncId({}, {})", index, generation)
	}
}

#[derive(PartialEq, Eq, Copy, Clone)]
pub enum FuncIdReal {
	InProgress,
	Complete(FuncId),
}

impl FuncIdReal {
	fn id(&self) -> FuncId {
		use FuncIdReal::*;
		match self {
			InProgress   => panic!("id() called on in-progress function"),
			Complete(id) => *id,
		}
	}

	fn is_in_progress(&self) -> bool {
		matches!(self, FuncIdReal::InProgress)
	}
}

impl Debug for FuncIdReal {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		use FuncIdReal::*;
		match self {
			InProgress   => write!(f, "(in progress)"),
			Complete(id) => write!(f, "{:?}", id),
		}
	}
}

/// A single function.
#[derive(Debug)]
pub struct Function {
	/// Its globally-unique identifier.
	id: FuncIdReal,
	name: Option<String>,

	/// All its `BasicBlock`s. The first entry is the head (entry point). There is no implied
	/// ordering of the rest of them.
	pub(crate) bbs: Vec<BasicBlock>, // [0] is head
}

impl Function {
	/// Its globally-unique identifier.
	pub fn id(&self) -> FuncId {
		self.id.id()
	}

	/// Its name, if it was given one. If `None`, an auto-generated name will be used instead.
	pub fn name(&self) -> Option<&String> {
		self.name.as_ref()
	}

	/// The basic block ID of the function's head.
	pub fn head_id(&self) -> BBId {
		self.bbs[0].id()
	}

	/// Where the function starts.
	pub fn start_loc(&self) -> Location {
		self.bbs[0].loc
	}

	/// Iterator over this function's basic blocks, starting with the head, but then
	/// in arbitrary order.
	pub fn all_bbs(&self) -> impl Iterator<Item = &BasicBlock> {
		self.bbs.iter()
	}

	/// How many basic blocks this function has.
	pub fn num_bbs(&self) -> usize {
		self.bbs.len()
	}

	/// Get the `idx`'th basic block.
	pub fn get_bb_by_idx(&self, idx: usize) -> &BasicBlock {
		assert!(self.id.is_in_progress());
		&self.bbs[idx]
	}

	/// Get the ID of the `idx`'th basic block.
	pub fn get_bbid(&self, idx: usize) -> BBId {
		assert!(idx < self.bbs.len());
		BBId(self.id.id(), idx)
	}

	/// Get the basic block with the given ID.
	pub fn get_bb(&self, id: BBId) -> &BasicBlock {
		assert!(id.0 == self.id.id());
		&self.bbs[id.1]
	}

	// ---------------------------------------------------------------------------------------------
	// crate

	pub(crate) fn new_inprogress() -> Self {
		Self { id: FuncIdReal::InProgress, name: None, bbs: Vec::new() }
	}

	pub(crate) fn mark_complete(&mut self, id: FuncId) {
		assert!(matches!(self.id, FuncIdReal::InProgress));
		self.id = FuncIdReal::Complete(id);
	}

	pub(crate) fn new_bb(&mut self, loc: Location, term: BBTerm, insts: Vec<Instruction>,
	state: MmuState) -> usize {
		log::trace!("new bb loc: {}, term: {:?}", loc, term);
		let id = self.bbs.len();
		self._push_bb(BasicBlock::new_inprogress(id, loc, term, insts, state));
		id
	}

	pub(crate) fn split_bb(&mut self, idx: usize, inst_idx: usize, new_start: Location) -> usize {
		let new_idx = self.bbs.len();
		let new = self.bbs[idx].split(new_idx, inst_idx, new_start);
		self._push_bb(new)
	}

	// ---------------------------------------------------------------------------------------------
	// private

	fn _push_bb(&mut self, bb: BasicBlock) -> usize {
		self.bbs.push(bb);
		self.bbs.len() - 1
	}
}

// ------------------------------------------------------------------------------------------------
// FuncIndex
// ------------------------------------------------------------------------------------------------

/// An index of all functions in the program. Functions are created, destroyed, and looked up
/// through this object.
#[derive(Default)]
pub struct FuncIndex {
	arena: Arena<Function>,
}

impl FuncIndex {
	pub fn new() -> Self {
		Self { arena: Arena::new() }
	}

	/// Creates a new function and returns its ID.
	pub fn new_func(&mut self, mut func: Function) -> FuncId {
		FuncId(self.arena.insert_with(move |id| {
			func.mark_complete(FuncId(id));
			func
		}))
	}

	/// Gets the function with the given ID.
	pub fn get(&self, id: FuncId) -> &Function {
		self.arena.get(id.0).expect("stale FuncId")
	}

	/// Same as above but mutable.
	pub fn get_mut(&mut self, id: FuncId) -> &mut Function {
		self.arena.get_mut(id.0).expect("stale FuncId")
	}

	/// Iterator over all functions in the index, in arbitrary order.
	pub fn all_funcs(&self) -> impl Iterator<Item = (Index, &Function)> {
		self.arena.iter()
	}
}
