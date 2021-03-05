
use std::fmt::{ Debug, Formatter, Result as FmtResult };

use generational_arena::{ Arena, Index };

use crate::memory::{ Location };
use crate::program::{ BasicBlock, BBId };

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

/// A single function.
#[derive(Debug)]
pub struct Function {
	/// Its globally-unique identifier.
	id: FuncId,
	name: Option<String>,

	/// All its `BasicBlock`s. The first entry is the head (entry point). There is no implied
	/// ordering of the rest of them.
	pub(crate) bbs: Vec<BasicBlock>, // [0] is head
}

impl Function {
	/// Its globally-unique identifier.
	pub fn id(&self) -> FuncId {
		self.id
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

	/// Same as above but mutable.
	pub fn all_bbs_mut(&mut self) -> impl Iterator<Item = &mut BasicBlock> {
		self.bbs.iter_mut()
	}

	/// How many basic blocks this function has.
	pub fn num_bbs(&self) -> usize {
		self.bbs.len()
	}

	/// Get the ID of the `idx`'th basic block.
	pub fn get_bbid(&self, idx: usize) -> BBId {
		assert!(idx < self.bbs.len());
		BBId(self.id, idx)
	}

	/// Get the basic block with the given ID.
	pub fn get_bb(&self, id: BBId) -> &BasicBlock {
		assert!(id.0 == self.id);
		&self.bbs[id.1]
	}

	/// Same as above but mutable.
	pub fn get_bb_mut(&mut self, id: BBId) -> &mut BasicBlock {
		assert!(id.0 == self.id);
		&mut self.bbs[id.1]
	}

	// ---------------------------------------------------------------------------------------------
	// crate

	pub(crate) fn new(id: FuncId, bbs: Vec<BasicBlock>) -> Self {
		Self { id, name: None, bbs }
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
	pub fn new_func(&mut self, bbs: Vec<BasicBlock>) -> FuncId {
		FuncId(self.arena.insert_with(move |id| { Function::new(FuncId(id), bbs) }))
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
