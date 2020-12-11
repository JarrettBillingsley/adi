
use std::iter::Chain;
use std::option;
use std::slice;
use std::fmt::{ Debug, Formatter, Result as FmtResult };

use derive_new::new;
use generational_arena::{ Arena, Index };

use crate::memory::Location;

// ------------------------------------------------------------------------------------------------
// Function
// ------------------------------------------------------------------------------------------------

/// Newtype which uniquely identifies a `Function`.
#[derive(PartialEq, Eq, Copy, Clone)]
pub struct FuncId(Index);

impl Debug for FuncId {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		let (index, generation) = self.0.into_raw_parts();
		write!(f, "FuncId({}, {})", index, generation)
	}
}

/// A single function.
#[derive(Debug)]
#[derive(new)]
pub struct Function {
	/// Its globally-unique identifier.
	id: FuncId,

	/// Its name, if it was given one. If `None`, an auto-generated name will be used instead.
	#[new(value = "None")]
	name: Option<String>,

	/// All its `BasicBlock`s. The first entry is the head (entry point). There is no implied
	/// ordering of the rest of them.
	#[new(default)]
	bbs: Vec<BasicBlock>, // [0] is head
}

impl Function {
	/// Add a new `BasicBlock` to this function. Panics if its ID does not match this function's.
	pub fn add_bb(&mut self, bb: BasicBlock) {
		assert!(bb.id == self.next_id());
		self.bbs.push(bb);
	}

	/// The next available basic block ID.
	pub fn next_id(&self) -> BBId {
		BBId(self.id, self.bbs.len())
	}

	/// The basic block ID of the function's head.
	pub fn head_id(&self) -> BBId {
		self.bbs[0].id
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
}

// ------------------------------------------------------------------------------------------------
// BasicBlock
// ------------------------------------------------------------------------------------------------

/// Newtype which uniquely identifies a `BasicBlock`. Consists of a `FuncId` and
/// an index into that function's `bbs` vec.
#[derive(PartialEq, Eq, Copy, Clone)]
pub struct BBId(FuncId, usize);

impl BBId {
	/// The ID of the function which owns this.
	pub fn func(&self) -> FuncId {
		self.0
	}
}

impl Debug for BBId {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		write!(f, "BBId({:?}, {})", self.0, self.1)
	}
}

/// A basic block within a function's control flow graph.
#[derive(Debug)]
#[derive(new)]
pub struct BasicBlock {
	/// Its globally-unique id.
	pub id: BBId,

	/// Its globally-unique location.
	pub loc: Location,

	/// Where its terminator (last instruction) is located.
	pub term_loc: Location,

	/// How it ends, and what its successors are.
	pub term: BBTerm,
}

impl BasicBlock {
	/// An iterator over this block's successors.
	pub fn successors(&self) -> Successors {
		self.term.successors()
	}

	/// The ID of the function which owns this.
	pub fn func(&self) -> FuncId {
		self.id.func()
	}
}

/// Trait used for defining functions in `Program`.
pub trait IntoBasicBlock {
	fn into_bb(self, id: BBId) -> BasicBlock;
}

// ------------------------------------------------------------------------------------------------
// BBTerm
// ------------------------------------------------------------------------------------------------

/// The kinds of terminators for a `BasicBlock`.
#[derive(Debug, Clone)]
pub enum BBTerm {
	/// Hit a dead end (e.g. invalid instruction, or user undefined some code).
	DeadEnd,
	/// A halt instruction.
	Halt,
	/// A return instruction.
	Return,
	/// Execution falls through to the next BB.
	FallThru(Location),
	/// Unconditional jump.
	Jump(Location),
	/// Conditional branch within a function.
	Cond { t: Location, f: Location },
	/// Jump table with any number of destinations.
	JumpTbl(Vec<Location>),
}

/// Iterator type of `BBTerm`'s successors. (Thanks for the type, librustc_middle)
pub type Successors<'a> = Chain<option::IntoIter<&'a Location>, slice::Iter<'a, Location>>;

impl BBTerm {
	/// An iterator over the owning block's successors.
	pub fn successors(&self) -> Successors {
		use BBTerm::*;

		match self {
			DeadEnd | Return | Halt => None    .into_iter().chain(&[]),
			FallThru(id) | Jump(id) => Some(id).into_iter().chain(&[]),
			Cond { t, f }           => Some(t) .into_iter().chain(slice::from_ref(f)),
			JumpTbl(ids)            => None    .into_iter().chain(ids),
		}
	}
}

// ------------------------------------------------------------------------------------------------
// FuncIndex
// ------------------------------------------------------------------------------------------------

/// An index of all functions in the program. Functions are created, destroyed, and looked up
/// through this object.
#[derive(Default)]
#[derive(new)]
pub struct FuncIndex {
	#[new(default)]
	arena: Arena<Function>,
}

impl FuncIndex {
	/// Creates a new function and returns its ID.
	pub fn new_func(&mut self) -> FuncId {
		FuncId(self.arena.insert_with(|id| Function::new(FuncId(id))))
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
	pub fn iter(&self) -> impl Iterator<Item = (FuncId, &Function)> {
		self.arena.iter().map(|(id, f)| (FuncId(id), f))
	}

	/// Same as above but mutable.
	pub fn iter_mut(&mut self) -> impl Iterator<Item = (FuncId, &mut Function)> {
		self.arena.iter_mut().map(|(id, f)| (FuncId(id), f))
	}
}