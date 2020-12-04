
use std::iter::Chain;
use std::option;
use std::slice;

use generational_arena::{ Arena, Index };

use crate::memory::{
	types::Location,
	spans::SpanOwner,
};

// ------------------------------------------------------------------------------------------------
// Function
// ------------------------------------------------------------------------------------------------

/// Newtype which uniquely identifies a `Function`.
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub struct FuncId(Index);

/// A single function.
#[derive(Debug)]
pub struct Function {
	/// Its globally-unique identifier.
	id: FuncId,

	/// Its name, if it was given one. If `None`, an auto-generated name will be used instead.
	name: Option<String>,

	/// All its `BasicBlock`s. The first entry is the head (entry point).
	bbs: Vec<BasicBlock>, // [0] is head
}

impl Function {
	fn new(id: FuncId) -> Self {
		Self { id, name: None, bbs: Vec::new() }
	}
}

// ------------------------------------------------------------------------------------------------
// BasicBlock
// ------------------------------------------------------------------------------------------------

/// Newtype which uniquely identifies a `BasicBlock`. Consists of a `FuncId` and
/// an index into that function's `bbs` vec.
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub struct BBId(FuncId, usize);

/// A basic block within a function's control flow graph.
#[derive(Debug)]
pub struct BasicBlock {
	/// Its globally-unique id.
	id: BBId,

	/// Its globally-unique location.
	loc: Location,

	/// How it ends, and what its successors are.
	term: BBTerm,
}

/// A `BasicBlock` owns a span.
impl SpanOwner for BasicBlock {}

impl BasicBlock {
	pub fn successors(&self) -> Successors {
		self.term.successors()
	}
}

// ------------------------------------------------------------------------------------------------
// BBTerm
// ------------------------------------------------------------------------------------------------

/// The kinds of terminators for a `BasicBlock`.
#[derive(Debug, Clone)]
pub enum BBTerm {
	/// Hit a dead end (e.g. invalid instruction).
	DeadEnd,
	/// A return instruction.
	Return,
	/// Execution falls through to the next BB.
	FallThru(BBId),
	/// Unconditional jump.
	Jump(BBId),
	/// Conditional branch within a function.
	Cond { t: BBId, f: BBId },
	/// Jump table with any number of destinations.
	JumpTbl(Vec<BBId>),
}

/// Iterator type of `BBTerm`'s successors.
pub type Successors<'a> = Chain<option::IntoIter<&'a BBId>, slice::Iter<'a, BBId>>;

impl BBTerm {
	/// Iterator of successors of this BB.
	pub fn successors(&self) -> Successors {
		use BBTerm::*;

		match self {
			DeadEnd | Return        => None    .into_iter().chain(&[]),
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
pub struct FuncIndex {
	arena: Arena<Function>,
}

impl FuncIndex {
	/// Ctor
	pub fn new() -> Self {
		Self { arena: Arena::new() }
	}

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
}