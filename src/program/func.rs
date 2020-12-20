
use std::iter::Chain;
use std::option;
use std::slice;
use std::fmt::{ Debug, Formatter, Result as FmtResult };

use generational_arena::{ Arena, Index };

use crate::memory::{ Location };
use crate::disasm::{ IInstruction };

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
pub struct BasicBlock<TInstruction: IInstruction> {
	id:       BBId,
	loc:      Location,
	term_loc: Location,
	term:     BBTerm,
	insts:    Vec<TInstruction>,
}

impl<T: IInstruction> BasicBlock<T> {
	pub fn new(
		id:       BBId,
		loc:      Location,
		term_loc: Location,
		term:     BBTerm,
		insts:    Vec<T>
	) -> Self {
		Self { id, loc, term_loc, term, insts }
	}

	/// Its globally-unique id.
	pub fn id      (&self) -> BBId     { self.id }
	/// Its globally-unique location.
	pub fn loc     (&self) -> Location { self.loc }
	/// Where its terminator (last instruction) is located.
	pub fn term_loc(&self) -> Location { self.term_loc }
	/// How it ends, and what its successors are.
	pub fn term    (&self) -> &BBTerm  { &self.term }
	/// Its instructions.
	pub fn insts   (&self) -> &[T]     { &self.insts }

	/// An iterator over this block's successors.
	pub fn successors(&self) -> Successors {
		self.term.successors()
	}

	/// An iterator over this block's explicit successors (those written in the terminator).
	pub fn explicit_successors(&self) -> Successors {
		self.term.explicit_successors()
	}

	/// The ID of the function which owns this.
	pub fn func(&self) -> FuncId {
		self.id.func()
	}
}

/// Trait used for defining functions in `Program`.
pub trait IntoBasicBlock<TInstruction: IInstruction> {
	fn into_bb(self, id: BBId) -> BasicBlock<TInstruction>;
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
			DeadEnd | Return | Halt   => None     .into_iter().chain(&[]),
			FallThru(loc) | Jump(loc) => Some(loc).into_iter().chain(&[]),
			Cond { t, f }             => Some(t)  .into_iter().chain(slice::from_ref(f)),
			JumpTbl(locs)             => None     .into_iter().chain(locs),
		}
	}

	/// An iterator over the owning block's *explicit* successors (those which are written
	/// in the terminating instruction).
	pub fn explicit_successors(&self) -> Successors {
		use BBTerm::*;

		match self {
			DeadEnd | Return | Halt | FallThru(..)   => None     .into_iter().chain(&[]),
			Jump(loc)                                => Some(loc).into_iter().chain(&[]),
			Cond { t, .. }                           => Some(t)  .into_iter().chain(&[]),
			JumpTbl(locs)                            => None     .into_iter().chain(locs),
		}
	}
}

// ------------------------------------------------------------------------------------------------
// Function
// ------------------------------------------------------------------------------------------------

/// Newtype which uniquely identifies a `Function`.
#[derive(PartialEq, Eq, Copy, Clone)]
pub struct FuncId(pub Index);

impl Debug for FuncId {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		let (index, generation) = self.0.into_raw_parts();
		write!(f, "FuncId({}, {})", index, generation)
	}
}

/// A single function.
#[derive(Debug)]
pub struct Function<TInstruction: IInstruction> {
	/// Its globally-unique identifier.
	id: FuncId,

	/// Its name, if it was given one. If `None`, an auto-generated name will be used instead.
	name: Option<String>,

	/// All its `BasicBlock`s. The first entry is the head (entry point). There is no implied
	/// ordering of the rest of them.
	bbs: Vec<BasicBlock<TInstruction>>, // [0] is head
}

impl<I: IInstruction> Function<I> {
	pub fn new(id: FuncId) -> Self {
		Self { id, name: None, bbs: Vec::new() }
	}

	/// Add a new `BasicBlock` to this function. Panics if its ID does not match this function's.
	pub fn add_bb(&mut self, bb: BasicBlock<I>) {
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
	pub fn all_bbs(&self) -> impl Iterator<Item = &BasicBlock<I>> {
		self.bbs.iter()
	}

	pub fn get_bb(&self, id: BBId) -> &BasicBlock<I> {
		assert!(id.0 == self.id);
		&self.bbs[id.1]
	}
}

// ------------------------------------------------------------------------------------------------
// FuncIndex
// ------------------------------------------------------------------------------------------------

/// An index of all functions in the program. Functions are created, destroyed, and looked up
/// through this object.
#[derive(Default)]
pub struct FuncIndex<TInstruction: IInstruction> {
	arena: Arena<Function<TInstruction>>,
}

impl<T: IInstruction> FuncIndex<T> {
	pub fn new() -> Self {
		Self { arena: Arena::new() }
	}

	/// Creates a new function and returns its ID.
	pub fn new_func(&mut self) -> FuncId {
		FuncId(self.arena.insert_with(|id| Function::new(FuncId(id))))
	}

	/// Gets the function with the given ID.
	pub fn get(&self, id: FuncId) -> &Function<T> {
		self.arena.get(id.0).expect("stale FuncId")
	}

	/// Same as above but mutable.
	pub fn get_mut(&mut self, id: FuncId) -> &mut Function<T> {
		self.arena.get_mut(id.0).expect("stale FuncId")
	}

	/// Iterator over all functions in the index, in arbitrary order.
	pub fn all_funcs(&self) -> impl Iterator<Item = (Index, &Function<T>)> {
		self.arena.iter()
	}
}
