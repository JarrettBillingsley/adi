
use std::iter::Chain;
use std::option;
use std::slice;
use std::fmt::{ Debug, Formatter, Result as FmtResult };

use generational_arena::{ Arena, Index };

use crate::memory::{ Location, MmuState };
use crate::program::{ Instruction, FuncId };

// ------------------------------------------------------------------------------------------------
// BBId
// ------------------------------------------------------------------------------------------------

/// Uniquely identifies a [`BasicBlock`].
#[derive(PartialEq, Eq, Copy, Clone, Hash)]
pub struct BBId(pub Index);

impl Debug for BBId {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		let (index, generation) = self.0.into_raw_parts();
		write!(f, "BBId({}, {})", index, generation)
	}
}

// ------------------------------------------------------------------------------------------------
// BasicBlock
// ------------------------------------------------------------------------------------------------

/// A basic block within a function's control flow graph.
#[derive(Debug)]
pub struct BasicBlock {
	id:               BBId,
	func:             Option<FuncId>,
	pub(crate) loc:   Location,
	pub(crate) term:  BBTerm,
	pub(crate) insts: Vec<Instruction>,
	state:            MmuState,
}

impl BasicBlock {
	fn new(id: Index, loc: Location, term: BBTerm, insts: Vec<Instruction>, state: MmuState)
	-> Self {
		assert_ne!(insts.len(), 0);
		Self { id: BBId(id), func: None, loc, term, insts, state }
	}

	/// Its globally-unique id.
	pub fn id      (&self) -> BBId     { self.id }
	/// Its globally-unique location.
	pub fn loc     (&self) -> Location { self.loc }
	/// Where its terminator (last instruction) is located.
	pub fn term_loc(&self) -> Location { self.term_inst().loc() }
	/// How it ends, and what its successors are.
	pub fn term    (&self) -> &BBTerm  { &self.term }
	/// The terminating instruction.
	pub fn term_inst(&self) -> &Instruction { &self.insts.last().unwrap() }
	/// Its instructions.
	pub fn insts   (&self) -> &[Instruction] { &self.insts }
	/// The MMU state at the beginning of this BB.
	pub fn mmu_state(&self) -> MmuState { self.state }

	/// An iterator over this block's successors.
	pub fn successors(&self) -> Successors {
		self.term.successors()
	}

	/// An iterator over this block's explicit successors (those written in the terminator).
	pub fn explicit_successors(&self) -> Successors {
		self.term.explicit_successors()
	}

	/// The ID of the function which owns this. Panics if it has no owner.
	pub fn func(&self) -> FuncId {
		self.func.unwrap()
	}

	pub fn inst_at_loc(&self, loc: Location) -> Option<&Instruction> {
		self.insts.iter().find(|&inst| inst.loc() == loc)
	}

	pub(crate) fn last_instr_before(&self, loc: Location) -> Option<usize> {
		for (i, inst) in self.insts.iter().enumerate() {
			if inst.loc() < loc {
				let next = inst.next_loc();

				use std::cmp::Ordering::*;

				match next.cmp(&loc) {
					// uh oh. loc is in the middle of this instruction.
					Greater => return None,
					Equal   => return Some(i),
					Less    => {}
				}
			}
		}

		unreachable!("should never be able to get here")
	}

	pub(crate) fn set_mmu_state(&mut self, new_state: MmuState) {
		self.state = new_state;
	}

	pub(crate) fn mark_complete(&mut self, func: FuncId) {
		assert!(self.func.is_none());
		self.func = Some(func);
	}

	pub(crate) fn change_func(&mut self, new_func: FuncId) {
		assert!(self.func.is_some());
		self.func = Some(new_func);
	}
}


// ------------------------------------------------------------------------------------------------
// BBIndex
// ------------------------------------------------------------------------------------------------

/// An index of all basic blocks in the program.
#[derive(Default)]
pub struct BBIndex {
	arena: Arena<BasicBlock>,
}

impl BBIndex {
	pub fn new() -> Self {
		Self { arena: Arena::new() }
	}

	/// Creates a new BB and returns its ID.
	pub fn new_bb(&mut self, loc: Location, term: BBTerm, insts: Vec<Instruction>, state: MmuState)
	-> BBId {
		BBId(self.arena.insert_with(move |id| {
			BasicBlock::new(id, loc, term, insts, state)
		}))
	}

	/// Gets the BB with the given ID.
	pub fn get(&self, id: BBId) -> &BasicBlock {
		self.arena.get(id.0).expect("stale BBId")
	}

	/// Same as above but mutable.
	pub fn get_mut(&mut self, id: BBId) -> &mut BasicBlock {
		self.arena.get_mut(id.0).expect("stale BBId")
	}

	/// Same as above but gets *two* mutable BBs.
	pub fn get2_mut(&mut self, id1: BBId, id2: BBId) -> (&mut BasicBlock, &mut BasicBlock) {
		let (bb1, bb2) = self.arena.get2_mut(id1.0, id2.0);
		(bb1.expect("stale BBId 1"), bb2.expect("stale BBId 2"))
	}

	/// Iterator over all BBs in the index, in arbitrary order.
	pub fn all_bbs(&self) -> impl Iterator<Item = (Index, &BasicBlock)> {
		self.arena.iter()
	}
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
	/// Function call (dst = function called, ret = return location).
	Call { dst: Location, ret: Location },
	/// Conditional branch within a function.
	Cond { t: Location, f: Location },
	/// Jump table with any number of destinations.
	JumpTbl(Vec<Location>),
	/// Like FallThru, but perform a bank change as well.
	BankChange(Location),
}

/// Iterator type of `BBTerm`'s successors. (Thanks for the type, librustc_middle)
pub type Successors<'a> = Chain<option::IntoIter<&'a Location>, slice::Iter<'a, Location>>;

impl BBTerm {
	/// An iterator over the owning block's successors.
	pub fn successors(&self) -> Successors {
		use BBTerm::*;

		match self {
			DeadEnd | Return | Halt   => None     .into_iter().chain(&[]),
			FallThru(loc) | Jump(loc) |
			BankChange(loc)           => Some(loc).into_iter().chain(&[]),
			Call { dst, ret }         => Some(dst).into_iter().chain(slice::from_ref(ret)),
			Cond { t, f }             => Some(t)  .into_iter().chain(slice::from_ref(f)),
			JumpTbl(locs)             => None     .into_iter().chain(locs),
		}
	}

	/// An iterator over the owning block's *explicit* successors (those which are written
	/// in the terminating instruction).
	pub fn explicit_successors(&self) -> Successors {
		use BBTerm::*;

		match self {
			DeadEnd | Return | Halt | FallThru(..) |
			BankChange(..)                           => None     .into_iter().chain(&[]),
			Jump(loc)                                => Some(loc).into_iter().chain(&[]),
			Call { dst, .. }                         => Some(dst).into_iter().chain(&[]),
			Cond { t, .. }                           => Some(t)  .into_iter().chain(&[]),
			JumpTbl(locs)                            => None     .into_iter().chain(locs),
		}
	}
}
