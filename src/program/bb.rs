
use std::iter::Chain;
use std::option;
use std::slice;
use std::fmt::{ Debug, Formatter, Result as FmtResult };

use crate::memory::{ Location, MmuState };
use crate::program::{ Instruction, FuncId };

// ------------------------------------------------------------------------------------------------
// BBId
// ------------------------------------------------------------------------------------------------

/// Uniquely identifies a `BasicBlock`. Consists of a `FuncId` and an index into that function's
/// `bbs` vec.
#[derive(PartialEq, Eq, Copy, Clone)]
pub struct BBId(pub(super) FuncId, pub(super) usize);

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

// ------------------------------------------------------------------------------------------------
// BBIdReal
// ------------------------------------------------------------------------------------------------

#[derive(PartialEq, Eq, Copy, Clone)]
pub enum BBIdReal {
	InProgress(usize),
	Complete(BBId),
}

impl BBIdReal {
	fn bbid(&self) -> BBId {
		use BBIdReal::*;
		match self {
			InProgress(..) => panic!("bbid() called on in-progress BB"),
			Complete(bbid) => *bbid,
		}
	}
}

impl Debug for BBIdReal {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		use BBIdReal::*;
		match self {
			InProgress(idx) => write!(f, "(in progress: {})", idx),
			Complete(bbid)  => write!(f, "{:?}", bbid),
		}
	}
}

// ------------------------------------------------------------------------------------------------
// BasicBlock
// ------------------------------------------------------------------------------------------------

/// A basic block within a function's control flow graph.
#[derive(Debug)]
pub struct BasicBlock {
	id:               BBIdReal,
	pub(crate) loc:   Location,
	pub(crate) term:  BBTerm,
	pub(crate) insts: Vec<Instruction>,
	state:            MmuState,
}

impl BasicBlock {
	pub fn new(id: BBId, loc: Location, term: BBTerm, insts: Vec<Instruction>, state: MmuState)
	-> Self {
		assert_ne!(insts.len(), 0);
		Self { id: BBIdReal::Complete(id), loc, term, insts, state }
	}

	pub fn new_inprogress(idx: usize, loc: Location, term: BBTerm, insts: Vec<Instruction>,
	state: MmuState) -> Self {
		assert_ne!(insts.len(), 0);
		Self { id: BBIdReal::InProgress(idx), loc, term, insts, state }
	}

	pub fn mark_complete(&mut self, func: FuncId) {
		match self.id {
			BBIdReal::InProgress(idx) => self.id = BBIdReal::Complete(BBId(func, idx)),
			_ => panic!(),
		}
	}

	/// Its globally-unique id.
	pub fn id      (&self) -> BBId     { self.id.bbid() }
	/// Its globally-unique location.
	pub fn loc     (&self) -> Location { self.loc }
	/// Where its terminator (last instruction) is located.
	pub fn term_loc(&self) -> Location { self.insts.last().unwrap().loc() }
	/// How it ends, and what its successors are.
	pub fn term    (&self) -> &BBTerm  { &self.term }
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

	/// The ID of the function which owns this.
	pub fn func(&self) -> FuncId {
		self.id().func()
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

	pub(crate) fn split(&mut self, new_idx: usize, inst_idx: usize, new_start: Location) -> Self {
		let term_loc = self.insts[inst_idx].loc();

		assert!(self.loc < new_start);
		assert!(term_loc < new_start);

		let new_insts = self.insts.split_off(inst_idx + 1);

		let mut new = BasicBlock::new_inprogress(
			new_idx,
			new_start,
			BBTerm::FallThru(new_start), // NOT WRONG, they get swapped below.
			new_insts,
			self.mmu_state(),
		);

		std::mem::swap(&mut self.term, &mut new.term);

		log::trace!("split bb loc: {}, term: {:?}", new.loc, new.term);

		new
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
