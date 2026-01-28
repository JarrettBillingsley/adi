
use std::iter::Chain;
use std::option;
use std::slice;
use std::fmt::{ Debug, Formatter, Result as FmtResult };

use generational_arena::{ Arena, Index };

use crate::memory::{ EA, MmuState };
use crate::program::{ Instruction, FuncId };

// ------------------------------------------------------------------------------------------------
// BBId
// ------------------------------------------------------------------------------------------------

/// Uniquely identifies a [`BasicBlock`].
#[derive(PartialEq, Eq, PartialOrd, Ord, Copy, Clone, Hash)]
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
	pub(crate) ea:    EA,
	pub(crate) term:  BBTerm,
	pub(crate) insts: Vec<Instruction>,
	state:            MmuState,
}

impl BasicBlock {
	fn new(id: Index, ea: EA, term: BBTerm, insts: Vec<Instruction>, state: MmuState)
	-> Self {
		assert_ne!(insts.len(), 0);
		Self { id: BBId(id), func: None, ea, term, insts, state }
	}

	/// Its globally-unique id.
	pub fn id      (&self) -> BBId     { self.id }
	/// Its globally-unique EA.
	pub fn ea     (&self) -> EA { self.ea }
	/// Where its terminator (last instruction) is located.
	pub fn term_ea(&self) -> EA { self.term_inst().ea() }
	/// How it ends, and what its successors are.
	pub fn term    (&self) -> &BBTerm  { &self.term }
	/// Same as above, but mutable.
	pub fn term_mut(&mut self) -> &mut BBTerm { &mut self.term }
	/// The terminating instruction.
	pub fn term_inst(&self) -> &Instruction { &self.insts.last().unwrap() }
	/// Its instructions.
	pub fn insts   (&self) -> &[Instruction] { &self.insts }
	/// The MMU state at the beginning of this BB.
	pub fn mmu_state(&self) -> MmuState { self.state }

	/// An iterator over this block's successors.
	pub fn successors(&'_ self) -> Successors<'_> {
		self.term.successors()
	}

	/// An iterator over this block's explicit successors (those written in the terminator).
	pub fn explicit_successors(&'_ self) -> Successors<'_> {
		self.term.explicit_successors()
	}

	/// The ID of the function which owns this. Panics if it has no owner.
	pub fn func(&self) -> FuncId {
		self.func.unwrap()
	}

	/// Given an EA within this BB, find the instruction at that EA, or `None` if it doesn't exist.
	/// WARNING: this is linear time.
	pub fn inst_at_ea(&self, ea: EA) -> Option<&Instruction> {
		self.insts.iter().find(|&inst| inst.ea() == ea)
	}

	/// If this BB ends in a jump, call, or conditional branch, returns the EA that it
	/// jumps/calls/branches to; else, returns `None`.
	pub fn control_target(&self) -> Option<EA> {
		match self.term() {
			BBTerm::DeadEnd
			| BBTerm::Halt
			| BBTerm::Return
			| BBTerm::FallThru(..)
			| BBTerm::BankChange(..) => None,

			BBTerm::Jump(dst)
			| BBTerm::Call { dst, .. }
			| BBTerm::Cond { t: dst, .. } => Some(*dst),

			// TODO: how would these even be implemented?
			BBTerm::JumpTbl(_targets)        => None, // unimplemented!(),
			BBTerm::IndirCall { dst: _, .. } => None, // unimplemented!(),
		}
	}

	pub(crate) fn last_instr_before(&self, ea: EA) -> Option<usize> {
		for (i, inst) in self.insts.iter().enumerate() {
			if inst.ea() < ea {
				let next = inst.next_ea();

				use std::cmp::Ordering::*;

				match next.cmp(&ea) {
					// uh oh. ea is in the middle of this instruction.
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
	pub fn new_bb(&mut self, ea: EA, term: BBTerm, insts: Vec<Instruction>, state: MmuState)
	-> BBId {
		BBId(self.arena.insert_with(move |id| {
			BasicBlock::new(id, ea, term, insts, state)
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
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BBTerm {
	/// Hit a dead end (e.g. invalid instruction, or user undefined some code).
	DeadEnd,
	/// A halt instruction.
	Halt,
	/// A return instruction.
	Return,
	/// Execution falls through to the next BB.
	FallThru(EA),
	/// Unconditional jump.
	Jump(EA),
	/// Function call (dst = function called, ret = return location).
	Call { dst: EA, ret: EA },
	/// Indirect function call (dst = any number of destinations, empty for unknown;
	/// ret = return location).
	IndirCall { dst: Vec<EA>, ret: EA },
	/// Conditional branch within a function.
	Cond { t: EA, f: EA },
	/// Jump table with any number of destinations.
	JumpTbl(Vec<EA>),
	/// Like FallThru, but perform a bank change as well.
	BankChange(EA),
}

/// Iterator type of `BBTerm`'s successors. (Thanks for the type, librustc_middle)
pub type Successors<'a> = Chain<option::IntoIter<&'a EA>, slice::Iter<'a, EA>>;

/// Same as above but mutable.
pub type SuccessorsMut<'a> = Chain<option::IntoIter<&'a mut EA>, slice::IterMut<'a, EA>>;

impl BBTerm {
	/// An iterator over the owning block's successors.
	pub fn successors(&'_ self) -> Successors<'_> {
		use BBTerm::*;

		match self {
			DeadEnd | Return | Halt => None     .into_iter().chain(&[]),
			FallThru(ea) | Jump(ea) |
			BankChange(ea)          => Some(ea) .into_iter().chain(&[]),
			Call { dst, ret }       => Some(dst).into_iter().chain(slice::from_ref(ret)),
			Cond { t, f }           => Some(t)  .into_iter().chain(slice::from_ref(f)),
			JumpTbl(eas)            => None     .into_iter().chain(eas),
			IndirCall { dst, ret }  => Some(ret).into_iter().chain(dst),
		}
	}

	/// Same as above, but mutable.
	pub fn successors_mut(&'_ mut self) -> SuccessorsMut<'_> {
		use BBTerm::*;

		match self {
			DeadEnd | Return | Halt => None     .into_iter().chain([].iter_mut()),
			FallThru(ea) | Jump(ea) |
			BankChange(ea)          => Some(ea) .into_iter().chain([].iter_mut()),
			Call { dst, ret }       => Some(dst).into_iter().chain(slice::from_mut(ret).iter_mut()),
			Cond { t, f }           => Some(t)  .into_iter().chain(slice::from_mut(f).iter_mut()),
			JumpTbl(eas)            => None     .into_iter().chain(eas.iter_mut()),
			IndirCall { dst, ret }  => Some(ret).into_iter().chain(dst),
		}
	}

	/// An iterator over the owning block's *explicit* successors (those which are written
	/// in the terminating instruction).
	pub fn explicit_successors(&'_ self) -> Successors<'_> {
		use BBTerm::*;

		match self {
			DeadEnd | Return | Halt | FallThru(..) |
			BankChange(..) | IndirCall { .. }        => None     .into_iter().chain(&[]),
			Jump(ea)                                 => Some(ea) .into_iter().chain(&[]),
			Call { dst, .. }                         => Some(dst).into_iter().chain(&[]),
			Cond { t, .. }                           => Some(t)  .into_iter().chain(&[]),
			JumpTbl(eas)                             => None     .into_iter().chain(eas),
		}
	}
}
