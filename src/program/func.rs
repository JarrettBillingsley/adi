
use std::iter::Chain;
use std::option;
use std::slice;
use std::fmt::{ Debug, Formatter, Result as FmtResult };

use generational_arena::{ Arena, Index };

use crate::memory::{ Location, MmuState };
use crate::disasm::{ IInstruction };

// ------------------------------------------------------------------------------------------------
// BBId
// ------------------------------------------------------------------------------------------------

/// Uniquely identifies a `BasicBlock`. Consists of a `FuncId` and an index into that function's
/// `bbs` vec.
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
pub struct BasicBlock<TInstruction: IInstruction> {
	id:               BBIdReal,
	pub(crate) loc:   Location,
	pub(crate) term:  BBTerm,
	pub(crate) insts: Vec<TInstruction>,
	state:            MmuState,
}

impl<T: IInstruction> BasicBlock<T> {
	pub fn new(id: BBId, loc: Location, term: BBTerm, insts: Vec<T>, state: MmuState) -> Self {
		assert_ne!(insts.len(), 0);
		Self { id: BBIdReal::Complete(id), loc, term, insts, state }
	}

	pub fn new_inprogress(idx: usize, loc: Location, term: BBTerm, insts: Vec<T>, state: MmuState)
	-> Self {
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
	pub fn insts   (&self) -> &[T]     { &self.insts }
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

	pub fn inst_at_loc(&self, loc: Location) -> Option<&T> {
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
			DeadEnd | Return | Halt | FallThru(..)   => None     .into_iter().chain(&[]),
			Jump(loc)                                => Some(loc).into_iter().chain(&[]),
			Call { dst, .. }                         => Some(dst).into_iter().chain(&[]),
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
pub struct Function<TInstruction: IInstruction> {
	/// Its globally-unique identifier.
	id: FuncIdReal,
	name: Option<String>,

	/// All its `BasicBlock`s. The first entry is the head (entry point). There is no implied
	/// ordering of the rest of them.
	pub(crate) bbs: Vec<BasicBlock<TInstruction>>, // [0] is head
}

impl<I: IInstruction> Function<I> {
	pub fn new_inprogress() -> Self {
		Self { id: FuncIdReal::InProgress, name: None, bbs: Vec::new() }
	}

	pub fn mark_complete(&mut self, id: FuncId) {
		assert!(matches!(self.id, FuncIdReal::InProgress));
		self.id = FuncIdReal::Complete(id);
	}

	pub(crate) fn new_bb(&mut self, loc: Location, term: BBTerm, insts: Vec<I>, state: MmuState)
	-> usize {
		log::trace!("new bb loc: {}, term: {:?}", loc, term);
		let id = self.bbs.len();
		self.push_bb(BasicBlock::new_inprogress(id, loc, term, insts, state));
		id
	}

	pub(crate) fn split_bb(&mut self, idx: usize, inst_idx: usize, new_start: Location) -> usize {
		let new_idx = self.bbs.len();
		let new = self.bbs[idx].split(new_idx, inst_idx, new_start);
		self.push_bb(new)
	}

	fn push_bb(&mut self, bb: BasicBlock<I>) -> usize {
		self.bbs.push(bb);
		self.bbs.len() - 1
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
	pub fn all_bbs(&self) -> impl Iterator<Item = &BasicBlock<I>> {
		self.bbs.iter()
	}

	/// Get the given basic block.
	pub fn get_bb(&self, id: BBId) -> &BasicBlock<I> {
		assert!(id.0 == self.id.id());
		&self.bbs[id.1]
	}

	pub fn get_bb_by_idx(&self, idx: usize) -> &BasicBlock<I> {
		assert!(self.id.is_in_progress());
		&self.bbs[idx]
	}

	/// Its name, if it was given one. If `None`, an auto-generated name will be used instead.
	pub fn name(&self) -> Option<&String> {
		self.name.as_ref()
	}

	/// How many basic blocks this function has.
	pub fn num_bbs(&self) -> usize {
		self.bbs.len()
	}

	/// Get the ID of the `idx`'th basic block.
	pub fn get_bbid(&self, idx: usize) -> BBId {
		assert!(idx < self.bbs.len());
		BBId(self.id.id(), idx)
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
	pub fn new_func(&mut self, mut func: Function<T>) -> FuncId {
		FuncId(self.arena.insert_with(move |id| {
			func.mark_complete(FuncId(id));
			func
		}))
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
