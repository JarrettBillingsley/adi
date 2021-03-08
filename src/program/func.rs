
use std::fmt::{ Debug, Formatter, Result as FmtResult };

use bitflags::bitflags;
use generational_arena::{ Arena, Index };
use smallvec::{ SmallVec };

use crate::memory::{ Location };
use crate::program::{ BBId };

// ------------------------------------------------------------------------------------------------
// FuncId
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

// ------------------------------------------------------------------------------------------------
// FuncAttrs
// ------------------------------------------------------------------------------------------------

bitflags! {
	/// Function attributes. Bits of semantic fluff that attach to functions and let us do more
	/// advanced analysis.
	pub struct FuncAttrs: u32 {
		const NONE =        0;
		const BANK_SWITCH = 1;
		const JUMP_TABLE =  2;
		const NO_RETURN =   4;
	}
}

// ------------------------------------------------------------------------------------------------
// Function
// ------------------------------------------------------------------------------------------------

/// A single function.
#[derive(Debug)]
pub struct Function {
	/// Its globally-unique identifier.
	id:    FuncId,
	/// Where its head BB begins.
	loc:   Location,
	/// Its user-given name, if any.
	name:  Option<String>,
	/// Attributes.
	attrs: FuncAttrs,
	/// The IDs of its `BasicBlock`s. The first entry is the head (entry point). The rest have no
	/// defined order.
	pub(crate) bbs: Vec<BBId>,
	/// The IDs of the `BasicBlock`s which are entry points into this function.
	pub(crate) entrypoints: SmallVec<[BBId; 2]>,
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
		self.bbs[0]
	}

	/// Where the function starts.
	pub fn loc(&self) -> Location {
		self.loc
	}

	/// Iterator over this function's basic blocks, starting with the head, but then
	/// in arbitrary order.
	pub fn all_bbs(&self) -> impl Iterator<Item = BBId> + '_ {
		self.bbs.iter().copied()
	}

	/// How many basic blocks this function has.
	pub fn num_bbs(&self) -> usize {
		self.bbs.len()
	}

	/// Get the attributes for this function.
	pub fn attrs(&self) -> &FuncAttrs {
		&self.attrs
	}

	/// Is this function used almost like an instruction to switch banks?
	pub fn is_bank_switch(&self) -> bool {
		self.attrs.contains(FuncAttrs::BANK_SWITCH)
	}

	/// Is this function used almost like an instruction to implement jump tables?
	pub fn is_jump_table(&self) -> bool {
		self.attrs.contains(FuncAttrs::JUMP_TABLE)
	}

	/// Does this function never return? Covers both infinite loop functions and those which
	/// *do* return, but *not to the caller* (like some jump table functions).
	pub fn is_no_return(&self) -> bool {
		self.attrs.contains(FuncAttrs::NO_RETURN)
	}

	// ---------------------------------------------------------------------------------------------
	// crate

	/// Ctor. The name defaults to `None` and the attributes default to `FuncAttrs::NONE`.
	pub(crate) fn new(id: FuncId, loc: Location, bbs: Vec<BBId>) -> Self {
		let entrypoints = SmallVec::from_slice(&bbs[..1]);

		Self {
			id,
			loc,
			name: None,
			attrs: FuncAttrs::NONE,
			bbs,
			entrypoints
		}
	}

	/// Mutable version of [`attrs`].
	pub(crate) fn attrs_mut(&mut self) -> &mut FuncAttrs {
		&mut self.attrs
	}

	/// Adds another entry point. Must exist in `bbs` and must not exist in `entrypoints`.
	pub(crate) fn add_entrypoint(&mut self, bbid: BBId) {
		assert!(self.bbs.contains(&bbid));
		assert!(!self.entrypoints.contains(&bbid));
		self.entrypoints.push(bbid);
	}

	/// Tries to add another entry point. Must exist in `bbs`. If it is added successfully,
	/// returns true; if it already exists in the `entrypoints`, returns false.
	pub(crate) fn try_add_entrypoint(&mut self, bbid: BBId) -> bool {
		assert!(self.bbs.contains(&bbid));

		if !self.entrypoints.contains(&bbid) {
			self.entrypoints.push(bbid);
			true
		} else {
			false
		}
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
	pub fn new_func(&mut self, loc: Location, bbs: Vec<BBId>) -> FuncId {
		FuncId(self.arena.insert_with(move |id| { Function::new(FuncId(id), loc, bbs) }))
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
