
use std::marker::PhantomData;

use parse_display::Display;

use crate::memory::{ MmuState, Location, VA };

// ------------------------------------------------------------------------------------------------
// Sub-modules
// ------------------------------------------------------------------------------------------------

pub mod error;

pub use error::*;

// ------------------------------------------------------------------------------------------------
// MemAccess
// ------------------------------------------------------------------------------------------------

/// How a memory operand is accessed by an instruction.
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum MemAccess {
	/// A read (load).
	Read,
	/// A write (store).
	Write,
	/// Read-modify-write (both read and write).
	Rmw,
	/// Used as an immediate (e.g. put into a register, or used as the base address in an EA).
	Offset,
	/// Used as the target of a jump or branch.
	Target,
}

// ------------------------------------------------------------------------------------------------
// IOperand
// ------------------------------------------------------------------------------------------------

/// Trait for instruction operands.
pub trait IOperand {
	/// Does this refer to a register?
	fn is_reg(&self) -> bool;

	/// Is this an immediate value (but NOT a memory address)?
	fn is_imm(&self) -> bool;

	/// How, if any way, does this operand access memory?
	fn access(&self) -> Option<MemAccess>;

	/// If `access` is Some (`is_mem` returns true), get the address it refers to; panics otherwise.
	fn addr(&self) -> VA;

	/// If this is an immediate value, get it as an unsigned number; panics otherwise.
	fn uimm(&self) -> u64;

	/// If this is an immediate value, get it as a signed number; panics otherwise.
	fn simm(&self) -> i64;

	// --------------------------------------------------------------------------------------------
	// Provided methods

	/// Does this operand access memory?
	fn is_mem(&self) -> bool {
		self.access().is_some()
	}
}

// ------------------------------------------------------------------------------------------------
// IInstruction
// ------------------------------------------------------------------------------------------------

#[derive(Debug, Display, PartialEq, Eq, Copy, Clone)]
/// What rough class of instruction one is.
pub enum InstructionKind {
	/// Invalid instruction.
	Invalid,
	/// Something else.
	Other,
	/// Control flow - function call.
	Call,
	/// Control flow - function return.
	Ret,
	/// Control flow - conditional jump/branch.
	Cond,
	/// Control flow - unconditional jump/branch.
	Uncond,
	/// Control flow - indirect jump/branch.
	Indir,
	/// Control flow - halt/stop.
	Halt,
}

impl InstructionKind {
	fn is_control(&self) -> bool {
		use InstructionKind::*;
		match self {
			Call | Ret | Cond | Uncond | Indir | Halt  => true,
			Invalid | Other => false,
		}
	}
}

/// Trait for instructions. Used by analysis and such.
pub trait IInstruction {
	/// Get Location.
	fn loc(&self) -> Location;
	/// Get virtual address.
	fn va(&self) -> VA;
	/// Get size, in bytes.
	fn size(&self) -> usize;
	/// How many operands it has.
	fn num_ops(&self) -> usize;
	/// Accessor for operands.
	fn get_op(&self, i: usize) -> &dyn IOperand;
	/// Accessor for original bytes.
	fn bytes(&self) -> &[u8];
	/// Get kind of instruction.
	fn kind(&self) -> InstructionKind;
	/// If this is a control instruction, the target address of that control, if it has one.
	fn control_target(&self) -> Option<VA>;
	/// Get the state of the MMU at this instruction.
	fn mmu_state(&self) -> MmuState;

	// --------------------------------------------------------------------------------------------
	// Provided methods

	/// Get the Location of the instruction after this one.
	fn next_loc(&self) -> Location {
		self.loc() + self.size()
	}

	/// Get the virtual address of the instruction after this one.
	fn next_addr(&self) -> VA {
		self.va() + self.size()
	}

	/// Is this a control flow instruction?
	fn is_control(&self) -> bool { self.kind().is_control() }

	/// Is this an invalid instruction?
	fn is_invalid(&self) -> bool { matches!(self.kind(), InstructionKind::Invalid) }

	/// Is this some other kind of instruction?
	fn is_other  (&self) -> bool { matches!(self.kind(), InstructionKind::Other) }

	/// Is this a function call?
	fn is_call   (&self) -> bool { matches!(self.kind(), InstructionKind::Call) }

	/// Is this a function return ?
	fn is_ret    (&self) -> bool { matches!(self.kind(), InstructionKind::Ret) }

	/// Is this a conditional jump/branch?
	fn is_cond   (&self) -> bool { matches!(self.kind(), InstructionKind::Cond) }

	/// Is this an unconditional jump/branch?
	fn is_uncond (&self) -> bool { matches!(self.kind(), InstructionKind::Uncond) }

	/// Is this an indirect jump/branch (i.e. through a register)?
	fn is_indir  (&self) -> bool { matches!(self.kind(), InstructionKind::Indir) }

	/// Is this some kind of halt instruction from which there is no recovery?
	fn is_halt   (&self) -> bool { matches!(self.kind(), InstructionKind::Halt) }
}

// ------------------------------------------------------------------------------------------------
// IDisassembler
// ------------------------------------------------------------------------------------------------

/// Trait for disassemblers.
pub trait IDisassembler : Sized {
	/// Associated type of instructions given by this disassembler.
	type TInstruction: IInstruction;

	/// Disassemble a single instruction from `img` with the given VA and Location.
	/// Returns the disassembled instruction and the new MMU state (which will be in effect
	/// on the *next* instruction).
	fn disas_instr(&self, img: &[u8], state: MmuState, va: VA, loc: Location)
	-> DisasResult<(Self::TInstruction, MmuState)>;

	// --------------------------------------------------------------------------------------------
	// Provided methods

	/// Find the last instruction in `img`. Returns `None` if `img` is empty.
	fn find_last_instr(&self, img: &[u8], state: MmuState, va: VA, loc: Location)
	-> DisasResult<(Self::TInstruction, MmuState)> {
		let mut iter = self.disas_all(img, state, va, loc);
		let last = (&mut iter).last();

		if let Some(err) = iter.err() {
			Err(err)
		} else {
			Ok(last.unwrap())
		}
	}

	/// Iterator over all instructions in a slice, where the first one has the given VA.
	fn disas_all<'dis, 'img>(&'dis self, img: &'img [u8], state: MmuState, va: VA, loc: Location)
	-> DisasAll<'dis, 'img, Self> {
		DisasAll::new(self, img, state, va, loc)
	}
}

/// Iterator type. Also lets you find out *why* iteration stopped, like:
///
/// ```ignore
/// let mut iter = dis.disas_all(image, va);
/// for inst in &mut iter {
///     // blah blah
/// }
///
/// if let Some(err) = iter.err() {
///     // do stuff with err and iter.err_offs()
/// }
/// ```
pub struct DisasAll<'dis, 'img, D: IDisassembler> {
	disas: &'dis D,
	img:   &'img [u8],
	state: MmuState,
	va:    VA,
	loc:   Location,
	offs:  usize,
	err:   Option<DisasError>,
	_inst: PhantomData<<D as IDisassembler>::TInstruction>,
}

impl<'dis, 'img, D: IDisassembler> DisasAll<'dis, 'img, D> {
	fn new(disas: &'dis D, img: &'img [u8], state: MmuState, va: VA, loc: Location) -> Self {
		Self { disas, img, state, va, loc, offs: 0, err: None, _inst: PhantomData }
	}

	/// If iteration stopped because of an error, returns that error.
	pub fn err(&self) -> Option<DisasError> {
		self.err
	}

	/// Whether or not iteration stopped because of an error.
	pub fn has_err(&self) -> bool {
		self.err().is_some()
	}

	/// The offset into the slice where an error occurred, if any.
	pub fn err_offset(&self) -> usize {
		self.offs
	}

	/// The VA where an error occurred, if any.
	pub fn err_va(&self) -> VA {
		self.va
	}

	/// The Location where an error occurred, if any.
	pub fn err_loc(&self) -> Location {
		self.loc
	}
}

impl<'dis, 'img, D: IDisassembler> Iterator for DisasAll<'dis, 'img, D> {
	type Item = (<D as IDisassembler>::TInstruction, MmuState);

	fn next(&mut self) -> Option<Self::Item> {
		if self.offs == self.img.len() {
			// don't want to produce an error when successfully disassembling all instructions
			None
		} else {
			match self.disas.disas_instr(&self.img[self.offs ..], self.state, self.va, self.loc) {
				Ok((inst, new_state)) => {
					let size = inst.size();
					self.va += size;
					self.loc += size;
					self.offs += size;
					self.state = new_state;
					Some((inst, new_state))
				}

				Err(e) => {
					self.err = Some(e);
					None
				}
			}
		}
	}
}

// ------------------------------------------------------------------------------------------------
// IPrinter
// ------------------------------------------------------------------------------------------------

/// Trait for instruction printers.
pub trait IPrinter {
	/// Associated type of instructions that this printer prints.
	type TInstruction: IInstruction;

	/// Give a string representation of an instruction's mnemonic.
	fn fmt_mnemonic(&self, i: &Self::TInstruction) -> String;

	/// Give a string representation of an instruction's operands.
	fn fmt_operands(&self, i: &Self::TInstruction, l: &dyn INameLookup) -> String;

	// --------------------------------------------------------------------------------------------
	// Provided methods

	/// Give a string representation of an instruction.
	fn fmt_instr(&self, i: &Self::TInstruction, l: &dyn INameLookup) -> String {
		format!("{} {}", self.fmt_mnemonic(i), self.fmt_operands(i, l))
	}
}

/// Trait to abstract the process of looking up names of addresses.
pub trait INameLookup {
	fn lookup(&self, state: MmuState, addr: VA) -> Option<String>;
}

/// A dummy struct that implements `INameLookup` whose `lookup` method always returns `None`.
pub struct NullLookup;

impl INameLookup for NullLookup {
	fn lookup(&self, _state: MmuState, _addr: VA) -> Option<String> {
		None
	}
}