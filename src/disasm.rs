
use std::marker::PhantomData;

use crate::memory::VA;

// ------------------------------------------------------------------------------------------------
// Sub-modules
// ------------------------------------------------------------------------------------------------

mod arch;
mod error;
mod platform;

pub use arch::*;
pub use error::*;
pub use platform::*;

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
// OperandTrait
// ------------------------------------------------------------------------------------------------

/// Trait for instruction operands.
pub trait OperandTrait {
	/// Does this refer to a register?
	fn is_reg(&self) -> bool;

	/// Is this an immediate value (but NOT a memory address)?
	fn is_imm(&self) -> bool;

	/// Does this operand access memory?
	fn is_mem(&self) -> bool {
		self.access().is_some()
	}

	/// How, if any way, does this operand access memory?
	fn access(&self) -> Option<MemAccess>;
}

// ------------------------------------------------------------------------------------------------
// InstructionTrait
// ------------------------------------------------------------------------------------------------

/// Trait for instructions. Used by analysis and such.
pub trait InstructionTrait {
	/// Associated type of operands returned by `get_op`.
	type TOperand: OperandTrait;

	/// Get virtual address.
	fn va(&self) -> VA;
	/// Get size, in bytes.
	fn size(&self) -> usize;
	/// How many operands it has.
	fn num_ops(&self) -> usize;
	/// Accessor for operands.
	fn get_op(&self, i: usize) -> Self::TOperand;
	/// Accessor for original bytes.
	fn bytes(&self) -> &[u8];

	/// Is this a control flow instruction?
	fn is_control    (&self) -> bool;
	/// Is this conditional or unconditional?
	fn is_conditional(&self) -> bool;
	/// Is this an absolute jump?
	fn is_jump       (&self) -> bool;
	/// Is this an indirect jump (i.e. through a register)?
	fn is_indir_jump (&self) -> bool;
	/// Is this a function call?
	fn is_call       (&self) -> bool;
	/// Is this a function return?
	fn is_return     (&self) -> bool;
	/// Is this an invalid instruction?
	fn is_invalid    (&self) -> bool;
	/// Is this some kind of halt instruction from which there is no recovery?
	fn is_halt       (&self) -> bool;
}

// ------------------------------------------------------------------------------------------------
// DisassemblerTrait
// ------------------------------------------------------------------------------------------------

/// Trait for disassemblers.
pub trait DisassemblerTrait {
	/// Associated type of instructions given by this disassembler.
	type TInstruction: InstructionTrait;

	/// Disassemble a single instruction from `img[offs..]` with the given VA.
	fn disas_instr(&self, img: &[u8], va: VA) -> DisasResult<Self::TInstruction>;

	/// Iterator over all instructions in a slice, where the first one has the given VA.
	fn disas_all<'dis, 'img>(&'dis self, img: &'img [u8], va: VA)
		-> DisasAll<'dis, 'img, Self>
	where Self: Sized {
		DisasAll::new(self, img, va)
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
pub struct DisasAll<'dis, 'img, D: DisassemblerTrait> {
	disas: &'dis D,
	img:   &'img [u8],
	va:    VA,
	offs:  usize,
	err:   Option<DisasError>,
	_inst: PhantomData<<D as DisassemblerTrait>::TInstruction>,
}

impl<'dis, 'img, D: DisassemblerTrait> DisasAll<'dis, 'img, D> {
	fn new(disas: &'dis D, img: &'img [u8], va: VA) -> Self {
		Self { disas, img, va, offs: 0, err: None, _inst: PhantomData }
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
}

impl<'dis, 'img, D: DisassemblerTrait> Iterator for DisasAll<'dis, 'img, D> {
	type Item = <D as DisassemblerTrait>::TInstruction;

	fn next(&mut self) -> Option<Self::Item> {
		if self.offs == self.img.len() {
			// don't want to produce an error when successfully disassembling all instructions
			None
		} else {
			match self.disas.disas_instr(&self.img[self.offs ..], self.va) {
				Ok(inst) => {
					let size = inst.size();
					self.va += size;
					self.offs += size;
					Some(inst)
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
// PrinterTrait
// ------------------------------------------------------------------------------------------------

/// Trait for instruction printers.
pub trait PrinterTrait {
	/// Associated type of instructions that this printer prints.
	type TInstruction: InstructionTrait;

	/// Give a string representation of an instruction's mnemonic.
	fn fmt_mnemonic(&self, i: &Self::TInstruction) -> String;

	/// Give a string representation of an instruction's operands.
	fn fmt_operands(&self, i: &Self::TInstruction, l: &dyn NameLookupTrait) -> String;

	/// Give a string representation of an instruction.
	fn fmt_instr(&self, i: &Self::TInstruction, l: &dyn NameLookupTrait) -> String {
		format!("{} {}", self.fmt_mnemonic(i), self.fmt_operands(i, l))
	}
}

/// Trait to abstract the process of looking up names of addresses.
pub trait NameLookupTrait {
	fn lookup(&self, addr: VA) -> Option<String>;
}

/// A dummy struct that implements `NameLookupTrait` whose `lookup` method always returns `None`.
pub struct NullLookup;

impl NameLookupTrait for NullLookup {
	fn lookup(&self, _addr: VA) -> Option<String> {
		None
	}
}