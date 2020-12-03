
// use std::iter::Iterator;

use crate::memory::*;
use crate::program::*;
use super::error::*;

// ------------------------------------------------------------------------------------------------
// OpcodeTrait
// ------------------------------------------------------------------------------------------------

/// Trait for opcodes. Methods are used by analysis to determine control flow.
pub trait OpcodeTrait {
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
	/// Is this an invalid opcode?
	fn is_invalid    (&self) -> bool;
}

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
	/// Associated type of opcodes returned by `opcode`.
	type TOpcode: OpcodeTrait;

	/// Associated type of operands returned by `get_op`.
	type TOperand: OperandTrait;

	/// Get virtual address.
	fn va(&self) -> VAddr;

	/// Get opcode.
	fn opcode(&self) -> Self::TOpcode;

	/// Get size, in bytes.
	fn size(&self) -> usize;

	/// How many operands it has.
	fn num_ops(&self) -> usize;

	/// Accessor for operands.
	fn get_op(&self, i: usize) -> Self::TOperand;

	// TODO: original bytes
	// TODO: implied ops as a separate thing?
}

// ------------------------------------------------------------------------------------------------
// DisassemblerTrait
// ------------------------------------------------------------------------------------------------

/// Trait for disassemblers.
pub trait DisassemblerTrait {
	/// Associated type of instructions given by this disassembler.
	type TInstruction: InstructionTrait;

	/// Disassemble a single instruction from `img[offs..]` with the given VA.
	fn disas_instr(&self, img: &[u8], offs: usize, va: VAddr) -> DisasResult<Self::TInstruction>;

	// TODO: this
	// fn disas_range(&self, start: VAddr, end: Option<VAddr>) -> dyn Iterator<Item = TInstruction>;
}

// ------------------------------------------------------------------------------------------------
// PrinterTrait
// ------------------------------------------------------------------------------------------------

/// Trait for instruction printers.
pub trait PrinterTrait {
	/// Associated type of instructions that this printer prints.
	type TInstruction: InstructionTrait;

	/// Construct a new printer using the given Program for looking up names.
	fn new(prog: &Program) -> Self;

	/// Get the Program this was constructed with.
	fn prog(&self) -> &Program;

	/// Give a string representation of an instruction's mnemonic.
	fn fmt_mnemonic(&self, i: &Self::TInstruction) -> String;

	/// Give a string representation of an instruction's operands.
	fn fmt_operands(&self, i: &Self::TInstruction) -> String;

	/// Give a string representation of an instruction. Default implementation
	/// pads mnemonic out to 8 spaces.
	fn fmt_instr(&self, i: &Self::TInstruction) -> String {
		format!("{:8} {}", self.fmt_mnemonic(i), self.fmt_operands(i))
	}
}