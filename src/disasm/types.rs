
// use std::iter::Iterator;

use crate::memory::*;
use crate::program::*;

// ------------------------------------------------------------------------------------------------
// Instruction
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

// hmmmmmmm
pub trait InstructionTrait<TOpcode, TOperand>
where
	TOpcode: OpcodeTrait,
	TOperand: OperandTrait
{
	fn va(&self) -> VAddr;
	fn opcode(&self) -> TOpcode;
	fn size(&self) -> usize;
	fn num_ops(&self) -> usize;
	fn get_op(&self, i: usize) -> TOperand;
	// TODO: implied ops as a separate thing?
}

pub trait OpcodeTrait {
	fn is_control    (&self) -> bool;
	fn is_conditional(&self) -> bool;
	fn is_jump       (&self) -> bool;
	fn is_indir_jump (&self) -> bool;
	fn is_call       (&self) -> bool;
	fn is_return     (&self) -> bool;
	fn is_invalid    (&self) -> bool;
}

pub trait OperandTrait {
	fn is_reg(&self) -> bool;
	fn is_imm(&self) -> bool;
	fn access(&self) -> Option<MemAccess>;
	fn is_mem(&self) -> bool {
		self.access().is_some()
	}
}

pub trait DisassemblerTrait<TOpcode, TOperand, TInstruction>
where
	TOpcode: OpcodeTrait,
	TOperand: OperandTrait,
	TInstruction: InstructionTrait<TOpcode, TOperand>
{
	// TODO: this should return a Result type
	fn disas_instr(&self, img: &[u8], offs: usize, va: VAddr) -> TInstruction;
	// fn disas_range(&self, start: VAddr, end: Option<VAddr>) -> dyn Iterator<Item = TInstruction>;
}

pub trait PrinterTrait<TOpcode, TOperand, TInstruction>
where
	TOpcode: OpcodeTrait,
	TOperand: OperandTrait,
	TInstruction: InstructionTrait<TOpcode, TOperand>
{
	fn new(prog: &Program) -> Self;
	fn prog(&self) -> &Program;
	fn fmt_mnemonic(&self, i: &TInstruction) -> String;
	fn fmt_operands(&self, i: &TInstruction) -> String;
	fn fmt_instr(&self, i: &TInstruction) -> String {
		format!("{} {}", self.fmt_mnemonic(i), self.fmt_operands(i))
	}
}