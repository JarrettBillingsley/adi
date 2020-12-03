
// use std::iter::Iterator;
use std::fmt::{ Display, Formatter, Result as FmtResult };

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

#[derive(Debug, PartialEq, Eq)]
pub enum DisasErrorKind {
	UnknownInstruction,
	OutOfBytes { expected: usize, got: usize },
}

impl Display for DisasErrorKind {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		use DisasErrorKind::*;

		match self {
			UnknownInstruction =>
				write!(f, "unknown instruction"),

			OutOfBytes { expected, got } =>
				write!(f, "out of bytes (expected {}, got {})", expected, got),
		}
	}
}

#[derive(Debug, PartialEq, Eq)]
pub struct DisasError {
	pub offs: usize,
	pub va:   VAddr,
	pub kind: DisasErrorKind,
}

impl Display for DisasError {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		write!(f, "disassembly error at VA 0x{:08X} (offs = {}): {}", self.va, self.offs, self.kind)
	}
}

impl std::error::Error for DisasError {}

pub type DisasResult<T> = Result<T, DisasError>;

// hmmmmmmm
pub trait InstructionTrait {
	type TOpcode: OpcodeTrait;
	type TOperand: OperandTrait;

	fn va(&self) -> VAddr;
	fn opcode(&self) -> Self::TOpcode;
	fn size(&self) -> usize;
	fn num_ops(&self) -> usize;
	fn get_op(&self, i: usize) -> Self::TOperand;
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

pub trait DisassemblerTrait {
	type TInstruction: InstructionTrait;
	fn disas_instr(&self, img: &[u8], offs: usize, va: VAddr) -> DisasResult<Self::TInstruction>;
	// fn disas_range(&self, start: VAddr, end: Option<VAddr>) -> dyn Iterator<Item = TInstruction>;
}

pub trait PrinterTrait {
	type TInstruction: InstructionTrait;
	fn new(prog: &Program) -> Self;
	fn prog(&self) -> &Program;
	fn fmt_mnemonic(&self, i: &Self::TInstruction) -> String;
	fn fmt_operands(&self, i: &Self::TInstruction) -> String;
	fn fmt_instr(&self, i: &Self::TInstruction) -> String {
		format!("{} {}", self.fmt_mnemonic(i), self.fmt_operands(i))
	}
}