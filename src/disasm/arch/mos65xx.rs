use std::collections::HashMap;
use std::default::Default;
use std::fmt::{ Debug, Formatter, Result as FmtResult };

use lazy_static::*;
use parse_display::*;

use crate::disasm::types::*;
use crate::disasm::error::*;
use crate::memory::types::*;

// ------------------------------------------------------------------------------------------------
// Sub-modules
// ------------------------------------------------------------------------------------------------

mod opcode_table;
mod types;
#[cfg(test)]
mod tests;

use opcode_table::*;
use types::*;

// ------------------------------------------------------------------------------------------------
// Opcode
// ------------------------------------------------------------------------------------------------

/// A 65xx opcode.
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub struct Opcode {
	/// The actual opcode byte.
	opcode:      u8,
	/// Which meta-op this is.
	meta_op:     MetaOp,
	/// What addressing mode is used.
	addr_mode:   AddrMode,
	/// Is this a control flow instruction?
	ctrl:        bool,
	/// Does this access memory, and how?
	access:      Option<MemAccess>,
}

impl Opcode {
	const fn new(
		opcode:      u8,
		meta_op:     MetaOp,
		addr_mode:   AddrMode,
		ctrl:        bool,
		access:      Option<MemAccess>,
	) -> Opcode {
		Self { opcode, meta_op, addr_mode, ctrl, access, }
	}
}

impl OpcodeTrait for &Opcode {
	fn is_control    (&self) -> bool { self.ctrl }

	/// True for conditional branches.
	fn is_conditional(&self) -> bool { self.addr_mode == AddrMode::REL }

	/// True for opcode `0x4C` (absolute `jmp`).
	fn is_jump       (&self) -> bool { self.opcode == 0x4C }                // jmp absolute

	/// True for opcode `0x6C` (indirect `jmp`).
	fn is_indir_jump (&self) -> bool { self.opcode == 0x6C }                // jmp indirect

	/// True for opcode `0x20` (`jsr`).
	fn is_call       (&self) -> bool { self.opcode == 0x20 }                // jsr

	/// True for opcodes `0x40` and `0x60` (`rts` and `rti`).
	fn is_return     (&self) -> bool { matches!(self.opcode, 0x40 | 0x60) } // rts/rti

	/// True for invalid opcodes.
	fn is_invalid    (&self) -> bool { self.opcode == INVALID_OPCODE }
}

/// Sentinel value used for invalid opcodes.
pub const INVALID_OPCODE: u8 = 0xFF;

// ------------------------------------------------------------------------------------------------
// Operand
// ------------------------------------------------------------------------------------------------

/// Kinds of operands. Explicit operands are always either `Imm` or `Mem`; `Reg` is only
/// implicit (aka inherent).
#[derive(Debug, Display, PartialEq, Eq, Copy, Clone)]
#[display("{:?}")]
pub enum Operand {
	Reg(Reg),
	Imm(u8),
	Mem(u16, MemAccess),
}

impl Default for Operand {
	fn default() -> Operand { Operand::Reg(Default::default()) }
}

impl OperandTrait for Operand {
	fn is_reg(&self) -> bool { matches!(self, Operand::Reg(..)) }
	fn is_imm(&self) -> bool { matches!(self, Operand::Imm(..)) }
	fn access(&self) -> Option<MemAccess> {
		match self {
			Operand::Mem(_, a) => Some(*a),
			_ => None,
		}
	}
}

// ------------------------------------------------------------------------------------------------
// Operands
// ------------------------------------------------------------------------------------------------

const MAX_OPS: usize = 2; // ? we'll see

/// Tiny container for instruction operands.
#[derive(PartialEq, Eq, Copy, Clone)]
pub struct Operands {
	len: usize,
	ops: [Operand; MAX_OPS],
}

impl Debug for Operands {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		write!(f, "Operands{:?}", &self.ops[..self.len])
	}
}

impl Operands {
	fn new() -> Self {
		Self { len: 0, ops: Default::default() }
	}

	fn push(&mut self, op: Operand) {
		assert!(self.len < MAX_OPS);
		self.ops[self.len] = op;
		self.len += 1;
	}

	pub fn len(&self) -> usize {
		self.len
	}

	pub fn get(&self, i: usize) -> Operand {
		assert!(i < self.len);
		self.ops[i]
	}
}

// ------------------------------------------------------------------------------------------------
// Instruction
// ------------------------------------------------------------------------------------------------

/// A 65xx instruction.
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub struct Instruction {
	va:       VAddr,
	opcode:   &'static Opcode,
	size:     usize,
	ops:      Operands,
}

// can't use #[derive(new)] cause that always makes a `pub` function...
impl Instruction {
	fn new(va: VAddr, opcode: &'static Opcode, size: usize, ops: Operands) -> Self {
		Self { va, opcode, size, ops }
	}
}

impl InstructionTrait for Instruction {
	type TOpcode = &'static Opcode;
	type TOperand = Operand;

	fn va(&self) -> VAddr                 { self.va }
	fn opcode(&self) -> &'static Opcode   { self.opcode }
	fn size(&self) -> usize               { self.size }
	fn num_ops(&self) -> usize            { self.ops.len() }
	fn get_op(&self, i: usize) -> Operand { self.ops.get(i) }
}

// ------------------------------------------------------------------------------------------------
// Disassembler
// ------------------------------------------------------------------------------------------------

fn out_of_bytes(offs: usize, va: VAddr, expected: usize, got: usize) -> DisasError {
	DisasError { offs, va, kind: DisasErrorKind::OutOfBytes { expected, got } }
}

fn unknown_instruction(offs: usize, va: VAddr) -> DisasError {
	DisasError { offs, va, kind: DisasErrorKind::UnknownInstruction }
}

/// The 65xx disassembler.
pub struct Disassembler;

impl DisassemblerTrait for Disassembler {
	type TInstruction = Instruction;

	fn disas_instr(&self, img: &[u8], offs: usize, va: VAddr) -> DisasResult<Instruction> {
		// do we have enough bytes?
		if offs == img.len() {
			return Err(out_of_bytes(offs, va, 1, 0));
		}

		// is the opcode OK?
		let opcode = lookup_opcode(img[offs]);

		if opcode.meta_op == MetaOp::UNK {
			return Err(unknown_instruction(offs, va));
		}

		// do we have enough bytes for the operands?
		let op_offs = offs + 1;
		let op_size = opcode.addr_mode.op_bytes();
		let inst_size = op_size + 1;

		if (op_offs + op_size) > img.len() {
			return Err(out_of_bytes(offs, va, inst_size, img.len() - offs));
		}

		// okay cool, let's decode
		let ops = decode_operands(opcode, va, &img[op_offs .. op_offs + op_size]);
		Ok(Instruction::new(va, opcode, inst_size, ops))
	}
}

fn decode_operands(opcode: &'static Opcode, va: VAddr, img: &[u8]) -> Operands {
	let mut ops = Operands::new();

	if opcode.addr_mode.op_bytes() > 0 {
		use AddrMode::*;
		if let Some(access) = opcode.access {
			let addr = match opcode.addr_mode {
				ZPG | ZPX | ZPY | IZX | IZY => img[0] as u16,

				// TODO: should really use u16::from_le_bytes but it's awkward with slices
				ABS | ABX | ABY | IND | LAB => img[0] as u16 | ((img[1] as u16) << 8),

				// +2 to include size of the branch instruction itself
				REL => ((va.0 as i32) + (img[0] as i8 as i32) + 2) as u16,
				_ => unreachable!()
			};

			ops.push(Operand::Mem(addr, access));
		} else {
			assert!(matches!(opcode.addr_mode, IMM));
			ops.push(Operand::Imm(img[0]));
		}
	}

	ops
}