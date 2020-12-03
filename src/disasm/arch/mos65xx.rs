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

mod descs;
mod opcodes;
mod types;
#[cfg(test)]
mod tests;

use descs::*;
use opcodes::*;
use types::*;

// ------------------------------------------------------------------------------------------------
// InstDesc
// ------------------------------------------------------------------------------------------------

/// A 65xx instruction desc.
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub struct InstDesc {
	/// The actual opcode byte.
	opcode:      Opcode,
	/// Which meta-op this is.
	meta_op:     MetaOp,
	/// What addressing mode is used.
	addr_mode:   AddrMode,
	/// Is this a control flow instruction?
	ctrl:        bool,
	/// Does this access memory, and how?
	access:      Option<MemAccess>,
}

impl InstDesc {
	const fn new(
		opcode:      Opcode,
		meta_op:     MetaOp,
		addr_mode:   AddrMode,
		ctrl:        bool,
		access:      Option<MemAccess>,
	) -> Self {
		Self { opcode, meta_op, addr_mode, ctrl, access, }
	}
}

impl InstDescTrait for &InstDesc {
	fn is_control    (&self) -> bool { self.ctrl }

	fn is_conditional(&self) -> bool { self.addr_mode == AddrMode::REL }
	fn is_jump       (&self) -> bool { matches!(self.opcode, Opcode::JMP_LAB) }
	fn is_indir_jump (&self) -> bool { matches!(self.opcode, Opcode::JMP_IND) }
	fn is_call       (&self) -> bool { matches!(self.opcode, Opcode::JSR_LAB) }
	fn is_return     (&self) -> bool { matches!(self.opcode, Opcode::RTS_IMP | Opcode::RTI_IMP) }
	fn is_invalid    (&self) -> bool { matches!(self.opcode, Opcode::INVALID) }
}

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

const MAX_BYTES: usize = 3;

/// A 65xx instruction.
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub struct Instruction {
	va:    VAddr,
	desc:  &'static InstDesc,
	size:  usize,
	ops:   Operands,
	bytes: [u8; MAX_BYTES],
}

// can't use #[derive(new)] cause that always makes a `pub` function...
impl Instruction {
	fn new(va: VAddr, desc: &'static InstDesc, size: usize, ops: Operands, bytes: &[u8]) -> Self {
		// TODO: this feels really overwritten
		assert!(bytes.len() <= MAX_BYTES);
		let mut b = [0u8; MAX_BYTES];
		for (i, x) in bytes.iter().enumerate() { b[i] = *x; }
		Self { va, desc, size, ops, bytes: b }
	}
}

impl InstructionTrait for Instruction {
	type TDesc = &'static InstDesc;
	type TOperand = Operand;

	fn va(&self) -> VAddr                 { self.va }
	fn desc(&self) -> &'static InstDesc   { self.desc }
	fn size(&self) -> usize               { self.size }
	fn num_ops(&self) -> usize            { self.ops.len() }
	fn get_op(&self, i: usize) -> Operand { self.ops.get(i) }
	fn get_bytes(&self) -> &[u8]          { &self.bytes[..self.size] }
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
		let desc = lookup_desc(img[offs]);

		if desc.meta_op == MetaOp::UNK {
			return Err(unknown_instruction(offs, va));
		}

		// do we have enough bytes for the operands?
		let op_offs = offs + 1;
		let op_size = desc.addr_mode.op_bytes();
		let inst_size = op_size + 1;

		if (op_offs + op_size) > img.len() {
			return Err(out_of_bytes(offs, va, inst_size, img.len() - offs));
		}

		// okay cool, let's decode
		let bytes = &img[offs .. offs + inst_size];
		let ops = decode_operands(desc, va, &img[op_offs .. op_offs + op_size]);
		Ok(Instruction::new(va, desc, inst_size, ops, bytes))
	}
}

fn decode_operands(desc: &'static InstDesc, va: VAddr, img: &[u8]) -> Operands {
	let mut ops = Operands::new();

	if desc.addr_mode.op_bytes() > 0 {
		use AddrMode::*;
		if let Some(access) = desc.access {
			let addr = match desc.addr_mode {
				ZPG | ZPX | ZPY | IZX | IZY => img[0] as u16,

				// TODO: should really use u16::from_le_bytes but it's awkward with slices
				ABS | ABX | ABY | IND | LAB => img[0] as u16 | ((img[1] as u16) << 8),

				// +2 to include size of the branch instruction itself
				REL => ((va.0 as i32) + (img[0] as i8 as i32) + 2) as u16,
				_ => unreachable!()
			};

			ops.push(Operand::Mem(addr, access));
		} else {
			assert!(matches!(desc.addr_mode, IMM));
			ops.push(Operand::Imm(img[0]));
		}
	}

	ops
}

// ------------------------------------------------------------------------------------------------
// Printer
// ------------------------------------------------------------------------------------------------

/// The 65xx instruction printer.
#[derive(Debug, Copy, Clone)]
pub struct Printer {
	flavor: SyntaxFlavor,
}

impl Printer {
	pub fn new(flavor: SyntaxFlavor) -> Self {
		Self { flavor }
	}

	fn fmt_imm(&self, imm: u8) -> String {
		match self.flavor {
			SyntaxFlavor::Old =>
				if imm < 0x10 { format!("#{}", imm) } else { format!("#${:X}", imm) },
			SyntaxFlavor::New =>
				if imm < 0x10 { format!("{}",  imm) } else { format!("0x{:X}", imm) },
		}
	}

	fn fmt_addr(&self, addr: u16, zpg: bool) -> String {
		match self.flavor {
			SyntaxFlavor::Old =>
				if zpg { format!("${:02X}", addr) } else { format!("${:04X}", addr) },
			SyntaxFlavor::New =>
				if zpg { format!("0x{:02X}", addr)} else { format!("0x{:04X}", addr) },
		}
	}
}

impl PrinterTrait for Printer {
	type TInstruction = Instruction;

	fn fmt_mnemonic(&self, i: &Instruction) -> String {
		i.desc.meta_op.mnemonic(self.flavor).into()
	}

	fn fmt_operands(&self, i: &Instruction, l: &dyn NameLookupTrait) -> String {
		if i.num_ops() == 0 {
			"".into()
		} else {
			let operand = match i.get_op(0) {
				Operand::Reg(..)       => unreachable!(),
				Operand::Imm(imm)      => self.fmt_imm(imm),
				Operand::Mem(addr, ..) => {
					match l.lookup(VAddr(addr as usize)) {
						Some(name) => name,
						None       => self.fmt_addr(addr, i.desc.addr_mode.is_zero_page()),
					}
				}
			};

			let template = i.desc.addr_mode.operand_template(self.flavor);
			template.replace("{}", &operand)
		}
	}
}