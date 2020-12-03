use std::collections::HashMap;
use std::default::Default;

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

/*
// Keeping this around if we need it for implicit operands.
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
}*/

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
	op:    Option<Operand>,
	bytes: [u8; MAX_BYTES],
}

impl Instruction {
	fn new(va: VAddr, desc: &'static InstDesc, size: usize, op: Option<Operand>, orig: &[u8])
	-> Self {
		let mut bytes = [0u8; MAX_BYTES];
		bytes[..orig.len()].copy_from_slice(orig);
		Self { va, desc, size, op, bytes }
	}
}

impl InstructionTrait for Instruction {
	type TDesc = &'static InstDesc;
	type TOperand = Operand;

	fn va(&self) -> VAddr                 { self.va }
	fn desc(&self) -> &'static InstDesc   { self.desc }
	fn size(&self) -> usize               { self.size }
	fn num_ops(&self) -> usize            { if self.op.is_some() { 1 } else { 0 } }
	fn get_op(&self, i: usize) -> Operand {
		assert!(i == 0);
		self.op.unwrap()
	}
	fn get_bytes(&self) -> &[u8]          { &self.bytes[..self.size] }
}

// ------------------------------------------------------------------------------------------------
// Disassembler
// ------------------------------------------------------------------------------------------------

/// The 65xx disassembler.
pub struct Disassembler;

impl DisassemblerTrait for Disassembler {
	type TInstruction = Instruction;

	fn disas_instr(&self, img: &[u8], va: VAddr) -> DisasResult<Instruction> {
		// do we have enough bytes?
		if img.len() == 0 {
			return Err(DisasError::out_of_bytes(va, 1, 0));
		}

		// is the opcode OK?
		let desc = lookup_desc(img[0]);

		if desc.meta_op == MetaOp::UNK {
			return Err(DisasError::unknown_instruction(va));
		}

		// do we have enough bytes for the operand?
		let inst_size = desc.addr_mode.op_bytes() + 1;

		if inst_size > img.len() {
			return Err(DisasError::out_of_bytes(va, inst_size, img.len()));
		}

		// okay cool, let's decode
		let bytes = &img[0 .. inst_size];
		let op = decode_operand(desc, va, &img[1 .. inst_size]);
		Ok(Instruction::new(va, desc, inst_size, op, bytes))
	}
}

fn decode_operand(desc: &'static InstDesc, va: VAddr, img: &[u8]) -> Option<Operand> {
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

			Some(Operand::Mem(addr, access))
		} else {
			assert!(matches!(desc.addr_mode, IMM));
			Some(Operand::Imm(img[0]))
		}
	} else {
		None
	}
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