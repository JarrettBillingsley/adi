use std::collections::HashMap;
use std::default::Default;

use lazy_static::*;
use parse_display::*;
use derive_new::*;

use crate::disasm::types::*;
use crate::memory::types::*;
use crate::program::*;

// ------------------------------------------------------------------------------------------------
// AddrMode
// ------------------------------------------------------------------------------------------------

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum AddrMode {
	IMP, // (no operand bytes)
	IMM, // #$11

	ZPG, // $11
	ZPX, // $11,X where $11 is ZPGaddr
	ZPY, // $11,Y where $11 is ZPGaddr

	ABS, // $2211
	ABX, // $2211,X
	ABY, // $2211,Y

	IND, // ($2211) (only used for JMP indirect)
	IZX, // ($11,X) where $11 is ZPGaddr
	IZY, // ($11),Y where $11 is ZPGaddr

	LAB, // alias for abs, but for jmp/jsr instructions
	REL, // $11 where $11 is signed 2's compl (used for branches)
}

impl AddrMode {
	pub fn is_mem(&self) -> bool {
		use AddrMode::*;
		matches!(self, ABS | ABX | ABY | IND | IZX | IZY | ZPG | ZPX | ZPY)
	}

	pub fn op_bytes(&self) -> usize {
		use AddrMode::*;
		match self {
			IMP => 0,
			IZX | IZY | ZPG | ZPX | ZPY | IMM | REL => 1,
			ABS | ABX | ABY | IND | LAB => 2,
		}
	}
}

// ------------------------------------------------------------------------------------------------
// Opcode
// ------------------------------------------------------------------------------------------------

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub struct Opcode {
	pub opcode:      u8,
	pub my_mnemonic: &'static str,
	pub mnemonic:    &'static str,
	pub addr_mode:   AddrMode,
	pub ctrl:        bool,
	pub access:      Option<MemAccess>,
}

impl Opcode {
	const fn new(
		opcode:      u8,
		my_mnemonic: &'static str,
		mnemonic:    &'static str,
		addr_mode:   AddrMode,
		ctrl:        bool,
		access:      Option<MemAccess>,
	) -> Opcode {
		Self { opcode, my_mnemonic, mnemonic, addr_mode, ctrl, access, }
	}
}

impl OpcodeTrait for Opcode {
	fn is_control    (&self) -> bool { self.ctrl }
	fn is_conditional(&self) -> bool { self.addr_mode == AddrMode::REL }
	fn is_jump       (&self) -> bool { self.opcode == 0x4C }                // jmp absolute
	fn is_indir_jump (&self) -> bool { self.opcode == 0x6C }                // jmp indirect
	fn is_call       (&self) -> bool { self.opcode == 0x20 }                // jsr
	fn is_return     (&self) -> bool { matches!(self.opcode, 0x40 | 0x60) } // rts/rti
	fn is_invalid    (&self) -> bool { self.opcode == INVALID_OPCODE }
}

pub const INVALID_OPCODE: u8 = 0xFF;

// ------------------------------------------------------------------------------------------------
// Operand
// ------------------------------------------------------------------------------------------------

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum Reg {
	A, X, Y, S, P
}

impl Default for Reg {
	fn default() -> Reg { Reg::A }
}

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
	fn is_mem(&self) -> bool { matches!(self, Operand::Mem(..)) }
	fn is_imm(&self) -> bool { matches!(self, Operand::Imm(..)) }
	fn access(&self) -> MemAccess {
		match self {
			Operand::Mem(_, a) => *a,
			_ => panic!("access called on a non-memory operand {}", self),
		}
	}
}

// ------------------------------------------------------------------------------------------------
// Instruction
// ------------------------------------------------------------------------------------------------

const MAX_OPS: usize = 2; // ? we'll see

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub struct Operands {
	len: usize,
	ops: [Operand; MAX_OPS],
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

	fn len(&self) -> usize {
		self.len
	}

	fn get(&self, i: usize) -> Operand {
		assert!(i < self.len);
		self.ops[i]
	}
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
#[derive(new)]
pub struct Instruction {
	va:       VAddr,
	opcode:   &'static Opcode,
	size:     usize,
	ops:      Operands,
}

impl InstructionTrait<&'static Opcode, Operand> for Instruction {
	fn va(&self) -> VAddr                 { self.va }
	fn opcode(&self) -> &'static Opcode   { self.opcode }
	fn size(&self) -> usize               { self.size }
	fn num_ops(&self) -> usize            { self.ops.len() }
	fn get_op(&self, i: usize) -> Operand { self.ops.get(i) }
}

// ------------------------------------------------------------------------------------------------
// Disassembler
// ------------------------------------------------------------------------------------------------

pub struct Disassembler<'a> {
	prog: &'a Program<'a>,
}

impl<'a> DisassemblerTrait<'a, Instruction> for Disassembler<'a> {
	fn new(prog: &'a Program<'a>) -> Disassembler<'a> {
		Self { prog }
	}

	fn prog(&self) -> &'a Program<'a> {
		self.prog
	}

	fn disas_instr(&self, img: &[u8], offs: usize, va: VAddr) -> Instruction {
		let opcode = lookup_opcode(img[offs]);
		let op_offs = offs + 1;
		let op_size = opcode.addr_mode.op_bytes();
		let inst_size = op_size + 1;

		// TODO: an assert is def not the right thing here
		assert!((op_offs + op_size) <= img.len());

		let ops = decode_operands(opcode, va, &img[op_offs .. op_offs + op_size]);
		Instruction::new(va, opcode, inst_size, ops)
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
				REL => ((va.0 as i32) + (img[0] as i32) + 2) as u16,
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

// ------------------------------------------------------------------------------------------------
// Opcode table
// ------------------------------------------------------------------------------------------------

pub fn lookup_opcode(opcode: u8) -> &'static Opcode {
	OPCODE_TABLE.get(&opcode).unwrap_or(&&OP_INVALID)
}

lazy_static! {
	static ref OPCODE_TABLE: HashMap<u8, &'static Opcode> = {
		let mut m = HashMap::new();
		for o in OPCODES { m.insert(o.opcode, o); }
		m
	};
}

const OP_INVALID: Opcode = Opcode::new(0xFF, "???", "???", AddrMode::IMP, false, None);

const OPCODES: &[Opcode] = &[
	Opcode::new(0x00, "brk",      "brk", AddrMode::IMP, false, None                   ),
	Opcode::new(0x01, "or  a,",   "ora", AddrMode::IZX, false, Some(MemAccess::Offset)),
	Opcode::new(0x05, "or  a,",   "ora", AddrMode::ZPG, false, Some(MemAccess::Read)  ),
	Opcode::new(0x06, "shl",      "asl", AddrMode::ZPG, false, Some(MemAccess::Rmw)   ),
	Opcode::new(0x08, "psh p",    "php", AddrMode::IMP, false, None                   ),
	Opcode::new(0x09, "or  a,",   "ora", AddrMode::IMM, false, None                   ),
	Opcode::new(0x0A, "shl a",    "asl", AddrMode::IMP, false, None                   ),
	Opcode::new(0x0D, "or  a,",   "ora", AddrMode::ABS, false, Some(MemAccess::Read)  ),
	Opcode::new(0x0E, "shl",      "asl", AddrMode::ABS, false, Some(MemAccess::Offset)),
	Opcode::new(0x10, "bpl",      "bpl", AddrMode::REL, true,  Some(MemAccess::Target)),
	Opcode::new(0x11, "or  a,",   "ora", AddrMode::IZY, false, Some(MemAccess::Read)  ),
	Opcode::new(0x15, "or  a,",   "ora", AddrMode::ZPX, false, Some(MemAccess::Offset)),
	Opcode::new(0x16, "shl",      "asl", AddrMode::ZPX, false, Some(MemAccess::Offset)),
	Opcode::new(0x18, "clr c",    "clc", AddrMode::IMP, false, None                   ),
	Opcode::new(0x19, "or  a,",   "ora", AddrMode::ABY, false, Some(MemAccess::Offset)),
	Opcode::new(0x1D, "or  a,",   "ora", AddrMode::ABX, false, Some(MemAccess::Offset)),
	Opcode::new(0x1E, "shl",      "asl", AddrMode::ABX, false, Some(MemAccess::Offset)),
	Opcode::new(0x20, "jsr",      "jsr", AddrMode::LAB, true,  Some(MemAccess::Target)),
	Opcode::new(0x21, "and a,",   "and", AddrMode::IZX, false, Some(MemAccess::Offset)),
	Opcode::new(0x24, "bit",      "bit", AddrMode::ZPG, false, Some(MemAccess::Read)  ),
	Opcode::new(0x25, "and a,",   "and", AddrMode::ZPG, false, Some(MemAccess::Read)  ),
	Opcode::new(0x26, "rol",      "rol", AddrMode::ZPG, false, Some(MemAccess::Rmw)   ),
	Opcode::new(0x28, "pul p",    "plp", AddrMode::IMP, false, None                   ),
	Opcode::new(0x29, "and a,",   "and", AddrMode::IMM, false, None                   ),
	Opcode::new(0x2A, "rol a",    "rol", AddrMode::IMP, false, None                   ),
	Opcode::new(0x2C, "bit",      "bit", AddrMode::ABS, false, Some(MemAccess::Read)  ),
	Opcode::new(0x2D, "and a,",   "and", AddrMode::ABS, false, Some(MemAccess::Read)  ),
	Opcode::new(0x2E, "rol",      "rol", AddrMode::ABS, false, Some(MemAccess::Offset)),
	Opcode::new(0x30, "bmi",      "bmi", AddrMode::REL, true,  Some(MemAccess::Target)),
	Opcode::new(0x31, "and a,",   "and", AddrMode::IZY, false, Some(MemAccess::Read)  ),
	Opcode::new(0x35, "and a,",   "and", AddrMode::ZPX, false, Some(MemAccess::Offset)),
	Opcode::new(0x36, "rol",      "rol", AddrMode::ZPX, false, Some(MemAccess::Offset)),
	Opcode::new(0x38, "set c",    "sec", AddrMode::IMP, false, None                   ),
	Opcode::new(0x39, "and a,",   "and", AddrMode::ABY, false, Some(MemAccess::Offset)),
	Opcode::new(0x3D, "and a,",   "and", AddrMode::ABX, false, Some(MemAccess::Offset)),
	Opcode::new(0x3E, "rol",      "rol", AddrMode::ABX, false, Some(MemAccess::Offset)),
	Opcode::new(0x40, "rti",      "rti", AddrMode::IMP, true , None                   ),
	Opcode::new(0x41, "xor a,",   "eor", AddrMode::IZX, false, Some(MemAccess::Offset)),
	Opcode::new(0x45, "xor a,",   "eor", AddrMode::ZPG, false, Some(MemAccess::Read)  ),
	Opcode::new(0x46, "shr",      "lsr", AddrMode::ZPG, false, Some(MemAccess::Rmw)   ),
	Opcode::new(0x48, "psh a",    "pha", AddrMode::IMP, false, None                   ),
	Opcode::new(0x49, "xor a,",   "eor", AddrMode::IMM, false, None                   ),
	Opcode::new(0x4A, "shr a",    "lsr", AddrMode::IMP, false, None                   ),
	Opcode::new(0x4C, "jmp",      "jmp", AddrMode::LAB, true,  Some(MemAccess::Target)),
	Opcode::new(0x4D, "xor a,",   "eor", AddrMode::ABS, false, Some(MemAccess::Read)  ),
	Opcode::new(0x4E, "shr",      "lsr", AddrMode::ABS, false, Some(MemAccess::Offset)),
	Opcode::new(0x50, "bvc",      "bvc", AddrMode::REL, true,  Some(MemAccess::Target)),
	Opcode::new(0x51, "xor a,",   "eor", AddrMode::IZY, false, Some(MemAccess::Read)  ),
	Opcode::new(0x55, "xor a,",   "eor", AddrMode::ZPX, false, Some(MemAccess::Offset)),
	Opcode::new(0x56, "shr",      "lsr", AddrMode::ZPX, false, Some(MemAccess::Offset)),
	Opcode::new(0x58, "clr i",    "cli", AddrMode::IMP, false, None                   ),
	Opcode::new(0x59, "xor a,",   "eor", AddrMode::ABY, false, Some(MemAccess::Offset)),
	Opcode::new(0x5D, "xor a,",   "eor", AddrMode::ABX, false, Some(MemAccess::Offset)),
	Opcode::new(0x5E, "shr",      "lsr", AddrMode::ABX, false, Some(MemAccess::Offset)),
	Opcode::new(0x60, "rts",      "rts", AddrMode::IMP, true , None                   ),
	Opcode::new(0x61, "adc a,",   "adc", AddrMode::IZX, false, Some(MemAccess::Offset)),
	Opcode::new(0x65, "adc a,",   "adc", AddrMode::ZPG, false, Some(MemAccess::Read)  ),
	Opcode::new(0x66, "ror",      "ror", AddrMode::ZPG, false, Some(MemAccess::Rmw)   ),
	Opcode::new(0x68, "pul a",    "pla", AddrMode::IMP, false, None                   ),
	Opcode::new(0x69, "adc a,",   "adc", AddrMode::IMM, false, None                   ),
	Opcode::new(0x6A, "ror a",    "ror", AddrMode::IMP, false, None                   ),
	Opcode::new(0x6C, "jmp",      "jmp", AddrMode::IND, true,  Some(MemAccess::Read)  ),
	Opcode::new(0x6D, "adc a,",   "adc", AddrMode::ABS, false, Some(MemAccess::Read)  ),
	Opcode::new(0x6E, "ror",      "ror", AddrMode::ABS, false, Some(MemAccess::Offset)),
	Opcode::new(0x70, "bvs",      "bvs", AddrMode::REL, true,  Some(MemAccess::Target)),
	Opcode::new(0x71, "adc a,",   "adc", AddrMode::IZY, false, Some(MemAccess::Read)  ),
	Opcode::new(0x75, "adc a,",   "adc", AddrMode::ZPX, false, Some(MemAccess::Offset)),
	Opcode::new(0x76, "ror",      "ror", AddrMode::ZPX, false, Some(MemAccess::Offset)),
	Opcode::new(0x78, "set i",    "sei", AddrMode::IMP, false, None                   ),
	Opcode::new(0x79, "adc a,",   "adc", AddrMode::ABY, false, Some(MemAccess::Offset)),
	Opcode::new(0x7D, "adc a,",   "adc", AddrMode::ABX, false, Some(MemAccess::Offset)),
	Opcode::new(0x7E, "ror",      "ror", AddrMode::ABX, false, Some(MemAccess::Offset)),
	Opcode::new(0x81, "st  a,",   "sta", AddrMode::IZX, false, Some(MemAccess::Offset)),
	Opcode::new(0x84, "st  y,",   "sty", AddrMode::ZPG, false, Some(MemAccess::Write) ),
	Opcode::new(0x85, "st  a,",   "sta", AddrMode::ZPG, false, Some(MemAccess::Write) ),
	Opcode::new(0x86, "st  x,",   "stx", AddrMode::ZPG, false, Some(MemAccess::Write) ),
	Opcode::new(0x88, "dec y",    "dey", AddrMode::IMP, false, None                   ),
	Opcode::new(0x8A, "mov a, x", "txa", AddrMode::IMP, false, None                   ),
	Opcode::new(0x8C, "st  y,",   "sty", AddrMode::ABS, false, Some(MemAccess::Write) ),
	Opcode::new(0x8D, "st  a,",   "sta", AddrMode::ABS, false, Some(MemAccess::Read)  ),
	Opcode::new(0x8E, "st  x,",   "stx", AddrMode::ABS, false, Some(MemAccess::Write) ),
	Opcode::new(0x90, "bcc",      "bcc", AddrMode::REL, true,  Some(MemAccess::Target)),
	Opcode::new(0x91, "st  a,",   "sta", AddrMode::IZY, false, Some(MemAccess::Read)  ),
	Opcode::new(0x94, "st  y,",   "sty", AddrMode::ZPX, false, Some(MemAccess::Offset)),
	Opcode::new(0x95, "st  a,",   "sta", AddrMode::ZPX, false, Some(MemAccess::Offset)),
	Opcode::new(0x96, "st  x,",   "stx", AddrMode::ZPY, false, Some(MemAccess::Offset)),
	Opcode::new(0x98, "mov a, y", "tya", AddrMode::IMP, false, None                   ),
	Opcode::new(0x99, "st  a,",   "sta", AddrMode::ABY, false, Some(MemAccess::Offset)),
	Opcode::new(0x9A, "mov s, x", "txs", AddrMode::IMP, false, None                   ),
	Opcode::new(0x9D, "st  a,",   "sta", AddrMode::ABX, false, Some(MemAccess::Offset)),
	Opcode::new(0xA0, "li  y,",   "ldy", AddrMode::IMM, false, None                   ),
	Opcode::new(0xA1, "ld  a,",   "lda", AddrMode::IZX, false, Some(MemAccess::Offset)),
	Opcode::new(0xA2, "li  x,",   "ldx", AddrMode::IMM, false, None                   ),
	Opcode::new(0xA4, "ld  y,",   "ldy", AddrMode::ZPG, false, Some(MemAccess::Read)  ),
	Opcode::new(0xA5, "ld  a,",   "lda", AddrMode::ZPG, false, Some(MemAccess::Read)  ),
	Opcode::new(0xA6, "ld  x,",   "ldx", AddrMode::ZPG, false, Some(MemAccess::Read)  ),
	Opcode::new(0xA8, "mov y, a", "tay", AddrMode::IMP, false, None                   ),
	Opcode::new(0xA9, "li  a,",   "lda", AddrMode::IMM, false, None                   ),
	Opcode::new(0xAA, "mov x, a", "tax", AddrMode::IMP, false, None                   ),
	Opcode::new(0xAC, "ld  y,",   "ldy", AddrMode::ABS, false, Some(MemAccess::Read)  ),
	Opcode::new(0xAD, "ld  a,",   "lda", AddrMode::ABS, false, Some(MemAccess::Read)  ),
	Opcode::new(0xAE, "ld  x,",   "ldx", AddrMode::ABS, false, Some(MemAccess::Read)  ),
	Opcode::new(0xB0, "bcs",      "bcs", AddrMode::REL, true,  Some(MemAccess::Target)),
	Opcode::new(0xB1, "ld  a,",   "lda", AddrMode::IZY, false, Some(MemAccess::Read)  ),
	Opcode::new(0xB4, "ld  y,",   "ldy", AddrMode::ZPX, false, Some(MemAccess::Offset)),
	Opcode::new(0xB5, "ld  a,",   "lda", AddrMode::ZPX, false, Some(MemAccess::Offset)),
	Opcode::new(0xB6, "ld  x,",   "ldx", AddrMode::ZPY, false, Some(MemAccess::Offset)),
	Opcode::new(0xB8, "clr v",    "clv", AddrMode::IMP, false, None                   ),
	Opcode::new(0xB9, "ld  a,",   "lda", AddrMode::ABY, false, Some(MemAccess::Offset)),
	Opcode::new(0xBA, "mov x, s", "tsx", AddrMode::IMP, false, None                   ),
	Opcode::new(0xBC, "ld  y,",   "ldy", AddrMode::ABX, false, Some(MemAccess::Offset)),
	Opcode::new(0xBD, "ld  a,",   "lda", AddrMode::ABX, false, Some(MemAccess::Offset)),
	Opcode::new(0xBE, "ld  x,",   "ldx", AddrMode::ABY, false, Some(MemAccess::Offset)),
	Opcode::new(0xC0, "cmp y,",   "cpy", AddrMode::IMM, false, None                   ),
	Opcode::new(0xC1, "cmp a,",   "cmp", AddrMode::IZX, false, Some(MemAccess::Offset)),
	Opcode::new(0xC4, "cmp y,",   "cpy", AddrMode::ZPG, false, Some(MemAccess::Read)  ),
	Opcode::new(0xC5, "cmp a,",   "cmp", AddrMode::ZPG, false, Some(MemAccess::Read)  ),
	Opcode::new(0xC6, "dec",      "dec", AddrMode::ZPG, false, Some(MemAccess::Rmw)   ),
	Opcode::new(0xC8, "inc y",    "iny", AddrMode::IMP, false, None                   ),
	Opcode::new(0xC9, "cmp a,",   "cmp", AddrMode::IMM, false, None                   ),
	Opcode::new(0xCA, "dec x",    "dex", AddrMode::IMP, false, None                   ),
	Opcode::new(0xCC, "cmp y,",   "cpy", AddrMode::ABS, false, Some(MemAccess::Read)  ),
	Opcode::new(0xCD, "cmp a,",   "cmp", AddrMode::ABS, false, Some(MemAccess::Read)  ),
	Opcode::new(0xCE, "dec",      "dec", AddrMode::ABS, false, Some(MemAccess::Offset)),
	Opcode::new(0xD0, "bne",      "bne", AddrMode::REL, true,  Some(MemAccess::Target)),
	Opcode::new(0xD1, "cmp a,",   "cmp", AddrMode::IZY, false, Some(MemAccess::Read)  ),
	Opcode::new(0xD5, "cmp a,",   "cmp", AddrMode::ZPX, false, Some(MemAccess::Offset)),
	Opcode::new(0xD6, "dec",      "dec", AddrMode::ZPX, false, Some(MemAccess::Offset)),
	Opcode::new(0xD8, "clr d",    "cld", AddrMode::IMP, false, None                   ),
	Opcode::new(0xD9, "cmp a,",   "cmp", AddrMode::ABY, false, Some(MemAccess::Offset)),
	Opcode::new(0xDD, "cmp a,",   "cmp", AddrMode::ABX, false, Some(MemAccess::Offset)),
	Opcode::new(0xDE, "dec",      "dec", AddrMode::ABX, false, Some(MemAccess::Offset)),
	Opcode::new(0xE0, "cmp x,",   "cpx", AddrMode::IMM, false, None                   ),
	Opcode::new(0xE1, "sbc a,",   "sbc", AddrMode::IZX, false, Some(MemAccess::Offset)),
	Opcode::new(0xE4, "cmp x,",   "cpx", AddrMode::ZPG, false, Some(MemAccess::Read)  ),
	Opcode::new(0xE5, "sbc a,",   "sbc", AddrMode::ZPG, false, Some(MemAccess::Read)  ),
	Opcode::new(0xE6, "inc",      "inc", AddrMode::ZPG, false, Some(MemAccess::Rmw)   ),
	Opcode::new(0xE8, "inc x",    "inx", AddrMode::IMP, false, None                   ),
	Opcode::new(0xE9, "sbc a,",   "sbc", AddrMode::IMM, false, None                   ),
	Opcode::new(0xEA, "nop",      "nop", AddrMode::IMP, false, None                   ),
	Opcode::new(0xEC, "cmp x,",   "cpx", AddrMode::ABS, false, Some(MemAccess::Read)  ),
	Opcode::new(0xED, "sbc a,",   "sbc", AddrMode::ABS, false, Some(MemAccess::Read)  ),
	Opcode::new(0xEE, "inc",      "inc", AddrMode::ABS, false, Some(MemAccess::Offset)),
	Opcode::new(0xF0, "beq",      "beq", AddrMode::REL, true,  Some(MemAccess::Target)),
	Opcode::new(0xF1, "sbc a,",   "sbc", AddrMode::IZY, false, Some(MemAccess::Read)  ),
	Opcode::new(0xF5, "sbc a,",   "sbc", AddrMode::ZPX, false, Some(MemAccess::Offset)),
	Opcode::new(0xF6, "inc",      "inc", AddrMode::ZPX, false, Some(MemAccess::Offset)),
	Opcode::new(0xF8, "set d",    "sed", AddrMode::IMP, false, None                   ),
	Opcode::new(0xF9, "sbc a,",   "sbc", AddrMode::ABY, false, Some(MemAccess::Offset)),
	Opcode::new(0xFD, "sbc a,",   "sbc", AddrMode::ABX, false, Some(MemAccess::Offset)),
	Opcode::new(0xFE, "inc",      "inc", AddrMode::ABX, false, Some(MemAccess::Offset)),
];