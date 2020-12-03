use std::collections::HashMap;
use std::default::Default;
use std::fmt::{ Debug, Formatter, Result as FmtResult };

use lazy_static::*;
use parse_display::*;
use derive_new::*;

use crate::disasm::types::*;
use crate::memory::types::*;

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
pub enum MetaOp {
	UNK,
	ADC, AND,  ASLA, ASL,  BCC,  BCS,  BEQ,  BIT, BMI, BNE,
	BPL, BRK,  BVC,  BVS,  CLC,  CLD,  CLI,  CLV, CMP, CPX,
	CPY, DEC,  DEX,  DEY,  EOR,  INC,  INX,  INY, JMP, JSR,
	LDA, LDAI, LDX,  LDXI, LDY,  LDYI, LSRA, LSR, NOP, ORA,
	PHA, PHP,  PLA,  PLP,  ROLA, ROL,  RORA, ROR, RTI, RTS,
	SBC, SEC,  SED,  SEI,  STA,  STX,  STY,  TAX, TAY, TSX,
	TXA, TXS,  TYA,
}

impl MetaOp {
	pub fn mnemonic_old(&self) -> &'static str {
		use MetaOp::*;
		match self {
			UNK  => "???",
			ADC  => "adc", AND  => "and", ASLA => "asl", ASL  => "asl",
			BCC  => "bcc", BCS  => "bcs", BEQ  => "beq", BIT  => "bit",
			BMI  => "bmi", BNE  => "bne", BPL  => "bpl", BRK  => "brk",
			BVC  => "bvc", BVS  => "bvs", CLC  => "clc", CLD  => "cld",
			CLI  => "cli", CLV  => "clv", CMP  => "cmp", CPX  => "cpx",
			CPY  => "cpy", DEC  => "dec", DEX  => "dex", DEY  => "dey",
			EOR  => "eor", INC  => "inc", INX  => "inx", INY  => "iny",
			JMP  => "jmp", JSR  => "jsr", LDA  => "lda", LDAI => "lda",
			LDX  => "ldx", LDXI => "ldx", LDY  => "ldy", LDYI => "ldy",
			LSRA => "lsr", LSR  => "lsr", NOP  => "nop", ORA  => "ora",
			PHA  => "pha", PHP  => "php", PLA  => "pla", PLP  => "plp",
			ROLA => "rol", ROL  => "rol", RORA => "ror", ROR  => "ror",
			RTI  => "rti", RTS  => "rts", SBC  => "sbc", SEC  => "sec",
			SED  => "sed", SEI  => "sei", STA  => "sta", STX  => "stx",
			STY  => "sty", TAX  => "tax", TAY  => "tay", TSX  => "tsx",
			TXA  => "txa", TXS  => "txs", TYA  => "tya",
		}
	}

	pub fn mnemonic_new(&self) -> &'static str {
		use MetaOp::*;
		match self {
			UNK  => "???",
			ADC  => "adc a,",   AND  => "and a,",   ASLA => "shl a",    ASL  => "shl",
			BCC  => "bcc",      BCS  => "bcs",      BEQ  => "beq",      BIT  => "bit",
			BMI  => "bmi",      BNE  => "bne",      BPL  => "bpl",      BRK  => "brk",
			BVC  => "bvc",      BVS  => "bvs",      CLC  => "clr c",    CLD  => "clr d",
			CLI  => "clr i",    CLV  => "clr v",    CMP  => "cmp a,",   CPX  => "cmp x,",
			CPY  => "cmp y,",   DEC  => "dec",      DEX  => "dec x",    DEY  => "dec y",
			EOR  => "xor a,",   INC  => "inc",      INX  => "inc x",    INY  => "inc y",
			JMP  => "jmp",      JSR  => "jsr",      LDA  => "ld  a,",   LDAI => "li  a,",
			LDX  => "ld  x,",   LDXI => "li  x,",   LDY  => "ld  y,",   LDYI => "li  y,",
			LSRA => "shr a",    LSR  => "shr",      NOP  => "nop",      ORA  => "or  a,",
			PHA  => "psh a",    PHP  => "psh p",    PLA  => "pul a",    PLP  => "pul p",
			ROLA => "rol a",    ROL  => "rol",      RORA => "ror a",    ROR  => "ror",
			RTI  => "rti",      RTS  => "rts",      SBC  => "sbc a,",   SEC  => "set c",
			SED  => "set d",    SEI  => "set i",    STA  => "st  a,",   STX  => "st  x,",
			STY  => "st  y,",   TAX  => "mov x, a", TAY  => "mov y, a", TSX  => "mov x, s",
			TXA  => "mov a, x", TXS  => "mov s, x", TYA  => "mov a, y",
		}
	}
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub struct Opcode {
	pub opcode:      u8,
	pub meta_op:     MetaOp,
	pub addr_mode:   AddrMode,
	pub ctrl:        bool,
	pub access:      Option<MemAccess>,
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
	fn is_imm(&self) -> bool { matches!(self, Operand::Imm(..)) }
	fn access(&self) -> Option<MemAccess> {
		match self {
			Operand::Mem(_, a) => Some(*a),
			_ => None,
		}
	}
}

// ------------------------------------------------------------------------------------------------
// Instruction
// ------------------------------------------------------------------------------------------------

const MAX_OPS: usize = 2; // ? we'll see

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

pub struct Disassembler;

fn out_of_bytes(offs: usize, va: VAddr, expected: usize, got: usize) -> DisasError {
	DisasError { offs, va, kind: DisasErrorKind::OutOfBytes { expected, got } }
}

fn unknown_instruction(offs: usize, va: VAddr) -> DisasError {
	DisasError { offs, va, kind: DisasErrorKind::UnknownInstruction }
}

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

const OP_INVALID: Opcode = Opcode::new(0xFF, MetaOp::UNK, AddrMode::IMP, false, None);

const OPCODES: &[Opcode] = &[
	Opcode::new(0x00, MetaOp::BRK,  AddrMode::IMP, false, None                   ),
	Opcode::new(0x01, MetaOp::ORA,  AddrMode::IZX, false, Some(MemAccess::Offset)),
	Opcode::new(0x05, MetaOp::ORA,  AddrMode::ZPG, false, Some(MemAccess::Read)  ),
	Opcode::new(0x06, MetaOp::ASL,  AddrMode::ZPG, false, Some(MemAccess::Rmw)   ),
	Opcode::new(0x08, MetaOp::PHP,  AddrMode::IMP, false, None                   ),
	Opcode::new(0x09, MetaOp::ORA,  AddrMode::IMM, false, None                   ),
	Opcode::new(0x0A, MetaOp::ASLA, AddrMode::IMP, false, None                   ),
	Opcode::new(0x0D, MetaOp::ORA,  AddrMode::ABS, false, Some(MemAccess::Read)  ),
	Opcode::new(0x0E, MetaOp::ASL,  AddrMode::ABS, false, Some(MemAccess::Offset)),
	Opcode::new(0x10, MetaOp::BPL,  AddrMode::REL, true,  Some(MemAccess::Target)),
	Opcode::new(0x11, MetaOp::ORA,  AddrMode::IZY, false, Some(MemAccess::Read)  ),
	Opcode::new(0x15, MetaOp::ORA,  AddrMode::ZPX, false, Some(MemAccess::Offset)),
	Opcode::new(0x16, MetaOp::ASL,  AddrMode::ZPX, false, Some(MemAccess::Offset)),
	Opcode::new(0x18, MetaOp::CLC,  AddrMode::IMP, false, None                   ),
	Opcode::new(0x19, MetaOp::ORA,  AddrMode::ABY, false, Some(MemAccess::Offset)),
	Opcode::new(0x1D, MetaOp::ORA,  AddrMode::ABX, false, Some(MemAccess::Offset)),
	Opcode::new(0x1E, MetaOp::ASL,  AddrMode::ABX, false, Some(MemAccess::Offset)),
	Opcode::new(0x20, MetaOp::JSR,  AddrMode::LAB, true,  Some(MemAccess::Target)),
	Opcode::new(0x21, MetaOp::AND,  AddrMode::IZX, false, Some(MemAccess::Offset)),
	Opcode::new(0x24, MetaOp::BIT,  AddrMode::ZPG, false, Some(MemAccess::Read)  ),
	Opcode::new(0x25, MetaOp::AND,  AddrMode::ZPG, false, Some(MemAccess::Read)  ),
	Opcode::new(0x26, MetaOp::ROL,  AddrMode::ZPG, false, Some(MemAccess::Rmw)   ),
	Opcode::new(0x28, MetaOp::PLP,  AddrMode::IMP, false, None                   ),
	Opcode::new(0x29, MetaOp::AND,  AddrMode::IMM, false, None                   ),
	Opcode::new(0x2A, MetaOp::ROLA, AddrMode::IMP, false, None                   ),
	Opcode::new(0x2C, MetaOp::BIT,  AddrMode::ABS, false, Some(MemAccess::Read)  ),
	Opcode::new(0x2D, MetaOp::AND,  AddrMode::ABS, false, Some(MemAccess::Read)  ),
	Opcode::new(0x2E, MetaOp::ROL,  AddrMode::ABS, false, Some(MemAccess::Offset)),
	Opcode::new(0x30, MetaOp::BMI,  AddrMode::REL, true,  Some(MemAccess::Target)),
	Opcode::new(0x31, MetaOp::AND,  AddrMode::IZY, false, Some(MemAccess::Read)  ),
	Opcode::new(0x35, MetaOp::AND,  AddrMode::ZPX, false, Some(MemAccess::Offset)),
	Opcode::new(0x36, MetaOp::ROL,  AddrMode::ZPX, false, Some(MemAccess::Offset)),
	Opcode::new(0x38, MetaOp::SEC,  AddrMode::IMP, false, None                   ),
	Opcode::new(0x39, MetaOp::AND,  AddrMode::ABY, false, Some(MemAccess::Offset)),
	Opcode::new(0x3D, MetaOp::AND,  AddrMode::ABX, false, Some(MemAccess::Offset)),
	Opcode::new(0x3E, MetaOp::ROL,  AddrMode::ABX, false, Some(MemAccess::Offset)),
	Opcode::new(0x40, MetaOp::RTI,  AddrMode::IMP, true , None                   ),
	Opcode::new(0x41, MetaOp::EOR,  AddrMode::IZX, false, Some(MemAccess::Offset)),
	Opcode::new(0x45, MetaOp::EOR,  AddrMode::ZPG, false, Some(MemAccess::Read)  ),
	Opcode::new(0x46, MetaOp::LSR,  AddrMode::ZPG, false, Some(MemAccess::Rmw)   ),
	Opcode::new(0x48, MetaOp::PHA,  AddrMode::IMP, false, None                   ),
	Opcode::new(0x49, MetaOp::EOR,  AddrMode::IMM, false, None                   ),
	Opcode::new(0x4A, MetaOp::LSRA, AddrMode::IMP, false, None                   ),
	Opcode::new(0x4C, MetaOp::JMP,  AddrMode::LAB, true,  Some(MemAccess::Target)),
	Opcode::new(0x4D, MetaOp::EOR,  AddrMode::ABS, false, Some(MemAccess::Read)  ),
	Opcode::new(0x4E, MetaOp::LSR,  AddrMode::ABS, false, Some(MemAccess::Offset)),
	Opcode::new(0x50, MetaOp::BVC,  AddrMode::REL, true,  Some(MemAccess::Target)),
	Opcode::new(0x51, MetaOp::EOR,  AddrMode::IZY, false, Some(MemAccess::Read)  ),
	Opcode::new(0x55, MetaOp::EOR,  AddrMode::ZPX, false, Some(MemAccess::Offset)),
	Opcode::new(0x56, MetaOp::LSR,  AddrMode::ZPX, false, Some(MemAccess::Offset)),
	Opcode::new(0x58, MetaOp::CLI,  AddrMode::IMP, false, None                   ),
	Opcode::new(0x59, MetaOp::EOR,  AddrMode::ABY, false, Some(MemAccess::Offset)),
	Opcode::new(0x5D, MetaOp::EOR,  AddrMode::ABX, false, Some(MemAccess::Offset)),
	Opcode::new(0x5E, MetaOp::LSR,  AddrMode::ABX, false, Some(MemAccess::Offset)),
	Opcode::new(0x60, MetaOp::RTS,  AddrMode::IMP, true , None                   ),
	Opcode::new(0x61, MetaOp::ADC,  AddrMode::IZX, false, Some(MemAccess::Offset)),
	Opcode::new(0x65, MetaOp::ADC,  AddrMode::ZPG, false, Some(MemAccess::Read)  ),
	Opcode::new(0x66, MetaOp::ROR,  AddrMode::ZPG, false, Some(MemAccess::Rmw)   ),
	Opcode::new(0x68, MetaOp::PLA,  AddrMode::IMP, false, None                   ),
	Opcode::new(0x69, MetaOp::ADC,  AddrMode::IMM, false, None                   ),
	Opcode::new(0x6A, MetaOp::RORA, AddrMode::IMP, false, None                   ),
	Opcode::new(0x6C, MetaOp::JMP,  AddrMode::IND, true,  Some(MemAccess::Read)  ),
	Opcode::new(0x6D, MetaOp::ADC,  AddrMode::ABS, false, Some(MemAccess::Read)  ),
	Opcode::new(0x6E, MetaOp::ROR,  AddrMode::ABS, false, Some(MemAccess::Offset)),
	Opcode::new(0x70, MetaOp::BVS,  AddrMode::REL, true,  Some(MemAccess::Target)),
	Opcode::new(0x71, MetaOp::ADC,  AddrMode::IZY, false, Some(MemAccess::Read)  ),
	Opcode::new(0x75, MetaOp::ADC,  AddrMode::ZPX, false, Some(MemAccess::Offset)),
	Opcode::new(0x76, MetaOp::ROR,  AddrMode::ZPX, false, Some(MemAccess::Offset)),
	Opcode::new(0x78, MetaOp::SEI,  AddrMode::IMP, false, None                   ),
	Opcode::new(0x79, MetaOp::ADC,  AddrMode::ABY, false, Some(MemAccess::Offset)),
	Opcode::new(0x7D, MetaOp::ADC,  AddrMode::ABX, false, Some(MemAccess::Offset)),
	Opcode::new(0x7E, MetaOp::ROR,  AddrMode::ABX, false, Some(MemAccess::Offset)),
	Opcode::new(0x81, MetaOp::STA,  AddrMode::IZX, false, Some(MemAccess::Offset)),
	Opcode::new(0x84, MetaOp::STY,  AddrMode::ZPG, false, Some(MemAccess::Write) ),
	Opcode::new(0x85, MetaOp::STA,  AddrMode::ZPG, false, Some(MemAccess::Write) ),
	Opcode::new(0x86, MetaOp::STX,  AddrMode::ZPG, false, Some(MemAccess::Write) ),
	Opcode::new(0x88, MetaOp::DEY,  AddrMode::IMP, false, None                   ),
	Opcode::new(0x8A, MetaOp::TXA,  AddrMode::IMP, false, None                   ),
	Opcode::new(0x8C, MetaOp::STY,  AddrMode::ABS, false, Some(MemAccess::Write) ),
	Opcode::new(0x8D, MetaOp::STA,  AddrMode::ABS, false, Some(MemAccess::Read)  ),
	Opcode::new(0x8E, MetaOp::STX,  AddrMode::ABS, false, Some(MemAccess::Write) ),
	Opcode::new(0x90, MetaOp::BCC,  AddrMode::REL, true,  Some(MemAccess::Target)),
	Opcode::new(0x91, MetaOp::STA,  AddrMode::IZY, false, Some(MemAccess::Read)  ),
	Opcode::new(0x94, MetaOp::STY,  AddrMode::ZPX, false, Some(MemAccess::Offset)),
	Opcode::new(0x95, MetaOp::STA,  AddrMode::ZPX, false, Some(MemAccess::Offset)),
	Opcode::new(0x96, MetaOp::STX,  AddrMode::ZPY, false, Some(MemAccess::Offset)),
	Opcode::new(0x98, MetaOp::TYA,  AddrMode::IMP, false, None                   ),
	Opcode::new(0x99, MetaOp::STA,  AddrMode::ABY, false, Some(MemAccess::Offset)),
	Opcode::new(0x9A, MetaOp::TXS,  AddrMode::IMP, false, None                   ),
	Opcode::new(0x9D, MetaOp::STA,  AddrMode::ABX, false, Some(MemAccess::Offset)),
	Opcode::new(0xA0, MetaOp::LDYI, AddrMode::IMM, false, None                   ),
	Opcode::new(0xA1, MetaOp::LDA,  AddrMode::IZX, false, Some(MemAccess::Offset)),
	Opcode::new(0xA2, MetaOp::LDXI, AddrMode::IMM, false, None                   ),
	Opcode::new(0xA4, MetaOp::LDY,  AddrMode::ZPG, false, Some(MemAccess::Read)  ),
	Opcode::new(0xA5, MetaOp::LDA,  AddrMode::ZPG, false, Some(MemAccess::Read)  ),
	Opcode::new(0xA6, MetaOp::LDX,  AddrMode::ZPG, false, Some(MemAccess::Read)  ),
	Opcode::new(0xA8, MetaOp::TAY,  AddrMode::IMP, false, None                   ),
	Opcode::new(0xA9, MetaOp::LDAI, AddrMode::IMM, false, None                   ),
	Opcode::new(0xAA, MetaOp::TAX,  AddrMode::IMP, false, None                   ),
	Opcode::new(0xAC, MetaOp::LDY,  AddrMode::ABS, false, Some(MemAccess::Read)  ),
	Opcode::new(0xAD, MetaOp::LDA,  AddrMode::ABS, false, Some(MemAccess::Read)  ),
	Opcode::new(0xAE, MetaOp::LDX,  AddrMode::ABS, false, Some(MemAccess::Read)  ),
	Opcode::new(0xB0, MetaOp::BCS,  AddrMode::REL, true,  Some(MemAccess::Target)),
	Opcode::new(0xB1, MetaOp::LDA,  AddrMode::IZY, false, Some(MemAccess::Read)  ),
	Opcode::new(0xB4, MetaOp::LDY,  AddrMode::ZPX, false, Some(MemAccess::Offset)),
	Opcode::new(0xB5, MetaOp::LDA,  AddrMode::ZPX, false, Some(MemAccess::Offset)),
	Opcode::new(0xB6, MetaOp::LDX,  AddrMode::ZPY, false, Some(MemAccess::Offset)),
	Opcode::new(0xB8, MetaOp::CLV,  AddrMode::IMP, false, None                   ),
	Opcode::new(0xB9, MetaOp::LDA,  AddrMode::ABY, false, Some(MemAccess::Offset)),
	Opcode::new(0xBA, MetaOp::TSX,  AddrMode::IMP, false, None                   ),
	Opcode::new(0xBC, MetaOp::LDY,  AddrMode::ABX, false, Some(MemAccess::Offset)),
	Opcode::new(0xBD, MetaOp::LDA,  AddrMode::ABX, false, Some(MemAccess::Offset)),
	Opcode::new(0xBE, MetaOp::LDX,  AddrMode::ABY, false, Some(MemAccess::Offset)),
	Opcode::new(0xC0, MetaOp::CPY,  AddrMode::IMM, false, None                   ),
	Opcode::new(0xC1, MetaOp::CMP,  AddrMode::IZX, false, Some(MemAccess::Offset)),
	Opcode::new(0xC4, MetaOp::CPY,  AddrMode::ZPG, false, Some(MemAccess::Read)  ),
	Opcode::new(0xC5, MetaOp::CMP,  AddrMode::ZPG, false, Some(MemAccess::Read)  ),
	Opcode::new(0xC6, MetaOp::DEC,  AddrMode::ZPG, false, Some(MemAccess::Rmw)   ),
	Opcode::new(0xC8, MetaOp::INY,  AddrMode::IMP, false, None                   ),
	Opcode::new(0xC9, MetaOp::CMP,  AddrMode::IMM, false, None                   ),
	Opcode::new(0xCA, MetaOp::DEX,  AddrMode::IMP, false, None                   ),
	Opcode::new(0xCC, MetaOp::CPY,  AddrMode::ABS, false, Some(MemAccess::Read)  ),
	Opcode::new(0xCD, MetaOp::CMP,  AddrMode::ABS, false, Some(MemAccess::Read)  ),
	Opcode::new(0xCE, MetaOp::DEC,  AddrMode::ABS, false, Some(MemAccess::Offset)),
	Opcode::new(0xD0, MetaOp::BNE,  AddrMode::REL, true,  Some(MemAccess::Target)),
	Opcode::new(0xD1, MetaOp::CMP,  AddrMode::IZY, false, Some(MemAccess::Read)  ),
	Opcode::new(0xD5, MetaOp::CMP,  AddrMode::ZPX, false, Some(MemAccess::Offset)),
	Opcode::new(0xD6, MetaOp::DEC,  AddrMode::ZPX, false, Some(MemAccess::Offset)),
	Opcode::new(0xD8, MetaOp::CLD,  AddrMode::IMP, false, None                   ),
	Opcode::new(0xD9, MetaOp::CMP,  AddrMode::ABY, false, Some(MemAccess::Offset)),
	Opcode::new(0xDD, MetaOp::CMP,  AddrMode::ABX, false, Some(MemAccess::Offset)),
	Opcode::new(0xDE, MetaOp::DEC,  AddrMode::ABX, false, Some(MemAccess::Offset)),
	Opcode::new(0xE0, MetaOp::CPX,  AddrMode::IMM, false, None                   ),
	Opcode::new(0xE1, MetaOp::SBC,  AddrMode::IZX, false, Some(MemAccess::Offset)),
	Opcode::new(0xE4, MetaOp::CPX,  AddrMode::ZPG, false, Some(MemAccess::Read)  ),
	Opcode::new(0xE5, MetaOp::SBC,  AddrMode::ZPG, false, Some(MemAccess::Read)  ),
	Opcode::new(0xE6, MetaOp::INC,  AddrMode::ZPG, false, Some(MemAccess::Rmw)   ),
	Opcode::new(0xE8, MetaOp::INX,  AddrMode::IMP, false, None                   ),
	Opcode::new(0xE9, MetaOp::SBC,  AddrMode::IMM, false, None                   ),
	Opcode::new(0xEA, MetaOp::NOP,  AddrMode::IMP, false, None                   ),
	Opcode::new(0xEC, MetaOp::CPX,  AddrMode::ABS, false, Some(MemAccess::Read)  ),
	Opcode::new(0xED, MetaOp::SBC,  AddrMode::ABS, false, Some(MemAccess::Read)  ),
	Opcode::new(0xEE, MetaOp::INC,  AddrMode::ABS, false, Some(MemAccess::Offset)),
	Opcode::new(0xF0, MetaOp::BEQ,  AddrMode::REL, true,  Some(MemAccess::Target)),
	Opcode::new(0xF1, MetaOp::SBC,  AddrMode::IZY, false, Some(MemAccess::Read)  ),
	Opcode::new(0xF5, MetaOp::SBC,  AddrMode::ZPX, false, Some(MemAccess::Offset)),
	Opcode::new(0xF6, MetaOp::INC,  AddrMode::ZPX, false, Some(MemAccess::Offset)),
	Opcode::new(0xF8, MetaOp::SED,  AddrMode::IMP, false, None                   ),
	Opcode::new(0xF9, MetaOp::SBC,  AddrMode::ABY, false, Some(MemAccess::Offset)),
	Opcode::new(0xFD, MetaOp::SBC,  AddrMode::ABX, false, Some(MemAccess::Offset)),
	Opcode::new(0xFE, MetaOp::INC,  AddrMode::ABX, false, Some(MemAccess::Offset)),
];

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn opcode_lookup() {
		assert_eq!(lookup_opcode(0x00).meta_op, MetaOp::BRK);
		assert_eq!(lookup_opcode(0x01).meta_op, MetaOp::ORA);
		assert_eq!(lookup_opcode(0xFE).meta_op, MetaOp::INC);
		assert_eq!(lookup_opcode(0x72).meta_op, MetaOp::UNK);
		assert_eq!(lookup_opcode(0xFF).meta_op, MetaOp::UNK);
	}

	#[test]
	fn mnemonics() {
		assert_eq!(MetaOp::BRK.mnemonic_old(),  "brk");
		assert_eq!(MetaOp::BRK.mnemonic_new(),  "brk");
		assert_eq!(MetaOp::LDA.mnemonic_old(),  "lda");
		assert_eq!(MetaOp::LDA.mnemonic_new(),  "ld  a,");
		assert_eq!(MetaOp::LDAI.mnemonic_old(), "lda");
		assert_eq!(MetaOp::LDAI.mnemonic_new(), "li  a,");
		assert_eq!(MetaOp::UNK.mnemonic_old(),  "???");
		assert_eq!(MetaOp::UNK.mnemonic_new(),  "???");
	}

	fn check_disas(va: usize, img: &[u8], meta_op: MetaOp, ops: &[Operand]) {
		let va = VAddr(va);
		match Disassembler.disas_instr(img, 0, va) {
			Ok(inst) => {
				let mut operands = Operands::new();
				for op in ops { operands.push(*op); }

				assert_eq!(inst.va, va);
				assert_eq!(inst.opcode.meta_op, meta_op);
				assert_eq!(inst.ops, operands);
			}

			Err(e) => {
				panic!("failed to disassemble: {}", e);
			}
		}
	}

	fn check_fail(va: usize, img: &[u8], expected: DisasError) {
		let va = VAddr(va);
		match Disassembler.disas_instr(img, 0, va) {
			Ok(inst) => {
				panic!("should have failed disassembling {:?}, but got {:?}", img, inst);
			}

			Err(e) => {
				assert_eq!(e, expected);
			}
		}
	}

	#[test]
	fn disasm_success() {
		use MetaOp::*;
		use Operand::*;
		use MemAccess::*;

		check_disas(0, &[0x00],               BRK,  &[]);
		check_disas(0, &[0xA9, 0xEF],         LDAI, &[Imm(0xEF)]);
		check_disas(0, &[0x6D, 0x56, 0x34],   ADC,  &[Mem(0x3456, Read)]);
		check_disas(0, &[0x84, 0x33],         STY,  &[Mem(0x0033, Write)]);
		check_disas(0, &[0x06, 0x99],         ASL,  &[Mem(0x0099, Rmw)]);
		check_disas(0, &[0x2E, 0xAA, 0x99],   ROL,  &[Mem(0x99AA, Offset)]);
		check_disas(0, &[0x4C, 0xFE, 0xFF],   JMP,  &[Mem(0xFFFE, Target)]);
		check_disas(0, &[0x6C, 0xFE, 0xFF],   JMP,  &[Mem(0xFFFE, Read)]);
		check_disas(3, &[0x90, 10],           BCC,  &[Mem(3 + 10 + 2, Target)]);
		check_disas(8, &[0x90, (-5i8) as u8], BCC,  &[Mem(8 - 5 + 2,  Target)]);
	}

	#[test]
	fn disasm_failure() {
		// offset == end of image
		check_fail(0, &[], out_of_bytes(0, VAddr(0), 1, 0));

		// bad opcode
		check_fail(0, &[0xCB], unknown_instruction(0, VAddr(0)));
		check_fail(0, &[0xFF], unknown_instruction(0, VAddr(0)));

		// 1 operand byte
		check_fail(0, &[0xA9], out_of_bytes(0, VAddr(0), 2, 1));

		// 2 operand bytes
		check_fail(0, &[0x4C], out_of_bytes(0, VAddr(0), 3, 1));
		check_fail(0, &[0x4C, 0x00], out_of_bytes(0, VAddr(0), 3, 2));
	}
}