
use crate::program::{ MemAccess, InstructionKind };

// ------------------------------------------------------------------------------------------------
// Reg
// ------------------------------------------------------------------------------------------------

#[repr(u8)]
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub(super) enum Reg {
	A, F, B, C, D, E, H, L,
	BC, DE, HL, SP,
}

impl Default for Reg {
	fn default() -> Reg { Reg::A }
}

impl Reg {
	pub(super) fn register_names() -> &'static [&'static str] {
		&["a", "f", "b", "c", "d", "e", "h", "l",
		"bc", "de", "hl", "sp"]
	}
}

// ------------------------------------------------------------------------------------------------
// MetaOp
// ------------------------------------------------------------------------------------------------

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub(super) enum MetaOp {
	UNK,
	ADC,  ADD, AND, BIT, CALL, CCF,  CP,   CPL,
	DA,   DEC, DI,  EI,  HALT, INC,  JP,   JR,
	LD,   LDH, NOP, OR,  POP,  PUSH, RES,  RET,
	RETI, RL,  RLC, RR,  RRC,  RST,  SBC,  SCF,
	SET,  SLA, SRA, SRL, STOP, SUB,  SWAP, XOR,
}

impl Default for MetaOp {
	fn default() -> Self { Self::UNK }
}

impl MetaOp {
	/// Instruction mnemonics
	pub(super) fn mnemonic(&self) -> &'static str {
		use MetaOp::*;
		match self {
			UNK  => "???",
			ADC  => "adc",  ADD  => "add",  AND  => "and",  BIT => "bit",
			CALL => "call", CCF  => "ccf",  CP   => "cp",   CPL => "cpl",
			DA   => "da",   DEC  => "dec",  DI   => "di",   EI  => "ei",
			HALT => "halt", INC  => "inc",  JP   => "jp",   JR  => "jr",
			LD   => "ld",   LDH  => "ldh",  NOP  => "nop",  OR  => "or",
			POP  => "pop",  PUSH => "push", RES  => "res",  RET => "ret",
			RETI => "reti", RL   => "rl",   RLC  => "rlc",  RR  => "rr",
			RRC  => "rrc",  RST  => "rst",  SBC  => "sbc",  SCF => "scf",
			SET  => "set",  SLA  => "sla",  SRA  => "sra",  SRL => "srl",
			STOP => "stop", SUB  => "sub",  SWAP => "swap", XOR => "xor",
		}
	}
}

// ------------------------------------------------------------------------------------------------
// GBOpKind
// ------------------------------------------------------------------------------------------------

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub(super) enum GBOpKind {
	Imp,                 // ld a, b
	Dummy,               // stop
	UImm8,               // ld a, imm
	SImm8,               // add sp, imm
	Imm16,               // ld hl, imm

	Rel,                 // jr offs; access always Target
	SPImm,               // ld hl, [sp+imm]; access always R
	Add16(MemAccess),    // could be R, W, or Target
	AddHi(MemAccess),    // ldh
	Ind(Reg, MemAccess), // [bc], [de], [hl]
	IndHi(MemAccess),    // [0xFF00 + c]

	LdHlImm,             // `ld [hl], imm` - only instruction with TWO operands
}

impl GBOpKind {
	pub(super) fn op_bytes(&self) -> usize {
		use GBOpKind::*;

		match self {
			Imp | Ind(..) | IndHi(..)                                 => 0,
			Dummy | UImm8 | SImm8 | SPImm | AddHi(..) | Rel | LdHlImm => 1,
			Imm16 | Add16(..)                                         => 2,
		}
	}

	pub(super) fn access(&self) -> Option<MemAccess> {
		use GBOpKind::*;

		match self {
			Ind(_, a) | AddHi(a) | Add16(a) => Some(*a),
			LdHlImm                         => Some(MemAccess::W),
			SPImm                           => Some(MemAccess::R),
			Rel                             => Some(MemAccess::Target),
			_                               => None,
		}
	}
}

// ------------------------------------------------------------------------------------------------
// InstDesc
// ------------------------------------------------------------------------------------------------

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub(super) struct InstDesc (
	u16,             // opcode
	MetaOp,          // meta_op
	&'static str,    // op_templ
	InstructionKind, // kind
	GBOpKind,        // op_kind
);

impl InstDesc {
	#[inline] pub(super) fn opcode  (&self) -> u16             { self.0 }
	#[inline] pub(super) fn meta_op (&self) -> MetaOp          { self.1 }
	#[inline] pub(super) fn op_templ(&self) -> &'static str    { self.2 }
	#[inline] pub(super) fn kind    (&self) -> InstructionKind { self.3 }
	#[inline] pub(super) fn op_kind (&self) -> GBOpKind        { self.4 }

	pub(super) fn access(&self) -> Option<MemAccess> {
		self.op_kind().access()
	}

	pub(super) fn mnemonic(&self) -> &'static str {
		self.meta_op().mnemonic()
	}

	pub(super) fn op_bytes(&self) -> usize {
		self.op_kind().op_bytes()
	}

	pub(super) fn inst_size(&self) -> usize {
		if self.opcode() > 0xFF {
			2
		} else {
			self.op_bytes() + 1
		}
	}

	pub(super) fn rst_target(&self) -> Option<u16> {
		match self.opcode() {
			0xC7 => Some(0x0000),
			0xCF => Some(0x0008),
			0xD7 => Some(0x0010),
			0xDF => Some(0x0018),
			0xE7 => Some(0x0020),
			0xEF => Some(0x0028),
			0xF7 => Some(0x0030),
			0xFF => Some(0x0038),
			_    => None,
		}
	}
}

// ------------------------------------------------------------------------------------------------
// InstDesc tables
// ------------------------------------------------------------------------------------------------

pub(super) fn lookup_desc(opcode: u8) -> Option<InstDesc> {
	assert!(opcode != 0xCB);
	let ret = INST_DESCS[opcode as usize];
	if ret.meta_op() == UNK { None } else { Some(ret) }
}

pub(super) fn lookup_desc_cb(opcode: u8) -> InstDesc {
	INST_DESCS_CB[opcode as usize]
}

use GBOpKind::*;
use InstructionKind::*;
use MemAccess::*;
use MetaOp::*;

const INVALID: InstDesc = InstDesc(0x00, UNK, "", Other, Imp);

const INST_DESCS: &[InstDesc] = &[
	InstDesc(   0x00, NOP,  "",            Other,  Imp),
	InstDesc(   0x01, LD,   "bc, {}",      Other,  Imm16),
	InstDesc(   0x02, LD,   "[bc], a",     Other,  Ind(Reg::BC, W)),
	InstDesc(   0x03, INC,  "bc",          Other,  Imp),
	InstDesc(   0x04, INC,  "b",           Other,  Imp),
	InstDesc(   0x05, DEC,  "b",           Other,  Imp),
	InstDesc(   0x06, LD,   "b, {}",       Other,  UImm8),
	InstDesc(   0x07, RLC,  "a",           Other,  Imp),
	InstDesc(   0x08, LD,   "[{}], sp",    Other,  Add16(W)),
	InstDesc(   0x09, ADD,  "hl, bc",      Other,  Imp),
	InstDesc(   0x0A, LD,   "a, [bc]",     Other,  Ind(Reg::BC, R)),
	InstDesc(   0x0B, DEC,  "bc",          Other,  Imp),
	InstDesc(   0x0C, INC,  "c",           Other,  Imp),
	InstDesc(   0x0D, DEC,  "c",           Other,  Imp),
	InstDesc(   0x0E, LD,   "c, {}",       Other,  UImm8),
	InstDesc(   0x0F, RRC,  "a",           Other,  Imp),
	InstDesc(   0x10, STOP, "",            Other,  Dummy),
	InstDesc(   0x11, LD,   "de, {}",      Other,  Imm16),
	InstDesc(   0x12, LD,   "[de], a",     Other,  Ind(Reg::DE, W)),
	InstDesc(   0x13, INC,  "de",          Other,  Imp),
	InstDesc(   0x14, INC,  "d",           Other,  Imp),
	InstDesc(   0x15, DEC,  "d",           Other,  Imp),
	InstDesc(   0x16, LD,   "d, {}",       Other,  UImm8),
	InstDesc(   0x17, RL,   "a",           Other,  Imp),
	InstDesc(   0x18, JR,   "{}",          Uncond, Rel),
	InstDesc(   0x19, ADD,  "hl, de",      Other,  Imp),
	InstDesc(   0x1A, LD,   "a, [de]",     Other,  Ind(Reg::DE, R)),
	InstDesc(   0x1B, DEC,  "de",          Other,  Imp),
	InstDesc(   0x1C, INC,  "e",           Other,  Imp),
	InstDesc(   0x1D, DEC,  "e",           Other,  Imp),
	InstDesc(   0x1E, LD,   "e, {}",       Other,  UImm8),
	InstDesc(   0x1F, RR,   "a",           Other,  Imp),
	InstDesc(   0x20, JR,   "nz, {}",      Cond,   Rel),
	InstDesc(   0x21, LD,   "hl, {}",      Other,  Imm16),
	InstDesc(   0x22, LD,   "[hl+], a",    Other,  Ind(Reg::HL, W)),
	InstDesc(   0x23, INC,  "hl",          Other,  Imp),
	InstDesc(   0x24, INC,  "h",           Other,  Imp),
	InstDesc(   0x25, DEC,  "h",           Other,  Imp),
	InstDesc(   0x26, LD,   "h, {}",       Other,  UImm8),
	InstDesc(   0x27, DA,   "a",           Other,  Imp),
	InstDesc(   0x28, JR,   "z, {}",       Cond,   Rel),
	InstDesc(   0x29, ADD,  "hl, hl",      Other,  Imp),
	InstDesc(   0x2A, LD,   "a, [hl+]",    Other,  Ind(Reg::HL, R)),
	InstDesc(   0x2B, DEC,  "hl",          Other,  Imp),
	InstDesc(   0x2C, INC,  "l",           Other,  Imp),
	InstDesc(   0x2D, DEC,  "l",           Other,  Imp),
	InstDesc(   0x2E, LD,   "l, {}",       Other,  UImm8),
	InstDesc(   0x2F, CPL,  "a",           Other,  Imp),
	InstDesc(   0x30, JR,   "nc, {}",      Cond,   Rel),
	InstDesc(   0x31, LD,   "sp, {}",      Other,  Imm16),
	InstDesc(   0x32, LD,   "[hl-], a",    Other,  Ind(Reg::HL, W)),
	InstDesc(   0x33, INC,  "sp",          Other,  Imp),
	InstDesc(   0x34, INC,  "[hl]",        Other,  Ind(Reg::HL, RW)),
	InstDesc(   0x35, DEC,  "[hl]",        Other,  Ind(Reg::HL, RW)),
	InstDesc(   0x36, LD,   "[hl], {}",    Other,  LdHlImm),
	InstDesc(   0x37, SCF,  "",            Other,  Imp),
	InstDesc(   0x38, JR,   "c, {}",       Cond,   Rel),
	InstDesc(   0x39, ADD,  "hl, sp",      Other,  Imp),
	InstDesc(   0x3A, LD,   "a, [hl-]",    Other,  Ind(Reg::HL, R)),
	InstDesc(   0x3B, DEC,  "sp",          Other,  Imp),
	InstDesc(   0x3C, INC,  "a",           Other,  Imp),
	InstDesc(   0x3D, DEC,  "a",           Other,  Imp),
	InstDesc(   0x3E, LD,   "a, {}",       Other,  UImm8),
	InstDesc(   0x3F, CCF,  "",            Other,  Imp),
	InstDesc(   0x40, LD,   "b, b",        Other,  Imp),
	InstDesc(   0x41, LD,   "b, c",        Other,  Imp),
	InstDesc(   0x42, LD,   "b, d",        Other,  Imp),
	InstDesc(   0x43, LD,   "b, e",        Other,  Imp),
	InstDesc(   0x44, LD,   "b, h",        Other,  Imp),
	InstDesc(   0x45, LD,   "b, l",        Other,  Imp),
	InstDesc(   0x46, LD,   "b, [hl]",     Other,  Ind(Reg::HL, R)),
	InstDesc(   0x47, LD,   "b, a",        Other,  Imp),
	InstDesc(   0x48, LD,   "c, b",        Other,  Imp),
	InstDesc(   0x49, LD,   "c, c",        Other,  Imp),
	InstDesc(   0x4A, LD,   "c, d",        Other,  Imp),
	InstDesc(   0x4B, LD,   "c, e",        Other,  Imp),
	InstDesc(   0x4C, LD,   "c, h",        Other,  Imp),
	InstDesc(   0x4D, LD,   "c, l",        Other,  Imp),
	InstDesc(   0x4E, LD,   "c, [hl]",     Other,  Ind(Reg::HL, R)),
	InstDesc(   0x4F, LD,   "c, a",        Other,  Imp),
	InstDesc(   0x50, LD,   "d, b",        Other,  Imp),
	InstDesc(   0x51, LD,   "d, c",        Other,  Imp),
	InstDesc(   0x52, LD,   "d, d",        Other,  Imp),
	InstDesc(   0x53, LD,   "d, e",        Other,  Imp),
	InstDesc(   0x54, LD,   "d, h",        Other,  Imp),
	InstDesc(   0x55, LD,   "d, l",        Other,  Imp),
	InstDesc(   0x56, LD,   "d, [hl]",     Other,  Ind(Reg::HL, R)),
	InstDesc(   0x57, LD,   "d, a",        Other,  Imp),
	InstDesc(   0x58, LD,   "e, b",        Other,  Imp),
	InstDesc(   0x59, LD,   "e, c",        Other,  Imp),
	InstDesc(   0x5A, LD,   "e, d",        Other,  Imp),
	InstDesc(   0x5B, LD,   "e, e",        Other,  Imp),
	InstDesc(   0x5C, LD,   "e, h",        Other,  Imp),
	InstDesc(   0x5D, LD,   "e, l",        Other,  Imp),
	InstDesc(   0x5E, LD,   "e, [hl]",     Other,  Ind(Reg::HL, R)),
	InstDesc(   0x5F, LD,   "e, a",        Other,  Imp),
	InstDesc(   0x60, LD,   "h, b",        Other,  Imp),
	InstDesc(   0x61, LD,   "h, c",        Other,  Imp),
	InstDesc(   0x62, LD,   "h, d",        Other,  Imp),
	InstDesc(   0x63, LD,   "h, e",        Other,  Imp),
	InstDesc(   0x64, LD,   "h, h",        Other,  Imp),
	InstDesc(   0x65, LD,   "h, l",        Other,  Imp),
	InstDesc(   0x66, LD,   "h, [hl]",     Other,  Ind(Reg::HL, R)),
	InstDesc(   0x67, LD,   "h, a",        Other,  Imp),
	InstDesc(   0x68, LD,   "l, b",        Other,  Imp),
	InstDesc(   0x69, LD,   "l, c",        Other,  Imp),
	InstDesc(   0x6A, LD,   "l, d",        Other,  Imp),
	InstDesc(   0x6B, LD,   "l, e",        Other,  Imp),
	InstDesc(   0x6C, LD,   "l, h",        Other,  Imp),
	InstDesc(   0x6D, LD,   "l, l",        Other,  Imp),
	InstDesc(   0x6E, LD,   "l, [hl]",     Other,  Ind(Reg::HL, R)),
	InstDesc(   0x6F, LD,   "l, a",        Other,  Imp),
	InstDesc(   0x70, LD,   "[hl], b",     Other,  Ind(Reg::HL, W)),
	InstDesc(   0x71, LD,   "[hl], c",     Other,  Ind(Reg::HL, W)),
	InstDesc(   0x72, LD,   "[hl], d",     Other,  Ind(Reg::HL, W)),
	InstDesc(   0x73, LD,   "[hl], e",     Other,  Ind(Reg::HL, W)),
	InstDesc(   0x74, LD,   "[hl], h",     Other,  Ind(Reg::HL, W)),
	InstDesc(   0x75, LD,   "[hl], l",     Other,  Ind(Reg::HL, W)),
	InstDesc(   0x76, HALT, "",            Other,  Imp),
	InstDesc(   0x77, LD,   "[hl], a",     Other,  Ind(Reg::HL, W)),
	InstDesc(   0x78, LD,   "a, b",        Other,  Imp),
	InstDesc(   0x79, LD,   "a, c",        Other,  Imp),
	InstDesc(   0x7A, LD,   "a, d",        Other,  Imp),
	InstDesc(   0x7B, LD,   "a, e",        Other,  Imp),
	InstDesc(   0x7C, LD,   "a, h",        Other,  Imp),
	InstDesc(   0x7D, LD,   "a, l",        Other,  Imp),
	InstDesc(   0x7E, LD,   "a, [hl]",     Other,  Ind(Reg::HL, R)),
	InstDesc(   0x7F, LD,   "a, a",        Other,  Imp),
	InstDesc(   0x80, ADD,  "a, b",        Other,  Imp),
	InstDesc(   0x81, ADD,  "a, c",        Other,  Imp),
	InstDesc(   0x82, ADD,  "a, d",        Other,  Imp),
	InstDesc(   0x83, ADD,  "a, e",        Other,  Imp),
	InstDesc(   0x84, ADD,  "a, h",        Other,  Imp),
	InstDesc(   0x85, ADD,  "a, l",        Other,  Imp),
	InstDesc(   0x86, ADD,  "a, [hl]",     Other,  Ind(Reg::HL, R)),
	InstDesc(   0x87, ADD,  "a, a",        Other,  Imp),
	InstDesc(   0x88, ADC,  "a, b",        Other,  Imp),
	InstDesc(   0x89, ADC,  "a, c",        Other,  Imp),
	InstDesc(   0x8A, ADC,  "a, d",        Other,  Imp),
	InstDesc(   0x8B, ADC,  "a, e",        Other,  Imp),
	InstDesc(   0x8C, ADC,  "a, h",        Other,  Imp),
	InstDesc(   0x8D, ADC,  "a, l",        Other,  Imp),
	InstDesc(   0x8E, ADC,  "a, [hl]",     Other,  Ind(Reg::HL, R)),
	InstDesc(   0x8F, ADC,  "a, a",        Other,  Imp),
	InstDesc(   0x90, SUB,  "a, b",        Other,  Imp),
	InstDesc(   0x91, SUB,  "a, c",        Other,  Imp),
	InstDesc(   0x92, SUB,  "a, d",        Other,  Imp),
	InstDesc(   0x93, SUB,  "a, e",        Other,  Imp),
	InstDesc(   0x94, SUB,  "a, h",        Other,  Imp),
	InstDesc(   0x95, SUB,  "a, l",        Other,  Imp),
	InstDesc(   0x96, SUB,  "a, [hl]",     Other,  Ind(Reg::HL, R)),
	InstDesc(   0x97, SUB,  "a, a",        Other,  Imp),
	InstDesc(   0x98, SBC,  "a, b",        Other,  Imp),
	InstDesc(   0x99, SBC,  "a, c",        Other,  Imp),
	InstDesc(   0x9A, SBC,  "a, d",        Other,  Imp),
	InstDesc(   0x9B, SBC,  "a, e",        Other,  Imp),
	InstDesc(   0x9C, SBC,  "a, h",        Other,  Imp),
	InstDesc(   0x9D, SBC,  "a, l",        Other,  Imp),
	InstDesc(   0x9E, SBC,  "a, [hl]",     Other,  Ind(Reg::HL, R)),
	InstDesc(   0x9F, SBC,  "a, a",        Other,  Imp),
	InstDesc(   0xA0, AND,  "a, b",        Other,  Imp),
	InstDesc(   0xA1, AND,  "a, c",        Other,  Imp),
	InstDesc(   0xA2, AND,  "a, d",        Other,  Imp),
	InstDesc(   0xA3, AND,  "a, e",        Other,  Imp),
	InstDesc(   0xA4, AND,  "a, h",        Other,  Imp),
	InstDesc(   0xA5, AND,  "a, l",        Other,  Imp),
	InstDesc(   0xA6, AND,  "a, [hl]",     Other,  Ind(Reg::HL, R)),
	InstDesc(   0xA7, AND,  "a, a",        Other,  Imp),
	InstDesc(   0xA8, XOR,  "a, b",        Other,  Imp),
	InstDesc(   0xA9, XOR,  "a, c",        Other,  Imp),
	InstDesc(   0xAA, XOR,  "a, d",        Other,  Imp),
	InstDesc(   0xAB, XOR,  "a, e",        Other,  Imp),
	InstDesc(   0xAC, XOR,  "a, h",        Other,  Imp),
	InstDesc(   0xAD, XOR,  "a, l",        Other,  Imp),
	InstDesc(   0xAE, XOR,  "a, [hl]",     Other,  Ind(Reg::HL, R)),
	InstDesc(   0xAF, XOR,  "a, a",        Other,  Imp),
	InstDesc(   0xB0, OR,   "a, b",        Other,  Imp),
	InstDesc(   0xB1, OR,   "a, c",        Other,  Imp),
	InstDesc(   0xB2, OR,   "a, d",        Other,  Imp),
	InstDesc(   0xB3, OR,   "a, e",        Other,  Imp),
	InstDesc(   0xB4, OR,   "a, h",        Other,  Imp),
	InstDesc(   0xB5, OR,   "a, l",        Other,  Imp),
	InstDesc(   0xB6, OR,   "a, [hl]",     Other,  Ind(Reg::HL, R)),
	InstDesc(   0xB7, OR,   "a, a",        Other,  Imp),
	InstDesc(   0xB8, CP,   "a, b",        Other,  Imp),
	InstDesc(   0xB9, CP,   "a, c",        Other,  Imp),
	InstDesc(   0xBA, CP,   "a, d",        Other,  Imp),
	InstDesc(   0xBB, CP,   "a, e",        Other,  Imp),
	InstDesc(   0xBC, CP,   "a, h",        Other,  Imp),
	InstDesc(   0xBD, CP,   "a, l",        Other,  Imp),
	InstDesc(   0xBE, CP,   "a, [hl]",     Other,  Ind(Reg::HL, R)),
	InstDesc(   0xBF, CP,   "a, a",        Other,  Imp),
	InstDesc(   0xC0, RET,  "nz",          Ret,    Imp),
	InstDesc(   0xC1, POP,  "bc",          Other,  Ind(Reg::SP, R)),
	InstDesc(   0xC2, JP,   "nz, {}",      Cond,   Add16(Target)),
	InstDesc(   0xC3, JP,   "{}",          Uncond, Add16(Target)),
	InstDesc(   0xC4, CALL, "nz, {}",      Call,   Add16(Target)),
	InstDesc(   0xC5, PUSH, "bc",          Other,  Ind(Reg::SP, W)),
	InstDesc(   0xC6, ADD,  "a, {}",       Other,  UImm8),
	InstDesc(   0xC7, RST,  "0x00",        Call,   Imp),
	InstDesc(   0xC8, RET,  "z",           Ret,    Imp),
	InstDesc(   0xC9, RET,  "",            Ret,    Imp),
	InstDesc(   0xCA, JP,   "z, {}",       Cond,   Add16(Target)),
	INVALID,
	InstDesc(   0xCC, CALL, "z, {}",       Call,   Add16(Target)),
	InstDesc(   0xCD, CALL, "{}",          Call,   Add16(Target)),
	InstDesc(   0xCE, ADC,  "a, {}",       Other,  UImm8),
	InstDesc(   0xCF, RST,  "0x08",        Call,   Imp),
	InstDesc(   0xD0, RET,  "nc",          Ret,    Imp),
	InstDesc(   0xD1, POP,  "de",          Other,  Ind(Reg::SP, R)),
	InstDesc(   0xD2, JP,   "nc, {}",      Cond,   Add16(Target)),
	INVALID,
	InstDesc(   0xD4, CALL, "nc, {}",      Call,   Add16(Target)),
	InstDesc(   0xD5, PUSH, "de",          Other,  Ind(Reg::SP, W)),
	InstDesc(   0xD6, SUB,  "a, {}",       Other,  UImm8),
	InstDesc(   0xD7, RST,  "0x10",        Call,   Imp),
	InstDesc(   0xD8, RET,  "c",           Ret,    Imp),
	InstDesc(   0xD9, RETI, "",            Ret,    Imp),
	InstDesc(   0xDA, JP,   "c, {}",       Cond,   Add16(Target)),
	INVALID,
	InstDesc(   0xDC, CALL, "c, {}",       Call,   Add16(Target)),
	INVALID,
	InstDesc(   0xDE, SBC,  "a, {}",       Other,  UImm8),
	InstDesc(   0xDF, RST,  "0x18",        Call,   Imp),
	InstDesc(   0xE0, LDH,  "[{}], a",     Other,  AddHi(W)),
	InstDesc(   0xE1, POP,  "hl",          Other,  Ind(Reg::SP, R)),
	InstDesc(   0xE2, LDH,  "[c], a",      Other,  IndHi(W)),
	INVALID,
	INVALID,
	InstDesc(   0xE5, PUSH, "hl",          Other,  Ind(Reg::SP, W)),
	InstDesc(   0xE6, AND,  "a, {}",       Other,  UImm8),
	InstDesc(   0xE7, RST,  "0x20",        Call,   Imp),
	InstDesc(   0xE8, ADD,  "sp, {}",      Other,  SImm8),
	InstDesc(   0xE9, JP,   "hl",          Indir,  Imp),
	InstDesc(   0xEA, LD,   "[{}], a",     Other,  Add16(W)),
	INVALID,
	INVALID,
	INVALID,
	InstDesc(   0xEE, XOR,  "a, {}",       Other,  UImm8),
	InstDesc(   0xEF, RST,  "0x28",        Call,   Imp),
	InstDesc(   0xF0, LDH,  "a, [{}]",     Other,  AddHi(R)),
	InstDesc(   0xF1, POP,  "af",          Other,  Ind(Reg::SP, R)),
	InstDesc(   0xF2, LDH,  "a, [c]",      Other,  IndHi(R)),
	InstDesc(   0xF3, DI,   "",            Other,  Imp),
	INVALID,
	InstDesc(   0xF5, PUSH, "a",           Other,  Ind(Reg::SP, W)),
	InstDesc(   0xF6, OR,   "a, {}",       Other,  UImm8),
	InstDesc(   0xF7, RST,  "0x30",        Call,   Imp),
	InstDesc(   0xF8, LD,   "hl, [sp{}]",  Other,  SPImm),
	InstDesc(   0xF9, LD,   "sp, hl",      Other,  Imp),
	InstDesc(   0xFA, LD,   "a, [{}]",     Other,  Add16(R)),
	InstDesc(   0xFB, EI,   "",            Other,  Imp),
	INVALID,
	INVALID,
	InstDesc(   0xFE, CP,   "a, {}",       Other,  UImm8),
	InstDesc(   0xFF, RST,  "0x38",        Call,   Imp),
];

const INST_DESCS_CB: &[InstDesc] = &[
	InstDesc(0xCB_00, RLC,  "b",           Other,  Imp),
	InstDesc(0xCB_01, RLC,  "c",           Other,  Imp),
	InstDesc(0xCB_02, RLC,  "d",           Other,  Imp),
	InstDesc(0xCB_03, RLC,  "e",           Other,  Imp),
	InstDesc(0xCB_04, RLC,  "h",           Other,  Imp),
	InstDesc(0xCB_05, RLC,  "l",           Other,  Imp),
	InstDesc(0xCB_06, RLC,  "[hl]",        Other,  Ind(Reg::HL, RW)),
	InstDesc(0xCB_07, RLC,  "a",           Other,  Imp),
	InstDesc(0xCB_08, RRC,  "b",           Other,  Imp),
	InstDesc(0xCB_09, RRC,  "c",           Other,  Imp),
	InstDesc(0xCB_0A, RRC,  "d",           Other,  Imp),
	InstDesc(0xCB_0B, RRC,  "e",           Other,  Imp),
	InstDesc(0xCB_0C, RRC,  "h",           Other,  Imp),
	InstDesc(0xCB_0D, RRC,  "l",           Other,  Imp),
	InstDesc(0xCB_0E, RRC,  "[hl]",        Other,  Ind(Reg::HL, RW)),
	InstDesc(0xCB_0F, RRC,  "a",           Other,  Imp),
	InstDesc(0xCB_10, RL,   "b",           Other,  Imp),
	InstDesc(0xCB_11, RL,   "c",           Other,  Imp),
	InstDesc(0xCB_12, RL,   "d",           Other,  Imp),
	InstDesc(0xCB_13, RL,   "e",           Other,  Imp),
	InstDesc(0xCB_14, RL,   "h",           Other,  Imp),
	InstDesc(0xCB_15, RL,   "l",           Other,  Imp),
	InstDesc(0xCB_16, RL,   "[hl]",        Other,  Ind(Reg::HL, RW)),
	InstDesc(0xCB_17, RL,   "a",           Other,  Imp),
	InstDesc(0xCB_18, RR,   "b",           Other,  Imp),
	InstDesc(0xCB_19, RR,   "c",           Other,  Imp),
	InstDesc(0xCB_1A, RR,   "d",           Other,  Imp),
	InstDesc(0xCB_1B, RR,   "e",           Other,  Imp),
	InstDesc(0xCB_1C, RR,   "h",           Other,  Imp),
	InstDesc(0xCB_1D, RR,   "l",           Other,  Imp),
	InstDesc(0xCB_1E, RR,   "[hl]",        Other,  Ind(Reg::HL, RW)),
	InstDesc(0xCB_1F, RR,   "a",           Other,  Imp),
	InstDesc(0xCB_20, SLA,  "b",           Other,  Imp),
	InstDesc(0xCB_21, SLA,  "c",           Other,  Imp),
	InstDesc(0xCB_22, SLA,  "d",           Other,  Imp),
	InstDesc(0xCB_23, SLA,  "e",           Other,  Imp),
	InstDesc(0xCB_24, SLA,  "h",           Other,  Imp),
	InstDesc(0xCB_25, SLA,  "l",           Other,  Imp),
	InstDesc(0xCB_26, SLA,  "[hl]",        Other,  Ind(Reg::HL, RW)),
	InstDesc(0xCB_27, SLA,  "a",           Other,  Imp),
	InstDesc(0xCB_28, SRA,  "b",           Other,  Imp),
	InstDesc(0xCB_29, SRA,  "c",           Other,  Imp),
	InstDesc(0xCB_2A, SRA,  "d",           Other,  Imp),
	InstDesc(0xCB_2B, SRA,  "e",           Other,  Imp),
	InstDesc(0xCB_2C, SRA,  "h",           Other,  Imp),
	InstDesc(0xCB_2D, SRA,  "l",           Other,  Imp),
	InstDesc(0xCB_2E, SRA,  "[hl]",        Other,  Ind(Reg::HL, RW)),
	InstDesc(0xCB_2F, SRA,  "a",           Other,  Imp),
	InstDesc(0xCB_30, SWAP, "b",           Other,  Imp),
	InstDesc(0xCB_31, SWAP, "c",           Other,  Imp),
	InstDesc(0xCB_32, SWAP, "d",           Other,  Imp),
	InstDesc(0xCB_33, SWAP, "e",           Other,  Imp),
	InstDesc(0xCB_34, SWAP, "h",           Other,  Imp),
	InstDesc(0xCB_35, SWAP, "l",           Other,  Imp),
	InstDesc(0xCB_36, SWAP, "[hl]",        Other,  Ind(Reg::HL, RW)),
	InstDesc(0xCB_37, SWAP, "a",           Other,  Imp),
	InstDesc(0xCB_38, SRL,  "b",           Other,  Imp),
	InstDesc(0xCB_39, SRL,  "c",           Other,  Imp),
	InstDesc(0xCB_3A, SRL,  "d",           Other,  Imp),
	InstDesc(0xCB_3B, SRL,  "e",           Other,  Imp),
	InstDesc(0xCB_3C, SRL,  "h",           Other,  Imp),
	InstDesc(0xCB_3D, SRL,  "l",           Other,  Imp),
	InstDesc(0xCB_3E, SRL,  "[hl]",        Other,  Ind(Reg::HL, RW)),
	InstDesc(0xCB_3F, SRL,  "a",           Other,  Imp),
	InstDesc(0xCB_40, BIT,  "0, b",        Other,  Imp),
	InstDesc(0xCB_41, BIT,  "0, c",        Other,  Imp),
	InstDesc(0xCB_42, BIT,  "0, d",        Other,  Imp),
	InstDesc(0xCB_43, BIT,  "0, e",        Other,  Imp),
	InstDesc(0xCB_44, BIT,  "0, h",        Other,  Imp),
	InstDesc(0xCB_45, BIT,  "0, l",        Other,  Imp),
	InstDesc(0xCB_46, BIT,  "0, [hl]",     Other,  Ind(Reg::HL, R)),
	InstDesc(0xCB_47, BIT,  "0, a",        Other,  Imp),
	InstDesc(0xCB_48, BIT,  "1, b",        Other,  Imp),
	InstDesc(0xCB_49, BIT,  "1, c",        Other,  Imp),
	InstDesc(0xCB_4A, BIT,  "1, d",        Other,  Imp),
	InstDesc(0xCB_4B, BIT,  "1, e",        Other,  Imp),
	InstDesc(0xCB_4C, BIT,  "1, h",        Other,  Imp),
	InstDesc(0xCB_4D, BIT,  "1, l",        Other,  Imp),
	InstDesc(0xCB_4E, BIT,  "1, [hl]",     Other,  Ind(Reg::HL, R)),
	InstDesc(0xCB_4F, BIT,  "1, a",        Other,  Imp),
	InstDesc(0xCB_50, BIT,  "2, b",        Other,  Imp),
	InstDesc(0xCB_51, BIT,  "2, c",        Other,  Imp),
	InstDesc(0xCB_52, BIT,  "2, d",        Other,  Imp),
	InstDesc(0xCB_53, BIT,  "2, e",        Other,  Imp),
	InstDesc(0xCB_54, BIT,  "2, h",        Other,  Imp),
	InstDesc(0xCB_55, BIT,  "2, l",        Other,  Imp),
	InstDesc(0xCB_56, BIT,  "2, [hl]",     Other,  Ind(Reg::HL, R)),
	InstDesc(0xCB_57, BIT,  "2, a",        Other,  Imp),
	InstDesc(0xCB_58, BIT,  "3, b",        Other,  Imp),
	InstDesc(0xCB_59, BIT,  "3, c",        Other,  Imp),
	InstDesc(0xCB_5A, BIT,  "3, d",        Other,  Imp),
	InstDesc(0xCB_5B, BIT,  "3, e",        Other,  Imp),
	InstDesc(0xCB_5C, BIT,  "3, h",        Other,  Imp),
	InstDesc(0xCB_5D, BIT,  "3, l",        Other,  Imp),
	InstDesc(0xCB_5E, BIT,  "3, [hl]",     Other,  Ind(Reg::HL, R)),
	InstDesc(0xCB_5F, BIT,  "3, a",        Other,  Imp),
	InstDesc(0xCB_60, BIT,  "4, b",        Other,  Imp),
	InstDesc(0xCB_61, BIT,  "4, c",        Other,  Imp),
	InstDesc(0xCB_62, BIT,  "4, d",        Other,  Imp),
	InstDesc(0xCB_63, BIT,  "4, e",        Other,  Imp),
	InstDesc(0xCB_64, BIT,  "4, h",        Other,  Imp),
	InstDesc(0xCB_65, BIT,  "4, l",        Other,  Imp),
	InstDesc(0xCB_66, BIT,  "4, [hl]",     Other,  Ind(Reg::HL, R)),
	InstDesc(0xCB_67, BIT,  "4, a",        Other,  Imp),
	InstDesc(0xCB_68, BIT,  "5, b",        Other,  Imp),
	InstDesc(0xCB_69, BIT,  "5, c",        Other,  Imp),
	InstDesc(0xCB_6A, BIT,  "5, d",        Other,  Imp),
	InstDesc(0xCB_6B, BIT,  "5, e",        Other,  Imp),
	InstDesc(0xCB_6C, BIT,  "5, h",        Other,  Imp),
	InstDesc(0xCB_6D, BIT,  "5, l",        Other,  Imp),
	InstDesc(0xCB_6E, BIT,  "5, [hl]",     Other,  Ind(Reg::HL, R)),
	InstDesc(0xCB_6F, BIT,  "5, a",        Other,  Imp),
	InstDesc(0xCB_70, BIT,  "6, b",        Other,  Imp),
	InstDesc(0xCB_71, BIT,  "6, c",        Other,  Imp),
	InstDesc(0xCB_72, BIT,  "6, d",        Other,  Imp),
	InstDesc(0xCB_73, BIT,  "6, e",        Other,  Imp),
	InstDesc(0xCB_74, BIT,  "6, h",        Other,  Imp),
	InstDesc(0xCB_75, BIT,  "6, l",        Other,  Imp),
	InstDesc(0xCB_76, BIT,  "6, [hl]",     Other,  Ind(Reg::HL, R)),
	InstDesc(0xCB_77, BIT,  "6, a",        Other,  Imp),
	InstDesc(0xCB_78, BIT,  "7, b",        Other,  Imp),
	InstDesc(0xCB_79, BIT,  "7, c",        Other,  Imp),
	InstDesc(0xCB_7A, BIT,  "7, d",        Other,  Imp),
	InstDesc(0xCB_7B, BIT,  "7, e",        Other,  Imp),
	InstDesc(0xCB_7C, BIT,  "7, h",        Other,  Imp),
	InstDesc(0xCB_7D, BIT,  "7, l",        Other,  Imp),
	InstDesc(0xCB_7E, BIT,  "7, [hl]",     Other,  Ind(Reg::HL, R)),
	InstDesc(0xCB_7F, BIT,  "7, a",        Other,  Imp),
	InstDesc(0xCB_80, RES,  "0, b",        Other,  Imp),
	InstDesc(0xCB_81, RES,  "0, c",        Other,  Imp),
	InstDesc(0xCB_82, RES,  "0, d",        Other,  Imp),
	InstDesc(0xCB_83, RES,  "0, e",        Other,  Imp),
	InstDesc(0xCB_84, RES,  "0, h",        Other,  Imp),
	InstDesc(0xCB_85, RES,  "0, l",        Other,  Imp),
	InstDesc(0xCB_86, RES,  "0, [hl]",     Other,  Ind(Reg::HL, RW)),
	InstDesc(0xCB_87, RES,  "0, a",        Other,  Imp),
	InstDesc(0xCB_88, RES,  "1, b",        Other,  Imp),
	InstDesc(0xCB_89, RES,  "1, c",        Other,  Imp),
	InstDesc(0xCB_8A, RES,  "1, d",        Other,  Imp),
	InstDesc(0xCB_8B, RES,  "1, e",        Other,  Imp),
	InstDesc(0xCB_8C, RES,  "1, h",        Other,  Imp),
	InstDesc(0xCB_8D, RES,  "1, l",        Other,  Imp),
	InstDesc(0xCB_8E, RES,  "1, [hl]",     Other,  Ind(Reg::HL, RW)),
	InstDesc(0xCB_8F, RES,  "1, a",        Other,  Imp),
	InstDesc(0xCB_90, RES,  "2, b",        Other,  Imp),
	InstDesc(0xCB_91, RES,  "2, c",        Other,  Imp),
	InstDesc(0xCB_92, RES,  "2, d",        Other,  Imp),
	InstDesc(0xCB_93, RES,  "2, e",        Other,  Imp),
	InstDesc(0xCB_94, RES,  "2, h",        Other,  Imp),
	InstDesc(0xCB_95, RES,  "2, l",        Other,  Imp),
	InstDesc(0xCB_96, RES,  "2, [hl]",     Other,  Ind(Reg::HL, RW)),
	InstDesc(0xCB_97, RES,  "2, a",        Other,  Imp),
	InstDesc(0xCB_98, RES,  "3, b",        Other,  Imp),
	InstDesc(0xCB_99, RES,  "3, c",        Other,  Imp),
	InstDesc(0xCB_9A, RES,  "3, d",        Other,  Imp),
	InstDesc(0xCB_9B, RES,  "3, e",        Other,  Imp),
	InstDesc(0xCB_9C, RES,  "3, h",        Other,  Imp),
	InstDesc(0xCB_9D, RES,  "3, l",        Other,  Imp),
	InstDesc(0xCB_9E, RES,  "3, [hl]",     Other,  Ind(Reg::HL, RW)),
	InstDesc(0xCB_9F, RES,  "3, a",        Other,  Imp),
	InstDesc(0xCB_A0, RES,  "4, b",        Other,  Imp),
	InstDesc(0xCB_A1, RES,  "4, c",        Other,  Imp),
	InstDesc(0xCB_A2, RES,  "4, d",        Other,  Imp),
	InstDesc(0xCB_A3, RES,  "4, e",        Other,  Imp),
	InstDesc(0xCB_A4, RES,  "4, h",        Other,  Imp),
	InstDesc(0xCB_A5, RES,  "4, l",        Other,  Imp),
	InstDesc(0xCB_A6, RES,  "4, [hl]",     Other,  Ind(Reg::HL, RW)),
	InstDesc(0xCB_A7, RES,  "4, a",        Other,  Imp),
	InstDesc(0xCB_A8, RES,  "5, b",        Other,  Imp),
	InstDesc(0xCB_A9, RES,  "5, c",        Other,  Imp),
	InstDesc(0xCB_AA, RES,  "5, d",        Other,  Imp),
	InstDesc(0xCB_AB, RES,  "5, e",        Other,  Imp),
	InstDesc(0xCB_AC, RES,  "5, h",        Other,  Imp),
	InstDesc(0xCB_AD, RES,  "5, l",        Other,  Imp),
	InstDesc(0xCB_AE, RES,  "5, [hl]",     Other,  Ind(Reg::HL, RW)),
	InstDesc(0xCB_AF, RES,  "5, a",        Other,  Imp),
	InstDesc(0xCB_B0, RES,  "6, b",        Other,  Imp),
	InstDesc(0xCB_B1, RES,  "6, c",        Other,  Imp),
	InstDesc(0xCB_B2, RES,  "6, d",        Other,  Imp),
	InstDesc(0xCB_B3, RES,  "6, e",        Other,  Imp),
	InstDesc(0xCB_B4, RES,  "6, h",        Other,  Imp),
	InstDesc(0xCB_B5, RES,  "6, l",        Other,  Imp),
	InstDesc(0xCB_B6, RES,  "6, [hl]",     Other,  Ind(Reg::HL, RW)),
	InstDesc(0xCB_B7, RES,  "6, a",        Other,  Imp),
	InstDesc(0xCB_B8, RES,  "7, b",        Other,  Imp),
	InstDesc(0xCB_B9, RES,  "7, c",        Other,  Imp),
	InstDesc(0xCB_BA, RES,  "7, d",        Other,  Imp),
	InstDesc(0xCB_BB, RES,  "7, e",        Other,  Imp),
	InstDesc(0xCB_BC, RES,  "7, h",        Other,  Imp),
	InstDesc(0xCB_BD, RES,  "7, l",        Other,  Imp),
	InstDesc(0xCB_BE, RES,  "7, [hl]",     Other,  Ind(Reg::HL, RW)),
	InstDesc(0xCB_BF, RES,  "7, a",        Other,  Imp),
	InstDesc(0xCB_C0, SET,  "0, b",        Other,  Imp),
	InstDesc(0xCB_C1, SET,  "0, c",        Other,  Imp),
	InstDesc(0xCB_C2, SET,  "0, d",        Other,  Imp),
	InstDesc(0xCB_C3, SET,  "0, e",        Other,  Imp),
	InstDesc(0xCB_C4, SET,  "0, h",        Other,  Imp),
	InstDesc(0xCB_C5, SET,  "0, l",        Other,  Imp),
	InstDesc(0xCB_C6, SET,  "0, [hl]",     Other,  Ind(Reg::HL, RW)),
	InstDesc(0xCB_C7, SET,  "0, a",        Other,  Imp),
	InstDesc(0xCB_C8, SET,  "1, b",        Other,  Imp),
	InstDesc(0xCB_C9, SET,  "1, c",        Other,  Imp),
	InstDesc(0xCB_CA, SET,  "1, d",        Other,  Imp),
	InstDesc(0xCB_CB, SET,  "1, e",        Other,  Imp),
	InstDesc(0xCB_CC, SET,  "1, h",        Other,  Imp),
	InstDesc(0xCB_CD, SET,  "1, l",        Other,  Imp),
	InstDesc(0xCB_CE, SET,  "1, [hl]",     Other,  Ind(Reg::HL, RW)),
	InstDesc(0xCB_CF, SET,  "1, a",        Other,  Imp),
	InstDesc(0xCB_D0, SET,  "2, b",        Other,  Imp),
	InstDesc(0xCB_D1, SET,  "2, c",        Other,  Imp),
	InstDesc(0xCB_D2, SET,  "2, d",        Other,  Imp),
	InstDesc(0xCB_D3, SET,  "2, e",        Other,  Imp),
	InstDesc(0xCB_D4, SET,  "2, h",        Other,  Imp),
	InstDesc(0xCB_D5, SET,  "2, l",        Other,  Imp),
	InstDesc(0xCB_D6, SET,  "2, [hl]",     Other,  Ind(Reg::HL, RW)),
	InstDesc(0xCB_D7, SET,  "2, a",        Other,  Imp),
	InstDesc(0xCB_D8, SET,  "3, b",        Other,  Imp),
	InstDesc(0xCB_D9, SET,  "3, c",        Other,  Imp),
	InstDesc(0xCB_DA, SET,  "3, d",        Other,  Imp),
	InstDesc(0xCB_DB, SET,  "3, e",        Other,  Imp),
	InstDesc(0xCB_DC, SET,  "3, h",        Other,  Imp),
	InstDesc(0xCB_DD, SET,  "3, l",        Other,  Imp),
	InstDesc(0xCB_DE, SET,  "3, [hl]",     Other,  Ind(Reg::HL, RW)),
	InstDesc(0xCB_DF, SET,  "3, a",        Other,  Imp),
	InstDesc(0xCB_E0, SET,  "4, b",        Other,  Imp),
	InstDesc(0xCB_E1, SET,  "4, c",        Other,  Imp),
	InstDesc(0xCB_E2, SET,  "4, d",        Other,  Imp),
	InstDesc(0xCB_E3, SET,  "4, e",        Other,  Imp),
	InstDesc(0xCB_E4, SET,  "4, h",        Other,  Imp),
	InstDesc(0xCB_E5, SET,  "4, l",        Other,  Imp),
	InstDesc(0xCB_E6, SET,  "4, [hl]",     Other,  Ind(Reg::HL, RW)),
	InstDesc(0xCB_E7, SET,  "4, a",        Other,  Imp),
	InstDesc(0xCB_E8, SET,  "5, b",        Other,  Imp),
	InstDesc(0xCB_E9, SET,  "5, c",        Other,  Imp),
	InstDesc(0xCB_EA, SET,  "5, d",        Other,  Imp),
	InstDesc(0xCB_EB, SET,  "5, e",        Other,  Imp),
	InstDesc(0xCB_EC, SET,  "5, h",        Other,  Imp),
	InstDesc(0xCB_ED, SET,  "5, l",        Other,  Imp),
	InstDesc(0xCB_EE, SET,  "5, [hl]",     Other,  Ind(Reg::HL, RW)),
	InstDesc(0xCB_EF, SET,  "5, a",        Other,  Imp),
	InstDesc(0xCB_F0, SET,  "6, b",        Other,  Imp),
	InstDesc(0xCB_F1, SET,  "6, c",        Other,  Imp),
	InstDesc(0xCB_F2, SET,  "6, d",        Other,  Imp),
	InstDesc(0xCB_F3, SET,  "6, e",        Other,  Imp),
	InstDesc(0xCB_F4, SET,  "6, h",        Other,  Imp),
	InstDesc(0xCB_F5, SET,  "6, l",        Other,  Imp),
	InstDesc(0xCB_F6, SET,  "6, [hl]",     Other,  Ind(Reg::HL, RW)),
	InstDesc(0xCB_F7, SET,  "6, a",        Other,  Imp),
	InstDesc(0xCB_F8, SET,  "7, b",        Other,  Imp),
	InstDesc(0xCB_F9, SET,  "7, c",        Other,  Imp),
	InstDesc(0xCB_FA, SET,  "7, d",        Other,  Imp),
	InstDesc(0xCB_FB, SET,  "7, e",        Other,  Imp),
	InstDesc(0xCB_FC, SET,  "7, h",        Other,  Imp),
	InstDesc(0xCB_FD, SET,  "7, l",        Other,  Imp),
	InstDesc(0xCB_FE, SET,  "7, [hl]",     Other,  Ind(Reg::HL, RW)),
	InstDesc(0xCB_FF, SET,  "7, a",        Other,  Imp),
];