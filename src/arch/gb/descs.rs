
use crate::program::{ InstructionKind };
use crate::memory::{ MemAccess };

// ------------------------------------------------------------------------------------------------
// Reg
// ------------------------------------------------------------------------------------------------

#[repr(u8)]
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub(super) enum Reg {
	A, F, B, C, D, E, H, L,
	AF, BC, DE, HL, SP,
}

impl Default for Reg {
	fn default() -> Reg { Reg::A }
}

impl Reg {
	pub(super) fn register_names() -> &'static [&'static str] {
		&["a", "f", "b", "c", "d", "e", "h", "l",
		"af", "bc", "de", "hl", "sp"]
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
// SynOp (syntactic operands)
// ------------------------------------------------------------------------------------------------

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub(super) enum Cc {
	C, NC, Z, NZ
}

const CC_C:  SynOp = SynOp::Cc(Cc::C);
const CC_NC: SynOp = SynOp::Cc(Cc::NC);
const CC_Z:  SynOp = SynOp::Cc(Cc::Z);
const CC_NZ: SynOp = SynOp::Cc(Cc::NZ);

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub(super) enum SynOp {
	Op,
	Op2,
	IndOp,
	SpPlusOp,
	Srg(Reg), // not named Reg cause that name has enough meanings already
	IndReg(Reg),
	IndHlPlus,
	IndHlMinus,
	Cc(Cc),
}

// ------------------------------------------------------------------------------------------------
// InstDesc
// ------------------------------------------------------------------------------------------------

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub(super) struct InstDesc (
	u16,              // opcode
	MetaOp,           // meta_op
	&'static [SynOp], // syn_ops
	InstructionKind,  // kind
	GBOpKind,         // op_kind
);

impl InstDesc {
	#[inline] pub(super) fn opcode  (&self) -> u16             { self.0 }
	#[inline] pub(super) fn meta_op (&self) -> MetaOp          { self.1 }
	#[inline] pub(super) fn syn_ops(&self) -> &'static [SynOp] { self.2 }
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
		if self.opcode() >= 0xCB_00 {
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

	pub(super) fn bit_operand(&self) -> Option<u64> {
		if self.opcode() < 0xCB_40 {
			None
		} else {
			Some(((self.opcode() >> 3) & 0b111) as u64)
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
use MemAccess::{ R, W, RW, Target };
use MetaOp::*;
use Reg::*;
use SynOp::*;

const INVALID: InstDesc = InstDesc(0x00, UNK, &[], Other, Imp);

const INST_DESCS: &[InstDesc] = &[
	InstDesc(   0x00, NOP,  &[],                       Other,  Imp),
	InstDesc(   0x01, LD,   &[Srg(BC), Op],            Other,  Imm16),
	InstDesc(   0x02, LD,   &[IndReg(BC), Srg(A)],     Other,  Ind(BC, W)),
	InstDesc(   0x03, INC,  &[Srg(BC)],                Other,  Imp),
	InstDesc(   0x04, INC,  &[Srg(B)],                 Other,  Imp),
	InstDesc(   0x05, DEC,  &[Srg(B)],                 Other,  Imp),
	InstDesc(   0x06, LD,   &[Srg(B), Op],             Other,  UImm8),
	InstDesc(   0x07, RLC,  &[Srg(A)],                 Other,  Imp),
	InstDesc(   0x08, LD,   &[IndOp, Srg(SP)],         Other,  Add16(W)),
	InstDesc(   0x09, ADD,  &[Srg(HL), Srg(BC)],       Other,  Imp),
	InstDesc(   0x0A, LD,   &[Srg(A), IndReg(BC)],     Other,  Ind(BC, R)),
	InstDesc(   0x0B, DEC,  &[Srg(BC)],                Other,  Imp),
	InstDesc(   0x0C, INC,  &[Srg(C)],                 Other,  Imp),
	InstDesc(   0x0D, DEC,  &[Srg(C)],                 Other,  Imp),
	InstDesc(   0x0E, LD,   &[Srg(C), Op],             Other,  UImm8),
	InstDesc(   0x0F, RRC,  &[Srg(A)],                 Other,  Imp),
	InstDesc(   0x10, STOP, &[],                       Other,  Dummy),
	InstDesc(   0x11, LD,   &[Srg(DE), Op],            Other,  Imm16),
	InstDesc(   0x12, LD,   &[IndReg(DE), Srg(A)],     Other,  Ind(DE, W)),
	InstDesc(   0x13, INC,  &[Srg(DE)],                Other,  Imp),
	InstDesc(   0x14, INC,  &[Srg(D)],                 Other,  Imp),
	InstDesc(   0x15, DEC,  &[Srg(D)],                 Other,  Imp),
	InstDesc(   0x16, LD,   &[Srg(D), Op],             Other,  UImm8),
	InstDesc(   0x17, RL,   &[Srg(A)],                 Other,  Imp),
	InstDesc(   0x18, JR,   &[Op],                     Uncond, Rel),
	InstDesc(   0x19, ADD,  &[Srg(HL), Srg(DE)],       Other,  Imp),
	InstDesc(   0x1A, LD,   &[Srg(A), IndReg(DE)],     Other,  Ind(DE, R)),
	InstDesc(   0x1B, DEC,  &[Srg(DE)],                Other,  Imp),
	InstDesc(   0x1C, INC,  &[Srg(E)],                 Other,  Imp),
	InstDesc(   0x1D, DEC,  &[Srg(E)],                 Other,  Imp),
	InstDesc(   0x1E, LD,   &[Srg(E), Op],             Other,  UImm8),
	InstDesc(   0x1F, RR,   &[Srg(A)],                 Other,  Imp),
	InstDesc(   0x20, JR,   &[CC_NZ, Op],              Cond,   Rel),
	InstDesc(   0x21, LD,   &[Srg(HL), Op],            Other,  Imm16),
	InstDesc(   0x22, LD,   &[IndHlPlus, Srg(A)],      Other,  Ind(HL, W)),
	InstDesc(   0x23, INC,  &[Srg(HL)],                Other,  Imp),
	InstDesc(   0x24, INC,  &[Srg(H)],                 Other,  Imp),
	InstDesc(   0x25, DEC,  &[Srg(H)],                 Other,  Imp),
	InstDesc(   0x26, LD,   &[Srg(H), Op],             Other,  UImm8),
	InstDesc(   0x27, DA,   &[Srg(A)],                 Other,  Imp),
	InstDesc(   0x28, JR,   &[CC_Z, Op],               Cond,   Rel),
	InstDesc(   0x29, ADD,  &[Srg(HL), Srg(HL)],       Other,  Imp),
	InstDesc(   0x2A, LD,   &[Srg(A), IndHlPlus],      Other,  Ind(HL, R)),
	InstDesc(   0x2B, DEC,  &[Srg(HL)],                Other,  Imp),
	InstDesc(   0x2C, INC,  &[Srg(L)],                 Other,  Imp),
	InstDesc(   0x2D, DEC,  &[Srg(L)],                 Other,  Imp),
	InstDesc(   0x2E, LD,   &[Srg(L), Op],             Other,  UImm8),
	InstDesc(   0x2F, CPL,  &[Srg(A)],                 Other,  Imp),
	InstDesc(   0x30, JR,   &[CC_NC, Op],              Cond,   Rel),
	InstDesc(   0x31, LD,   &[Srg(SP), Op],            Other,  Imm16),
	InstDesc(   0x32, LD,   &[IndHlMinus, Srg(A)],     Other,  Ind(HL, W)),
	InstDesc(   0x33, INC,  &[Srg(SP)],                Other,  Imp),
	InstDesc(   0x34, INC,  &[IndReg(HL)],             Other,  Ind(HL, RW)),
	InstDesc(   0x35, DEC,  &[IndReg(HL)],             Other,  Ind(HL, RW)),
	InstDesc(   0x36, LD,   &[IndReg(HL), Op2],        Other,  LdHlImm),
	InstDesc(   0x37, SCF,  &[],                       Other,  Imp),
	InstDesc(   0x38, JR,   &[CC_C, Op],               Cond,   Rel),
	InstDesc(   0x39, ADD,  &[Srg(HL), Srg(SP)],       Other,  Imp),
	InstDesc(   0x3A, LD,   &[Srg(A), IndHlMinus],     Other,  Ind(HL, R)),
	InstDesc(   0x3B, DEC,  &[Srg(SP)],                Other,  Imp),
	InstDesc(   0x3C, INC,  &[Srg(A)],                 Other,  Imp),
	InstDesc(   0x3D, DEC,  &[Srg(A)],                 Other,  Imp),
	InstDesc(   0x3E, LD,   &[Srg(A), Op],             Other,  UImm8),
	InstDesc(   0x3F, CCF,  &[],                       Other,  Imp),
	InstDesc(   0x40, LD,   &[Srg(B), Srg(B)],         Other,  Imp),
	InstDesc(   0x41, LD,   &[Srg(B), Srg(C)],         Other,  Imp),
	InstDesc(   0x42, LD,   &[Srg(B), Srg(D)],         Other,  Imp),
	InstDesc(   0x43, LD,   &[Srg(B), Srg(E)],         Other,  Imp),
	InstDesc(   0x44, LD,   &[Srg(B), Srg(H)],         Other,  Imp),
	InstDesc(   0x45, LD,   &[Srg(B), Srg(L)],         Other,  Imp),
	InstDesc(   0x46, LD,   &[Srg(B), IndReg(HL)],     Other,  Ind(HL, R)),
	InstDesc(   0x47, LD,   &[Srg(B), Srg(A)],         Other,  Imp),
	InstDesc(   0x48, LD,   &[Srg(C), Srg(B)],         Other,  Imp),
	InstDesc(   0x49, LD,   &[Srg(C), Srg(C)],         Other,  Imp),
	InstDesc(   0x4A, LD,   &[Srg(C), Srg(D)],         Other,  Imp),
	InstDesc(   0x4B, LD,   &[Srg(C), Srg(E)],         Other,  Imp),
	InstDesc(   0x4C, LD,   &[Srg(C), Srg(H)],         Other,  Imp),
	InstDesc(   0x4D, LD,   &[Srg(C), Srg(L)],         Other,  Imp),
	InstDesc(   0x4E, LD,   &[Srg(C), IndReg(HL)],     Other,  Ind(HL, R)),
	InstDesc(   0x4F, LD,   &[Srg(C), Srg(A)],         Other,  Imp),
	InstDesc(   0x50, LD,   &[Srg(D), Srg(B)],         Other,  Imp),
	InstDesc(   0x51, LD,   &[Srg(D), Srg(C)],         Other,  Imp),
	InstDesc(   0x52, LD,   &[Srg(D), Srg(D)],         Other,  Imp),
	InstDesc(   0x53, LD,   &[Srg(D), Srg(E)],         Other,  Imp),
	InstDesc(   0x54, LD,   &[Srg(D), Srg(H)],         Other,  Imp),
	InstDesc(   0x55, LD,   &[Srg(D), Srg(L)],         Other,  Imp),
	InstDesc(   0x56, LD,   &[Srg(D), IndReg(HL)],     Other,  Ind(HL, R)),
	InstDesc(   0x57, LD,   &[Srg(D), Srg(A)],         Other,  Imp),
	InstDesc(   0x58, LD,   &[Srg(E), Srg(B)],         Other,  Imp),
	InstDesc(   0x59, LD,   &[Srg(E), Srg(C)],         Other,  Imp),
	InstDesc(   0x5A, LD,   &[Srg(E), Srg(D)],         Other,  Imp),
	InstDesc(   0x5B, LD,   &[Srg(E), Srg(E)],         Other,  Imp),
	InstDesc(   0x5C, LD,   &[Srg(E), Srg(H)],         Other,  Imp),
	InstDesc(   0x5D, LD,   &[Srg(E), Srg(L)],         Other,  Imp),
	InstDesc(   0x5E, LD,   &[Srg(E), IndReg(HL)],     Other,  Ind(HL, R)),
	InstDesc(   0x5F, LD,   &[Srg(E), Srg(A)],         Other,  Imp),
	InstDesc(   0x60, LD,   &[Srg(H), Srg(B)],         Other,  Imp),
	InstDesc(   0x61, LD,   &[Srg(H), Srg(C)],         Other,  Imp),
	InstDesc(   0x62, LD,   &[Srg(H), Srg(D)],         Other,  Imp),
	InstDesc(   0x63, LD,   &[Srg(H), Srg(E)],         Other,  Imp),
	InstDesc(   0x64, LD,   &[Srg(H), Srg(H)],         Other,  Imp),
	InstDesc(   0x65, LD,   &[Srg(H), Srg(L)],         Other,  Imp),
	InstDesc(   0x66, LD,   &[Srg(H), IndReg(HL)],     Other,  Ind(HL, R)),
	InstDesc(   0x67, LD,   &[Srg(H), Srg(A)],         Other,  Imp),
	InstDesc(   0x68, LD,   &[Srg(L), Srg(B)],         Other,  Imp),
	InstDesc(   0x69, LD,   &[Srg(L), Srg(C)],         Other,  Imp),
	InstDesc(   0x6A, LD,   &[Srg(L), Srg(D)],         Other,  Imp),
	InstDesc(   0x6B, LD,   &[Srg(L), Srg(E)],         Other,  Imp),
	InstDesc(   0x6C, LD,   &[Srg(L), Srg(H)],         Other,  Imp),
	InstDesc(   0x6D, LD,   &[Srg(L), Srg(L)],         Other,  Imp),
	InstDesc(   0x6E, LD,   &[Srg(L), IndReg(HL)],     Other,  Ind(HL, R)),
	InstDesc(   0x6F, LD,   &[Srg(L), Srg(A)],         Other,  Imp),
	InstDesc(   0x70, LD,   &[IndReg(HL), Srg(B)],     Other,  Ind(HL, W)),
	InstDesc(   0x71, LD,   &[IndReg(HL), Srg(C)],     Other,  Ind(HL, W)),
	InstDesc(   0x72, LD,   &[IndReg(HL), Srg(D)],     Other,  Ind(HL, W)),
	InstDesc(   0x73, LD,   &[IndReg(HL), Srg(E)],     Other,  Ind(HL, W)),
	InstDesc(   0x74, LD,   &[IndReg(HL), Srg(H)],     Other,  Ind(HL, W)),
	InstDesc(   0x75, LD,   &[IndReg(HL), Srg(L)],     Other,  Ind(HL, W)),
	InstDesc(   0x76, HALT, &[],                       Other,  Imp),
	InstDesc(   0x77, LD,   &[IndReg(HL), Srg(A)],     Other,  Ind(HL, W)),
	InstDesc(   0x78, LD,   &[Srg(A), Srg(B)],         Other,  Imp),
	InstDesc(   0x79, LD,   &[Srg(A), Srg(C)],         Other,  Imp),
	InstDesc(   0x7A, LD,   &[Srg(A), Srg(D)],         Other,  Imp),
	InstDesc(   0x7B, LD,   &[Srg(A), Srg(E)],         Other,  Imp),
	InstDesc(   0x7C, LD,   &[Srg(A), Srg(H)],         Other,  Imp),
	InstDesc(   0x7D, LD,   &[Srg(A), Srg(L)],         Other,  Imp),
	InstDesc(   0x7E, LD,   &[Srg(A), IndReg(HL)],     Other,  Ind(HL, R)),
	InstDesc(   0x7F, LD,   &[Srg(A), Srg(A)],         Other,  Imp),
	InstDesc(   0x80, ADD,  &[Srg(A), Srg(B)],         Other,  Imp),
	InstDesc(   0x81, ADD,  &[Srg(A), Srg(C)],         Other,  Imp),
	InstDesc(   0x82, ADD,  &[Srg(A), Srg(D)],         Other,  Imp),
	InstDesc(   0x83, ADD,  &[Srg(A), Srg(E)],         Other,  Imp),
	InstDesc(   0x84, ADD,  &[Srg(A), Srg(H)],         Other,  Imp),
	InstDesc(   0x85, ADD,  &[Srg(A), Srg(L)],         Other,  Imp),
	InstDesc(   0x86, ADD,  &[Srg(A), IndReg(HL)],     Other,  Ind(HL, R)),
	InstDesc(   0x87, ADD,  &[Srg(A), Srg(A)],         Other,  Imp),
	InstDesc(   0x88, ADC,  &[Srg(A), Srg(B)],         Other,  Imp),
	InstDesc(   0x89, ADC,  &[Srg(A), Srg(C)],         Other,  Imp),
	InstDesc(   0x8A, ADC,  &[Srg(A), Srg(D)],         Other,  Imp),
	InstDesc(   0x8B, ADC,  &[Srg(A), Srg(E)],         Other,  Imp),
	InstDesc(   0x8C, ADC,  &[Srg(A), Srg(H)],         Other,  Imp),
	InstDesc(   0x8D, ADC,  &[Srg(A), Srg(L)],         Other,  Imp),
	InstDesc(   0x8E, ADC,  &[Srg(A), IndReg(HL)],     Other,  Ind(HL, R)),
	InstDesc(   0x8F, ADC,  &[Srg(A), Srg(A)],         Other,  Imp),
	InstDesc(   0x90, SUB,  &[Srg(A), Srg(B)],         Other,  Imp),
	InstDesc(   0x91, SUB,  &[Srg(A), Srg(C)],         Other,  Imp),
	InstDesc(   0x92, SUB,  &[Srg(A), Srg(D)],         Other,  Imp),
	InstDesc(   0x93, SUB,  &[Srg(A), Srg(E)],         Other,  Imp),
	InstDesc(   0x94, SUB,  &[Srg(A), Srg(H)],         Other,  Imp),
	InstDesc(   0x95, SUB,  &[Srg(A), Srg(L)],         Other,  Imp),
	InstDesc(   0x96, SUB,  &[Srg(A), IndReg(HL)],     Other,  Ind(HL, R)),
	InstDesc(   0x97, SUB,  &[Srg(A), Srg(A)],         Other,  Imp),
	InstDesc(   0x98, SBC,  &[Srg(A), Srg(B)],         Other,  Imp),
	InstDesc(   0x99, SBC,  &[Srg(A), Srg(C)],         Other,  Imp),
	InstDesc(   0x9A, SBC,  &[Srg(A), Srg(D)],         Other,  Imp),
	InstDesc(   0x9B, SBC,  &[Srg(A), Srg(E)],         Other,  Imp),
	InstDesc(   0x9C, SBC,  &[Srg(A), Srg(H)],         Other,  Imp),
	InstDesc(   0x9D, SBC,  &[Srg(A), Srg(L)],         Other,  Imp),
	InstDesc(   0x9E, SBC,  &[Srg(A), IndReg(HL)],     Other,  Ind(HL, R)),
	InstDesc(   0x9F, SBC,  &[Srg(A), Srg(A)],         Other,  Imp),
	InstDesc(   0xA0, AND,  &[Srg(A), Srg(B)],         Other,  Imp),
	InstDesc(   0xA1, AND,  &[Srg(A), Srg(C)],         Other,  Imp),
	InstDesc(   0xA2, AND,  &[Srg(A), Srg(D)],         Other,  Imp),
	InstDesc(   0xA3, AND,  &[Srg(A), Srg(E)],         Other,  Imp),
	InstDesc(   0xA4, AND,  &[Srg(A), Srg(H)],         Other,  Imp),
	InstDesc(   0xA5, AND,  &[Srg(A), Srg(L)],         Other,  Imp),
	InstDesc(   0xA6, AND,  &[Srg(A), IndReg(HL)],     Other,  Ind(HL, R)),
	InstDesc(   0xA7, AND,  &[Srg(A), Srg(A)],         Other,  Imp),
	InstDesc(   0xA8, XOR,  &[Srg(A), Srg(B)],         Other,  Imp),
	InstDesc(   0xA9, XOR,  &[Srg(A), Srg(C)],         Other,  Imp),
	InstDesc(   0xAA, XOR,  &[Srg(A), Srg(D)],         Other,  Imp),
	InstDesc(   0xAB, XOR,  &[Srg(A), Srg(E)],         Other,  Imp),
	InstDesc(   0xAC, XOR,  &[Srg(A), Srg(H)],         Other,  Imp),
	InstDesc(   0xAD, XOR,  &[Srg(A), Srg(L)],         Other,  Imp),
	InstDesc(   0xAE, XOR,  &[Srg(A), IndReg(HL)],     Other,  Ind(HL, R)),
	InstDesc(   0xAF, XOR,  &[Srg(A), Srg(A)],         Other,  Imp),
	InstDesc(   0xB0, OR,   &[Srg(A), Srg(B)],         Other,  Imp),
	InstDesc(   0xB1, OR,   &[Srg(A), Srg(C)],         Other,  Imp),
	InstDesc(   0xB2, OR,   &[Srg(A), Srg(D)],         Other,  Imp),
	InstDesc(   0xB3, OR,   &[Srg(A), Srg(E)],         Other,  Imp),
	InstDesc(   0xB4, OR,   &[Srg(A), Srg(H)],         Other,  Imp),
	InstDesc(   0xB5, OR,   &[Srg(A), Srg(L)],         Other,  Imp),
	InstDesc(   0xB6, OR,   &[Srg(A), IndReg(HL)],     Other,  Ind(HL, R)),
	InstDesc(   0xB7, OR,   &[Srg(A), Srg(A)],         Other,  Imp),
	InstDesc(   0xB8, CP,   &[Srg(A), Srg(B)],         Other,  Imp),
	InstDesc(   0xB9, CP,   &[Srg(A), Srg(C)],         Other,  Imp),
	InstDesc(   0xBA, CP,   &[Srg(A), Srg(D)],         Other,  Imp),
	InstDesc(   0xBB, CP,   &[Srg(A), Srg(E)],         Other,  Imp),
	InstDesc(   0xBC, CP,   &[Srg(A), Srg(H)],         Other,  Imp),
	InstDesc(   0xBD, CP,   &[Srg(A), Srg(L)],         Other,  Imp),
	InstDesc(   0xBE, CP,   &[Srg(A), IndReg(HL)],     Other,  Ind(HL, R)),
	InstDesc(   0xBF, CP,   &[Srg(A), Srg(A)],         Other,  Imp),
	InstDesc(   0xC0, RET,  &[CC_NZ],                  Ret,    Imp),
	InstDesc(   0xC1, POP,  &[Srg(BC)],                Other,  Ind(SP, R)),
	InstDesc(   0xC2, JP,   &[CC_NZ, Op],              Cond,   Add16(Target)),
	InstDesc(   0xC3, JP,   &[Op],                     Uncond, Add16(Target)),
	InstDesc(   0xC4, CALL, &[CC_NZ, Op],              Call,   Add16(Target)),
	InstDesc(   0xC5, PUSH, &[Srg(BC)],                Other,  Ind(SP, W)),
	InstDesc(   0xC6, ADD,  &[Srg(A), Op],             Other,  UImm8),
	InstDesc(   0xC7, RST,  &[Op],                     Call,   Imp), // rst 0x00
	InstDesc(   0xC8, RET,  &[CC_Z],                   Ret,    Imp),
	InstDesc(   0xC9, RET,  &[],                       Ret,    Imp),
	InstDesc(   0xCA, JP,   &[CC_Z, Op],               Cond,   Add16(Target)),
	INVALID,
	InstDesc(   0xCC, CALL, &[CC_Z, Op],               Call,   Add16(Target)),
	InstDesc(   0xCD, CALL, &[Op],                     Call,   Add16(Target)),
	InstDesc(   0xCE, ADC,  &[Srg(A), Op],             Other,  UImm8),
	InstDesc(   0xCF, RST,  &[Op],                     Call,   Imp), // rst 0x08
	InstDesc(   0xD0, RET,  &[CC_NC],                  Ret,    Imp),
	InstDesc(   0xD1, POP,  &[Srg(DE)],                Other,  Ind(SP, R)),
	InstDesc(   0xD2, JP,   &[CC_NC, Op],              Cond,   Add16(Target)),
	INVALID,
	InstDesc(   0xD4, CALL, &[CC_NC, Op],              Call,   Add16(Target)),
	InstDesc(   0xD5, PUSH, &[Srg(DE)],                Other,  Ind(SP, W)),
	InstDesc(   0xD6, SUB,  &[Srg(A), Op],             Other,  UImm8),
	InstDesc(   0xD7, RST,  &[Op],                     Call,   Imp), // rst 0x10
	InstDesc(   0xD8, RET,  &[CC_C],                   Ret,    Imp),
	InstDesc(   0xD9, RETI, &[],                       Ret,    Imp),
	InstDesc(   0xDA, JP,   &[CC_C, Op],               Cond,   Add16(Target)),
	INVALID,
	InstDesc(   0xDC, CALL, &[CC_C, Op],               Call,   Add16(Target)),
	INVALID,
	InstDesc(   0xDE, SBC,  &[Srg(A), Op],             Other,  UImm8),
	InstDesc(   0xDF, RST,  &[Op],                     Call,   Imp), // rst 0x18
	InstDesc(   0xE0, LDH,  &[IndOp, Srg(A)],          Other,  AddHi(W)),
	InstDesc(   0xE1, POP,  &[Srg(HL)],                Other,  Ind(SP, R)),
	InstDesc(   0xE2, LDH,  &[IndReg(C), Srg(A)],      Other,  IndHi(W)),
	INVALID,
	INVALID,
	InstDesc(   0xE5, PUSH, &[Srg(HL)],                Other,  Ind(SP, W)),
	InstDesc(   0xE6, AND,  &[Srg(A), Op],             Other,  UImm8),
	InstDesc(   0xE7, RST,  &[Op],                     Call,   Imp), // rst 0x20
	InstDesc(   0xE8, ADD,  &[Srg(SP), Op],            Other,  SImm8),
	InstDesc(   0xE9, JP,   &[Srg(HL)],                Indir,  Imp),
	InstDesc(   0xEA, LD,   &[IndOp, Srg(A)],          Other,  Add16(W)),
	INVALID,
	INVALID,
	INVALID,
	InstDesc(   0xEE, XOR,  &[Srg(A), Op],             Other,  UImm8),
	InstDesc(   0xEF, RST,  &[Op],                     Call,   Imp), // rst 0x28
	InstDesc(   0xF0, LDH,  &[Srg(A), IndOp],          Other,  AddHi(R)),
	InstDesc(   0xF1, POP,  &[Srg(AF)],                Other,  Ind(SP, R)),
	InstDesc(   0xF2, LDH,  &[Srg(A), IndReg(C)],      Other,  IndHi(R)),
	InstDesc(   0xF3, DI,   &[],                       Other,  Imp),
	INVALID,
	InstDesc(   0xF5, PUSH, &[Srg(AF)],                Other,  Ind(SP, W)),
	InstDesc(   0xF6, OR,   &[Srg(A), Op],             Other,  UImm8),
	InstDesc(   0xF7, RST,  &[Op],                     Call,   Imp), // rst 0x30
	InstDesc(   0xF8, LD,   &[Srg(HL), SpPlusOp],      Other,  SPImm),
	InstDesc(   0xF9, LD,   &[Srg(SP), Srg(HL)],       Other,  Imp),
	InstDesc(   0xFA, LD,   &[Srg(A), IndOp],          Other,  Add16(R)),
	InstDesc(   0xFB, EI,   &[],                       Other,  Imp),
	INVALID,
	INVALID,
	InstDesc(   0xFE, CP,   &[Srg(A), Op],             Other,  UImm8),
	InstDesc(   0xFF, RST,  &[Op],                     Call,   Imp), // rst 0x38
];

const INST_DESCS_CB: &[InstDesc] = &[
	InstDesc(0xCB_00, RLC,  &[Srg(B)],                 Other,  Imp),
	InstDesc(0xCB_01, RLC,  &[Srg(C)],                 Other,  Imp),
	InstDesc(0xCB_02, RLC,  &[Srg(D)],                 Other,  Imp),
	InstDesc(0xCB_03, RLC,  &[Srg(E)],                 Other,  Imp),
	InstDesc(0xCB_04, RLC,  &[Srg(H)],                 Other,  Imp),
	InstDesc(0xCB_05, RLC,  &[Srg(L)],                 Other,  Imp),
	InstDesc(0xCB_06, RLC,  &[IndReg(HL)],             Other,  Ind(HL, RW)),
	InstDesc(0xCB_07, RLC,  &[Srg(A)],                 Other,  Imp),
	InstDesc(0xCB_08, RRC,  &[Srg(B)],                 Other,  Imp),
	InstDesc(0xCB_09, RRC,  &[Srg(C)],                 Other,  Imp),
	InstDesc(0xCB_0A, RRC,  &[Srg(D)],                 Other,  Imp),
	InstDesc(0xCB_0B, RRC,  &[Srg(E)],                 Other,  Imp),
	InstDesc(0xCB_0C, RRC,  &[Srg(H)],                 Other,  Imp),
	InstDesc(0xCB_0D, RRC,  &[Srg(L)],                 Other,  Imp),
	InstDesc(0xCB_0E, RRC,  &[IndReg(HL)],             Other,  Ind(HL, RW)),
	InstDesc(0xCB_0F, RRC,  &[Srg(A)],                 Other,  Imp),
	InstDesc(0xCB_10, RL,   &[Srg(B)],                 Other,  Imp),
	InstDesc(0xCB_11, RL,   &[Srg(C)],                 Other,  Imp),
	InstDesc(0xCB_12, RL,   &[Srg(D)],                 Other,  Imp),
	InstDesc(0xCB_13, RL,   &[Srg(E)],                 Other,  Imp),
	InstDesc(0xCB_14, RL,   &[Srg(H)],                 Other,  Imp),
	InstDesc(0xCB_15, RL,   &[Srg(L)],                 Other,  Imp),
	InstDesc(0xCB_16, RL,   &[IndReg(HL)],             Other,  Ind(HL, RW)),
	InstDesc(0xCB_17, RL,   &[Srg(A)],                 Other,  Imp),
	InstDesc(0xCB_18, RR,   &[Srg(B)],                 Other,  Imp),
	InstDesc(0xCB_19, RR,   &[Srg(C)],                 Other,  Imp),
	InstDesc(0xCB_1A, RR,   &[Srg(D)],                 Other,  Imp),
	InstDesc(0xCB_1B, RR,   &[Srg(E)],                 Other,  Imp),
	InstDesc(0xCB_1C, RR,   &[Srg(H)],                 Other,  Imp),
	InstDesc(0xCB_1D, RR,   &[Srg(L)],                 Other,  Imp),
	InstDesc(0xCB_1E, RR,   &[IndReg(HL)],             Other,  Ind(HL, RW)),
	InstDesc(0xCB_1F, RR,   &[Srg(A)],                 Other,  Imp),
	InstDesc(0xCB_20, SLA,  &[Srg(B)],                 Other,  Imp),
	InstDesc(0xCB_21, SLA,  &[Srg(C)],                 Other,  Imp),
	InstDesc(0xCB_22, SLA,  &[Srg(D)],                 Other,  Imp),
	InstDesc(0xCB_23, SLA,  &[Srg(E)],                 Other,  Imp),
	InstDesc(0xCB_24, SLA,  &[Srg(H)],                 Other,  Imp),
	InstDesc(0xCB_25, SLA,  &[Srg(L)],                 Other,  Imp),
	InstDesc(0xCB_26, SLA,  &[IndReg(HL)],             Other,  Ind(HL, RW)),
	InstDesc(0xCB_27, SLA,  &[Srg(A)],                 Other,  Imp),
	InstDesc(0xCB_28, SRA,  &[Srg(B)],                 Other,  Imp),
	InstDesc(0xCB_29, SRA,  &[Srg(C)],                 Other,  Imp),
	InstDesc(0xCB_2A, SRA,  &[Srg(D)],                 Other,  Imp),
	InstDesc(0xCB_2B, SRA,  &[Srg(E)],                 Other,  Imp),
	InstDesc(0xCB_2C, SRA,  &[Srg(H)],                 Other,  Imp),
	InstDesc(0xCB_2D, SRA,  &[Srg(L)],                 Other,  Imp),
	InstDesc(0xCB_2E, SRA,  &[IndReg(HL)],             Other,  Ind(HL, RW)),
	InstDesc(0xCB_2F, SRA,  &[Srg(A)],                 Other,  Imp),
	InstDesc(0xCB_30, SWAP, &[Srg(B)],                 Other,  Imp),
	InstDesc(0xCB_31, SWAP, &[Srg(C)],                 Other,  Imp),
	InstDesc(0xCB_32, SWAP, &[Srg(D)],                 Other,  Imp),
	InstDesc(0xCB_33, SWAP, &[Srg(E)],                 Other,  Imp),
	InstDesc(0xCB_34, SWAP, &[Srg(H)],                 Other,  Imp),
	InstDesc(0xCB_35, SWAP, &[Srg(L)],                 Other,  Imp),
	InstDesc(0xCB_36, SWAP, &[IndReg(HL)],             Other,  Ind(HL, RW)),
	InstDesc(0xCB_37, SWAP, &[Srg(A)],                 Other,  Imp),
	InstDesc(0xCB_38, SRL,  &[Srg(B)],                 Other,  Imp),
	InstDesc(0xCB_39, SRL,  &[Srg(C)],                 Other,  Imp),
	InstDesc(0xCB_3A, SRL,  &[Srg(D)],                 Other,  Imp),
	InstDesc(0xCB_3B, SRL,  &[Srg(E)],                 Other,  Imp),
	InstDesc(0xCB_3C, SRL,  &[Srg(H)],                 Other,  Imp),
	InstDesc(0xCB_3D, SRL,  &[Srg(L)],                 Other,  Imp),
	InstDesc(0xCB_3E, SRL,  &[IndReg(HL)],             Other,  Ind(HL, RW)),
	InstDesc(0xCB_3F, SRL,  &[Srg(A)],                 Other,  Imp),
	InstDesc(0xCB_40, BIT,  &[Op, Srg(B)],             Other,  Imp),
	InstDesc(0xCB_41, BIT,  &[Op, Srg(C)],             Other,  Imp),
	InstDesc(0xCB_42, BIT,  &[Op, Srg(D)],             Other,  Imp),
	InstDesc(0xCB_43, BIT,  &[Op, Srg(E)],             Other,  Imp),
	InstDesc(0xCB_44, BIT,  &[Op, Srg(H)],             Other,  Imp),
	InstDesc(0xCB_45, BIT,  &[Op, Srg(L)],             Other,  Imp),
	InstDesc(0xCB_46, BIT,  &[Op, IndReg(HL)],         Other,  Ind(HL, R)),
	InstDesc(0xCB_47, BIT,  &[Op, Srg(A)],             Other,  Imp),
	InstDesc(0xCB_48, BIT,  &[Op, Srg(B)],             Other,  Imp),
	InstDesc(0xCB_49, BIT,  &[Op, Srg(C)],             Other,  Imp),
	InstDesc(0xCB_4A, BIT,  &[Op, Srg(D)],             Other,  Imp),
	InstDesc(0xCB_4B, BIT,  &[Op, Srg(E)],             Other,  Imp),
	InstDesc(0xCB_4C, BIT,  &[Op, Srg(H)],             Other,  Imp),
	InstDesc(0xCB_4D, BIT,  &[Op, Srg(L)],             Other,  Imp),
	InstDesc(0xCB_4E, BIT,  &[Op, IndReg(HL)],         Other,  Ind(HL, R)),
	InstDesc(0xCB_4F, BIT,  &[Op, Srg(A)],             Other,  Imp),
	InstDesc(0xCB_50, BIT,  &[Op, Srg(B)],             Other,  Imp),
	InstDesc(0xCB_51, BIT,  &[Op, Srg(C)],             Other,  Imp),
	InstDesc(0xCB_52, BIT,  &[Op, Srg(D)],             Other,  Imp),
	InstDesc(0xCB_53, BIT,  &[Op, Srg(E)],             Other,  Imp),
	InstDesc(0xCB_54, BIT,  &[Op, Srg(H)],             Other,  Imp),
	InstDesc(0xCB_55, BIT,  &[Op, Srg(L)],             Other,  Imp),
	InstDesc(0xCB_56, BIT,  &[Op, IndReg(HL)],         Other,  Ind(HL, R)),
	InstDesc(0xCB_57, BIT,  &[Op, Srg(A)],             Other,  Imp),
	InstDesc(0xCB_58, BIT,  &[Op, Srg(B)],             Other,  Imp),
	InstDesc(0xCB_59, BIT,  &[Op, Srg(C)],             Other,  Imp),
	InstDesc(0xCB_5A, BIT,  &[Op, Srg(D)],             Other,  Imp),
	InstDesc(0xCB_5B, BIT,  &[Op, Srg(E)],             Other,  Imp),
	InstDesc(0xCB_5C, BIT,  &[Op, Srg(H)],             Other,  Imp),
	InstDesc(0xCB_5D, BIT,  &[Op, Srg(L)],             Other,  Imp),
	InstDesc(0xCB_5E, BIT,  &[Op, IndReg(HL)],         Other,  Ind(HL, R)),
	InstDesc(0xCB_5F, BIT,  &[Op, Srg(A)],             Other,  Imp),
	InstDesc(0xCB_60, BIT,  &[Op, Srg(B)],             Other,  Imp),
	InstDesc(0xCB_61, BIT,  &[Op, Srg(C)],             Other,  Imp),
	InstDesc(0xCB_62, BIT,  &[Op, Srg(D)],             Other,  Imp),
	InstDesc(0xCB_63, BIT,  &[Op, Srg(E)],             Other,  Imp),
	InstDesc(0xCB_64, BIT,  &[Op, Srg(H)],             Other,  Imp),
	InstDesc(0xCB_65, BIT,  &[Op, Srg(L)],             Other,  Imp),
	InstDesc(0xCB_66, BIT,  &[Op, IndReg(HL)],         Other,  Ind(HL, R)),
	InstDesc(0xCB_67, BIT,  &[Op, Srg(A)],             Other,  Imp),
	InstDesc(0xCB_68, BIT,  &[Op, Srg(B)],             Other,  Imp),
	InstDesc(0xCB_69, BIT,  &[Op, Srg(C)],             Other,  Imp),
	InstDesc(0xCB_6A, BIT,  &[Op, Srg(D)],             Other,  Imp),
	InstDesc(0xCB_6B, BIT,  &[Op, Srg(E)],             Other,  Imp),
	InstDesc(0xCB_6C, BIT,  &[Op, Srg(H)],             Other,  Imp),
	InstDesc(0xCB_6D, BIT,  &[Op, Srg(L)],             Other,  Imp),
	InstDesc(0xCB_6E, BIT,  &[Op, IndReg(HL)],         Other,  Ind(HL, R)),
	InstDesc(0xCB_6F, BIT,  &[Op, Srg(A)],             Other,  Imp),
	InstDesc(0xCB_70, BIT,  &[Op, Srg(B)],             Other,  Imp),
	InstDesc(0xCB_71, BIT,  &[Op, Srg(C)],             Other,  Imp),
	InstDesc(0xCB_72, BIT,  &[Op, Srg(D)],             Other,  Imp),
	InstDesc(0xCB_73, BIT,  &[Op, Srg(E)],             Other,  Imp),
	InstDesc(0xCB_74, BIT,  &[Op, Srg(H)],             Other,  Imp),
	InstDesc(0xCB_75, BIT,  &[Op, Srg(L)],             Other,  Imp),
	InstDesc(0xCB_76, BIT,  &[Op, IndReg(HL)],         Other,  Ind(HL, R)),
	InstDesc(0xCB_77, BIT,  &[Op, Srg(A)],             Other,  Imp),
	InstDesc(0xCB_78, BIT,  &[Op, Srg(B)],             Other,  Imp),
	InstDesc(0xCB_79, BIT,  &[Op, Srg(C)],             Other,  Imp),
	InstDesc(0xCB_7A, BIT,  &[Op, Srg(D)],             Other,  Imp),
	InstDesc(0xCB_7B, BIT,  &[Op, Srg(E)],             Other,  Imp),
	InstDesc(0xCB_7C, BIT,  &[Op, Srg(H)],             Other,  Imp),
	InstDesc(0xCB_7D, BIT,  &[Op, Srg(L)],             Other,  Imp),
	InstDesc(0xCB_7E, BIT,  &[Op, IndReg(HL)],         Other,  Ind(HL, R)),
	InstDesc(0xCB_7F, BIT,  &[Op, Srg(A)],             Other,  Imp),
	InstDesc(0xCB_80, RES,  &[Op, Srg(B)],             Other,  Imp),
	InstDesc(0xCB_81, RES,  &[Op, Srg(C)],             Other,  Imp),
	InstDesc(0xCB_82, RES,  &[Op, Srg(D)],             Other,  Imp),
	InstDesc(0xCB_83, RES,  &[Op, Srg(E)],             Other,  Imp),
	InstDesc(0xCB_84, RES,  &[Op, Srg(H)],             Other,  Imp),
	InstDesc(0xCB_85, RES,  &[Op, Srg(L)],             Other,  Imp),
	InstDesc(0xCB_86, RES,  &[Op, IndReg(HL)],         Other,  Ind(HL, RW)),
	InstDesc(0xCB_87, RES,  &[Op, Srg(A)],             Other,  Imp),
	InstDesc(0xCB_88, RES,  &[Op, Srg(B)],             Other,  Imp),
	InstDesc(0xCB_89, RES,  &[Op, Srg(C)],             Other,  Imp),
	InstDesc(0xCB_8A, RES,  &[Op, Srg(D)],             Other,  Imp),
	InstDesc(0xCB_8B, RES,  &[Op, Srg(E)],             Other,  Imp),
	InstDesc(0xCB_8C, RES,  &[Op, Srg(H)],             Other,  Imp),
	InstDesc(0xCB_8D, RES,  &[Op, Srg(L)],             Other,  Imp),
	InstDesc(0xCB_8E, RES,  &[Op, IndReg(HL)],         Other,  Ind(HL, RW)),
	InstDesc(0xCB_8F, RES,  &[Op, Srg(A)],             Other,  Imp),
	InstDesc(0xCB_90, RES,  &[Op, Srg(B)],             Other,  Imp),
	InstDesc(0xCB_91, RES,  &[Op, Srg(C)],             Other,  Imp),
	InstDesc(0xCB_92, RES,  &[Op, Srg(D)],             Other,  Imp),
	InstDesc(0xCB_93, RES,  &[Op, Srg(E)],             Other,  Imp),
	InstDesc(0xCB_94, RES,  &[Op, Srg(H)],             Other,  Imp),
	InstDesc(0xCB_95, RES,  &[Op, Srg(L)],             Other,  Imp),
	InstDesc(0xCB_96, RES,  &[Op, IndReg(HL)],         Other,  Ind(HL, RW)),
	InstDesc(0xCB_97, RES,  &[Op, Srg(A)],             Other,  Imp),
	InstDesc(0xCB_98, RES,  &[Op, Srg(B)],             Other,  Imp),
	InstDesc(0xCB_99, RES,  &[Op, Srg(C)],             Other,  Imp),
	InstDesc(0xCB_9A, RES,  &[Op, Srg(D)],             Other,  Imp),
	InstDesc(0xCB_9B, RES,  &[Op, Srg(E)],             Other,  Imp),
	InstDesc(0xCB_9C, RES,  &[Op, Srg(H)],             Other,  Imp),
	InstDesc(0xCB_9D, RES,  &[Op, Srg(L)],             Other,  Imp),
	InstDesc(0xCB_9E, RES,  &[Op, IndReg(HL)],         Other,  Ind(HL, RW)),
	InstDesc(0xCB_9F, RES,  &[Op, Srg(A)],             Other,  Imp),
	InstDesc(0xCB_A0, RES,  &[Op, Srg(B)],             Other,  Imp),
	InstDesc(0xCB_A1, RES,  &[Op, Srg(C)],             Other,  Imp),
	InstDesc(0xCB_A2, RES,  &[Op, Srg(D)],             Other,  Imp),
	InstDesc(0xCB_A3, RES,  &[Op, Srg(E)],             Other,  Imp),
	InstDesc(0xCB_A4, RES,  &[Op, Srg(H)],             Other,  Imp),
	InstDesc(0xCB_A5, RES,  &[Op, Srg(L)],             Other,  Imp),
	InstDesc(0xCB_A6, RES,  &[Op, IndReg(HL)],         Other,  Ind(HL, RW)),
	InstDesc(0xCB_A7, RES,  &[Op, Srg(A)],             Other,  Imp),
	InstDesc(0xCB_A8, RES,  &[Op, Srg(B)],             Other,  Imp),
	InstDesc(0xCB_A9, RES,  &[Op, Srg(C)],             Other,  Imp),
	InstDesc(0xCB_AA, RES,  &[Op, Srg(D)],             Other,  Imp),
	InstDesc(0xCB_AB, RES,  &[Op, Srg(E)],             Other,  Imp),
	InstDesc(0xCB_AC, RES,  &[Op, Srg(H)],             Other,  Imp),
	InstDesc(0xCB_AD, RES,  &[Op, Srg(L)],             Other,  Imp),
	InstDesc(0xCB_AE, RES,  &[Op, IndReg(HL)],         Other,  Ind(HL, RW)),
	InstDesc(0xCB_AF, RES,  &[Op, Srg(A)],             Other,  Imp),
	InstDesc(0xCB_B0, RES,  &[Op, Srg(B)],             Other,  Imp),
	InstDesc(0xCB_B1, RES,  &[Op, Srg(C)],             Other,  Imp),
	InstDesc(0xCB_B2, RES,  &[Op, Srg(D)],             Other,  Imp),
	InstDesc(0xCB_B3, RES,  &[Op, Srg(E)],             Other,  Imp),
	InstDesc(0xCB_B4, RES,  &[Op, Srg(H)],             Other,  Imp),
	InstDesc(0xCB_B5, RES,  &[Op, Srg(L)],             Other,  Imp),
	InstDesc(0xCB_B6, RES,  &[Op, IndReg(HL)],         Other,  Ind(HL, RW)),
	InstDesc(0xCB_B7, RES,  &[Op, Srg(A)],             Other,  Imp),
	InstDesc(0xCB_B8, RES,  &[Op, Srg(B)],             Other,  Imp),
	InstDesc(0xCB_B9, RES,  &[Op, Srg(C)],             Other,  Imp),
	InstDesc(0xCB_BA, RES,  &[Op, Srg(D)],             Other,  Imp),
	InstDesc(0xCB_BB, RES,  &[Op, Srg(E)],             Other,  Imp),
	InstDesc(0xCB_BC, RES,  &[Op, Srg(H)],             Other,  Imp),
	InstDesc(0xCB_BD, RES,  &[Op, Srg(L)],             Other,  Imp),
	InstDesc(0xCB_BE, RES,  &[Op, IndReg(HL)],         Other,  Ind(HL, RW)),
	InstDesc(0xCB_BF, RES,  &[Op, Srg(A)],             Other,  Imp),
	InstDesc(0xCB_C0, SET,  &[Op, Srg(B)],             Other,  Imp),
	InstDesc(0xCB_C1, SET,  &[Op, Srg(C)],             Other,  Imp),
	InstDesc(0xCB_C2, SET,  &[Op, Srg(D)],             Other,  Imp),
	InstDesc(0xCB_C3, SET,  &[Op, Srg(E)],             Other,  Imp),
	InstDesc(0xCB_C4, SET,  &[Op, Srg(H)],             Other,  Imp),
	InstDesc(0xCB_C5, SET,  &[Op, Srg(L)],             Other,  Imp),
	InstDesc(0xCB_C6, SET,  &[Op, IndReg(HL)],         Other,  Ind(HL, RW)),
	InstDesc(0xCB_C7, SET,  &[Op, Srg(A)],             Other,  Imp),
	InstDesc(0xCB_C8, SET,  &[Op, Srg(B)],             Other,  Imp),
	InstDesc(0xCB_C9, SET,  &[Op, Srg(C)],             Other,  Imp),
	InstDesc(0xCB_CA, SET,  &[Op, Srg(D)],             Other,  Imp),
	InstDesc(0xCB_CB, SET,  &[Op, Srg(E)],             Other,  Imp),
	InstDesc(0xCB_CC, SET,  &[Op, Srg(H)],             Other,  Imp),
	InstDesc(0xCB_CD, SET,  &[Op, Srg(L)],             Other,  Imp),
	InstDesc(0xCB_CE, SET,  &[Op, IndReg(HL)],         Other,  Ind(HL, RW)),
	InstDesc(0xCB_CF, SET,  &[Op, Srg(A)],             Other,  Imp),
	InstDesc(0xCB_D0, SET,  &[Op, Srg(B)],             Other,  Imp),
	InstDesc(0xCB_D1, SET,  &[Op, Srg(C)],             Other,  Imp),
	InstDesc(0xCB_D2, SET,  &[Op, Srg(D)],             Other,  Imp),
	InstDesc(0xCB_D3, SET,  &[Op, Srg(E)],             Other,  Imp),
	InstDesc(0xCB_D4, SET,  &[Op, Srg(H)],             Other,  Imp),
	InstDesc(0xCB_D5, SET,  &[Op, Srg(L)],             Other,  Imp),
	InstDesc(0xCB_D6, SET,  &[Op, IndReg(HL)],         Other,  Ind(HL, RW)),
	InstDesc(0xCB_D7, SET,  &[Op, Srg(A)],             Other,  Imp),
	InstDesc(0xCB_D8, SET,  &[Op, Srg(B)],             Other,  Imp),
	InstDesc(0xCB_D9, SET,  &[Op, Srg(C)],             Other,  Imp),
	InstDesc(0xCB_DA, SET,  &[Op, Srg(D)],             Other,  Imp),
	InstDesc(0xCB_DB, SET,  &[Op, Srg(E)],             Other,  Imp),
	InstDesc(0xCB_DC, SET,  &[Op, Srg(H)],             Other,  Imp),
	InstDesc(0xCB_DD, SET,  &[Op, Srg(L)],             Other,  Imp),
	InstDesc(0xCB_DE, SET,  &[Op, IndReg(HL)],         Other,  Ind(HL, RW)),
	InstDesc(0xCB_DF, SET,  &[Op, Srg(A)],             Other,  Imp),
	InstDesc(0xCB_E0, SET,  &[Op, Srg(B)],             Other,  Imp),
	InstDesc(0xCB_E1, SET,  &[Op, Srg(C)],             Other,  Imp),
	InstDesc(0xCB_E2, SET,  &[Op, Srg(D)],             Other,  Imp),
	InstDesc(0xCB_E3, SET,  &[Op, Srg(E)],             Other,  Imp),
	InstDesc(0xCB_E4, SET,  &[Op, Srg(H)],             Other,  Imp),
	InstDesc(0xCB_E5, SET,  &[Op, Srg(L)],             Other,  Imp),
	InstDesc(0xCB_E6, SET,  &[Op, IndReg(HL)],         Other,  Ind(HL, RW)),
	InstDesc(0xCB_E7, SET,  &[Op, Srg(A)],             Other,  Imp),
	InstDesc(0xCB_E8, SET,  &[Op, Srg(B)],             Other,  Imp),
	InstDesc(0xCB_E9, SET,  &[Op, Srg(C)],             Other,  Imp),
	InstDesc(0xCB_EA, SET,  &[Op, Srg(D)],             Other,  Imp),
	InstDesc(0xCB_EB, SET,  &[Op, Srg(E)],             Other,  Imp),
	InstDesc(0xCB_EC, SET,  &[Op, Srg(H)],             Other,  Imp),
	InstDesc(0xCB_ED, SET,  &[Op, Srg(L)],             Other,  Imp),
	InstDesc(0xCB_EE, SET,  &[Op, IndReg(HL)],         Other,  Ind(HL, RW)),
	InstDesc(0xCB_EF, SET,  &[Op, Srg(A)],             Other,  Imp),
	InstDesc(0xCB_F0, SET,  &[Op, Srg(B)],             Other,  Imp),
	InstDesc(0xCB_F1, SET,  &[Op, Srg(C)],             Other,  Imp),
	InstDesc(0xCB_F2, SET,  &[Op, Srg(D)],             Other,  Imp),
	InstDesc(0xCB_F3, SET,  &[Op, Srg(E)],             Other,  Imp),
	InstDesc(0xCB_F4, SET,  &[Op, Srg(H)],             Other,  Imp),
	InstDesc(0xCB_F5, SET,  &[Op, Srg(L)],             Other,  Imp),
	InstDesc(0xCB_F6, SET,  &[Op, IndReg(HL)],         Other,  Ind(HL, RW)),
	InstDesc(0xCB_F7, SET,  &[Op, Srg(A)],             Other,  Imp),
	InstDesc(0xCB_F8, SET,  &[Op, Srg(B)],             Other,  Imp),
	InstDesc(0xCB_F9, SET,  &[Op, Srg(C)],             Other,  Imp),
	InstDesc(0xCB_FA, SET,  &[Op, Srg(D)],             Other,  Imp),
	InstDesc(0xCB_FB, SET,  &[Op, Srg(E)],             Other,  Imp),
	InstDesc(0xCB_FC, SET,  &[Op, Srg(H)],             Other,  Imp),
	InstDesc(0xCB_FD, SET,  &[Op, Srg(L)],             Other,  Imp),
	InstDesc(0xCB_FE, SET,  &[Op, IndReg(HL)],         Other,  Ind(HL, RW)),
	InstDesc(0xCB_FF, SET,  &[Op, Srg(A)],             Other,  Imp),
];