use std::default::Default;

use parse_display::Display;

use crate::disasm::{
	MemAccess,
	OperandTrait,
	InstructionTrait,
	PrinterTrait,
	NameLookupTrait,
	DisassemblerTrait,
};
use crate::disasm::error::{ DisasError, DisasResult };
use crate::memory::VA;

// ------------------------------------------------------------------------------------------------
// Sub-modules
// ------------------------------------------------------------------------------------------------

mod descs;
mod opcodes;
#[cfg(test)]
mod tests;

use descs::{ lookup_desc };
use opcodes::{ Opcode };


// ------------------------------------------------------------------------------------------------
// SyntaxFlavor
// ------------------------------------------------------------------------------------------------

/// Which flavor of 65xx syntax the printer will use.
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum SyntaxFlavor {
	/// The traditional syntax everyone uses.
	Old,

	/// A new less-confusing one I made.
	New,
}

// ------------------------------------------------------------------------------------------------
// AddrMode
// ------------------------------------------------------------------------------------------------

/// All 65xx addressing modes.
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum AddrMode {
	/// Implied (no operand bytes), e.g. `rol`.
	IMP,
	/// Immediate (1 byte), e.g. `lda #$30`.
	IMM,

	/// Zero-page absolute (1 byte), e.g. `lda $10`.
	ZPG,
	/// Zero-page, X-indexed (1 byte), e.g. `lda $80,X`.
	ZPX,
	/// Zero-page, Y-indexed (1 byte), e.g. `lda $80,Y`.
	ZPY,

	/// Absolute (2 bytes), e.g. `lda $8040`.
	ABS,
	/// Absolute, X-indexed (2 bytes), e.g. `lda $8040,X`
	ABX,
	/// Absolute, Y-indexed (2 bytes), e.g. `lda $8040,Y`
	ABY,

	/// Indirect (2 bytes); used only for indirect jump i.e. `jmp ($2000)`.
	IND,
	/// "Indexed Indirect" - double-indirect zero-page X-indexed (1 byte), e.g. `lda ($10,X)`.
	/// Loads a 2-byte address from `X + offset`, then accesses the byte at that address.
	IZX, // ($11,X) where $11 is ZPGaddr
	/// "Indirect Indexed" - double-indirect zero-page Y-indexed (1 byte), e.g. `lda ($10),Y`.
	/// Loads a 2-byte address from `offset`, then accesses the byte at `Y + address`.
	IZY, // ($11),Y where $11 is ZPGaddr

	/// PC-relative (1 byte), e.g. `bcc whatever`.
	/// Signed offset added to PC (+2 for size of branch instruction).
	REL,
	/// Alias for `ABS` but for `jmp`/`jsr` instructions, to distinguish their operand types.
	LAB,
}

impl AddrMode {
	/// How many operand bytes are needed for this mode?
	pub fn op_bytes(self) -> usize {
		use AddrMode::*;
		match self {
			IMP => 0,
			IZX | IZY | ZPG | ZPX | ZPY | IMM | REL => 1,
			ABS | ABX | ABY | IND | LAB => 2,
		}
	}

	/// Is this a zero-page addressing mode?
	pub fn is_zero_page(self) -> bool {
		use AddrMode::*;
		matches!(self, ZPG | ZPX | ZPY | IZX | IZY)
	}

	/// Operand printing template
	pub fn operand_template(self, flavor: SyntaxFlavor) -> &'static str {
		use AddrMode::*;

		match flavor {
			SyntaxFlavor::Old =>
				match self {
					ABS | ZPG | REL | LAB | IMM => "{}",
					ABX | ZPX                   => "{},x",
					ABY | ZPY                   => "{},y",
					IND                         => "({})",
					IZX                         => "({},x)",
					IZY                         => "({}),y",
					IMP                         => unreachable!(),
				},

			SyntaxFlavor::New =>
				match self {
					REL | LAB | IMM => "{}",
					ZPG | ABS | IND => "[{}]",
					ZPX | ABX       => "[{} + x]",
					ZPY | ABY       => "[{} + y]",
					IZX             => "[[{} + x]]",
					IZY             => "[[{}] + y]",
					IMP             => unreachable!(),
				},
		}
	}
}

// ------------------------------------------------------------------------------------------------
// MetaOp
// ------------------------------------------------------------------------------------------------

/// The "fundamental operation" that an instruction performs, regardless of addressing mode.
/// Each meta-op can cover multiple real instructions with different addressing modes.
/// These correspond (mostly) with what a programmer would type.
///
/// The few exceptions are `LDAI, LDXI, LDYI, LSRA, ROLA,` and `RORA`. These are separated because
/// in the new syntax, these look different than their original forms. `LDAI/LDXI/LDYI` are the
/// "load immediate" instructions and look like `li`, while `LSRA/ROLA/RORA` have an implied
/// `a` operand.
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
	/// Instruction mnemonics
	pub fn mnemonic(self, flavor: SyntaxFlavor) -> &'static str {
		use MetaOp::*;
		match flavor {
			SyntaxFlavor::Old =>
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
				},

			SyntaxFlavor::New =>
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
}

// ------------------------------------------------------------------------------------------------
// Reg
// ------------------------------------------------------------------------------------------------

/// 65xx registers.
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum Reg {
	A, X, Y, S, P
}

impl Default for Reg {
	fn default() -> Reg { Reg::A }
}

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
	va:    VA,
	desc:  InstDesc,
	size:  usize,
	op:    Option<Operand>,
	bytes: [u8; MAX_BYTES],
}

impl Instruction {
	fn new(va: VA, desc: InstDesc, size: usize, op: Option<Operand>, orig: &[u8])
	-> Self {
		let mut bytes = [0u8; MAX_BYTES];
		bytes[..orig.len()].copy_from_slice(orig);
		Self { va, desc, size, op, bytes }
	}
}

impl InstructionTrait for Instruction {
	type TOperand = Operand;

	fn va(&self) -> VA                 { self.va }
	fn size(&self) -> usize               { self.size }
	fn num_ops(&self) -> usize            { if self.op.is_some() { 1 } else { 0 } }
	fn get_op(&self, i: usize) -> Operand {
		assert!(i == 0);
		self.op.unwrap()
	}
	fn bytes(&self) -> &[u8]          { &self.bytes[..self.size] }

	fn is_control    (&self) -> bool { self.desc.ctrl }
	fn is_conditional(&self) -> bool { self.desc.addr_mode == AddrMode::REL }
	fn is_jump       (&self) -> bool { matches!(self.desc.opcode, Opcode::JMP_LAB) }
	fn is_indir_jump (&self) -> bool { matches!(self.desc.opcode, Opcode::JMP_IND) }
	fn is_call       (&self) -> bool { matches!(self.desc.opcode, Opcode::JSR_LAB) }
	fn is_invalid    (&self) -> bool { matches!(self.desc.opcode, Opcode::INVALID) }
	fn is_halt       (&self) -> bool { false }
	fn is_return     (&self) -> bool {
		matches!(self.desc.opcode, Opcode::RTS_IMP | Opcode::RTI_IMP)
	}
}

// ------------------------------------------------------------------------------------------------
// Disassembler
// ------------------------------------------------------------------------------------------------

/// The 65xx disassembler.
pub struct Disassembler;

impl DisassemblerTrait for Disassembler {
	type TInstruction = Instruction;

	fn disas_instr(&self, img: &[u8], va: VA) -> DisasResult<Instruction> {
		// do we have enough bytes?
		if img.is_empty() {
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

fn decode_operand(desc: InstDesc, va: VA, img: &[u8]) -> Option<Operand> {
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

	fn fmt_imm(self, imm: u8) -> String {
		match self.flavor {
			SyntaxFlavor::Old =>
				if imm < 0x10 { format!("#{}", imm) } else { format!("#${:X}", imm) },
			SyntaxFlavor::New =>
				if imm < 0x10 { format!("{}",  imm) } else { format!("0x{:X}", imm) },
		}
	}

	fn fmt_addr(self, addr: u16, zpg: bool) -> String {
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
					match l.lookup(VA(addr as usize)) {
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