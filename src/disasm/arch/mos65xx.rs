use std::default::Default;
use std::convert::TryInto;

use parse_display::Display;
use derive_new::new;

use crate::disasm::{
	MemAccess,
	OperandTrait,
	InstructionTrait,
	PrinterTrait,
	NameLookupTrait,
	DisassemblerTrait,
	InstructionKind,
};
use crate::disasm::error::{ DisasError, DisasResult };
use crate::memory::{ Location, VA };

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
	pub fn mnemonic(&self, flavor: SyntaxFlavor) -> &'static str {
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
					ADC  => "adc", AND  => "and", ASLA => "shl", ASL  => "shl",
					BCC  => "bcc", BCS  => "bcs", BEQ  => "beq", BIT  => "bit",
					BMI  => "bmi", BNE  => "bne", BPL  => "bpl", BRK  => "brk",
					BVC  => "bvc", BVS  => "bvs", CLC  => "clr", CLD  => "clr",
					CLI  => "clr", CLV  => "clr", CMP  => "cmp", CPX  => "cmp",
					CPY  => "cmp", DEC  => "dec", DEX  => "dec", DEY  => "dec",
					EOR  => "xor", INC  => "inc", INX  => "inc", INY  => "inc",
					JMP  => "jmp", JSR  => "jsr", LDA  => "ld",  LDAI => "li",
					LDX  => "ld",  LDXI => "li",  LDY  => "ld",  LDYI => "li",
					LSRA => "shr", LSR  => "shr", NOP  => "nop", ORA  => "or",
					PHA  => "psh", PHP  => "psh", PLA  => "pul", PLP  => "pul",
					ROLA => "rol", ROL  => "rol", RORA => "ror", ROR  => "ror",
					RTI  => "rti", RTS  => "rts", SBC  => "sbc", SEC  => "set",
					SED  => "set", SEI  => "set", STA  => "st",  STX  => "st",
					STY  => "st",  TAX  => "mov", TAY  => "mov", TSX  => "mov",
					TXA  => "mov", TXS  => "mov", TYA  => "mov",
				}
		}
	}

	fn extra_operands(&self, flavor: SyntaxFlavor) -> &'static [&'static str] {
		if flavor == SyntaxFlavor::Old {
			return &[];
		}

		use MetaOp::*;

		match self {
			ADC  => &["a"], AND  => &["a"], ASLA => &["a"], CLC  => &["c"], CLD  => &["d"],
			CLI  => &["i"], CLV  => &["v"], CMP  => &["a"], CPX  => &["x"], CPY  => &["y"],
			DEX  => &["x"], DEY  => &["y"], EOR  => &["a"], INX  => &["x"], INY  => &["y"],
			LDA  => &["a"], LDAI => &["a"], LDX  => &["x"], LDXI => &["x"], LDY  => &["y"],
			LDYI => &["y"], LSRA => &["a"], ORA  => &["a"], PHA  => &["a"], PHP  => &["p"],
			PLA  => &["a"], PLP  => &["p"], ROLA => &["a"], RORA => &["a"], SBC  => &["a"],
			SEC  => &["c"], SED  => &["d"], SEI  => &["i"], STA  => &["a"], STX  => &["x"],
			STY  => &["y"], TAX  => &["x", "a"], TAY  => &["y", "a"], TSX  => &["x", "s"],
			TXA  => &["a", "x"], TXS  => &["s", "x"], TYA  => &["a", "y"],
			_ => &[],
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
	// can't use derive_new cause this has to be const!
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

	fn addr(&self) -> VA {
		match self {
			Operand::Mem(a, _) => VA(*a as usize),
			_ => panic!("not a memory operand"),
		}
	}

	fn uimm(&self) -> u64 {
		match self {
			Operand::Imm(i) => *i as u64,
			_ => panic!("not an immediate operand"),
		}
	}

	fn simm(&self) -> i64 {
		match self {
			Operand::Imm(i) => (*i as i8) as i64,
			_ => panic!("not an immediate operand"),
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
	loc:   Location,
	desc:  InstDesc,
	size:  usize,
	op:    Option<Operand>,
	bytes: [u8; MAX_BYTES],
}

impl Instruction {
	fn new(va: VA, loc: Location, desc: InstDesc, size: usize, op: Option<Operand>, orig: &[u8])
	-> Self {
		let mut bytes = [0u8; MAX_BYTES];
		bytes[..orig.len()].copy_from_slice(orig);
		Self { va, loc, desc, size, op, bytes }
	}
}

impl InstructionTrait for Instruction {
	type TOperand = Operand;

	fn va(&self) -> VA                    { self.va }
	fn loc(&self) -> Location             { self.loc }
	fn size(&self) -> usize               { self.size }
	fn num_ops(&self) -> usize            { if self.op.is_some() { 1 } else { 0 } }
	fn get_op(&self, i: usize) -> Operand {
		assert!(i == 0);
		self.op.unwrap()
	}
	fn bytes(&self) -> &[u8]          { &self.bytes[..self.size] }

	fn kind(&self) -> InstructionKind {
		use Opcode::*;
		use InstructionKind::*;

		match self.desc.opcode {
			INVALID           => Invalid,
			JSR_LAB           => Call,
			RTS_IMP | RTI_IMP => Ret,
			JMP_LAB           => Uncond,
			JMP_IND           => Indir,
			_ if self.desc.addr_mode == AddrMode::REL => Cond,
			_                 => Other,
		}
	}

	fn control_target(&self) -> Option<VA> {
		if self.desc.ctrl {
			if let Some(operand) = self.op {
				if let Operand::Mem(addr, _) = operand {
					return Some(VA(addr as usize))
				}

				unreachable!("control instructions can only have memory operands");
			}
		}

		None
	}
}

// ------------------------------------------------------------------------------------------------
// Disassembler
// ------------------------------------------------------------------------------------------------

/// The 65xx disassembler.
pub struct Disassembler;

impl DisassemblerTrait for Disassembler {
	type TInstruction = Instruction;

	fn disas_instr(&self, img: &[u8], va: VA, loc: Location) -> DisasResult<Instruction> {
		// do we have enough bytes?
		if img.is_empty() {
			return Err(DisasError::out_of_bytes(va, loc, 1, 0));
		}

		// is the opcode OK?
		let desc = lookup_desc(img[0]);

		if desc.meta_op == MetaOp::UNK {
			return Err(DisasError::unknown_instruction(va, loc));
		}

		// do we have enough bytes for the operand?
		let inst_size = desc.addr_mode.op_bytes() + 1;

		if inst_size > img.len() {
			return Err(DisasError::out_of_bytes(va, loc, inst_size, img.len()));
		}

		// okay cool, let's decode
		let bytes = &img[0 .. inst_size];
		let op = decode_operand(desc, va, &img[1 .. inst_size]);
		Ok(Instruction::new(va, loc, desc, inst_size, op, bytes))
	}
}

fn decode_operand(desc: InstDesc, va: VA, img: &[u8]) -> Option<Operand> {
	if desc.addr_mode.op_bytes() > 0 {
		use AddrMode::*;
		if let Some(access) = desc.access {
			let addr = match desc.addr_mode {
				ZPG | ZPX | ZPY | IZX | IZY => img[0] as u16,

				ABS | ABX | ABY | IND | LAB => u16::from_le_bytes(img.try_into().unwrap()),

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
#[derive(new)]
pub struct Printer {
	flavor: SyntaxFlavor,
}

impl Printer {
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
		use std::fmt::Write;

		let mut ret = String::new();

		match i.desc.meta_op.extra_operands(self.flavor) {
			[]       => {},
			[o1]     => {
				if i.num_ops() == 0 {
					write!(ret, "{}", o1).unwrap();
				} else {
					write!(ret, "{}, ", o1).unwrap();
				}
			}
			// impossible to have more operands in this case (e.g. mov a, x)
			[o1, o2] => write!(ret, "{}, {}", o1, o2).unwrap(),
			_        => unreachable!()
		}

		if i.num_ops() != 0 {
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
			ret += &template.replace("{}", &operand);
		}

		ret
	}
}