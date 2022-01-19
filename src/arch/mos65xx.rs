//! The architecture of the MOSTEK 6502 and derivatives (e.g. 6510).

use std::default::Default;
use std::convert::TryInto;

use crate::program::{
	Operand,
	Instruction,
	InstructionKind,
	Radix,
};
use crate::arch::{
	DisasError, DisasResult,
	Printer, IPrinter, PrinterCtx, FmtResult,
	Disassembler, IDisassembler,
	IArchitecture,
};
use crate::memory::{ MmuState, Endian, EA, VA };

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
enum AddrMode {
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
	IZX,
	/// "Indirect Indexed" - double-indirect zero-page Y-indexed (1 byte), e.g. `lda ($10),Y`.
	/// Loads a 2-byte address from `offset`, then accesses the byte at `Y + address`.
	IZY,

	/// PC-relative (1 byte), e.g. `bcc whatever`.
	/// Signed offset added to PC (+2 for size of branch instruction).
	REL,
	/// Alias for `ABS` but for `jmp`/`jsr` instructions, to distinguish their operand types.
	LAB,
}

impl AddrMode {
	/// How many operand bytes are needed for this mode?
	fn op_bytes(self) -> usize {
		use AddrMode::*;
		match self {
			IMP => 0,
			IZX | IZY | ZPG | ZPX | ZPY | IMM | REL => 1,
			ABS | ABX | ABY | IND | LAB => 2,
		}
	}

	/// Is this a zero-page addressing mode?
	fn is_zero_page(self) -> bool {
		use AddrMode::*;
		matches!(self, ZPG | ZPX | ZPY | IZX | IZY)
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
enum MetaOp {
	UNK,
	ADC, AND,  ASLA, ASL,  BCC,  BCS,  BEQ,  BIT, BMI, BNE,
	BPL, BRK,  BVC,  BVS,  CLC,  CLD,  CLI,  CLV, CMP, CPX,
	CPY, DEC,  DEX,  DEY,  EOR,  INC,  INX,  INY, JMP, JSR,
	LDA, LDAI, LDX,  LDXI, LDY,  LDYI, LSRA, LSR, NOP, ORA,
	PHA, PHP,  PLA,  PLP,  ROLA, ROL,  RORA, ROR, RTI, RTS,
	SBC, SEC,  SED,  SEI,  STA,  STX,  STY,  TAX, TAY, TSX,
	TXA, TXS,  TYA,

	DOP,
}

impl MetaOp {
	/// Instruction mnemonics
	fn mnemonic(&self, flavor: SyntaxFlavor) -> &'static str {
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
					TXA  => "txa", TXS  => "txs", TYA  => "tya", DOP  => "dop",
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
					TXA  => "mov", TXS  => "mov", TYA  => "mov", DOP  => "dop",
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
#[repr(u8)]
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
enum Reg {
	A, X, Y, S, P
}

impl Default for Reg {
	fn default() -> Reg { Reg::A }
}

impl Reg {
	fn register_names() -> &'static [&'static str] {
		&["a", "x", "y", "s", "p"]
	}
}

// ------------------------------------------------------------------------------------------------
// InstDesc
// ------------------------------------------------------------------------------------------------

/// A 65xx instruction desc.
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
struct InstDesc {
	/// The actual opcode byte.
	opcode:    Opcode,
	/// Which meta-op this is.
	meta_op:   MetaOp,
	/// What addressing mode is used.
	addr_mode: AddrMode,
	/// Does this access memory, and how?
	access:    Option<fn(u64) -> Operand>,
}

impl InstDesc {
	fn kind(&self) -> InstructionKind {
		match self.opcode {
			Opcode::JSR_LAB                      => InstructionKind::Call,
			Opcode::RTS_IMP | Opcode::RTI_IMP    => InstructionKind::Ret,
			Opcode::JMP_LAB                      => InstructionKind::Uncond,
			Opcode::JMP_IND                      => InstructionKind::Indir,
			_ if self.addr_mode == AddrMode::REL => InstructionKind::Cond,
			_                                    => InstructionKind::Other,
		}
	}
}

// ------------------------------------------------------------------------------------------------
// Disassembler
// ------------------------------------------------------------------------------------------------

/// The 65xx disassembler.
pub struct Mos65xxDisassembler;

impl IDisassembler for Mos65xxDisassembler {
	fn disas_instr(&self, img: &[u8], _state: MmuState, va: VA, ea: EA)
	-> DisasResult<Instruction> {
		// do we have enough bytes?
		if img.is_empty() {
			return Err(DisasError::out_of_bytes(va, ea, 1, 0));
		}

		// is the opcode OK?
		let desc = lookup_desc(img[0]);

		if desc.meta_op == MetaOp::UNK {
			log::trace!("ran into opcode 0x{:02X}", img[0]);
			return Err(DisasError::unknown_instruction(va, ea));
		}

		// do we have enough bytes for the operand?
		let inst_size = desc.addr_mode.op_bytes() + 1;

		if inst_size > img.len() {
			return Err(DisasError::out_of_bytes(va, ea, inst_size, img.len()));
		}

		// okay cool, let's decode
		let bytes = &img[0 .. inst_size];
		let (op, target) = decode_operand(desc, va, &img[1 .. inst_size]);
		let ops = match &op {
			Some(op) => std::slice::from_ref(op),
			None     => &[],
		};
		Ok(Instruction::new(va, ea, desc.kind(), target, ops, bytes))
	}
}

fn decode_operand(desc: InstDesc, va: VA, img: &[u8]) -> (Option<Operand>, Option<VA>) {
	if desc.addr_mode.op_bytes() > 0 {
		use AddrMode::*;
		if let Some(operand_ctor) = desc.access {
			let addr = match desc.addr_mode {
				ZPG | ZPX | ZPY | IZX | IZY => img[0] as u16,

				ABS | ABX | ABY | IND | LAB => u16::from_le_bytes(img.try_into().unwrap()),

				// +2 to include size of the branch instruction itself
				REL => ((va.0 as i32) + (img[0] as i8 as i32) + 2) as u16,
				_ => unreachable!()
			};

			let target = if desc.kind().has_control_target() {
				Some(VA(addr as usize))
			} else {
				None
			};
			(Some(operand_ctor(addr as u64)), target)
		} else {
			assert!(matches!(desc.addr_mode, IMM));
			(Some(Operand::UImm(img[0] as u64, None)), None)
		}
	} else {
		(None, None)
	}
}

// ------------------------------------------------------------------------------------------------
// Mos65xxPrinter
// ------------------------------------------------------------------------------------------------

/// The 65xx instruction printer.
#[derive(Debug, Copy, Clone)]
pub struct Mos65xxPrinter {
	flavor: SyntaxFlavor,
}

impl Mos65xxPrinter {
	pub fn new(flavor: SyntaxFlavor) -> Self {
		Self { flavor }
	}

	fn fmt_imm(self, imm: u64, radix: Option<Radix>) -> String {
		let radix = radix.unwrap_or_else(|| if imm < 0x10 { Radix::Dec } else { Radix::Hex });

		match self.flavor {
			SyntaxFlavor::Old => {
				match radix {
					Radix::Dec => format!("#{}", imm),
					Radix::Hex => format!("#${:X}", imm),
					Radix::Bin => format!("#%{:b}", imm),
				}
			}
			SyntaxFlavor::New => {
				match radix {
					Radix::Dec => format!("{}",  imm),
					Radix::Hex => format!("0x{:X}", imm),
					Radix::Bin => format!("0b{:b}", imm),
				}
			}
		}
	}

	fn fmt_addr(self, addr: VA, zpg: bool) -> String {
		match self.flavor {
			SyntaxFlavor::Old =>
				if zpg { format!("${:02X}", addr) } else { format!("${:04X}", addr) },
			SyntaxFlavor::New =>
				if zpg { format!("0x{:02X}", addr)} else { format!("0x{:04X}", addr) },
		}
	}
}

impl IPrinter for Mos65xxPrinter {
	fn mnemonic_max_len(&self) -> usize {
		3
	}

	fn get_mnemonic(&self, i: &Instruction) -> String {
		let desc = lookup_desc(i.bytes()[0]);
		desc.meta_op.mnemonic(self.flavor).into()
	}

	// TODO: customize print_[u]int_hex based on syntax

	fn print_register(&self, ctx: &mut PrinterCtx, r: u8) -> FmtResult {
		ctx.style_register(&|ctx| ctx.write_str(Reg::register_names()[r as usize]))
	}

	// TODO: use is_zero_page to customize address display? I think 6502 assemblers
	// use a different syntax to select zero page addressing
	fn print_raw_va(&self, ctx: &mut PrinterCtx, va: VA) -> FmtResult {
		ctx.style_number(&|ctx| {
			match self.flavor {
				SyntaxFlavor::Old => write!(ctx, "${:04X}", va),
				SyntaxFlavor::New => write!(ctx, "0x{:04X}", va),
			}
		})
	}

	fn print_operands(&self, ctx: &mut PrinterCtx) -> FmtResult {
		let desc = lookup_desc(ctx.get_inst().bytes()[0]);

		// "extra operands" are always registers, as shown in the new-style syntax.
		match desc.meta_op.extra_operands(self.flavor) {
			[] => {},
			[o1] => {
				ctx.style_register(&|ctx| write!(ctx, "{}", o1))?;

				if ctx.num_ops() != 0 {
					ctx.write_str(", ")?;
				}
			}
			// impossible to have more operands in this case (e.g. mov a, x)
			[o1, o2] => {
				ctx.style_register(&|ctx| write!(ctx, "{}", o1))?;
				ctx.write_str(", ")?;
				ctx.style_register(&|ctx| write!(ctx, "{}", o2))?;
			}
			_ => unreachable!()
		}

		if ctx.num_ops() > 0 {
			use AddrMode::*;

			match self.flavor {
				SyntaxFlavor::Old => {
					match desc.addr_mode {
						ABS | ZPG | REL | LAB | IMM => {
							// {}
							// TODO: IMM should put a # before its operand, right?
							self.print_operand(ctx, 0)?;
						}
						ABX | ZPX => {
							// {},x
							self.print_operand(ctx, 0)?;
							ctx.write_char(',')?;
							self.print_register(ctx, Reg::X as u8)?;
						}
						ABY | ZPY => {
							// {},y
							self.print_operand(ctx, 0)?;
							ctx.write_char(',')?;
							self.print_register(ctx, Reg::Y as u8)?;
						}
						IND => {
							// ({})
							ctx.write_char('(')?;
							self.print_operand(ctx, 0)?;
							ctx.write_char(')')?;
						}
						IZX => {
							// ({},x)
							ctx.write_char('(')?;
							self.print_operand(ctx, 0)?;
							ctx.write_char(',')?;
							self.print_register(ctx, Reg::X as u8)?;
							ctx.write_char(')')?;
						}
						IZY => {
							// ({}),y
							ctx.write_char('(')?;
							self.print_operand(ctx, 0)?;
							ctx.write_char(')')?;
							ctx.write_char(',')?;
							self.print_register(ctx, Reg::Y as u8)?;
						}
						IMP => unreachable!(),
					}
				}

				SyntaxFlavor::New => {
					match desc.addr_mode {
						REL | LAB | IMM => {
							// {}
							self.print_operand(ctx, 0)?;
						}
						ZPG | ABS | IND => {
							// [{}]
							ctx.write_char('[')?;
							self.print_operand(ctx, 0)?;
							ctx.write_char(']')?;
						}
						ZPX | ABX => {
							// [{} + x]
							ctx.write_char('[')?;
							self.print_operand(ctx, 0)?;
							ctx.write_str(" + ")?;
							self.print_register(ctx, Reg::X as u8)?;
							ctx.write_char(']')?;
						}
						ZPY | ABY => {
							// [{} + y]
							ctx.write_char('[')?;
							self.print_operand(ctx, 0)?;
							ctx.write_str(" + ")?;
							self.print_register(ctx, Reg::Y as u8)?;
							ctx.write_char(']')?;
						}
						IZX => {
							// [[{} + x]]
							ctx.write_str("[[")?;
							self.print_operand(ctx, 0)?;
							ctx.write_str(" + ")?;
							self.print_register(ctx, Reg::X as u8)?;
							ctx.write_str("]]")?;
						}
						IZY => {
							// [[{}] + y]
							ctx.write_str("[[")?;
							self.print_operand(ctx, 0)?;
							ctx.write_str("] + ")?;
							self.print_register(ctx, Reg::Y as u8)?;
							ctx.write_char(']')?;
						}
						IMP => unreachable!(),
					}
				}
			}
		}

		Ok(())
	}

	// Most of the work is done print_operands above, including printing the register;
	// here we just have to display the displacement as an address.
	fn print_indir_reg_disp(&self, ctx: &mut PrinterCtx, _reg: u8, disp: i64) -> FmtResult {
		self.print_va(ctx, VA(disp as usize))
	}
}

// ------------------------------------------------------------------------------------------------
// Architecture
// ------------------------------------------------------------------------------------------------

pub struct Mos65xxArchitecture;

impl IArchitecture for Mos65xxArchitecture {
	fn endianness      (&self) -> Endian       { Endian::Little }
	fn addr_bits       (&self) -> usize        { 16 }
	fn new_disassembler(&self) -> Disassembler { Mos65xxDisassembler.into() }
	fn new_printer     (&self) -> Printer      { Mos65xxPrinter::new(SyntaxFlavor::New).into() }
}
