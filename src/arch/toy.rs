//! This is a toy architecture for an imaginary CPU. It exists to make it easier
//! to write tests. It is not stable and can change at any time.

use std::convert::TryInto;

use crate::program::{
	MemAccess,
	MemIndir,
	Operand,
	Instruction,
	InstructionKind,
};
use crate::arch::{
	DisasError, DisasResult,
	Printer, IPrinter, PrinterCtx, FmtResult,
	Disassembler, IDisassembler,
	IArchitecture,
	IIrCompiler,
};
use crate::memory::{ MmuState, Endian, EA, VA };

// ------------------------------------------------------------------------------------------------
// Submodules
// ------------------------------------------------------------------------------------------------

pub mod builder;
pub mod ir;

pub use builder::*;
pub(crate) use ir::*;

// ------------------------------------------------------------------------------------------------
// Reg
// ------------------------------------------------------------------------------------------------

#[repr(u8)]
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum Reg {
	A, B, C, D, // data regs
	DC,         // paired reg
	SP,         // stack ptr
	NF, ZF, CF, // flag regs

	// temporary regs for the IR
	Tmp,
	Tmp16,
	TmpCF,
}

impl Reg {
	/// how many bytes each register takes up
	const fn byte_size(&self) -> usize {
		match self {
			Reg::DC | Reg::SP | Reg::Tmp16 => 2,
			_ => 1,
		}
	}

	/// offset into registers "segment" for the IR
	const fn offset(&self) -> u16 {
		match self {
			Reg::A     => 0,
			Reg::B     => 1,
			Reg::C     => 2,
			Reg::D     => 3,
			Reg::DC    => panic!(),
			Reg::SP    => 4,
			Reg::NF    => 6,
			Reg::ZF    => 7,
			Reg::CF    => 8,
			Reg::Tmp   => 9,
			Reg::Tmp16 => 10,
			Reg::TmpCF => 12,
		}
	}

	fn name(&self) -> &'static str {
		match self {
			Reg::A     => "a",
			Reg::B     => "b",
			Reg::C     => "c",
			Reg::D     => "d",
			Reg::DC    => "dc",
			Reg::SP    => "sp",
			Reg::NF    => "nf",
			Reg::ZF    => "zf",
			Reg::CF    => "cf",
			Reg::Tmp   => "tmp",
			Reg::Tmp16 => "tmp16",
			Reg::TmpCF => "tmpcf",
		}
	}

	fn register_names() -> &'static [&'static str] {
		&["a", "b", "c", "d", "dc", "sp", "nf", "zf", "cf", "tmp", "tmp16", "tmpcf"]
	}
}

impl Default for Reg {
	fn default() -> Reg { Reg::A }
}

// ------------------------------------------------------------------------------------------------
// MetaOp
// ------------------------------------------------------------------------------------------------

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
enum MetaOp {
	MOV,                // copy reg/imm into reg
	ADD, ADC, SUB, SBC, // math(s)
	AND, OR,  XOR, NOT, // bitwise
	CMP, CMC,           // compare (and with carry)
	BLT, BLE, BEQ, BNE, // branch on flags
	JMP, JMI, CAL, RET, // control flow
	LD,  ST,            // memory
}

impl MetaOp {
	/// Instruction mnemonics
	fn mnemonic(&self) -> &'static str {
		use MetaOp::*;
		match self {
			MOV => "mov",
			ADD => "add", ADC => "adc", SUB => "sub", SBC => "sbc",
			AND => "and", OR  => "or",  XOR => "xor", NOT => "not",
			CMP => "cmp", CMC => "cmc",
			BLT => "blt", BLE => "ble", BEQ => "beq", BNE => "bne",
			JMP => "jmp", JMI => "jmi", CAL => "cal", RET => "ret",
			LD  => "ld",  ST  => "st",
		}
	}

	fn access(&self) -> Option<MemAccess> {
		use MetaOp::*;
		match self {
			LD                                      => Some(MemAccess::R),
			ST                                      => Some(MemAccess::W),
			BLT | BLE | BEQ | BNE | JMP | JMI | CAL => Some(MemAccess::Target),
			_                                       => None,
		}
	}
}

// ------------------------------------------------------------------------------------------------
// AddrMode
// ------------------------------------------------------------------------------------------------

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
enum AddrMode {
	IMP,   // no operands
	IMPDC, // hard-coded to dc
	RR,    // (dst, src)
	RI8,   // (dst, imm8)
	S8,    // (  -, simm8)
	I16,   // (  -, imm16)
	RI16,  // (reg, imm16)
}

impl AddrMode {
	fn op_bytes(&self) -> usize {
		use AddrMode::*;

		match self {
			IMP | IMPDC   => 0,
			RR | RI8 | S8 => 1,
			I16 | RI16    => 2,
		}
	}
}

// ------------------------------------------------------------------------------------------------
// Opcode (the REAL opcodes)
// ------------------------------------------------------------------------------------------------

#[allow(non_camel_case_types)]
#[repr(u8)]
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum Opcode {
	MOV_RR, MOV_RI8,
	ADD_RR, ADD_RI8, ADC_RR, ADC_RI8, SUB_RR, SUB_RI8, SBC_RR, SBC_RI8,
	AND_RR, AND_RI8, OR_RR,  OR_RI8,  XOR_RR, XOR_RI8, NOT_RR, NOT_RI8,
	CMP_RR, CMP_RI8, CMC_RR, CMC_RI8,
	BLT_S8, BLE_S8, BEQ_S8, BNE_S8,
	JMP_I16, JMI_IMPDC, CAL_I16, RET_IMP,
	LD_RI16, LD_RR, ST_RI16, ST_RR,
}

const OPCODE_MASK: u8 = 0x3F;
const OPCODE_SHIFT: usize = 6;

// ------------------------------------------------------------------------------------------------
// InstDesc
// ------------------------------------------------------------------------------------------------

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
struct InstDesc {
	opcode:    u8,
	meta_op:   MetaOp,
	addr_mode: AddrMode,
	op_templ:  &'static str,
	kind:      InstructionKind,
}

#[allow(non_snake_case)]
const fn InstDesc(
	opcode:    Opcode,
	meta_op:   MetaOp,
	addr_mode: AddrMode,
	op_templ:  &'static str,
	kind:      InstructionKind,
) -> InstDesc {
	InstDesc { opcode: opcode as u8, meta_op, addr_mode, op_templ, kind }
}

impl InstDesc {
	fn mnemonic(&self) -> &'static str {
		self.meta_op.mnemonic()
	}

	fn op_bytes(&self) -> usize {
		self.addr_mode.op_bytes()
	}

	fn inst_size(&self) -> usize {
		self.op_bytes() + 1
	}
}

mod descs {
	use super::AddrMode::*;
	use super::InstructionKind::*;
	use super::MetaOp::*;
	use super::InstDesc;
	use super::Opcode::*;

	// IMPORTANT: MUST STAY IN SAME ORDER AS Opcode ENUM
	pub(super) const INST_DESCS: &[InstDesc] = &[
		InstDesc(MOV_RR,    MOV, RR,      "{0}, {1}",   Other),
		InstDesc(MOV_RI8,   MOV, RI8,     "{0}, {1}",   Other),
		InstDesc(ADD_RR,    ADD, RR,      "{0}, {1}",   Other),
		InstDesc(ADD_RI8,   ADD, RI8,     "{0}, {1}",   Other),
		InstDesc(ADC_RR,    ADC, RR,      "{0}, {1}",   Other),
		InstDesc(ADC_RI8,   ADC, RI8,     "{0}, {1}",   Other),
		InstDesc(SUB_RR,    SUB, RR,      "{0}, {1}",   Other),
		InstDesc(SUB_RI8,   SUB, RI8,     "{0}, {1}",   Other),
		InstDesc(SBC_RR,    SBC, RR,      "{0}, {1}",   Other),
		InstDesc(SBC_RI8,   SBC, RI8,     "{0}, {1}",   Other),
		InstDesc(AND_RR,    AND, RR,      "{0}, {1}",   Other),
		InstDesc(AND_RI8,   AND, RI8,     "{0}, {1}",   Other),
		InstDesc(OR_RR,     OR,  RR,      "{0}, {1}",   Other),
		InstDesc(OR_RI8,    OR,  RI8,     "{0}, {1}",   Other),
		InstDesc(XOR_RR,    XOR, RR,      "{0}, {1}",   Other),
		InstDesc(XOR_RI8,   XOR, RI8,     "{0}, {1}",   Other),
		InstDesc(NOT_RR,    NOT, RR,      "{0}, {1}",   Other),
		InstDesc(NOT_RI8,   NOT, RI8,     "{0}, {1}",   Other),
		InstDesc(CMP_RR,    CMP, RR,      "{0}, {1}",   Other),
		InstDesc(CMP_RI8,   CMP, RI8,     "{0}, {1}",   Other),
		InstDesc(CMC_RR,    CMC, RR,      "{0}, {1}",   Other),
		InstDesc(CMC_RI8,   CMC, RI8,     "{0}, {1}",   Other),
		InstDesc(BLT_S8,    BLT, S8,      "{0}",        Cond),
		InstDesc(BLE_S8,    BLE, S8,      "{0}",        Cond),
		InstDesc(BEQ_S8,    BEQ, S8,      "{0}",        Cond),
		InstDesc(BNE_S8,    BNE, S8,      "{0}",        Cond),
		InstDesc(JMP_I16,   JMP, I16,     "{0}",        Uncond),
		InstDesc(JMI_IMPDC, JMI, IMPDC,   "[dc]",       Indir),
		InstDesc(CAL_I16,   CAL, I16,     "{0}",        Call),
		InstDesc(RET_IMP,   RET, IMP,     "",           Ret),
		InstDesc(LD_RI16,   LD,  RI16,    "{0}, [{1}]", Other),
		InstDesc(LD_RR,     LD,  RR,      "{0}, [{1}]", Other),
		InstDesc(ST_RI16,   ST,  RI16,    "{0}, [{1}]", Other),
		InstDesc(ST_RR,     ST,  RR,      "{0}, [{1}]", Other),
	];
}

fn lookup_desc(opcode: u8) -> Option<&'static InstDesc> {
	descs::INST_DESCS.get((opcode & OPCODE_MASK) as usize)
}

// ------------------------------------------------------------------------------------------------
// Disassembler
// ------------------------------------------------------------------------------------------------

pub struct ToyDisassembler;

impl IDisassembler for ToyDisassembler {
	fn disas_inst(&self, img: &[u8], _state: MmuState, va: VA, ea: EA)
	-> DisasResult<Instruction> {
		// do we have enough bytes?
		if img.is_empty() {
			return Err(DisasError::out_of_bytes(va, ea, 1, 0));
		}

		// is the opcode OK?
		let desc = match lookup_desc(img[0]) {
			Some(d) => d,
			None => {
				log::trace!("ran into opcode 0x{:02X}", img[0]);
				return Err(DisasError::unknown_instruction(va, ea));
			}
		};

		// do we have enough bytes for the operand?
		let inst_size = desc.inst_size();

		if inst_size > img.len() {
			return Err(DisasError::out_of_bytes(va, ea, inst_size, img.len()));
		}

		// okay cool, let's decode
		let bytes = &img[0 .. inst_size];
		let mut ops = [Operand::Reg(0), Operand::Reg(0)];
		let (num_ops, target) = decode_operands(desc, va, bytes, &mut ops);
		Ok(Instruction::new(va, ea, desc.kind, target, &ops[0 .. num_ops], bytes))
	}
}

fn decode_reg(num: u8) -> Reg {
	match num {
		0 => Reg::A,
		1 => Reg::B,
		2 => Reg::C,
		3 => Reg::D,
		4 => Reg::DC,
		// it's a fake arch, if there's any incorrectly-encoded programs it's MY fault
		_ => panic!("{}", num),
	}
}

fn decode_operands(desc: &InstDesc, va: VA, img: &[u8], ops: &mut [Operand; 2])
-> (usize, Option<VA>) {
	use AddrMode::*;

	let opcode_reg = Operand::Reg(decode_reg(img[0] >> OPCODE_SHIFT) as u8);

	match desc.addr_mode {
		IMP => {
			// only IMP instruction is ret.
			(0, None)
		}
		IMPDC => {
			// only IMPDC instruction is jmi.
			ops[0] = Operand::Reg(Reg::DC as u8);
			(1, None)
		}
		RR => {
			ops[0] = opcode_reg;
			ops[1] = if let Some(access) = desc.meta_op.access() {
				Operand::Indir(MemIndir::Reg { reg: decode_reg(img[1]) as u8 }, access)
			} else {
				let r1 = decode_reg(img[1]) as u8;
				assert!(r1 < 4); // can't use dc as a second operand
				Operand::Reg(r1)
			};

			(2, None)
		}
		RI8 => {
			ops[0] = opcode_reg;
			ops[1] = Operand::UImm(img[1] as u64, None);
			(2, None)
		}
		S8 => {
			let target = VA(((va.0 as isize) + (img[1] as i8 as isize) + 2) as usize);
			ops[0] = Operand::Mem(target, MemAccess::Target);
			(1, Some(target))
		}
		I16 => {
			let target = VA(u16::from_le_bytes(img[1..].try_into().unwrap()) as usize);
			ops[0] = Operand::Mem(target, MemAccess::Target);
			(1, Some(target))
		}
		RI16 => {
			let addr = VA(u16::from_le_bytes(img[1..].try_into().unwrap()) as usize);
			ops[0] = opcode_reg;
			ops[1] = Operand::Mem(addr, desc.meta_op.access().unwrap());
			(2, None)
		}
	}
}

// ------------------------------------------------------------------------------------------------
// Printer
// ------------------------------------------------------------------------------------------------

#[derive(Debug, Copy, Clone)]
pub struct ToyPrinter;

impl ToyPrinter {
	pub fn new() -> Self {
		Self { }
	}

	fn lookup_desc(self, bytes: &[u8]) -> &InstDesc {
		lookup_desc(bytes[0]).expect("ono")
	}
}

impl IPrinter for ToyPrinter {
	// --------------------------------------------------------------------------------------------
	// Required methods

	fn get_mnemonic(&self, i: &Instruction) -> String {
		self.lookup_desc(i.bytes()).mnemonic().into()
	}

	fn print_register(&self, ctx: &mut PrinterCtx, r: u8) -> FmtResult {
		ctx.style_register(&|ctx| ctx.write_str(Reg::register_names()[r as usize]))
	}

	fn print_indir_reg(&self, ctx: &mut PrinterCtx, reg: u8) -> FmtResult {
		ctx.style_symbol(&|ctx| ctx.write_char('['))?;
		self.print_register(ctx, reg)?;
		ctx.style_symbol(&|ctx| ctx.write_char(']'))
	}

	fn print_indir_reg_disp(&self, _ctx: &mut PrinterCtx, _reg: u8, _disp: i64) -> FmtResult {
		unreachable!();
	}

	fn print_raw_va(&self, ctx: &mut PrinterCtx, va: VA) -> FmtResult {
		ctx.style_number(&|ctx| write!(ctx, "0x{:04X}", va))
	}

	// --------------------------------------------------------------------------------------------
	// Provided method overrides

	fn mnemonic_max_len(&self) -> usize {
		3
	}
}

// ------------------------------------------------------------------------------------------------
// Architecture
// ------------------------------------------------------------------------------------------------

pub struct ToyArchitecture;

impl IArchitecture for ToyArchitecture {
	fn endianness      (&self) -> Endian       { Endian::Little }
	fn addr_bits       (&self) -> usize        { 16 }
	fn new_disassembler(&self) -> Disassembler { ToyDisassembler.into() }
	fn new_printer     (&self) -> Printer      { ToyPrinter::new().into() }
}