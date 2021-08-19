//! This is a toy architecture for an imaginary CPU. It exists to make it easier
//! to write tests. It is not stable and can change at any time.

use std::convert::TryInto;

use crate::program::{
	MemAccess,
	MemIndir,
	Operand,
	Instruction,
	InstructionKind,
	BasicBlock,
};
use crate::arch::{
	DisasError, DisasResult,
	Printer, IPrinter,
	Disassembler, IDisassembler,
	Interpreter,
	INameLookup,
	IArchitecture,
	IInterpreter, ValueKind,
};
use crate::memory::{ Memory, MmuState, Endian, EA, VA };

// ------------------------------------------------------------------------------------------------
// Reg
// ------------------------------------------------------------------------------------------------

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
enum Reg {
	A, B, C, D, // data regs
	DC,         // paired reg
	NF, ZF, CF, // flag regs

	Tmp, // temporary reg only used in the IR
}

impl Reg {
	/// how many bytes are needed to represent all regs
	const ALL_BYTES: usize = 8;

	/// how many bytes each register takes up
	fn byte_size(&self) -> usize {
		if matches!(self, Reg::DC) { 2 } else { 1 }
	}

	/// offset into registers "segment" for the IR
	fn offset(&self) -> usize {
		match self {
			Reg::A           => 0,
			Reg::B           => 1,
			Reg::C | Reg::DC => 2,
			Reg::D           => 3,
			Reg::NF          => 4,
			Reg::ZF          => 5,
			Reg::CF          => 6,
			Reg::Tmp         => 7,
		}
	}

	fn name(&self) -> &'static str {
		match self {
			Reg::A   => "a",
			Reg::B   => "b",
			Reg::C   => "c",
			Reg::D   => "d",
			Reg::DC  => "dc",
			Reg::NF  => "nf",
			Reg::ZF  => "zf",
			Reg::CF  => "cf",
			Reg::Tmp => "tmp",
		}
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
	UNK,
	LI,                 // load immediate
	ADD, ADC, SUB, SBC, // math(s)
	AND, OR,  XOR, NOT, // bitwise
	CMP, CMC,           // compare (and with carry)
	BLT, BLE, BEQ, BNE, // branch on flags
	JMP, CAL, RET,      // control flow
	LD,  ST,            // memory
}

impl Default for MetaOp {
	fn default() -> Self { Self::UNK }
}

impl MetaOp {
	/// Instruction mnemonics
	fn mnemonic(&self) -> &'static str {
		use MetaOp::*;
		match self {
			UNK => "???",
			LI  => "li",
			ADD => "add", ADC => "adc", SUB => "sub", SBC => "sbc",
			AND => "and", OR  => "or",  XOR => "xor", NOT => "not",
			CMP => "cmp", CMC => "cmc",
			BLT => "blt", BLE => "ble", BEQ => "beq", BNE => "bne",
			JMP => "jmp", CAL => "cal", RET => "ret",
			LD  => "ld",  ST  => "st",
		}
	}

	fn access(&self) -> Option<MemAccess> {
		use MetaOp::*;
		match self {
			LD                          => Some(MemAccess::R),
			ST                          => Some(MemAccess::W),
			BLT | BLE | BEQ | BNE | JMP => Some(MemAccess::Target),
			_                           => None,
		}
	}
}

// ------------------------------------------------------------------------------------------------
// AddrMode
// ------------------------------------------------------------------------------------------------

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
enum AddrMode {
	IMP,  // no operands
	RR,   // (dst, src)
	RI8,  // (dst, imm8)
	S8,   // (-, simm8)
	I16,  // (-, imm16)
	RI16, // (reg, imm16)
}

impl AddrMode {
	fn op_bytes(&self) -> usize {
		use AddrMode::*;

		match self {
			IMP           => 0,
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
	LI_RI8,
	ADD_RR, ADD_RI8, ADC_RR, ADC_RI8, SUB_RR, SUB_RI8, SBC_RR, SBC_RI8,
	AND_RR, AND_RI8, OR_RR,  OR_RI8,  XOR_RR, XOR_RI8, NOT_RR, NOT_RI8,
	CMP_RR, CMP_RI8, CMC_RR, CMC_RI8,
	BLT_S8, BLE_S8, BEQ_S8, BNE_S8,
	JMP_I16, CAL_I16, RET_IMP,
	LD_RI8, LD_RI16, LD_RR, ST_RI8, ST_RI16, ST_RR,
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
		InstDesc(LI_RI8,  LI,  RI8,  "{0}, {1}",   Other),  // 0x00
		InstDesc(ADD_RR,  ADD, RR,   "{0}, {1}",   Other),  // 0x01
		InstDesc(ADD_RI8, ADD, RI8,  "{0}, {1}",   Other),  // 0x02
		InstDesc(ADC_RR,  ADC, RR,   "{0}, {1}",   Other),  // 0x03
		InstDesc(ADC_RI8, ADC, RI8,  "{0}, {1}",   Other),  // 0x04
		InstDesc(SUB_RR,  SUB, RR,   "{0}, {1}",   Other),  // 0x05
		InstDesc(SUB_RI8, SUB, RI8,  "{0}, {1}",   Other),  // 0x06
		InstDesc(SBC_RR,  SBC, RR,   "{0}, {1}",   Other),  // 0x07
		InstDesc(SBC_RI8, SBC, RI8,  "{0}, {1}",   Other),  // 0x08
		InstDesc(AND_RR,  AND, RR,   "{0}, {1}",   Other),  // 0x09
		InstDesc(AND_RI8, AND, RI8,  "{0}, {1}",   Other),  // 0x0A
		InstDesc(OR_RR,   OR,  RR,   "{0}, {1}",   Other),  // 0x0B
		InstDesc(OR_RI8,  OR,  RI8,  "{0}, {1}",   Other),  // 0x0C
		InstDesc(XOR_RR,  XOR, RR,   "{0}, {1}",   Other),  // 0x0D
		InstDesc(XOR_RI8, XOR, RI8,  "{0}, {1}",   Other),  // 0x0E
		InstDesc(NOT_RR,  NOT, RR,   "{0}, {1}",   Other),  // 0x0F
		InstDesc(NOT_RI8, NOT, RI8,  "{0}, {1}",   Other),  // 0x10
		InstDesc(CMP_RR,  CMP, RR,   "{0}, {1}",   Other),  // 0x11
		InstDesc(CMP_RI8, CMP, RI8,  "{0}, {1}",   Other),  // 0x12
		InstDesc(CMC_RR,  CMC, RR,   "{0}, {1}",   Other),  // 0x13
		InstDesc(CMC_RI8, CMC, RI8,  "{0}, {1}",   Other),  // 0x14
		InstDesc(BLT_S8,  BLT, S8,   "{0}",        Cond),   // 0x15
		InstDesc(BLE_S8,  BLE, S8,   "{0}",        Cond),   // 0x16
		InstDesc(BEQ_S8,  BEQ, S8,   "{0}",        Cond),   // 0x17
		InstDesc(BNE_S8,  BNE, S8,   "{0}",        Cond),   // 0x18
		InstDesc(JMP_I16, JMP, I16,  "{0}",        Uncond), // 0x19
		InstDesc(CAL_I16, CAL, I16,  "{0}",        Call),   // 0x1A
		InstDesc(RET_IMP, RET, IMP,  "",           Ret),    // 0x1B
		InstDesc(LD_RI8,  LD,  RI8,  "{0}, [{1}]", Other),  // 0x1C
		InstDesc(LD_RI16, LD,  RI16, "{0}, [{1}]", Other),  // 0x1D
		InstDesc(LD_RR,   LD,  RR,   "{0}, [{1}]", Other),  // 0x1E
		InstDesc(ST_RI8,  ST,  RI8,  "{0}, [{1}]", Other),  // 0x1F
		InstDesc(ST_RI16, ST,  RI16, "{0}, [{1}]", Other),  // 0x20
		InstDesc(ST_RR,   ST,  RR,   "{0}, [{1}]", Other),  // 0x21
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
	fn disas_instr(&self, img: &[u8], _state: MmuState, va: VA, ea: EA)
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

	let opcode_reg = Operand::Reg(decode_reg(img[0] >> OPCODE_SHIFT) as u64);

	match desc.addr_mode {
		IMP => {
			// only IMP instruction is ret.
			(0, None)
		}
		RR => {
			ops[0] = opcode_reg;
			ops[1] = if let Some(access) = desc.meta_op.access() {
				Operand::Indir(MemIndir::Reg { reg: decode_reg(img[1]) as u8 }, access)
			} else {
				Operand::Reg(decode_reg(img[1]) as u64)
			};

			(2, None)
		}
		RI8 => {
			ops[0] = opcode_reg;
			ops[1] = if let Some(access) = desc.meta_op.access() {
				Operand::Mem(img[1] as u64, access)
			} else {
				Operand::UImm(img[1] as u64)
			};

			(2, None)
		}
		S8 => {
			let target = ((va.0 as isize) + (img[1] as i8 as isize) + 2) as usize;
			ops[0] = Operand::Mem(target as u64, MemAccess::Target);
			(1, Some(VA(target)))
		}
		I16 => {
			let target = u16::from_le_bytes(img[1..].try_into().unwrap());
			ops[0] = Operand::Mem(target as u64, MemAccess::Target);
			(1, Some(VA(target as usize)))
		}
		RI16 => {
			let addr = u16::from_le_bytes(img[1..].try_into().unwrap());
			ops[0] = opcode_reg;
			ops[1] = Operand::Mem(addr as u64, desc.meta_op.access().unwrap());
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

	fn fmt_uimm(self, imm: u64) -> String {
		if imm < 0x10 { format!("{}",  imm) } else { format!("0x{:X}", imm) }
	}

	fn fmt_simm(self, imm: i64) -> String {
		if imm.abs() < 0x10 {
			format!("{}",  imm)
		} else {
			let sign = if imm < 0 { "-" } else { "" };
			format!("{}0x{:X}", sign, imm.abs())
		}
	}

	fn fmt_addr(self, addr: VA, state: MmuState, l: &impl INameLookup) -> String {
		match l.lookup(state, addr) {
			Some(name) => name,
			None       => format!("0x{:04X}", addr),
		}
	}

	fn lookup_desc(self, bytes: &[u8]) -> &InstDesc {
		lookup_desc(bytes[0]).expect("ono")
	}
}

impl IPrinter for ToyPrinter {
	fn fmt_mnemonic(&self, i: &Instruction) -> String {
		self.lookup_desc(i.bytes()).mnemonic().into()
	}

	fn fmt_operands(&self, i: &Instruction, state: MmuState, l: &impl INameLookup) -> String {
		let desc = self.lookup_desc(i.bytes());
		let templ = desc.op_templ;

		use AddrMode::*;

		match desc.addr_mode {
			IMP => templ.into(),
			RR => {
				let r0 = decode_reg(i.ops()[0].reg() as u8).name();
				let r1 = decode_reg(i.ops()[1].reg() as u8).name();
				templ.replace("{0}", r0).replace("{1}", r1)
			}
			RI8 => {
				let r0 = decode_reg(i.ops()[0].reg() as u8).name();
				if desc.meta_op.access().is_some() {
					templ.replace("{0}", r0)
						.replace("{1}", &self.fmt_addr(i.ops()[1].addr(), state, l))
				} else {
					templ.replace("{0}", r0)
						.replace("{1}", &self.fmt_uimm(i.ops()[1].uimm()))
				}
			}
			S8 | I16 => {
				let imm = self.fmt_addr(i.ops()[0].addr(), state, l);
				templ.replace("{0}", &imm)
			}
			RI16 => {
				let r0 = decode_reg(i.ops()[0].reg() as u8).name();
				let imm = self.fmt_addr(i.ops()[1].addr(), state, l);
				templ.replace("{0}", r0).replace("{1}", &imm)
			}
		}
	}
}

// ------------------------------------------------------------------------------------------------
// Interpreter (dummied out)
// ------------------------------------------------------------------------------------------------

pub struct ToyInterpreter;

impl ToyInterpreter {
	fn new() -> Self {
		Self {}
	}
}

impl IInterpreter for ToyInterpreter {
	fn reset(&mut self) {}

	fn interpret_bb(&mut self, _mem: &Memory, _bb: &BasicBlock, _state: Option<MmuState>)
	-> Option<EA> {
		None
	}

	fn last_mmu_state_change(&self) -> Option<(MmuState, ValueKind)> {
		None
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
	fn new_interpreter (&self) -> Interpreter  { ToyInterpreter::new().into() }
}
