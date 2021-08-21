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
	IIrCompiler,
};
use crate::memory::{ Memory, MmuState, Endian, EA, VA };
use crate::ir::{ IrBuilder };

// ------------------------------------------------------------------------------------------------
// Reg
// ------------------------------------------------------------------------------------------------

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum Reg {
	A, B, C, D, // data regs
	DC,         // paired reg
	SP,         // stack ptr
	NF, ZF, CF, // flag regs

	Tmp, Tmp16, // temporary reg for the IR
	TmpCF,      // temporary carry flag
}

impl Reg {
	/// how many bytes are needed to represent all regs
	const ALL_BYTES: usize = 12;

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
			Reg::A                => 0,
			Reg::B                => 1,
			Reg::C | Reg::DC      => 2,
			Reg::D                => 3,
			Reg::NF               => 4,
			Reg::ZF               => 5,
			Reg::CF               => 6,
			Reg::Tmp | Reg::Tmp16 => 7,
			Reg::TmpCF            => 9,
			Reg::SP               => 10,
		}
	}

	fn name(&self) -> &'static str {
		match self {
			Reg::A     => "a",
			Reg::B     => "b",
			Reg::C     => "c",
			Reg::D     => "d",
			Reg::DC    => "dc",
			Reg::NF    => "nf",
			Reg::ZF    => "zf",
			Reg::CF    => "cf",
			Reg::Tmp   => "tmp",
			Reg::Tmp16 => "tmp16",
			Reg::TmpCF => "tmpcf",
			Reg::SP    => "sp",
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
	LI,  MOV,           // load immediate, reg-reg move
	ADD, ADC, SUB, SBC, // math(s)
	AND, OR,  XOR, NOT, // bitwise
	CMP, CMC,           // compare (and with carry)
	BLT, BLE, BEQ, BNE, // branch on flags
	JMP, CAL, RET,      // control flow
	LD,  ST,            // memory
}

impl MetaOp {
	/// Instruction mnemonics
	fn mnemonic(&self) -> &'static str {
		use MetaOp::*;
		match self {
			LI  => "li",  MOV => "mov",
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
			LD                                => Some(MemAccess::R),
			ST                                => Some(MemAccess::W),
			BLT | BLE | BEQ | BNE | JMP | CAL => Some(MemAccess::Target),
			_                                 => None,
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
	LI_RI8, MOV_RR,
	ADD_RR, ADD_RI8, ADC_RR, ADC_RI8, SUB_RR, SUB_RI8, SBC_RR, SBC_RI8,
	AND_RR, AND_RI8, OR_RR,  OR_RI8,  XOR_RR, XOR_RI8, NOT_RR, NOT_RI8,
	CMP_RR, CMP_RI8, CMC_RR, CMC_RI8,
	BLT_S8, BLE_S8, BEQ_S8, BNE_S8,
	JMP_I16, CAL_I16, RET_IMP,
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
		InstDesc(LI_RI8,  LI,  RI8,  "{0}, {1}",   Other),
		InstDesc(MOV_RR,  MOV, RR,   "{0}, {1}",   Other),
		InstDesc(ADD_RR,  ADD, RR,   "{0}, {1}",   Other),
		InstDesc(ADD_RI8, ADD, RI8,  "{0}, {1}",   Other),
		InstDesc(ADC_RR,  ADC, RR,   "{0}, {1}",   Other),
		InstDesc(ADC_RI8, ADC, RI8,  "{0}, {1}",   Other),
		InstDesc(SUB_RR,  SUB, RR,   "{0}, {1}",   Other),
		InstDesc(SUB_RI8, SUB, RI8,  "{0}, {1}",   Other),
		InstDesc(SBC_RR,  SBC, RR,   "{0}, {1}",   Other),
		InstDesc(SBC_RI8, SBC, RI8,  "{0}, {1}",   Other),
		InstDesc(AND_RR,  AND, RR,   "{0}, {1}",   Other),
		InstDesc(AND_RI8, AND, RI8,  "{0}, {1}",   Other),
		InstDesc(OR_RR,   OR,  RR,   "{0}, {1}",   Other),
		InstDesc(OR_RI8,  OR,  RI8,  "{0}, {1}",   Other),
		InstDesc(XOR_RR,  XOR, RR,   "{0}, {1}",   Other),
		InstDesc(XOR_RI8, XOR, RI8,  "{0}, {1}",   Other),
		InstDesc(NOT_RR,  NOT, RR,   "{0}, {1}",   Other),
		InstDesc(NOT_RI8, NOT, RI8,  "{0}, {1}",   Other),
		InstDesc(CMP_RR,  CMP, RR,   "{0}, {1}",   Other),
		InstDesc(CMP_RI8, CMP, RI8,  "{0}, {1}",   Other),
		InstDesc(CMC_RR,  CMC, RR,   "{0}, {1}",   Other),
		InstDesc(CMC_RI8, CMC, RI8,  "{0}, {1}",   Other),
		InstDesc(BLT_S8,  BLT, S8,   "{0}",        Cond),
		InstDesc(BLE_S8,  BLE, S8,   "{0}",        Cond),
		InstDesc(BEQ_S8,  BEQ, S8,   "{0}",        Cond),
		InstDesc(BNE_S8,  BNE, S8,   "{0}",        Cond),
		InstDesc(JMP_I16, JMP, I16,  "{0}",        Uncond),
		InstDesc(CAL_I16, CAL, I16,  "{0}",        Call),
		InstDesc(RET_IMP, RET, IMP,  "",           Ret),
		InstDesc(LD_RI16, LD,  RI16, "{0}, [{1}]", Other),
		InstDesc(LD_RR,   LD,  RR,   "{0}, [{1}]", Other),
		InstDesc(ST_RI16, ST,  RI16, "{0}, [{1}]", Other),
		InstDesc(ST_RR,   ST,  RR,   "{0}, [{1}]", Other),
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
				let r1 = decode_reg(img[1]) as u64;
				assert!(r1 < 4); // can't use dc as a second operand
				Operand::Reg(r1)
			};

			(2, None)
		}
		RI8 => {
			ops[0] = opcode_reg;
			ops[1] = Operand::UImm(img[1] as u64);
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

fn inst_reg(i: &Instruction, op: usize) -> Reg {
	decode_reg(i.ops()[op].reg() as u8)
}

fn inst_addr(i: &Instruction, op: usize) -> VA {
	i.ops()[op].addr()
}

fn inst_imm(i: &Instruction) -> u8 {
	i.ops()[1].uimm() as u8
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
				let r0 = inst_reg(i, 0).name();
				let r1 = inst_reg(i, 1).name();
				templ.replace("{0}", r0).replace("{1}", r1)
			}
			RI8 => {
				let r0 = inst_reg(i, 0).name();
				if desc.meta_op.access().is_some() {
					templ.replace("{0}", r0)
						.replace("{1}", &self.fmt_addr(inst_addr(i, 1), state, l))
				} else {
					templ.replace("{0}", r0)
						.replace("{1}", &self.fmt_uimm(inst_imm(i) as u64))
				}
			}
			S8 | I16 => {
				let imm = self.fmt_addr(inst_addr(i, 0), state, l);
				templ.replace("{0}", &imm)
			}
			RI16 => {
				let r0 = inst_reg(i, 0).name();
				let imm = self.fmt_addr(inst_addr(i, 1), state, l);
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

// ------------------------------------------------------------------------------------------------
// IR
// ------------------------------------------------------------------------------------------------

pub struct ToyIrCompiler;

impl IIrCompiler for ToyIrCompiler {
	fn to_ir(&self, i: &Instruction, target: Option<EA>, b: &mut IrBuilder) {
		lookup_desc(i.bytes()[0]).expect("ono").to_ir(i, target, b);
	}
}

mod ir {
	use super::*;
	use crate::ir::{ IrReg, Const, Src, IrBuilder };

	const REG_A:     IrReg = IrReg::reg8(Reg::A.offset());
	const REG_B:     IrReg = IrReg::reg8(Reg::B.offset());
	const REG_C:     IrReg = IrReg::reg8(Reg::C.offset());
	const REG_D:     IrReg = IrReg::reg8(Reg::D.offset());
	const REG_DC:    IrReg = IrReg::reg16(Reg::DC.offset());
	const REG_NF:    IrReg = IrReg::reg8(Reg::NF.offset());
	const REG_ZF:    IrReg = IrReg::reg8(Reg::ZF.offset());
	const REG_CF:    IrReg = IrReg::reg8(Reg::CF.offset());
	const REG_TMP:   IrReg = IrReg::reg8(Reg::Tmp.offset());
	const REG_TMP16: IrReg = IrReg::reg16(Reg::Tmp16.offset());
	const REG_TMPCF: IrReg = IrReg::reg8(Reg::TmpCF.offset());
	const REG_SP:    IrReg = IrReg::reg16(Reg::SP.offset());

	fn reg_to_ir_reg(reg: Reg) -> IrReg {
		match reg {
			Reg::A  => REG_A,
			Reg::B  => REG_B,
			Reg::C  => REG_C,
			Reg::D  => REG_D,
			Reg::DC => REG_DC,
			_       => panic!(),
		}
	}

	impl InstDesc {
		fn r1(&self, i: &Instruction) -> Src {
			match self.addr_mode {
				AddrMode::RR   => reg_to_ir_reg(inst_reg(i, 1)).into(),
				AddrMode::RI8  => Const::_8(inst_imm(i)).into(),
				AddrMode::RI16 => Const::_16(inst_addr(i, 1).0 as u16).into(),
				_ => panic!(),
			}
		}

		pub(super) fn to_ir(&self, i: &Instruction, target: Option<EA>, b: &mut IrBuilder) {
			use MetaOp::*;

			fn r0(i: &Instruction) -> IrReg {
				reg_to_ir_reg(inst_reg(i, 0))
			}

			match self.meta_op {
				LI | MOV => {
					b.assign(i.ea(), r0(i), self.r1(i));
				}
				ADD => {
					let s1 = r0(i);
					let s2 = self.r1(i);
					b.icarry(i.ea(), REG_CF, s1, s2);
					b.iuadd(i.ea(),  s1,     s1, s2);
				}
				ADC => {
					let s1 = r0(i);
					let s2 = self.r1(i);
					b.assign(i.ea(),  REG_TMPCF, REG_CF);
					b.icarryc(i.ea(), REG_CF,    s1, s2, REG_CF);
					b.iuaddc(i.ea(),  s1,        s1, s2, REG_TMPCF);
				}
				SUB => {
					let s1 = r0(i);
					let s2 = self.r1(i);
					b.isborrow(i.ea(), REG_CF, s1, s2);
					b.iusub(i.ea(),    s1,     s1, s2);
				}
				SBC => {
					let s1 = r0(i);
					let s2 = self.r1(i);
					b.assign(i.ea(),    REG_TMPCF, REG_CF);
					b.isborrowc(i.ea(), REG_CF,    s1, s2, REG_CF);
					b.iusubb(i.ea(),    s1,        s1, s2, REG_TMPCF);
				}
				AND => {
					let s1 = r0(i);
					let s2 = self.r1(i);
					b.iand(i.ea(), s1, s1, s2);
				}
				OR => {
					let s1 = r0(i);
					let s2 = self.r1(i);
					b.ior(i.ea(), s1, s1, s2);
				}
				XOR => {
					let s1 = r0(i);
					let s2 = self.r1(i);
					b.ixor(i.ea(), s1, s1, s2);
				}
				NOT => {
					let s1 = r0(i);
					let s2 = self.r1(i);
					b.inot(i.ea(), s1, s2);
				}
				CMP => {
					let s1 = r0(i);
					let s2 = self.r1(i);
					b.ieq(i.ea(),  REG_ZF, s1, s2);
					b.islt(i.ea(), REG_NF, s1, s2);
					b.iult(i.ea(), REG_CF, s1, s2);
				}
				CMC => {
					let s1 = r0(i);
					let s2 = self.r1(i);
					b.assign(i.ea(),    REG_TMPCF, REG_CF);
					b.isborrowc(i.ea(), REG_CF,    s1, s2, REG_CF);
					b.iusubb(i.ea(),    REG_TMP,   s1, s2, REG_TMPCF);
					b.ieq(i.ea(),       REG_ZF, REG_TMP, Const::ZERO_8);
					b.islt(i.ea(),      REG_NF, REG_TMP, Const::ZERO_8);
				}
				BLT => {
					b.cbranch(i.ea(), REG_NF, target.unwrap());
				}
				BLE => {
					b.bor(i.ea(),     REG_TMP, REG_CF, REG_ZF);
					b.cbranch(i.ea(), REG_TMP, target.unwrap());
				}
				BEQ => {
					b.cbranch(i.ea(), REG_ZF, target.unwrap());
				}
				BNE => {
					b.bnot(i.ea(),    REG_TMP, REG_ZF);
					b.cbranch(i.ea(), REG_TMP, target.unwrap());
				}
				JMP => {
					b.branch(i.ea(), target.unwrap());
				}
				CAL => {
					b.iusub(i.ea(), REG_SP, REG_SP, Const::_16(2));
					b.store(i.ea(), REG_SP, Const::_16(i.next_va().0 as u16));
					b.call(i.ea(), target.unwrap());
				}
				RET => {
					b.load(i.ea(),  REG_TMP16, REG_SP);
					b.iuadd(i.ea(), REG_SP, REG_SP, Const::_16(2));
					b.ret(i.ea(),   REG_TMP16);
				}
				LD => {
					let reg = r0(i);

					let addr = if self.addr_mode == AddrMode::RR && inst_reg(i, 1) == Reg::DC {
						b.ipair(i.ea(), REG_TMP16, REG_D, REG_C);
						REG_TMP16.into()
					} else {
						self.r1(i)
					};

					b.load(i.ea(), reg, addr);
				}
				ST => {
					let reg = r0(i);

					let addr = if self.addr_mode == AddrMode::RR && inst_reg(i, 1) == Reg::DC {
						b.ipair(i.ea(), REG_TMP16, REG_D, REG_C);
						REG_TMP16.into()
					} else {
						self.r1(i)
					};

					b.store(i.ea(), addr, reg);
				}
			}
		}
	}
}

// ------------------------------------------------------------------------------------------------
// ToyBuilder
// ------------------------------------------------------------------------------------------------

fn encode_op(op: Opcode) -> u8 {
	op as u8
}

fn encode_opr0(op: Opcode, r0: Reg) -> u8 {
	check_8bit_reg(r0);
	op as u8 | ((r0 as u8) << 6)
}

fn encode_r1_8bit(r1: Reg) -> u8 {
	check_8bit_reg(r1);
	r1 as u8
}

fn encode_r1_any(r1: Reg) -> u8 {
	match r1 {
		Reg::A | Reg::B | Reg::C | Reg::D | Reg::DC => r1 as u8,
		_ => panic!("cannot use register {:?}", r1),
	}
}

fn check_8bit_reg(reg: Reg) {
	match reg {
		Reg::A | Reg::B | Reg::C | Reg::D => {},
		_ => panic!("cannot use register {:?}", reg),
	}
}

fn calc_branch_offset(from: usize, to: usize) -> u8 {
	let diff = (to as isize) - (from as isize) - 2;
	let diff: i8 = diff.try_into().expect("branch offset too far");
	diff as u8
}

fn encode_16bit_addr(target: usize) -> [u8; 2] {
	assert!(matches!(target, 0 ..= 0xFFFF), "invalid 16-bit address");
	[
		(target & 0xFF) as u8,
		((target >> 8) & 0xFF) as u8
	]
}

pub struct ToyBuilder {
	bytes: Vec<u8>,
}

impl ToyBuilder {
	pub fn new() -> Self {
		Self { bytes: vec![] }
	}

	pub fn finish(self) -> Vec<u8> {
		let mut insts = self.bytes;
		insts.resize(0x8000, 0);
		let mut ret = vec![b'T', b'O', b'Y'];
		ret.append(&mut insts);
		ret
	}

	pub fn org(&mut self, addr: usize) {
		assert!(addr >= self.bytes.len(), "can only move origin up");
		assert!(addr <= 0x7FFF, "invalid address {:X} (valid addresses 0..=0x7FFF)", addr);
		self.bytes.resize(addr, 0);
	}

	pub fn append(&mut self, bytes: &[u8]) -> usize {
		let ret = self.bytes.len();
		assert!(ret + bytes.len() <= 0x8000, "ROM segment too big");
		self.bytes.extend_from_slice(bytes);
		ret
	}

	pub fn branch_here(&mut self, from: usize) {
		assert!(from < self.bytes.len(), "bad source address {:X}", from);
		let opc = self.bytes[from];
		if opc != Opcode::BLT_S8 as u8 && opc != Opcode::BLE_S8 as u8 &&
			opc != Opcode::BEQ_S8 as u8 && opc != Opcode::BNE_S8 as u8 {
			panic!("patching something that isn't a branch at {:X}", from);
		}

		self.bytes[from + 1] = calc_branch_offset(from, self.bytes.len());
	}

	pub fn jump_here(&mut self, from: usize) {
		assert!(from < self.bytes.len(), "bad source address {:X}", from);
		let opc = self.bytes[from];
		if opc != Opcode::JMP_I16 as u8 && opc != Opcode::CAL_I16 as u8 {
			panic!("patching something that isn't a jump/call at {:X}", from);
		}

		let target = encode_16bit_addr(self.bytes.len());
		self.bytes[from + 1] = target[0];
		self.bytes[from + 2] = target[1];
	}

	pub fn li(&mut self, dst: Reg, src: impl Into<u8>) -> usize {
		self.append(&[encode_opr0(Opcode::LI_RI8, dst), src.into()])
	}

	pub fn mov(&mut self, dst: Reg, src: Reg) -> usize {
		self.append(&[encode_opr0(Opcode::MOV_RR, dst), encode_r1_8bit(src)])
	}

	pub fn add(&mut self, dst: Reg, src: Reg) -> usize {
		self.append(&[encode_opr0(Opcode::ADD_RR, dst), encode_r1_8bit(src)])
	}

	pub fn addi(&mut self, dst: Reg, src: impl Into<u8>) -> usize {
		self.append(&[encode_opr0(Opcode::ADD_RI8, dst), src.into()])
	}

	pub fn adc(&mut self, dst: Reg, src: Reg) -> usize {
		self.append(&[encode_opr0(Opcode::ADC_RR, dst), encode_r1_8bit(src)])
	}

	pub fn adci(&mut self, dst: Reg, src: impl Into<u8>) -> usize {
		self.append(&[encode_opr0(Opcode::ADC_RI8, dst), src.into()])
	}

	pub fn sub(&mut self, dst: Reg, src: Reg) -> usize {
		self.append(&[encode_opr0(Opcode::SUB_RR, dst), encode_r1_8bit(src)])
	}

	pub fn subi(&mut self, dst: Reg, src: impl Into<u8>) -> usize {
		self.append(&[encode_opr0(Opcode::SUB_RI8, dst), src.into()])
	}

	pub fn sbc(&mut self, dst: Reg, src: Reg) -> usize {
		self.append(&[encode_opr0(Opcode::SBC_RR, dst), encode_r1_8bit(src)])
	}

	pub fn sbci(&mut self, dst: Reg, src: impl Into<u8>) -> usize {
		self.append(&[encode_opr0(Opcode::SBC_RI8, dst), src.into()])
	}

	pub fn and(&mut self, dst: Reg, src: Reg) -> usize {
		self.append(&[encode_opr0(Opcode::AND_RR, dst), encode_r1_8bit(src)])
	}

	pub fn andi(&mut self, dst: Reg, src: impl Into<u8>) -> usize {
		self.append(&[encode_opr0(Opcode::AND_RI8, dst), src.into()])
	}

	pub fn or(&mut self, dst: Reg, src: Reg) -> usize {
		self.append(&[encode_opr0(Opcode::OR_RR, dst), encode_r1_8bit(src)])
	}

	pub fn ori(&mut self, dst: Reg, src: impl Into<u8>) -> usize {
		self.append(&[encode_opr0(Opcode::OR_RI8, dst), src.into()])
	}

	pub fn xor(&mut self, dst: Reg, src: Reg) -> usize {
		self.append(&[encode_opr0(Opcode::XOR_RR, dst), encode_r1_8bit(src)])
	}

	pub fn xori(&mut self, dst: Reg, src: impl Into<u8>) -> usize {
		self.append(&[encode_opr0(Opcode::XOR_RI8, dst), src.into()])
	}

	pub fn not(&mut self, dst: Reg, src: Reg) -> usize {
		self.append(&[encode_opr0(Opcode::NOT_RR, dst), encode_r1_8bit(src)])
	}

	pub fn noti(&mut self, dst: Reg, src: impl Into<u8>) -> usize {
		self.append(&[encode_opr0(Opcode::NOT_RI8, dst), src.into()])
	}

	pub fn cmp(&mut self, dst: Reg, src: Reg) -> usize {
		self.append(&[encode_opr0(Opcode::CMP_RR, dst), encode_r1_8bit(src)])
	}

	pub fn cmpi(&mut self, dst: Reg, src: impl Into<u8>) -> usize {
		self.append(&[encode_opr0(Opcode::CMP_RI8, dst), src.into()])
	}

	pub fn cmc(&mut self, dst: Reg, src: Reg) -> usize {
		self.append(&[encode_opr0(Opcode::CMC_RR, dst), encode_r1_8bit(src)])
	}

	pub fn cmci(&mut self, dst: Reg, src: impl Into<u8>) -> usize {
		self.append(&[encode_opr0(Opcode::CMC_RI8, dst), src.into()])
	}

	pub fn blt(&mut self) -> usize {
		self.append(&[encode_op(Opcode::BLT_S8), 0])
	}

	pub fn blt_to(&mut self, to: usize) -> usize {
		self.append(&[encode_op(Opcode::BLT_S8), calc_branch_offset(self.bytes.len(), to)])
	}

	pub fn ble(&mut self) -> usize {
		self.append(&[encode_op(Opcode::BLE_S8), 0])
	}

	pub fn ble_to(&mut self, to: usize) -> usize {
		self.append(&[encode_op(Opcode::BLE_S8), calc_branch_offset(self.bytes.len(), to)])
	}

	pub fn beq(&mut self) -> usize {
		self.append(&[encode_op(Opcode::BEQ_S8), 0])
	}

	pub fn beq_to(&mut self, to: usize) -> usize {
		self.append(&[encode_op(Opcode::BEQ_S8), calc_branch_offset(self.bytes.len(), to)])
	}

	pub fn bne(&mut self) -> usize {
		self.append(&[encode_op(Opcode::BNE_S8), 0])
	}

	pub fn bne_to(&mut self, to: usize) -> usize {
		self.append(&[encode_op(Opcode::BNE_S8), calc_branch_offset(self.bytes.len(), to)])
	}

	pub fn jmp(&mut self) -> usize {
		self.append(&[encode_op(Opcode::JMP_I16), 0, 0])
	}

	pub fn jmp_to(&mut self, target: usize) -> usize {
		let target = encode_16bit_addr(target);
		self.append(&[encode_op(Opcode::JMP_I16), target[0], target[1]])
	}

	pub fn cal(&mut self) -> usize {
		self.append(&[encode_op(Opcode::CAL_I16), 0, 0])
	}

	pub fn cal_to(&mut self, target: usize) -> usize {
		let target = encode_16bit_addr(target);
		self.append(&[encode_op(Opcode::CAL_I16), target[0], target[1]])
	}

	pub fn ret(&mut self) -> usize {
		self.append(&[encode_op(Opcode::RET_IMP)])
	}

	pub fn ld(&mut self, dst: Reg, src: Reg) -> usize {
		self.append(&[encode_opr0(Opcode::LD_RR, dst), encode_r1_any(src)])
	}

	pub fn ldi(&mut self, dst: Reg, addr: u16) -> usize {
		let addr = encode_16bit_addr(addr as usize);
		self.append(&[encode_opr0(Opcode::LD_RI16, dst), addr[0], addr[1]])
	}

	pub fn st(&mut self, dst: Reg, src: Reg) -> usize {
		self.append(&[encode_opr0(Opcode::ST_RR, dst), encode_r1_any(src)])
	}

	pub fn sti(&mut self, dst: Reg, addr: u16) -> usize {
		let addr = encode_16bit_addr(addr as usize);
		self.append(&[encode_opr0(Opcode::ST_RI16, dst), addr[0], addr[1]])
	}
}