
use crate::ir::{ IrReg, IrConst, IrSrc, IrBuilder, BuildReg, BuildSrc };
use crate::program::{ BBTerm };

use super::*;

// ------------------------------------------------------------------------------------------------
// IR
// ------------------------------------------------------------------------------------------------

pub(crate) struct ToyIrCompiler;

impl IIrCompiler for ToyIrCompiler {
	fn build_ir(&self, i: &Instruction, b: &mut IrBuilder) {
		b.set_ea(i.ea());
		lookup_desc(i.bytes()[0]).expect("ono").build_ir(i, None, b);
	}

	fn build_ir_term(&self, i: &Instruction, term: &BBTerm, b: &mut IrBuilder) {
		b.set_ea(i.ea());
		lookup_desc(i.bytes()[0]).expect("ono").build_ir(i, Some(term), b);
	}

	fn arg_regs(&self) -> &'static [IrReg] {
		ARG_REGS
	}

	fn return_regs(&self) -> &'static [IrReg] {
		RETURN_REGS
	}

	fn stack_ptr_reg(&self) -> IrReg {
		REG_SP
	}
}

const REG_A:     IrReg = IrReg::reg8 (0);
const REG_B:     IrReg = IrReg::reg8 (1);
const REG_C:     IrReg = IrReg::reg8 (2);
const REG_D:     IrReg = IrReg::reg8 (3);
const REG_SP:    IrReg = IrReg::reg16(4);
const REG_NF:    IrReg = IrReg::reg8 (6);
const REG_ZF:    IrReg = IrReg::reg8 (7);
const REG_CF:    IrReg = IrReg::reg8 (8);
const REG_TMP:   IrReg = IrReg::reg8 (9);
const REG_TMP16: IrReg = IrReg::reg16(10);
const REG_TMPCF: IrReg = IrReg::reg8 (12);

static ARG_REGS: &[IrReg]    = &[ REG_A, REG_B, REG_C, REG_D, REG_NF, REG_ZF, REG_CF ];
static RETURN_REGS: &[IrReg] = &[ REG_A, REG_B, REG_C, REG_D, REG_SP, REG_NF, REG_ZF, REG_CF ];

fn reg_to_ir_reg(reg: Reg) -> IrReg {
	match reg {
		Reg::A  => REG_A,
		Reg::B  => REG_B,
		Reg::C  => REG_C,
		Reg::D  => REG_D,
		Reg::DC => panic!(),
		Reg::SP => REG_SP,
	}
}

fn inst_reg(i: &Instruction, op: usize) -> Reg {
	let reg = match i.ops()[op] {
		Operand::Reg(r) => r,
		Operand::Indir(MemIndir::Reg { reg: r }, ..) => r,
		_ => panic!("not a register operand"),
	};

	decode_reg(reg as u8)
}

fn inst_addr(i: &Instruction, op: usize) -> VA {
	let Operand::Mem(va, _) = i.ops()[op] else { panic!("not a memory operand"); };
	va
}

fn inst_imm(i: &Instruction) -> u8 {
	let Operand::UImm(uimm) = i.ops()[1] else { panic!("not a uimm operand"); };
	uimm as u8
}

impl InstDesc {
	fn r1(&self, i: &Instruction) -> IrSrc {
		match self.addr_mode {
			AddrMode::RR   => reg_to_ir_reg(inst_reg(i, 1)).into(),
			AddrMode::RI8  => IrConst::_8(inst_imm(i)).into(),
			AddrMode::RI16 => IrConst::_16(inst_addr(i, 1).0 as u16).into(),
			_ => panic!(),
		}
	}

	pub(super) fn build_ir(&self, i: &Instruction, term: Option<&BBTerm>, b: &mut IrBuilder) {
		use MetaOp::*;

		let r0 = | | -> BuildReg {
			(reg_to_ir_reg(inst_reg(i, 0)), 0).into()
		};

		let r1 = | | -> BuildSrc {
			(self.r1(i), 1).into()
		};

		match self.meta_op {
			MOV => {
				b.mov(r0(), r1());
			}
			ADD => {
				let op0 = r0();
				let op1 = r1();
				b.ucarry(REG_CF, op0, op1);
				b.add  (   op0, op0, op1);
			}
			ADC => {
				let op0 = r0();
				let op1 = r1();
				b.mov( REG_TMPCF, REG_CF);
				b.ucarryc(REG_CF,    op0, op1, REG_CF);
				b.addc  (   op0,    op0, op1, REG_TMPCF);
			}
			SUB => {
				let op0 = r0();
				let op1 = r1();
				b.sborrow(REG_CF, op0, op1);
				b.sub(   op0,    op0, op1);
			}
			SBC => {
				let op0 = r0();
				let op1 = r1();
				b.mov(      REG_TMPCF, REG_CF);
				b.sborrowb(REG_CF,    op0, op1, REG_CF);
				b.subb(   op0,       op0, op1, REG_TMPCF);
			}
			AND => {
				let op0 = r0();
				let op1 = r1();
				b.iand(op0, op0, op1);
			}
			OR => {
				let op0 = r0();
				let op1 = r1();
				b.ior(op0, op0, op1);
			}
			XOR => {
				let op0 = r0();
				let op1 = r1();
				b.ixor(op0, op0, op1);
			}
			NOT => {
				let op0 = r0();
				let op1 = r1();
				b.inot(op0, op1);
			}
			CMP => {
				let op0 = r0();
				let op1 = r1();
				b.eq( REG_ZF, op0, op1);
				b.slt(REG_NF, op0, op1);
				b.ult(REG_CF, op0, op1);
			}
			CMC => {
				let op0 = r0();
				let op1 = r1();
				b.mov(      REG_TMPCF, REG_CF);
				b.sborrowb(REG_CF,    op0, op1, REG_CF);
				b.subb(   REG_TMP,   op0, op1, REG_TMPCF);
				b.eq(      REG_ZF, REG_TMP, IrConst::ZERO_8);
				b.slt(     REG_NF, REG_TMP, IrConst::ZERO_8);
			}
			BLT => {
				let term = term.unwrap();
				let dst = term.one_explicit_successor().unwrap();
				let cont = term.continuation_successor().unwrap();
				b.cbranch(REG_NF, (dst, 0), cont);
			}
			BLE => {
				let term = term.unwrap();
				let dst = term.one_explicit_successor().unwrap();
				let cont = term.continuation_successor().unwrap();
				b.bor(    REG_TMP, REG_CF, REG_ZF);
				b.cbranch(REG_TMP, (dst, 0),   cont);
			}
			BEQ => {
				let term = term.unwrap();
				let dst = term.one_explicit_successor().unwrap();
				let cont = term.continuation_successor().unwrap();
				b.cbranch(REG_ZF, (dst, 0), cont);
			}
			BNE => {
				let term = term.unwrap();
				let dst = term.one_explicit_successor().unwrap();
				let cont = term.continuation_successor().unwrap();
				b.bnot(   REG_TMP, REG_ZF);
				b.cbranch(REG_TMP, (dst, 0), cont);
			}
			JMP => {
				let dst = term.unwrap().one_explicit_successor().unwrap();
				b.branch((dst, 0));
			}
			JMPI => {
				b.pair(  REG_TMP16, REG_D, REG_C);
				b.ibranch((REG_TMP16, 0));
			}
			CALL => {
				let term = term.unwrap();
				let dst = term.one_explicit_successor().unwrap();
				let cont = term.continuation_successor().unwrap();
				b.sub(REG_SP, REG_SP, IrConst::_16(2));
				b.store(REG_SP, IrConst::_16(i.next_va().0 as u16));
				b.call ((dst, 0), cont);
			}
			CALI => {
				let cont = term.unwrap().continuation_successor().unwrap();
				b.pair(REG_TMP16, REG_D, REG_C);
				b.icall((REG_TMP16, 0), cont);
			}
			CALZ => {
				let term = term.unwrap();
				let dst = term.one_explicit_successor().unwrap();
				let cont = term.continuation_successor().unwrap();

				b.bnot             (REG_TMP, REG_ZF);
				b.cbranch_and_split(REG_TMP, cont);
				b.sub            (REG_SP,  REG_SP, IrConst::_16(2));
				b.store            (REG_SP,  IrConst::_16(i.next_va().0 as u16));
				b.call             ((dst, 0),  cont);
			}
			RET => {
				b.load( REG_TMP16, REG_SP);
				b.add(REG_SP, REG_SP, IrConst::_16(2));
				b.ret(  REG_TMP16);
			}
			RETZ => {
				let next = term.unwrap().continuation_successor().unwrap();

				b.bnot             (REG_TMP, REG_ZF);
				b.cbranch_and_split(REG_TMP, next);
				b.load             (REG_TMP16, REG_SP);
				b.add            (REG_SP, REG_SP, IrConst::_16(2));
				b.ret              (REG_TMP16);
			}
			LD => {
				let reg = r0();

				let addr = if self.addr_mode == AddrMode::RR && inst_reg(i, 1) == Reg::DC {
					b.pair(REG_TMP16, REG_D, REG_C);
					REG_TMP16.into()
				} else {
					r1()
				};

				b.load(reg, addr);
			}
			ST => {
				let reg = r0();

				let addr = if self.addr_mode == AddrMode::RR && inst_reg(i, 1) == Reg::DC {
					b.pair(REG_TMP16, REG_D, REG_C);
					REG_TMP16.into()
				} else {
					r1()
				};

				b.store(addr, reg);
			}
		}
	}
}