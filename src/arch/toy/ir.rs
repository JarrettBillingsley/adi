
use crate::ir::{ IrReg, IrConst, IrSrc, IrBuilder };

use super::*;

// ------------------------------------------------------------------------------------------------
// IR
// ------------------------------------------------------------------------------------------------

pub(crate) struct ToyIrCompiler;

impl IIrCompiler for ToyIrCompiler {
	fn build_ir(&self, i: &Instruction, target: Option<EA>, b: &mut IrBuilder) {
		lookup_desc(i.bytes()[0]).expect("ono").build_ir(i, target, b);
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
	let Operand::UImm(uimm, _) = i.ops()[1] else { panic!("not a uimm operand"); };
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

	pub(super) fn build_ir(&self, i: &Instruction, target: Option<EA>, b: &mut IrBuilder) {
		use MetaOp::*;

		let r0 = | | -> IrReg {
			reg_to_ir_reg(inst_reg(i, 0))
		};

		let r1 = | | -> IrSrc {
			self.r1(i)
		};

		let ea = i.ea();

		match self.meta_op {
			MOV => {
				b.assign(ea, r0(), r1(), 0, 1);
			}
			ADD => {
				let op0 = r0();
				let op1 = r1();
				b.icarry(ea, REG_CF, op0, op1,   -1, 0, 1);
				b.iuadd(ea,  op0,    op0, op1,    0, 0, 1);
			}
			ADC => {
				let op0 = r0();
				let op1 = r1();
				b.assign(ea,  REG_TMPCF, REG_CF,               -1, -1);
				b.icarryc(ea, REG_CF,    op0, op1, REG_CF,     -1,  0,  1, -1);
				b.iuaddc(ea,  op0,       op0, op1, REG_TMPCF,   0,  0,  1, -1);
			}
			SUB => {
				let op0 = r0();
				let op1 = r1();
				b.isborrow(ea, REG_CF, op0, op1,   -1, 0, 1);
				b.iusub(ea,    op0,    op0, op1,    0, 0, 1);
			}
			SBC => {
				let op0 = r0();
				let op1 = r1();
				b.assign(ea,    REG_TMPCF, REG_CF,                -1, -1);
				b.isborrowc(ea, REG_CF,    op0, op1, REG_CF,      -1,  0, 1, -1);
				b.iusubb(ea,    op0,       op0, op1, REG_TMPCF,    0,  0, 1, -1);
			}
			AND => {
				let op0 = r0();
				let op1 = r1();
				b.iand(ea, op0, op0, op1, 0, 0, 1);
			}
			OR => {
				let op0 = r0();
				let op1 = r1();
				b.ior(ea, op0, op0, op1, 0, 0, 1);
			}
			XOR => {
				let op0 = r0();
				let op1 = r1();
				b.ixor(ea, op0, op0, op1, 0, 0, 1);
			}
			NOT => {
				let op0 = r0();
				let op1 = r1();
				b.inot(ea, op0, op1, 0, 1);
			}
			CMP => {
				let op0 = r0();
				let op1 = r1();
				b.ieq(ea,  REG_ZF, op0, op1,   -1, 0, 1);
				b.islt(ea, REG_NF, op0, op1,   -1, 0, 1);
				b.iult(ea, REG_CF, op0, op1,   -1, 0, 1);
			}
			CMC => {
				let op0 = r0();
				let op1 = r1();
				b.assign(ea,    REG_TMPCF, REG_CF,                -1, -1);
				b.isborrowc(ea, REG_CF,    op0, op1, REG_CF,      -1,  0,  1, -1);
				b.iusubb(ea,    REG_TMP,   op0, op1, REG_TMPCF,   -1,  0,  1, -1);
				b.ieq(ea,       REG_ZF, REG_TMP, IrConst::ZERO_8, -1, -1, -1);
				b.islt(ea,      REG_NF, REG_TMP, IrConst::ZERO_8, -1, -1, -1);
			}
			BLT => {
				b.cbranch(ea, REG_NF, target.unwrap(), -1, 0);
			}
			BLE => {
				b.bor(ea,     REG_TMP, REG_CF, REG_ZF,   -1, -1, -1);
				b.cbranch(ea, REG_TMP, target.unwrap(),  -1,  0);
			}
			BEQ => {
				b.cbranch(ea, REG_ZF, target.unwrap(), -1, 0);
			}
			BNE => {
				b.bnot(ea,    REG_TMP, REG_ZF,           -1, -1);
				b.cbranch(ea, REG_TMP, target.unwrap(),  -1,  0);
			}
			JMP => {
				b.branch(ea, target.unwrap(), 0);
			}
			JMPI => {
				b.ipair(ea,   REG_TMP16, REG_D, REG_C, -1, -1, -1);
				b.ibranch(ea, REG_TMP16, 0);
			}
			CALL => {
				b.iusub(ea, REG_SP, REG_SP, IrConst::_16(2),             -1, -1, -1);
				b.store(ea, REG_SP, IrConst::_16(i.next_va().0 as u16),  -1, -1);
				b.call(ea, target.unwrap(),                               0);
			}
			CALI => {
				b.ipair(ea, REG_TMP16, REG_D, REG_C, -1, -1, -1);
				b.icall(ea, REG_TMP16, 0);
			}
			RET => {
				b.load(ea,  REG_TMP16, REG_SP,               -1, -1);
				b.iuadd(ea, REG_SP, REG_SP, IrConst::_16(2), -1, -1, -1);
				b.ret(ea,   REG_TMP16,                       -1);
			}
			LD => {
				let reg = r0();

				let addr = if self.addr_mode == AddrMode::RR && inst_reg(i, 1) == Reg::DC {
					b.ipair(ea, REG_TMP16, REG_D, REG_C, -1, -1, -1);
					REG_TMP16.into()
				} else {
					r1()
				};

				b.load(ea, reg, addr, 0, 1);
			}
			ST => {
				let reg = r0();

				let addr = if self.addr_mode == AddrMode::RR && inst_reg(i, 1) == Reg::DC {
					b.ipair(ea, REG_TMP16, REG_D, REG_C, -1, -1, -1);
					REG_TMP16.into()
				} else {
					r1()
				};

				b.store(ea, addr, reg, 1, 0);
			}
		}
	}
}