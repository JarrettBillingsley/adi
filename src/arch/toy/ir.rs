
use super::*;

// ------------------------------------------------------------------------------------------------
// IR
// ------------------------------------------------------------------------------------------------

pub(crate) struct ToyIrCompiler;

impl IIrCompiler for ToyIrCompiler {
	fn to_ir(&self, i: &Instruction, target: Option<EA>, b: &mut IrBuilder) {
		lookup_desc(i.bytes()[0]).expect("ono").to_ir(i, target, b);
	}
}

mod ir {
	use super::*;
	use crate::ir::{ IrReg, IrConst, IrSrc, IrBuilder };

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
		fn r1(&self, i: &Instruction) -> IrSrc {
			match self.addr_mode {
				AddrMode::RR   => reg_to_ir_reg(inst_reg(i, 1)).into(),
				AddrMode::RI8  => IrConst::_8(inst_imm(i)).into(),
				AddrMode::RI16 => IrConst::_16(inst_addr(i, 1).0 as u16).into(),
				_ => panic!(),
			}
		}

		pub(super) fn to_ir(&self, i: &Instruction, target: Option<EA>, b: &mut IrBuilder) {
			use MetaOp::*;

			fn r0(i: &Instruction) -> IrReg {
				reg_to_ir_reg(inst_reg(i, 0))
			}

			match self.meta_op {
				MOV => {
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
					b.ieq(i.ea(),       REG_ZF, REG_TMP, IrConst::ZERO_8);
					b.islt(i.ea(),      REG_NF, REG_TMP, IrConst::ZERO_8);
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
					b.iusub(i.ea(), REG_SP, REG_SP, IrConst::_16(2));
					b.store(i.ea(), REG_SP, IrConst::_16(i.next_va().0 as u16));
					b.call(i.ea(), target.unwrap());
				}
				RET => {
					b.load(i.ea(),  REG_TMP16, REG_SP);
					b.iuadd(i.ea(), REG_SP, REG_SP, IrConst::_16(2));
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