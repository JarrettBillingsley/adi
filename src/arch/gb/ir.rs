//! IR compiler for Game Boy/LR35902/SM83.

use crate::arch::{ IIrCompiler };
// use crate::program::{ MemIndir };
use crate::ir::{ IrReg, IrConst, IrSrc, IrBuilder };

use super::*;

// ------------------------------------------------------------------------------------------------
// IR
// ------------------------------------------------------------------------------------------------

pub(crate) struct GBIrCompiler;

impl IIrCompiler for GBIrCompiler {
	fn build_ir(&self, i: &Instruction, target: Option<EA>, b: &mut IrBuilder) {
		let bytes = i.bytes();
		if bytes[0] == 0xCB {
			lookup_desc_cb(bytes[1]).build_ir(i, target, b)
		} else {
			lookup_desc(bytes[0]).expect("ono").build_ir(i, target, b)
		}
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

const REG_A:  IrReg = IrReg::reg8(0);
const REG_B:  IrReg = IrReg::reg8(1);
const REG_C:  IrReg = IrReg::reg8(2);
const REG_D:  IrReg = IrReg::reg8(3);
const REG_E:  IrReg = IrReg::reg8(4);
const REG_H:  IrReg = IrReg::reg8(5);
const REG_L:  IrReg = IrReg::reg8(6);

const REG_CF: IrReg = IrReg::reg8(7);   // 4 Carry
const REG_HF: IrReg = IrReg::reg8(8);   // 5 Half-carry (BCD)
const REG_NF: IrReg = IrReg::reg8(9);   // 6 Subtraction (BCD)
const REG_ZF: IrReg = IrReg::reg8(10);  // 7 Zero

const REG_SP: IrReg = IrReg::reg16(11);

const REG_W:  IrReg = IrReg::reg8(13); // 8-bit temporary
const REG_Z:  IrReg = IrReg::reg8(14); // 8-bit temporary

const REG_AF_TMP: IrReg = IrReg::reg16(15); // 16-bit temporary
const REG_BC_TMP: IrReg = IrReg::reg16(17); // 16-bit temporary
const REG_DE_TMP: IrReg = IrReg::reg16(19); // 16-bit temporary
const REG_HL_TMP: IrReg = IrReg::reg16(21); // 16-bit temporary
const REG_WZ_TMP: IrReg = IrReg::reg16(23); // 16-bit temporary

static ARG_REGS: &[IrReg] =
	&[REG_A, REG_B, REG_C, REG_D, REG_E, REG_H, REG_L, REG_CF, REG_HF, REG_NF, REG_ZF];

static RETURN_REGS: &[IrReg] =
	&[REG_A, REG_B, REG_C, REG_D, REG_E, REG_H, REG_L, REG_CF, REG_HF, REG_NF, REG_ZF, REG_SP];

impl From<Reg> for IrReg {
	fn from(reg: Reg) -> IrReg {
		match reg {
			Reg::A  => REG_A,
			Reg::B  => REG_B,
			Reg::C  => REG_C,
			Reg::D  => REG_D,
			Reg::E  => REG_E,
			Reg::H  => REG_H,
			Reg::L  => REG_L,
			Reg::SP => REG_SP,

			// don't actually *want* AF, BC, DE, HL or F to be easily-convertible, since they all
			// need to be synthesized from other registers
			_ => panic!("hey. hey. did you synthesize the paired register?"),
		}
	}
}

impl IrBuilder {
	/// Evaluate the condition code `cc` and return a register which contains its truth value.
	fn cc(&mut self, ea: EA, cc: Cc) -> IrReg {
		match cc {
			Cc::C  => REG_CF,
			Cc::Z  => REG_ZF,
			Cc::NC => {
				self.bnot(ea, REG_Z, REG_CF, -1, -1);
				REG_Z
			}
			Cc::NZ => {
				self.bnot(ea, REG_Z, REG_ZF, -1, -1);
				REG_Z
			}
		}
	}

	/// Evaluate the logical inversion of the condition code `cc` and return a register which
	/// contains the inverted truth value.
	fn not_cc(&mut self, ea: EA, cc: Cc) -> IrReg {
		match cc {
			Cc::C  => self.cc(ea, Cc::NC),
			Cc::Z  => self.cc(ea, Cc::NZ),
			Cc::NC => self.cc(ea, Cc::C),
			Cc::NZ => self.cc(ea, Cc::Z),
		}
	}

	/// Pair the constituent registers of a paired register into its corresponding `REG_XX_TMP`.
	/// Named `rr` to match the ISA docs.
	fn rr(&mut self, ea: EA, reg: Reg) {
		match reg {
			// Reg::AF => self.ipair(ea, REG_AF_TMP, REG_A, REG_F,  -1, -1, -1),
			Reg::BC => self.ipair(ea, REG_BC_TMP, REG_B, REG_C,  -1, -1, -1),
			Reg::DE => self.ipair(ea, REG_DE_TMP, REG_D, REG_E,  -1, -1, -1),
			Reg::HL => self.ipair(ea, REG_HL_TMP, REG_H, REG_L,  -1, -1, -1),
			_ => panic!("given something other than a paired register"),
		};
	}

	/// Pair `REG_W` with `REG_Z` into `REG_WZ_TMP`.
	fn wz(&mut self, ea: EA) {
		self.ipair(ea, REG_WZ_TMP, REG_W, REG_Z,  -1, -1, -1);
	}

	/// Push an 8-bit value `src` onto the stack.
	fn push8(&mut self, src: impl Into<IrSrc>, ea: EA) {
		// full stack convention - subtract before storing
		self.iusub(ea, REG_SP, REG_SP, IrConst::_16(1),  -1, -1, -1);
		self.store(ea, REG_SP, src,                      -1, -1);
	}

	/// Pop an 8-bit value off the stack into `dst`.
	fn pop8(&mut self, dst: impl Into<IrReg>, ea: EA) {
		let dst = dst.into();
		// full stack convention - load before adding
		self.load (ea, dst,    REG_SP,                   -1, -1);
		self.iuadd(ea, REG_SP, REG_SP, IrConst::_16(1),  -1, -1, -1);
	}

	/// Pop a 16-bit value off the stack as two 8-bit halves into `dstlo` and `dsthi`.
	fn pop16(&mut self, dsthi: impl Into<IrReg>, dstlo: impl Into<IrReg>, ea: EA) {
		self.pop8(dstlo, ea);
		self.pop8(dsthi, ea);
	}

	/// Pop a value into the WZ register and pair it, so it's ready to use as REG_WZ_TMP.
	fn pop_wz(&mut self, ea: EA) {
		self.pop16(REG_W, REG_Z, ea);
		self.wz(ea);
	}

	/// Push the return address to the stack.
	fn push_return_addr(&mut self, ea: EA, ret_addr: VA) {
	 	let ret_addr = ret_addr.0 as u16;
		// push hi then lo
		self.push8(IrConst::_8((ret_addr >> 8  ) as u8), ea);
		self.push8(IrConst::_8((ret_addr & 0xFF) as u8), ea);
	}

	/// Pop the return address and `ret` to it.
	fn return_(&mut self, ea: EA) {
		self.pop_wz(ea);
		self.ret(ea, REG_WZ_TMP, -1);
	}

	/// Set the Z, N, and H flags to 0.
	fn z0n0h0(&mut self, ea: EA) {
		self.assign(ea, REG_ZF, IrConst::ZERO_8,  -1, -1);
		self.assign(ea, REG_NF, IrConst::ZERO_8,  -1, -1);
		self.assign(ea, REG_HF, IrConst::ZERO_8,  -1, -1);
	}

	/// Set the N and H flags to 0, and the Z flag to whether or not `reg == 0`.
	fn z_n0h0(&mut self, ea: EA, reg: impl Into<IrReg>) {
		let reg = reg.into();
		self.ieq   (ea, REG_ZF, reg, IrConst::ZERO_8,  -1, -1, -1);
		self.assign(ea, REG_NF, IrConst::ZERO_8,       -1, -1);
		self.assign(ea, REG_HF, IrConst::ZERO_8,       -1, -1);
	}

	/// Rotate the given `reg` left. The carry flag is set to the MSB of `reg`, but otherwise does
	/// not participate. If `set_zero_flag`, the zero flag will be set if the result is 0;
	/// otherwise the zero flag will be set to 0 always.
	fn rol_no_carry(&mut self, ea: EA, reg: impl Into<IrReg>, set_zero_flag: bool) {
		let reg = reg.into();
		self.ibit(ea, REG_CF, reg, IrConst::_8(7),  -1, -1, -1);
		self.irol(ea, reg,    reg, IrConst::ONE_8,  -1, -1, -1);

		if set_zero_flag {
			self.z_n0h0(ea, reg)
		} else {
			self.z0n0h0(ea);
		}
	}

	/// Rotate the given `reg` right. The carry flag is set to the MSB of `reg`, but otherwise does
	/// not participate. If `set_zero_flag`, the zero flag will be set if the result is 0;
	/// otherwise the zero flag will be set to 0 always.
	fn ror_no_carry(&mut self, ea: EA, reg: impl Into<IrReg>, set_zero_flag: bool) {
		let reg = reg.into();
		self.ibit(ea, REG_CF, reg, IrConst::_8(7),  -1, -1, -1);
		self.iror(ea, reg,    reg, IrConst::ONE_8,  -1, -1, -1);

		if set_zero_flag {
			self.z_n0h0(ea, reg)
		} else {
			self.z0n0h0(ea);
		}
	}

	/// Rotate the given `reg` left through the carry flag. If `set_zero_flag`, the zero flag will
	/// be set if the result is 0; otherwise the zero flag will be set to 0 always.
	fn rol_carry(&mut self, ea: EA, reg: impl Into<IrReg>, set_zero_flag: bool) {
		let reg = reg.into();
		self.assign (ea, REG_Z,  REG_CF,                       -1, -1);
		self.ibit   (ea, REG_CF, reg, IrConst::_8(7),          -1, -1, -1);
		self.irol   (ea, reg,    reg, IrConst::ONE_8,          -1, -1, -1);
		self.ibitset(ea, reg,    reg, IrConst::ZERO_8, REG_Z,  -1, -1, -1, -1);

		if set_zero_flag {
			self.z_n0h0(ea, reg)
		} else {
			self.z0n0h0(ea);
		}
	}

	/// Rotate the given `reg` right through the carry flag. If `set_zero_flag`, the zero flag will
	/// be set if the result is 0; otherwise the zero flag will be set to 0 always.
	fn ror_carry(&mut self, ea: EA, reg: impl Into<IrReg>, set_zero_flag: bool) {
		let reg = reg.into();
		self.assign (ea, REG_Z,  REG_CF,                        -1, -1);
		self.ibit   (ea, REG_CF, reg,   IrConst::ZERO_8,        -1, -1, -1);
		self.iror   (ea, reg,    reg,   IrConst::ONE_8,         -1, -1, -1);
		self.ibitset(ea, reg,    reg,   IrConst::_8(7), REG_Z,  -1, -1, -1, -1);

		if set_zero_flag {
			self.z_n0h0(ea, reg)
		} else {
			self.z0n0h0(ea);
		}
	}
}

/// Perform some read-modify-write operation using `[hl]` as the source/dest. `f` is passed
/// a temporary register containing the 8-bit value loaded from `[hl]`; it must place the
/// result back into this same register.
fn hl_rmw(b: &mut IrBuilder, ea: EA, f: impl Fn(&mut IrBuilder, IrReg)) {
	b.rr   (ea, Reg::HL);
	b.load (ea, REG_Z, REG_HL_TMP, -1, 0);
	f      (b,  REG_Z);
	b.store(ea, REG_HL_TMP, REG_Z, 0, -1);
}

impl InstDesc {
	pub(super) fn build_ir(&self, i: &Instruction, target: Option<EA>, b: &mut IrBuilder) {
		use MetaOp::*;

		let ea = i.ea();

		match (self.meta_op(), self.syn_ops().get(0).copied()) {
			(UNK, _) => { panic!("what the hell is an unknown instruction doing in a BB?"); }

			// for all these, have to emit *something* to avoid empty IR BBs.
			(NOP,  None) => { b.nop(ea); } // no flag changes
			(DI,   None) => { b.nop(ea); } // no flag changes
			(EI,   None) => { b.nop(ea); } // no flag changes
			(HALT, None) => { b.nop(ea); } // no flag changes
			(STOP, None) => { b.nop(ea); } // no flag changes

			// ------------------------------------------------------------------------------------
			// Computation

			(ADD, _) => {
				b.nop(ea); // TODO
				// a += r8
					// {Z*, N0, H*, C*} 0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x87 (add a, r)
					// InstDesc(   0x80, ADD,  &[Srg(A), Srg(B)],         Other,  Imp),
					// ...

				// a += [hl]
					// {Z*, N0, H*, C*} 0x86 (add a, [hl])
					// InstDesc(   0x86, ADD,  &[Srg(A), IndReg(HL)],     Other,  Ind(HL, R)),

				// a += uimm8
					// {Z*, N0, H*, C*} 0xC6 (add a, imm8)
					// InstDesc(   0xC6, ADD,  &[Srg(A), Op],             Other,  UImm8),

				// hl += r16
					// {Z-, N0, H*, C*} 0x09, 0x19, 0x29, 0x39 (add hl, rr)
					// InstDesc(   0x09, ADD,  &[Srg(HL), Srg(BC)],       Other,  Imp),
					// ...

				// sp += simm8
					// {Z0, N0, H*, C*} 0xE8 (add sp, imm)
					// InstDesc(   0xE8, ADD,  &[Srg(SP), Op],            Other,  SImm8),
			}
			(ADC, _) => {
				b.nop(ea); // TODO
				// a += r8 + cf
					// {Z*, N0, H*, C*} 0x88, 0x89, 0x8A, 0x8B, 0x8C, 0x8D, 0x8F (adc a, r)
					// InstDesc(   0x88, ADC,  &[Srg(A), Srg(B)],         Other,  Imp),
					// ...

				// a += [hl] + cf
					// {Z*, N0, H*, C*} 0x8E (adc a, [hl])
					// InstDesc(   0x8E, ADC,  &[Srg(A), IndReg(HL)],     Other,  Ind(HL, R)),

				// a += uimm8 + cf
					// {Z*, N0, H*, C*} 0xCE (adc a, imm8)
					// InstDesc(   0xCE, ADC,  &[Srg(A), Op],             Other,  UImm8),
			}
			(SUB, _) => {
				b.nop(ea); // TODO
				// a -= r8
					// {Z*, N1, H*, C*} 0x90, 0x91, 0x92, 0x93, 0x94, 0x95 (sub a, r)
					// {Z1, N1, H0, C0} 0x97 (sub a, a) (just a special case?)
					// InstDesc(   0x90, SUB,  &[Srg(A), Srg(B)],         Other,  Imp),
					// ...

				// a -= [hl]
					// {Z*, N1, H*, C*} 0x96 (sub a, [hl])
					// InstDesc(   0x96, SUB,  &[Srg(A), IndReg(HL)],     Other,  Ind(HL, R)),

				// a -= uimm8
					// {Z*, N1, H*, C*} 0xD6 (sub a, imm8)
					// InstDesc(   0xD6, SUB,  &[Srg(A), Op],             Other,  UImm8),
			}
			(SBC, _) => {
				b.nop(ea); // TODO
				// a -= r8 - cf
					// {Z*, N1, H*, C*} 0x98, 0x99, 0x9A, 0x9B, 0x9C, 0x9D (sbc a, r)
					// {Z*, N1, H*, C-} 0x9F (sbc a, a) (just a special case?)
					// InstDesc(   0x98, SBC,  &[Srg(A), Srg(B)],         Other,  Imp),
					// ...

				// a -= [hl] - cf
					// {Z*, N1, H*, C*} 0x9E (sbc a, [hl])
					// InstDesc(   0x9E, SBC,  &[Srg(A), IndReg(HL)],     Other,  Ind(HL, R)),

				// a -= uimm8 - cf
					// {Z*, N1, H*, C*} 0xDE (sbc a, imm8)
					// InstDesc(   0xDE, SBC,  &[Srg(A), Op],             Other,  UImm8),
			}
			(AND, _) => {
				b.nop(ea); // TODO
				// a &= r8
					// {Z*, N0, H1, C0} 0xA0, 0xA1, 0xA2, 0xA3, 0xA4, 0xA5, 0xA7 (and a, r)
					// InstDesc(   0xA0, AND,  &[Srg(A), Srg(B)],         Other,  Imp),
					// ...

				// a &= [hl]
					// {Z*, N0, H1, C0} 0xA6 (and a, [hl])
					// InstDesc(   0xA6, AND,  &[Srg(A), IndReg(HL)],     Other,  Ind(HL, R)),

				// a &= uimm8
					// {Z*, N0, H1, C0} 0xE6 (and a, imm)
					// InstDesc(   0xE6, AND,  &[Srg(A), Op],             Other,  UImm8),
			}
			(OR, _) => {
				b.nop(ea); // TODO
				// a |= r8
					// {Z*, N0, H0, C0} 0xB0, 0xB1, 0xB2, 0xB3, 0xB4, 0xB5, 0xB7 (or a, r)
					// InstDesc(   0xB0, OR,   &[Srg(A), Srg(B)],         Other,  Imp),
					// ...

				// a |= [hl]
					// {Z*, N0, H0, C0} 0xB6 (or a, [hl])
					// InstDesc(   0xB6, OR,   &[Srg(A), IndReg(HL)],     Other,  Ind(HL, R)),

				// a |= uimm8
					// {Z*, N0, H0, C0} 0xF6 (or a, imm8)
					// InstDesc(   0xF6, OR,   &[Srg(A), Op],             Other,  UImm8),
			}
			(XOR, _) => {
				b.nop(ea); // TODO
				// a ^= r8
					// {Z*, N0, H0, C0} 0xA8, 0xA9, 0xAA, 0xAB, 0xAC, 0xAD (xor a, r)
					// {Z1, N0, H0, C0} 0xAF (xor a, a) (just a special case?)
					// InstDesc(   0xA8, XOR,  &[Srg(A), Srg(B)],         Other,  Imp),
					// ...

				// a ^= [hl]
					// {Z*, N0, H0, C0} 0xAE (xor a, [hl])
					// InstDesc(   0xAE, XOR,  &[Srg(A), IndReg(HL)],     Other,  Ind(HL, R)),

				// a ^= uimm8
					// {Z*, N0, H0, C0} 0xEE (xor a, imm8)
					// InstDesc(   0xEE, XOR,  &[Srg(A), Op],             Other,  UImm8),
			}
			(CP, _) => {
				b.nop(ea); // TODO
				// a - r8
					// {Z*, N1, H*, C*} 0xB8, 0xB9, 0xBA, 0xBB, 0xBC, 0xBD (cp a, r)
					// {Z1, N1, H0, C0} 0xBF (cp a, a) (just a special case?)
					// InstDesc(   0xB8, CP,   &[Srg(A), Srg(B)],         Other,  Imp),
					// ...

				// a - [hl]
					// {Z*, N1, H*, C*} 0xBE (cp a, [hl])
					// InstDesc(   0xBE, CP,   &[Srg(A), IndReg(HL)],     Other,  Ind(HL, R)),

				// a - uimm8
					// {Z*, N1, H*, C*} 0xFE (cp a, imm8)
					// InstDesc(   0xFE, CP,   &[Srg(A), Op],             Other,  UImm8),
			}
			(INC, _) => {
				b.nop(ea); // TODO
				// r8++
					// {Z*, N0, H*, C-} 0x04, 0x0C, 0x14, 0x1C, 0x24, 0x2C, 0x3C (inc r)
					// InstDesc(   0x04, INC,  &[Srg(B)],                 Other,  Imp),
					// ...

				// [hl]++
					// {Z*, N0, H*, C-} 0x34 (inc [hl])
					// InstDesc(   0x34, INC,  &[IndReg(HL)],             Other,  Ind(HL, RW)),

				// r16++
					// no flag changes
					// InstDesc(   0x03, INC,  &[Srg(BC)],                Other,  Imp),
					// ...
			}
			(DEC, _) => {
				b.nop(ea); // TODO

				// r8--
					// {Z*, N1, H*, C-} 0x05, 0x0D, 0x15, 0x1D, 0x25, 0x2D, 0x3D (dec r)
					// InstDesc(   0x05, DEC,  &[Srg(B)],                 Other,  Imp),
					// ...

				// [hl]--
					// {Z*, N1, H*, C-} 0x3f (dec [hl])
					// InstDesc(   0x35, DEC,  &[IndReg(HL)],             Other,  Ind(HL, RW)),

				// r16--
					// no flag changes
					// InstDesc(   0x0B, DEC,  &[Srg(BC)],                Other,  Imp),
					// ...
			}
			(CPL, _) => {
				b.nop(ea); // TODO
				// {Z-, N1, H1, C-} 0x2F (cpl)
			}
			(DA, _) => {
				b.nop(ea); // TODO
				// {Z*, N-, H0, C*} 0x27 (da a)
			}

			// ------------------------------------------------------------------------------------
			// Bitwise

			(SLA, _) => {
				b.nop(ea); // TODO
				// r8 <<= 1
					// {Z*, N0, H0, C*} 0xCB_{0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x27} (sla r)
					// InstDesc(0xCB_20, SLA,  &[Srg(B)],                 Other,  Imp),
					// ...

				// [hl] <<= 1
					// {Z*, N0, H0, C*} 0xCB_26 (sla [hl])
					// InstDesc(0xCB_26, SLA,  &[IndReg(HL)],             Other,  Ind(HL, RW)),
			}
			(SRA, _) => {
				b.nop(ea); // TODO
				// r8 >>= 1
					// {Z*, N0, H0, C*} 0xCB_{0x28, 0x29, 0x2A, 0x2B, 0x2C, 0x2D, 0x2F} (sra r)
					// InstDesc(0xCB_28, SRA,  &[Srg(B)],                 Other,  Imp),
					// ...

				// [hl] >>= 1
					// {Z*, N0, H0, C*} 0xCB_2E (sra [hl])
					// InstDesc(0xCB_2E, SRA,  &[IndReg(HL)],             Other,  Ind(HL, RW)),
			}
			(SRL, _) => {
				b.nop(ea); // TODO
				// r8 >>>= 1
					// {Z*, N0, H0, C*} 0xCB_{0x38, 0x39, 0x3A, 0x3B, 0x3C, 0x3D, 0x3F} (srl r)
					// InstDesc(0xCB_38, SRL,  &[Srg(B)],                 Other,  Imp),
					// ...

				// [hl] >>>= 1
					// {Z*, N0, H0, C*} 0xCB_3E (srl [hl])
					// InstDesc(0xCB_3E, SRL,  &[IndReg(HL)],             Other,  Ind(HL, RW)),
			}
			(RLA, None) =>                                               // {Z0, N0, H0, C*}
				b.rol_carry(ea, REG_A, false),
			(RL, Some(SynOp::Srg(Reg::HL))) =>                           // {Z*, N0, H0, C*}
				hl_rmw(b, ea, |b, tmp| b.rol_carry(ea, tmp, true)),
			(RL, Some(SynOp::Srg(reg))) =>                               // {Z*, N0, H0, C*}
				b.rol_carry(ea, reg, true),
			(RLCA, None) =>                                              // {Z0, N0, H0, C*}
				b.rol_no_carry(ea, REG_A, false),
			(RLC, Some(SynOp::Srg(Reg::HL))) =>                          // {Z*, N0, H0, C*}
				hl_rmw(b, ea, |b, tmp| b.rol_no_carry(ea, tmp, true)),
			(RLC, Some(SynOp::Srg(reg))) =>                              // {Z*, N0, H0, C*}
				b.rol_no_carry(ea, reg, true),
			(RRA, None) =>                                               // {Z0, N0, H0, C*}
				b.ror_carry(ea, REG_A, false),
			(RR, Some(SynOp::Srg(Reg::HL))) =>                           // {Z*, N0, H0, C*}
				hl_rmw(b, ea, |b, tmp| b.ror_carry(ea, tmp, true)),
			(RR, Some(SynOp::Srg(reg))) =>                               // {Z*, N0, H0, C*}
				b.ror_carry(ea, reg, true),
			(RRCA, None) =>                                              // {Z0, N0, H0, C*}
				b.ror_no_carry(ea, REG_A, false),
			(RRC, Some(SynOp::Srg(Reg::HL))) =>                          // {Z*, N0, H0, C*}
				hl_rmw(b, ea, |b, tmp| b.ror_no_carry(ea, tmp, true)),
			(RRC, Some(SynOp::Srg(reg))) =>                              // {Z*, N0, H0, C*}
				b.ror_no_carry(ea, reg, true),
			(SWAP, _) => {
				b.nop(ea); // TODO
				// swap(r8)
					// {Z*, N0, H0, C0} 0xCB_{0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x37} (swap r)
					// InstDesc(0xCB_30, SWAP, &[Srg(B)],                 Other,  Imp),
					// ...

				// swap([hl])
					// {Z*, N0, H0, C0} 0xCB_36 (swap [hl])
					// InstDesc(0xCB_36, SWAP, &[IndReg(HL)],             Other,  Ind(HL, RW)),
			}
			(BIT, _) => {
				b.nop(ea); // TODO
				// zf <- r8.n
					// {Z*, N0, H1, C-} 0xCB_{{4,5,6,7}{^6,E}}} (bit n, r)
					// InstDesc(0xCB_40, BIT,  &[Op, Srg(B)],             Other,  Imp),
					// ...

				// zf <- [hl].n
					// {Z*, N0, H1, C-} 0xCB_{{4,5,6,7}{6,E}}} (bit n, [hl])
					// InstDesc(0xCB_46, BIT,  &[Op, IndReg(HL)],         Other,  Ind(HL, R)),
			}
			(RES, _) => {
				b.nop(ea); // TODO
				// r8.n <- 0
					// no flag changes
					// InstDesc(0xCB_80, RES,  &[Op, Srg(B)],             Other,  Imp),
					// ...

				// [hl].n <- 0
					// no flag changes
					// InstDesc(0xCB_86, RES,  &[Op, IndReg(HL)],         Other,  Ind(HL, RW)),
			}
			(SET, _) => {
				b.nop(ea); // TODO
				// r8.n <- 1
					// no flag changes
					// InstDesc(0xCB_C0, SET,  &[Op, Srg(B)],             Other,  Imp),
					// ...

				// [hl].n <- 1
					// no flag changes
					// InstDesc(0xCB_C6, SET,  &[Op, IndReg(HL)],         Other,  Ind(HL, RW)),
			}

			// ------------------------------------------------------------------------------------
			// Flag manipulation

			(CCF, _) => {
				b.nop(ea); // TODO
				// {Z-, N0, H0, C*} 0x3F (ccf)
			}
			(SCF, _) => {
				b.nop(ea); // TODO
				// {Z-, N0, H0, C1} 0x37 (scf)
			}

			// ------------------------------------------------------------------------------------
			// Control flow

			(JP, Some(SynOp::Op)) => { // no flag changes
				// jp nn
				b.branch(ea, target.unwrap(), 0);
			}
			(JP, Some(SynOp::Srg(Reg::HL))) => { // no flag changes
				// jp hl
				b.rr     (ea, Reg::HL);
				b.ibranch(ea, REG_HL_TMP, 0);
			}
			(JP, Some(SynOp::Cc(cond))) => { // no flag changes
				// jp cc, nn
				let cond = b.cc(ea, cond);
				b.cbranch(ea, cond, target.unwrap(), -1, 0);
			}
			(JR, Some(SynOp::Op)) => { // no flag changes
				// jr e
				b.branch(ea, target.unwrap(), 0);
			}
			(JR, Some(SynOp::Cc(cond))) => { // no flag changes
				// jr cc, e
				let cond = b.cc(ea, cond);
				b.cbranch(ea, cond, target.unwrap(), -1, 0);
			}
			(CALL, Some(SynOp::Op)) => { // no flag changes
				// call nn
				b.push_return_addr(ea, i.next_va());
				b.call(ea, target.unwrap(), 0);
			}
			(CALL, Some(SynOp::Cc(cond))) => { // no flag changes
				// call cc, nn
				let cond = b.not_cc(ea, cond);
				b.cbranch_and_split(ea, cond, i.next_ea(), -1, -1);
				b.push_return_addr (ea, i.next_va());
				b.call             (ea, target.unwrap(),   0);
			}
			(RET, None) => { // no flag changes
				// ret
				b.return_(ea);
			}
			(RET, Some(SynOp::Cc(cond))) => { // no flag changes
				// ret cc
				let cond = b.not_cc(ea, cond);
				b.cbranch_and_split(ea, cond, i.next_ea(), -1, -1);
				b.return_(ea);
			}
			(RST, Some(_)) => { // no flag changes
				// rst n
				b.push_return_addr(ea, i.next_va());
				b.call            (ea, target.unwrap(), 0);
			}
			(RETI, None) => { // no flag changes
				// reti
				b.return_(ea);
			}

			// ------------------------------------------------------------------------------------
			// Data transfer

			(LD, _) => {
				b.nop(ea); // TODO
				// no flag changes EXCEPT for 0xF8

				// REG <- REG
					// r8 <- r8
						// InstDesc(   0x40, LD,   &[Srg(B), Srg(B)],         Other,  Imp),
						// ...

					// r16 <- r16
						// InstDesc(   0xF9, LD,   &[Srg(SP), Srg(HL)],       Other,  Imp),
						// ...

					// hl <- sp + imm8
						// {Z0, N0, H*, C*} 0xF8 (ld hl, sp + imm)
						// InstDesc(   0xF8, LD,   &[Srg(HL), SpPlusOp],      Other,  SPImm),

				// REG <- IMM
					// r8 <- uimm8
						// InstDesc(   0x06, LD,   &[Srg(B), Op],             Other,  UImm8),
						// ...

					// r16 <- nn
						// InstDesc(   0x01, LD,   &[Srg(BC), Op],            Other,  Imm16),
						// ...

				// REG <- [MEM]
					// r8 <- [r16]
						// InstDesc(   0x0A, LD,   &[Srg(A), IndReg(BC)],     Other,  Ind(BC, R)),
						// ...

					// a <- [hl±]
						// InstDesc(   0x2A, LD,   &[Srg(A), IndHlPlus],      Other,  Ind(HL, R)),
						// InstDesc(   0x3A, LD,   &[Srg(A), IndHlMinus],     Other,  Ind(HL, R)),

					// a <- [nn]
						// InstDesc(   0xFA, LD,   &[Srg(A), IndOp],          Other,  Add16(R)),

				// [MEM] <- REG
					// [r16] <- r8
						// InstDesc(   0x02, LD,   &[IndReg(BC), Srg(A)],     Other,  Ind(BC, W)),
						// ...

					// [hl±] <- a
						// InstDesc(   0x22, LD,   &[IndHlPlus, Srg(A)],      Other,  Ind(HL, W)),
						// InstDesc(   0x32, LD,   &[IndHlMinus, Srg(A)],     Other,  Ind(HL, W)),

					// [nn] <- sp
						// InstDesc(   0x08, LD,   &[IndOp, Srg(SP)],         Other,  Add16(W)),

					// [nn] <- a
						// InstDesc(   0xEA, LD,   &[IndOp, Srg(A)],          Other,  Add16(W)),

				// [MEM] <- IMM
					// [hl] <- uimm8
						// InstDesc(   0x36, LD,   &[IndReg(HL), Op2],        Other,  LdHlImm),

			}
			(LDH, _) => {
				b.nop(ea); // TODO
				// no flag changes

				// [MEM] <- REG
					// InstDesc(   0xE0, LDH,  &[IndOp, Srg(A)],          Other,  AddHi(W)),
					// InstDesc(   0xE2, LDH,  &[IndReg(C), Srg(A)],      Other,  IndHi(W)),

				// REG <- [MEM]
					// InstDesc(   0xF0, LDH,  &[Srg(A), IndOp],          Other,  AddHi(R)),
					// InstDesc(   0xF2, LDH,  &[Srg(A), IndReg(C)],      Other,  IndHi(R)),
			}
			(PUSH, _) => {
				b.nop(ea); // TODO
				// no flag changes
				// InstDesc(   0xC5, PUSH, &[Srg(BC)],                Other,  Ind(SP, W)),
				// ...
			}
			(POP, _) => {
				b.nop(ea); // TODO
				// no flag changes EXCEPT for 0xF1
				// {Z*, N*, H*, C*} 0xF1 (pop af)

				// InstDesc(   0xC1, POP,  &[Srg(BC)],                Other,  Ind(SP, R)),
				// ...
			}
			_ => {
				panic!("IR unimplemented: {:?}", self);
			}
		}
	}
}
