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

	fn call_(&mut self, ea: EA, ret_addr: VA, target: EA, targetn: i8) {
		self.push_return_addr(ea, ret_addr);
		self.call            (ea, target, targetn);
	}

	/// Pop the return address and `ret` to it.
	fn return_(&mut self, ea: EA) {
		self.pop_wz(ea);
		self.ret(ea, REG_WZ_TMP, -1);
	}

	/// Set the Z, N, and H flags to 0.
	fn z0n0h0(&mut self, ea: EA) {
		self.assign(ea, REG_ZF, IrConst::_8(0),  -1, -1);
		self.assign(ea, REG_NF, IrConst::_8(0),  -1, -1);
		self.assign(ea, REG_HF, IrConst::_8(0),  -1, -1);
	}

	/// Set the N and H flags to 0, and the Z flag to whether or not `reg == 0`.
	fn z_n0h0(&mut self, ea: EA, reg: impl Into<IrReg>, regn: i8) {
		let reg = reg.into();
		self.ieq   (ea, REG_ZF, reg, IrConst::_8(0),  -1, regn, -1);
		self.assign(ea, REG_NF, IrConst::_8(0),       -1, -1);
		self.assign(ea, REG_HF, IrConst::_8(0),       -1, -1);
	}

	/// Shift the given `reg` left. The carry flag is set to the MSB of `reg`, and the zero flag is
	/// set if the result is 0. N and H flags are set to 0.
	fn sla(&mut self, ea: EA, reg: impl Into<IrReg>, regn: i8) {
		let reg = reg.into();
		self.ibit  (ea, REG_CF, reg, IrConst::_8(7),    -1, regn, -1);
		self.ishl  (ea, reg,    reg, IrConst::_8(1),  regn, regn, -1);
		self.z_n0h0(ea, reg,                          regn);
	}

	/// Shift the given `reg` right arithmetic. The carry flag is set to the MSB of `reg`, and the
	/// zero flag is set if the result is 0. N and H flags are set to 0.
	fn sra(&mut self, ea: EA, reg: impl Into<IrReg>, regn: i8) {
		let reg = reg.into();
		self.ibit  (ea, REG_CF, reg, IrConst::_8(0),    -1, regn, -1);
		self.isshr (ea, reg,    reg, IrConst::_8(1),  regn, regn, -1);
		self.z_n0h0(ea, reg,                          regn);
	}

	/// Shift the given `reg` right logical. The carry flag is set to the MSB of `reg`, and the
	/// zero flag is set if the result is 0. N and H flags are set to 0.
	fn srl(&mut self, ea: EA, reg: impl Into<IrReg>, regn: i8) {
		let reg = reg.into();
		self.ibit  (ea, REG_CF, reg, IrConst::_8(0),    -1, regn, -1);
		self.iushr (ea, reg,    reg, IrConst::_8(1),  regn, regn, -1);
		self.z_n0h0(ea, reg,                          regn);
	}

	/// Rotate the given `reg` left. The carry flag is set to the MSB of `reg`, but otherwise does
	/// not participate. If `set_zero_flag`, the zero flag will be set if the result is 0;
	/// otherwise the zero flag will be set to 0 always. N and H flags are set to 0.
	fn rol(&mut self, ea: EA, reg: impl Into<IrReg>, set_zero_flag: bool, regn: i8) {
		let reg = reg.into();
		self.ibit(ea, REG_CF, reg, IrConst::_8(7),    -1, regn, -1);
		self.irol(ea, reg,    reg, IrConst::_8(1),  regn, regn, -1);

		if set_zero_flag {
			self.z_n0h0(ea, reg, regn)
		} else {
			self.z0n0h0(ea);
		}
	}

	/// Rotate the given `reg` right. The carry flag is set to the MSB of `reg`, but otherwise does
	/// not participate. If `set_zero_flag`, the zero flag will be set if the result is 0;
	/// otherwise the zero flag will be set to 0 always. N and H flags are set to 0.
	fn ror(&mut self, ea: EA, reg: impl Into<IrReg>, set_zero_flag: bool, regn: i8) {
		let reg = reg.into();
		self.ibit(ea, REG_CF, reg, IrConst::_8(7),    -1, regn, -1);
		self.iror(ea, reg,    reg, IrConst::_8(1),  regn, regn, -1);

		if set_zero_flag {
			self.z_n0h0(ea, reg, regn)
		} else {
			self.z0n0h0(ea);
		}
	}

	/// Rotate the given `reg` left through the carry flag. If `set_zero_flag`, the zero flag will
	/// be set if the result is 0; otherwise the zero flag will be set to 0 always. N and H flags
	/// are set to 0.
	fn rolc(&mut self, ea: EA, reg: impl Into<IrReg>, set_zero_flag: bool, regn: i8) {
		let reg = reg.into();
		self.assign (ea, REG_Z,  REG_CF,                        -1,   -1);
		self.ibit   (ea, REG_CF, reg, IrConst::_8(7),           -1, regn, -1);
		self.irol   (ea, reg,    reg, IrConst::_8(1),         regn, regn, -1);
		self.ibitset(ea, reg,    reg, IrConst::_8(0), REG_Z,  regn, regn, -1, -1);

		if set_zero_flag {
			self.z_n0h0(ea, reg, regn)
		} else {
			self.z0n0h0(ea);
		}
	}

	/// Rotate the given `reg` right through the carry flag. If `set_zero_flag`, the zero flag will
	/// be set if the result is 0; otherwise the zero flag will be set to 0 always. N and H flags
	/// are set to 0.
	fn rorc(&mut self, ea: EA, reg: impl Into<IrReg>, set_zero_flag: bool, regn: i8) {
		let reg = reg.into();
		self.assign (ea, REG_Z,  REG_CF,                          -1,   -1);
		self.ibit   (ea, REG_CF, reg,   IrConst::_8(0),           -1, regn, -1);
		self.iror   (ea, reg,    reg,   IrConst::_8(1),         regn, regn, -1);
		self.ibitset(ea, reg,    reg,   IrConst::_8(7), REG_Z,  regn, regn, -1, -1);

		if set_zero_flag {
			self.z_n0h0(ea, reg, regn)
		} else {
			self.z0n0h0(ea);
		}
	}

	/// Do a conditional branch using the condition code `cc`.
	fn cc_branch(&mut self, ea: EA, cc: Cc, target: EA, targetn: i8) {
		let cond = self.cc(ea, cc);
		self.cbranch(ea, cond, target, -1, targetn);
	}

	/// Load indirect, using one of the paired registers as the source address.
	fn load_ind(&mut self, ea: EA, dst: impl Into<IrReg>, src: Reg, srcn: i8) {
		self.rr(ea, src);
		match src {
			Reg::BC => self.load(ea, dst.into(), REG_BC_TMP,  -1, srcn),
			Reg::DE => self.load(ea, dst.into(), REG_DE_TMP,  -1, srcn),
			Reg::HL => self.load(ea, dst.into(), REG_HL_TMP,  -1, srcn),
			_       => panic!("invalid paired reg used as source"),
		}
	}

	/// Store indirect, using one of the paired registers as the destination address.
	fn store_ind(&mut self, ea: EA, dst: Reg, src: impl Into<IrSrc>, dstn: i8, srcn: i8) {
		self.rr(ea, dst);
		match dst {
			Reg::BC => self.store(ea, REG_BC_TMP, src.into(),  dstn, srcn),
			Reg::DE => self.store(ea, REG_DE_TMP, src.into(),  dstn, srcn),
			Reg::HL => self.store(ea, REG_HL_TMP, src.into(),  dstn, srcn),
			_       => panic!("invalid paired reg used as dest"),
		}
	}

	/// Increment or decrement HL. Assumes HL has already been paired. Increments `REG_HL_TMP`,
	/// then extracts the components into `REG_H` and `REG_L`.
	fn inc_hl(&mut self, ea: EA, plus: bool) {
		if plus {
			self.iuadd(ea, REG_HL_TMP, REG_HL_TMP, IrConst::ONE_16,  -1, -1, -1);
		} else {
			self.iusub(ea, REG_HL_TMP, REG_HL_TMP, IrConst::ONE_16,  -1, -1, -1);
		}

		self.ilo(ea, REG_L, REG_HL_TMP,  -1, -1);
		self.ihi(ea, REG_H, REG_HL_TMP,  -1, -1);
	}
}

/// Perform some read-modify-write operation using `[hl]` as the source/dest. `f` is passed
/// a temporary register containing the 8-bit value loaded from `[hl]`; it must place the
/// result back into this same register.
fn hl_rmw(b: &mut IrBuilder, ea: EA, f: impl Fn(&mut IrBuilder, IrReg), hln: i8) {
	b.rr   (ea, Reg::HL);
	b.load (ea, REG_Z, REG_HL_TMP, -1, hln);
	f      (b,  REG_Z);
	b.store(ea, REG_HL_TMP, REG_Z, hln, -1);
}

impl InstDesc {
	pub(super) fn build_ir(&self, i: &Instruction, target: Option<EA>, b: &mut IrBuilder) {
		use { MetaOp::*, SynOp::* };

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

			// {Z0, N0, H0, C*}
			(RLA,  None) => b.rolc(ea, REG_A, false, -1),
			(RLCA, None) => b.rol (ea, REG_A, false, -1),
			(RRA,  None) => b.rorc(ea, REG_A, false, -1),
			(RRCA, None) => b.ror (ea, REG_A, false, -1),

			// {Z*, N0, H0, C*}
			(SLA, Some(Srg(Reg::HL))) => hl_rmw(b, ea, |b, reg| b.sla(ea, reg, -1), 0),
			(SLA, Some(Srg(reg)))     =>                        b.sla(ea, reg,  0),
			(SRA, Some(Srg(Reg::HL))) => hl_rmw(b, ea, |b, reg| b.sra(ea, reg, -1), 0),
			(SRA, Some(Srg(reg)))     =>                        b.sra(ea, reg,  0),
			(SRL, Some(Srg(Reg::HL))) => hl_rmw(b, ea, |b, reg| b.srl(ea, reg, -1), 0),
			(SRL, Some(Srg(reg)))     =>                        b.srl(ea, reg,  0),

			(RL,  Some(Srg(Reg::HL))) => hl_rmw(b, ea, |b, reg| b.rolc(ea, reg, true, -1), 0),
			(RL,  Some(Srg(reg)))     =>                        b.rolc(ea, reg, true,  0),
			(RLC, Some(Srg(Reg::HL))) => hl_rmw(b, ea, |b, reg| b.rol (ea, reg, true, -1), 0),
			(RLC, Some(Srg(reg)))     =>                        b.rol (ea, reg, true,  0),
			(RR,  Some(Srg(Reg::HL))) => hl_rmw(b, ea, |b, reg| b.rorc(ea, reg, true, -1), 0),
			(RR,  Some(Srg(reg)))     =>                        b.rorc(ea, reg, true,  0),
			(RRC, Some(Srg(Reg::HL))) => hl_rmw(b, ea, |b, reg| b.ror (ea, reg, true, -1), 0),
			(RRC, Some(Srg(reg)))     =>                        b.ror (ea, reg, true,  0),

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

			// no flag changes
			(JP, Some(Op)) => b.branch(ea, target.unwrap(), 0),
			(JR, Some(Op)) => b.branch(ea, target.unwrap(), 0),

			(JP, Some(Cc(cond))) => b.cc_branch(ea, cond, target.unwrap(), 0),
			(JR, Some(Cc(cond))) => b.cc_branch(ea, cond, target.unwrap(), 0),

			(JP, Some(Srg(Reg::HL))) => {
				b.rr     (ea, Reg::HL);
				b.ibranch(ea, REG_HL_TMP, 0);
			}

			(CALL, Some(Op)) => b.call_(ea, i.next_va(), target.unwrap(), 0),
			(RST,  Some(Op)) => b.call_(ea, i.next_va(), target.unwrap(), 0),
			(CALL, Some(Cc(cond))) => {
				let cond = b.not_cc(ea, cond);
				b.cbranch_and_split(ea, cond, i.next_ea(), -1, -1);
				b.push_return_addr (ea, i.next_va());
				b.call             (ea, target.unwrap(),   0);
			}

			(RETI, None) => b.return_(ea),
			(RET,  None) => b.return_(ea),
			(RET,  Some(Cc(cond))) => {
				let cond = b.not_cc(ea, cond);
				b.cbranch_and_split(ea, cond, i.next_ea(), -1, -1);
				b.return_(ea);
			}

			// ------------------------------------------------------------------------------------
			// Data transfer

			// no flag changes EXCEPT for ld hl, sp+e (0xF8)
			(LD, Some(op0)) => match (op0, self.syn_ops()[1]) {
				// ld sp, hl (0xF9)
				(Srg(Reg::SP), Srg(Reg::HL)) => {
					b.rr    (ea, Reg::HL);
					b.assign(ea, REG_SP, REG_HL_TMP,  -1, -1);
				}

				// ld r, r (many, many opcodes in [0x40 .. 0x7F] range)
				(Srg(dst), Srg(src)) => b.assign(ea, dst.into(), IrReg::from(src),  -1, -1),

				// ld hl, sp+e (0xF8)
				(Srg(Reg::HL), SpPlusOp) => { // {Z0, N0, H*, C*}
					let Operand::SImm(val) = i.ops()[0] else { panic!() };
					// it adds the sign-extended operand to SP, as if it were unsigned.
					let val = IrConst::_16((val as u64) as u16);
					b.iuadd (ea, REG_HL_TMP, REG_SP, val,       -1, -1, 0);
					b.ilo   (ea, REG_L,      REG_HL_TMP,        -1, -1);
					b.ihi   (ea, REG_H,      REG_HL_TMP,        -1, -1);
					b.assign(ea, REG_ZF,     IrConst::ZERO_8,   -1, -1);
					b.assign(ea, REG_NF,     IrConst::ZERO_8,   -1, -1);
					// TODO: this is wrong, but the behavior of these on this instruction is very
					// strange (set to the half-carry and carry of only the *lower* 8 bits of the
					// addition...) so I doubt much/any code actually relies on it working right?
					b.assign(ea, REG_HF,     IrConst::ZERO_8,   -1, -1);
					b.assign(ea, REG_CF,     IrConst::ZERO_8,   -1, -1);
				}

				// ld rr, nn (0x01, 0x11, 0x21)
				(Srg(dst @ (Reg::BC | Reg::DE | Reg::HL)), Op) => {
					let Operand::UImm(val) = i.ops()[0] else { panic!() };
					b.assign(ea, REG_WZ_TMP, IrConst::_16(val as u16),  -1,  0);
					b.ilo   (ea, dst.lo().into(), REG_WZ_TMP,           -1, -1);
					b.ihi   (ea, dst.hi().into(), REG_WZ_TMP,           -1, -1);
				}

				// ld sp, nn (0x31) (same as above but I represent SP differently)
				(Srg(Reg::SP), Op) => {
					let Operand::UImm(val) = i.ops()[0] else { panic!() };
					b.assign(ea, REG_SP, IrConst::_16(val as u16),  -1,  0);
				}

				// ld r, n (various)
				(Srg(dst), Op) => {
					let Operand::UImm(val) = i.ops()[0] else { panic!() };
					b.assign(ea, dst.into(), IrConst::_8(val as u8),  -1, 0);
				}

				// ld r, [rr] (various)
				(Srg(dst), IndReg(src @ (Reg::BC | Reg::DE | Reg::HL))) => {
					b.load_ind(ea, dst, src,  0);
				}

				// ld [rr], r (various)
				(IndReg(dst @ (Reg::BC | Reg::DE | Reg::HL)), Srg(src)) => {
					b.store_ind(ea, dst, IrReg::from(src),  0, -1);
				}

				// ld a, [nn] (0xFA)
				(Srg(Reg::A), IndOp) => {
					let Operand::Mem(src, _) = i.ops()[0] else { panic!() };
					b.load(ea, REG_A, IrConst::_16(src.0 as u16),  -1, 0);
				}

				// ld [nn], a (0xEA)
				(IndOp, Srg(Reg::A)) => {
					let Operand::Mem(dst, _) = i.ops()[0] else { panic!() };
					b.store(ea, IrConst::_16(dst.0 as u16), REG_A,  -1, 0);
				}

				// ld [hl+], a (0x22)
				// ld [hl-], a (0x32)
				(pm @ (IndHlPlus | IndHlMinus), Srg(Reg::A)) => {
					b.store_ind(ea, Reg::HL, REG_A,  0, -1);
					b.inc_hl   (ea, pm == IndHlPlus);
				}

				// ld a, [hl+] (0x2A)
				// ld a, [hl-] (0x3A)
				(Srg(Reg::A), pm @ (IndHlPlus | IndHlMinus)) => {
					b.load_ind(ea, Reg::A, Reg::HL,  0);
					b.inc_hl  (ea, pm == IndHlPlus);
				}

				// ld [hl], n (0x36)
				(IndReg(Reg::HL), Op2) => {
					let Operand::UImm(src) = i.ops()[1] else { panic!() };
					b.store_ind(ea, Reg::HL, IrConst::_8(src as u8),  0, 1);
				}

				// ld [nn], sp (0x08)
				(IndOp, Srg(Reg::SP)) => {
					let Operand::Mem(dst, _) = i.ops()[0] else { panic!() };
					b.store(ea, IrConst::_16(dst.0 as u16), REG_SP,  -1, 0);
				}

				_ => panic!("`ld` IR unimplemented: {:?}", self),
			}
			(LDH, Some(op0)) => match (op0, self.syn_ops()[1]) {
				// no flag changes

				// ld a, [0xFF00 + n] (0xF0)
				(Srg(Reg::A), IndOp) => {
					let Operand::Mem(src, _) = i.ops()[0] else { panic!() };
					b.load(ea, REG_A, IrConst::_16(src.0 as u16),  -1, 0);
				}
				// ld a, [0xFF00 + c] (0xF2)
				(Srg(Reg::A), IndReg(Reg::C)) => {
					b.izxt (ea, REG_WZ_TMP, REG_C,                            -1, -1);
					b.iuadd(ea, REG_WZ_TMP, REG_WZ_TMP, IrConst::_16(0xFF00), -1, -1, -1);
					b.load (ea, REG_A,      REG_WZ_TMP,                       -1, 0);
				}
				// ld [0xFF00 + n], a (0xE0)
				(IndOp, Srg(Reg::A)) => {
					let Operand::Mem(dst, _) = i.ops()[0] else { panic!() };
					b.store(ea, IrConst::_16(dst.0 as u16), REG_A,  -1, 0);
				}
				// ld [0xFF00 + c], a (0xE2)
				(IndReg(Reg::C), Srg(Reg::A)) => {
					b.izxt (ea, REG_WZ_TMP, REG_C,                            -1, -1);
					b.iuadd(ea, REG_WZ_TMP, REG_WZ_TMP, IrConst::_16(0xFF00), -1, -1, -1);
					b.store(ea, REG_WZ_TMP, REG_A,                             0, -1);
				}

				_ => panic!("`ldh` IR unimplemented: {:?}", self),
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
