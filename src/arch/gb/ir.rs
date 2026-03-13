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

const REG_A:      IrReg = IrReg::reg8 (0);
const REG_B:      IrReg = IrReg::reg8 (1);
const REG_C:      IrReg = IrReg::reg8 (2);
const REG_D:      IrReg = IrReg::reg8 (3);
const REG_E:      IrReg = IrReg::reg8 (4);
const REG_H:      IrReg = IrReg::reg8 (5);
const REG_L:      IrReg = IrReg::reg8 (6);
const REG_CF:     IrReg = IrReg::reg8 (7);   // 4 Carry
const REG_HF:     IrReg = IrReg::reg8 (8);   // 5 Half-carry (BCD)
const REG_NF:     IrReg = IrReg::reg8 (9);   // 6 Subtraction (BCD)
const REG_ZF:     IrReg = IrReg::reg8 (10);  // 7 Zero
const REG_SP:     IrReg = IrReg::reg16(11);
const REG_W:      IrReg = IrReg::reg8 (13); // 8-bit temporary
const REG_X:      IrReg = IrReg::reg8 (14); // 8-bit temporary
const REG_Y:      IrReg = IrReg::reg8 (15); // 8-bit temporary
const REG_Z:      IrReg = IrReg::reg8 (16); // 8-bit temporary
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
	/// Named `rr` to match the ISA docs. Returns the temporary register.
	fn rr(&mut self, ea: EA, reg: Reg) -> IrReg {
		match reg {
			Reg::BC => { self.ipair(ea, REG_BC_TMP, REG_B, REG_C,  -1, -1, -1); REG_BC_TMP }
			Reg::DE => { self.ipair(ea, REG_DE_TMP, REG_D, REG_E,  -1, -1, -1); REG_DE_TMP }
			Reg::HL => { self.ipair(ea, REG_HL_TMP, REG_H, REG_L,  -1, -1, -1); REG_HL_TMP }
			_ => panic!("given something other than a paired register"),
		}
	}

	/// Pair `REG_W` with `REG_Z` into `REG_WZ_TMP`.
	fn wz(&mut self, ea: EA) {
		self.ipair(ea, REG_WZ_TMP, REG_W, REG_Z,  -1, -1, -1);
	}

	/// Combine all the flag registers into an 8-bit value in `dst`.
	fn merge_flags(&mut self, ea: EA, dst: IrReg) {
		self.assign (ea, dst, IrConst::ZERO_8,                -1, -1);
		self.ibitset(ea, dst, dst, IrConst::_8(4), REG_CF,  -1, -1, -1, -1);
		self.ibitset(ea, dst, dst, IrConst::_8(5), REG_HF,  -1, -1, -1, -1);
		self.ibitset(ea, dst, dst, IrConst::_8(6), REG_NF,  -1, -1, -1, -1);
		self.ibitset(ea, dst, dst, IrConst::_8(7), REG_ZF,  -1, -1, -1, -1);
	}

	/// Extracts all the flag values from `src` into the flag registers.
	fn extract_flags(&mut self, ea: EA, src: IrReg) {
		self.ibit(ea, REG_CF, src, IrConst::_8(4),  -1, -1, -1);
		self.ibit(ea, REG_HF, src, IrConst::_8(5),  -1, -1, -1);
		self.ibit(ea, REG_NF, src, IrConst::_8(6),  -1, -1, -1);
		self.ibit(ea, REG_ZF, src, IrConst::_8(7),  -1, -1, -1);
	}

	/// Push an 8-bit value `src` onto the stack.
	fn push8(&mut self, ea: EA, src: impl Into<IrSrc>) {
		// full stack convention - subtract before storing
		self.iusub(ea, REG_SP, REG_SP, IrConst::_16(1),  -1, -1, -1);
		self.store(ea, REG_SP, src,                      -1, -1);
	}

	/// Push a 16-bit value onto the stack as two 8-bit halves, pushing the high half first so that
	/// the resultant value is little-endian in memory.
	fn push16(&mut self, ea: EA, srchi: impl Into<IrSrc>, srclo: impl Into<IrSrc>) {
		self.push8(ea, srchi);
		self.push8(ea, srclo);
	}

	/// Pop an 8-bit value off the stack into `dst`.
	fn pop8(&mut self, ea: EA, dst: impl Into<IrReg>) {
		let dst = dst.into();
		// full stack convention - load before adding
		self.load (ea, dst,    REG_SP,                   -1, -1);
		self.iuadd(ea, REG_SP, REG_SP, IrConst::_16(1),  -1, -1, -1);
	}

	/// Pop a 16-bit value off the stack as two 8-bit halves into `dstlo` and `dsthi`.
	fn pop16(&mut self, ea: EA, dsthi: impl Into<IrReg>, dstlo: impl Into<IrReg>) {
		self.pop8(ea, dstlo);
		self.pop8(ea, dsthi);
	}

	/// Pop a value into the WZ register and pair it, so it's ready to use as REG_WZ_TMP.
	fn pop_wz(&mut self, ea: EA) {
		self.pop16(ea, REG_W, REG_Z);
		self.wz   (ea);
	}

	/// Push the return address to the stack.
	fn push_return_addr(&mut self, ea: EA, ret_addr: VA) {
	 	let ret_addr = ret_addr.0 as u16;
		// push hi then lo
		self.push8(ea, IrConst::_8((ret_addr >> 8  ) as u8));
		self.push8(ea, IrConst::_8((ret_addr & 0xFF) as u8));
	}

	/// Push `ret_addr` to the stack, and then call `target`.
	fn call_(&mut self, ea: EA, ret_addr: VA, target: EA, targetn: i8) {
		self.push_return_addr(ea, ret_addr);
		self.call            (ea, target, targetn);
	}

	/// Pop the return address and `ret` to it.
	fn return_(&mut self, ea: EA) {
		self.pop_wz(ea);
		self.ret   (ea, REG_WZ_TMP, -1);
	}

	/// Set the Z flag to whether or not `reg == 0`.
	fn z_(&mut self, ea: EA, reg: impl Into<IrReg>, regn: i8) {
		let reg = reg.into();
		self.ieq(ea, REG_ZF, reg, IrConst::_8(0),  -1, regn, -1);
	}

	/// Set the N and H flags to 0.
	fn n0h0(&mut self, ea: EA) {
		self.assign(ea, REG_NF, IrConst::_8(0),  -1, -1);
		self.assign(ea, REG_HF, IrConst::_8(0),  -1, -1);
	}

	/// Set the N and H flags to 1.
	fn n1h1(&mut self, ea: EA) {
		self.assign(ea, REG_NF, IrConst::_8(1),  -1, -1);
		self.assign(ea, REG_HF, IrConst::_8(1),  -1, -1);
	}

	/// Set the Z, N, and H flags to 0.
	fn z0n0h0(&mut self, ea: EA) {
		self.assign(ea, REG_ZF, IrConst::_8(0),  -1, -1);
		self.n0h0  (ea);
	}

	/// Set the N and H flags to 0, and the Z flag to whether or not `reg == 0`.
	fn z_n0h0(&mut self, ea: EA, reg: impl Into<IrReg>, regn: i8) {
		self.z_  (ea, reg, regn);
		self.n0h0(ea);
	}

	/// Set the N, H, and C flags to 0, and the Z flag to whether or not `reg == 0`.
	fn z_n0h0c0(&mut self, ea: EA, reg: impl Into<IrReg>, regn: i8) {
		self.z_    (ea, reg, regn);
		self.n0h0  (ea);
		self.assign(ea, REG_CF, IrConst::_8(0),       -1, -1);
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

	/// Swap the nybbles of the given register. The zero flag is set according to whether the result
	/// is zero, and the N, H, and C flags are all set to 0.
	fn swap(&mut self, ea: EA, reg: impl Into<IrReg>, regn: i8) {
		let reg = reg.into();
		self.irol    (ea, reg, reg, IrConst::_8(4),  regn, regn, -1);
		self.z_n0h0c0(ea, reg,                       regn);
	}

	/// Perform an increment or decrement on `reg`. `delta == 1` increments, `delta == -1`
	/// decrements. If `change_flags`, the zero flag is set according to if `reg == 0` after the
	/// crement; N is set to 0 if `delta == 1` and 1 otherwise; and H is set according to the
	/// half-carry rules.
	fn inc_dec(&mut self, ea: EA, reg: impl Into<IrReg>, delta: isize, change_flags: bool) {
		let reg = reg.into();
		match delta {
			1  => self.iuadd(ea, reg, reg, IrConst::with_size(reg.size(), 1),  -1, -1, -1),
			-1 => self.iusub(ea, reg, reg, IrConst::with_size(reg.size(), 1),  -1, -1, -1),
			_  => panic!("bad delta"),
		}

		if change_flags {
			self.z_    (ea, reg, -1);
			self.assign(ea, REG_NF, IrConst::_8(if delta == 1 { 0 } else { 1 }),  -1, -1);
			// TODO: half carry
			self.assign(ea, REG_HF, IrConst::_8(0),  -1, -1);
		}
	}

	/// Do a conditional branch using the condition code `cc`.
	fn cc_branch(&mut self, ea: EA, cc: Cc, target: EA, targetn: i8) {
		let cond = self.cc(ea, cc);
		self.cbranch(ea, cond, target, -1, targetn);
	}

	/// Load indirect, using one of the paired registers as the source address.
	fn load_ind(&mut self, ea: EA, dst: impl Into<IrReg>, src: Reg, srcn: i8) {
		let src = self.rr(ea, src);
		self.load(ea, dst.into(), src,  -1, srcn);
	}

	/// Store indirect, using one of the paired registers as the destination address.
	fn store_ind(&mut self, ea: EA, dst: Reg, src: impl Into<IrSrc>, dstn: i8, srcn: i8) {
		let dst = self.rr(ea, dst);
		self.store(ea, dst, src.into(),  dstn, srcn);
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
	b.load_ind(ea, REG_Z, Reg::HL,    hln);
	f         (b,  REG_Z);
	b.store   (ea, REG_HL_TMP, REG_Z, hln, -1);
}

impl InstDesc {
	pub(super) fn build_ir(&self, i: &Instruction, target: Option<EA>, b: &mut IrBuilder) {
		use { MetaOp::*, SynOp::*, Reg::* };

		let ea = i.ea();

		match (self.meta_op(), self.syn_ops()) {
			(UNK,  &[]) => { panic!("what the hell is an unknown instruction doing in a BB?"); }

			// for all these, have to emit *something* to avoid empty IR BBs.
			(NOP,  &[]) => { b.nop(ea); } // no flag changes
			(DI,   &[]) => { b.nop(ea); } // no flag changes
			(EI,   &[]) => { b.nop(ea); } // no flag changes
			(HALT, &[]) => { b.nop(ea); } // no flag changes
			(STOP, &[]) => { b.nop(ea); } // no flag changes

			// ------------------------------------------------------------------------------------
			// Computation

			// add r
			(ADD, [Srg(A), Srg(_reg)]) => { // {Z*, N0, H*, C*}
				b.nop(ea); // TODO
			}
			// add [hl]
			(ADD, [Srg(A), IndReg(HL)]) => { // {Z*, N0, H*, C*}
				b.nop(ea); // TODO
			}
			// add n
			(ADD, [Srg(A), Op]) => { // {Z*, N0, H*, C*}
				b.nop(ea); // TODO
			}
			// add hl, rr
			(ADD, [Srg(HL), Srg(_reg)]) => { // {Z-, N0, H*, C*}
				b.nop(ea); // TODO
			}
			// add sp, e
			(ADD, [Srg(SP), Op]) => { // {Z0, N0, H*, C*}
				b.nop(ea); // TODO
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

			// inc bc, inc de, inc hl
			(INC, &[Srg(reg @ (BC | DE | HL))]) => { // no flag changes
				let tmp_reg = b.rr(ea, reg);
				b.inc_dec(ea, tmp_reg, 1, false);
				b.ihi    (ea, reg.hi().into(), tmp_reg,          -1, -1);
				b.ilo    (ea, reg.lo().into(), tmp_reg,          -1, -1);
			}

			// inc sp
			(INC, [Srg(SP)]) => { // no flag changes
				b.inc_dec(ea, REG_SP, 1, false);
			}

			// inc r
			(INC, &[Srg(reg)]) => { // {Z*, N0, H*, C-}
				let reg = IrReg::from(reg);
				b.inc_dec(ea, reg, 1, true);
			}

			// inc [hl]
			(INC, [IndReg(HL)]) => {  // {Z*, N0, H*, C-}
				hl_rmw(b, ea, |b, reg| b.inc_dec(ea, reg, 1, true), 0);
			}

			// dec bc, inc de, inc hl
			(DEC, &[Srg(reg @ (BC | DE | HL))]) => { // no flag changes
				let tmp_reg = b.rr(ea, reg);
				b.inc_dec(ea, tmp_reg, -1, false);
				b.ihi    (ea, reg.hi().into(), tmp_reg,          -1, -1);
				b.ilo    (ea, reg.lo().into(), tmp_reg,          -1, -1);
			}

			// dec sp
			(DEC, [Srg(SP)]) => { // no flag changes
				b.inc_dec(ea, REG_SP, -1, false);
			}

			// dec r
			(DEC, &[Srg(reg)]) => { // {Z*, N0, H*, C-}
				let reg = IrReg::from(reg);
				b.inc_dec(ea, reg, -1, true);
			}

			// dec [hl]
			(DEC, [IndReg(HL)]) => {  // {Z*, N0, H*, C-}
				hl_rmw(b, ea, |b, reg| b.inc_dec(ea, reg, -1, true), 0);
			}

			// cpl a
			(CPL, [Srg(A)]) => { // {Z-, N1, H1, C-}
				b.inot(ea, REG_A, REG_A,  -1, -1);
				b.n1h1(ea);
			}

			// daa
			(DAA, []) => { // {Z*, N-, H0, C*}
				// The logic is something like this. oof.
				// REG_WZ = zxt(REG_A)
				// if(REG_NF) {
				//     // .0 = NF & HF
				//     // .4 = NF & CF
				//     if(REG_HF) REG_WZ -= 0x06;
				//     if(REG_CF) REG_WZ -= 0x60;
				// } else {
				//     // .0 = !NF & (HF | (A & 0xF > 9))
				//     // .4 = !NF & (CF | (A > 0x99))
				//     if(REG_HF || (REG_WZ & 0x0F > 0x09)) REG_WZ += 0x06;
				//     if(REG_CF || REG_WZ > 0x99)          REG_WZ += 0x60;
				// }
				// REG_A  = lo(REG_WZ)
				// REG_ZF = REG_A == 0
				// REG_CF = REG_WZ.8
				// REG_HF = 0

				// This stuff is to make the code below more readable
				use { REG_A as A, REG_W as W, REG_X as X, REG_Y as Y, REG_Z as Z,
					REG_NF as NF, REG_HF as HF, REG_CF as CF, REG_ZF as ZF };
				const fn c(val: u8) -> IrConst { IrConst::_8(val) }

				// Z = subtraction adjustment { 0x00, -0x06, -0x60, -0x66 }
				b.band   (ea, Z, NF, HF,            -1, -1, -1);     // Z.0 = NF & HF
				b.band   (ea, X, NF, CF,            -1, -1, -1);     // X   = NF & CF
				b.ibitset(ea, Z, Z, c(4), X,        -1, -1, -1, -1); // Z.4 = NF & CF
				b.imul   (ea, Z, Z, c(-6i8 as u8),  -1, -1, -1);     // Z   = Z * -6

				// W = addition adjustment { 0x00, 0x06, 0x60, 0x66 }
				b.inot   (ea, X, NF,          -1, -1);         // X   = !NF
				b.iand   (ea, Y, A, c(0xF),   -1, -1, -1);     // Y   = A & 0xF
				b.iugt   (ea, Y, Y, c(9),     -1, -1, -1);     // Y   = A & 0xF > 9
				b.bor    (ea, Y, Y, HF,       -1, -1, -1);     // Y   = HF | (A & 0xF > 9)
				b.band   (ea, W, X, Y,        -1, -1, -1);     // W.0 = !NF & (HF | (A & 0xF > 9))
				b.iugt   (ea, Y, A, c(0x99),  -1, -1, -1);     // Y   = A > 0x99
				b.bor    (ea, Y, Y, CF,       -1, -1, -1);     // Y   = CF | (A > 0x99)
				b.band   (ea, Y, Y, X,        -1, -1, -1);     // Y   = !NF & (CF | (A > 0x99))
				b.ibitset(ea, W, W, c(4), Y,  -1, -1, -1, -1); // W.4 = !NF & (CF | (A > 0x99))
				b.imul   (ea, W, W, c(6),     -1, -1, -1);     // W   = W * 6

				// because the above two values were calculated based on NF and !NF, either they are
				// both 0 or exactly one is 0. so adding them together has the effect of choosing
				// between them.

				// Z = adjustment
				b.iuadd  (ea, Z, Z, W,     -1, -1, -1);     // Z = NF ? Z : W (effectively)

				// now we can do the actual addition and set the flags
				b.icarry (ea, CF, A, Z,     -1, -1, -1);
				b.iuadd  (ea, A,  A, Z,     -1, -1, -1);
				b.ieq    (ea, ZF, A, c(0),  -1, -1, -1);
				b.assign (ea, HF, c(0),     -1, -1);
			}

			// ------------------------------------------------------------------------------------
			// Bitwise

			// {Z0, N0, H0, C*}
			(RLA,  []) => b.rolc(ea, REG_A, false, -1),
			(RLCA, []) => b.rol (ea, REG_A, false, -1),
			(RRA,  []) => b.rorc(ea, REG_A, false, -1),
			(RRCA, []) => b.ror (ea, REG_A, false, -1),

			// {Z*, N0, H0, C*}
			(SLA, &[Srg(HL)])  => hl_rmw(b, ea, |b, reg| b.sla(ea, reg, -1), 0),
			(SLA, &[Srg(reg)]) =>                        b.sla(ea, reg,  0),
			(SRA, &[Srg(HL)])  => hl_rmw(b, ea, |b, reg| b.sra(ea, reg, -1), 0),
			(SRA, &[Srg(reg)]) =>                        b.sra(ea, reg,  0),
			(SRL, &[Srg(HL)])  => hl_rmw(b, ea, |b, reg| b.srl(ea, reg, -1), 0),
			(SRL, &[Srg(reg)]) =>                        b.srl(ea, reg,  0),

			(RL,  &[Srg(HL)])  => hl_rmw(b, ea, |b, reg| b.rolc(ea, reg, true, -1), 0),
			(RL,  &[Srg(reg)]) =>                        b.rolc(ea, reg, true,  0),
			(RLC, &[Srg(HL)])  => hl_rmw(b, ea, |b, reg| b.rol (ea, reg, true, -1), 0),
			(RLC, &[Srg(reg)]) =>                        b.rol (ea, reg, true,  0),
			(RR,  &[Srg(HL)])  => hl_rmw(b, ea, |b, reg| b.rorc(ea, reg, true, -1), 0),
			(RR,  &[Srg(reg)]) =>                        b.rorc(ea, reg, true,  0),
			(RRC, &[Srg(HL)])  => hl_rmw(b, ea, |b, reg| b.ror (ea, reg, true, -1), 0),
			(RRC, &[Srg(reg)]) =>                        b.ror (ea, reg, true,  0),

			// {Z*, N0, H0, C0}
			(SWAP, &[Srg(reg)])   =>                        b.swap(ea, reg, -1),
			(SWAP, &[IndReg(HL)]) => hl_rmw(b, ea, |b, reg| b.swap(ea, reg, -1), 0),

			// {Z*, N0, H1, C-}
			(BIT, &[Op, Srg(reg)]) => {
				let Operand::UImm(bit) = i.ops()[0] else { panic!() };
				let bit = IrConst::_8(bit as u8);
				let reg = IrReg::from(reg);
				b.ibit(ea, REG_ZF, reg, bit, -1, -1, -1);
			}
			(BIT, [Op, IndReg(HL)]) => {
				let Operand::UImm(bit) = i.ops()[0] else { panic!() };
				let bit = IrConst::_8(bit as u8);
				// operand 0 is the bit number, operand 1 is [hl]
				b.load_ind(ea, REG_Z,  HL,          1);
				b.ibit    (ea, REG_ZF, REG_Z, bit, -1, -1, -1);
			}

			// no flag changes
			(RES, &[Op, Srg(reg)]) => {
				let Operand::UImm(bit) = i.ops()[0] else { panic!() };
				let bit = IrConst::_8(bit as u8);
				let reg = IrReg::from(reg);
				b.ibitset(ea, reg, reg, bit, IrConst::ZERO_8,  -1, -1, -1, -1);
			}
			(RES, [Op, IndReg(HL)]) => {
				let Operand::UImm(bit) = i.ops()[0] else { panic!() };
				let bit = IrConst::_8(bit as u8);
				hl_rmw(b, ea, |b, reg| {
					b.ibitset(ea, reg, reg, bit, IrConst::ZERO_8, -1, -1, -1, -1);
				}, 1); // operand 0 is the bit number, operand 1 is [hl]
			}

			// no flag changes
			(SET, &[Op, Srg(reg)]) => {
				let Operand::UImm(bit) = i.ops()[0] else { panic!() };
				let bit = IrConst::_8(bit as u8);
				let reg = IrReg::from(reg);
				b.ibitset(ea, reg, reg, bit, IrConst::ONE_8,  -1, -1, -1, -1);
			}
			(SET, [Op, IndReg(HL)]) => {
				let Operand::UImm(bit) = i.ops()[0] else { panic!() };
				let bit = IrConst::_8(bit as u8);
				hl_rmw(b, ea, |b, reg| {
					b.ibitset(ea, reg, reg, bit, IrConst::ONE_8, -1, -1, -1, -1);
				}, 1); // operand 0 is the bit number, operand 1 is [hl]
			}

			// ------------------------------------------------------------------------------------
			// Flag manipulation

			(CCF, []) => { // {Z-, N0, H0, C*}
				b.n0h0(ea);
				b.bnot(ea, REG_CF, REG_CF,  -1, -1);
			}
			(SCF, []) => { // {Z-, N0, H0, C1}
				b.n0h0(ea);
				b.assign(ea, REG_CF, IrConst::ONE_8,  -1, -1);
			}

			// ------------------------------------------------------------------------------------
			// Control flow

			// no flag changes
			(JP, [Op]) => b.branch(ea, target.unwrap(), 0),
			(JR, [Op]) => b.branch(ea, target.unwrap(), 0),

			(JP, &[Cc(cond), Op]) => b.cc_branch(ea, cond, target.unwrap(), 0),
			(JR, &[Cc(cond), Op]) => b.cc_branch(ea, cond, target.unwrap(), 0),

			(JP, [Srg(HL)]) => {
				b.rr     (ea, HL);
				b.ibranch(ea, REG_HL_TMP, 0);
			}

			(CALL, [Op]) => b.call_(ea, i.next_va(), target.unwrap(), 0),
			(RST,  [Op]) => b.call_(ea, i.next_va(), target.unwrap(), 0),
			(CALL, &[Cc(cond), Op]) => {
				let cond = b.not_cc(ea, cond);
				b.cbranch_and_split(ea, cond, i.next_ea(), -1, -1);
				b.push_return_addr (ea, i.next_va());
				b.call             (ea, target.unwrap(),   0);
			}

			(RETI, []) => b.return_(ea),
			(RET,  []) => b.return_(ea),
			(RET,  &[Cc(cond)]) => {
				let cond = b.not_cc(ea, cond);
				b.cbranch_and_split(ea, cond, i.next_ea(), -1, -1);
				b.return_(ea);
			}

			// ------------------------------------------------------------------------------------
			// Data transfer

			// ld sp, hl (0xF9)
			(LD, &[Srg(SP), Srg(HL)]) => { // no flag changes
				b.rr    (ea, HL);
				b.assign(ea, REG_SP, REG_HL_TMP,  -1, -1);
			}

			// ld r, r (many, many opcodes in [0x40 .. 0x7F] range)
			(LD, &[Srg(dst), Srg(src)]) => { // no flag changes
				b.assign(ea, dst.into(), IrReg::from(src),  -1, -1);
			}

			// ld hl, sp+e (0xF8)
			(LD, &[Srg(HL), SpPlusOp]) => { // {Z0, N0, H]*, C*}
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
			(LD, &[Srg(dst @ (BC | DE | HL)), Op]) => { // no flag changes
				let Operand::UImm(val) = i.ops()[0] else { panic!() };
				b.assign(ea, REG_WZ_TMP, IrConst::_16(val as u16),  -1,  0);
				b.ihi   (ea, dst.hi().into(), REG_WZ_TMP,           -1, -1);
				b.ilo   (ea, dst.lo().into(), REG_WZ_TMP,           -1, -1);
			}

			// ld sp, nn (0x31) (same as above but SP is represented differently)
			(LD, &[Srg(SP), Op]) => { // no flag changes
				let Operand::UImm(val) = i.ops()[0] else { panic!() };
				b.assign(ea, REG_SP, IrConst::_16(val as u16),  -1,  0);
			}

			// ld r, n (various)
			(LD, &[Srg(dst), Op]) => { // no flag changes
				let Operand::UImm(val) = i.ops()[0] else { panic!() };
				b.assign(ea, dst.into(), IrConst::_8(val as u8),  -1, 0);
			}

			// ld r, [rr] (various)
			(LD, &[Srg(dst), IndReg(src @ (BC | DE | HL))]) => { // no flag changes
				b.load_ind(ea, dst, src,  0);
			}

			// ld [rr], r (various)
			(LD, &[IndReg(dst @ (BC | DE | HL)), Srg(src)]) => { // no flag changes
				b.store_ind(ea, dst, IrReg::from(src),  0, -1);
			}

			// ld a, [nn] (0xFA)
			(LD, &[Srg(A), IndOp]) => { // no flag changes
				let Operand::Mem(src, _) = i.ops()[0] else { panic!() };
				b.load(ea, REG_A, IrConst::_16(src.0 as u16),  -1, 0);
			}

			// ld [nn], a (0xEA)
			(LD, &[IndOp, Srg(A)]) => { // no flag changes
				let Operand::Mem(dst, _) = i.ops()[0] else { panic!() };
				b.store(ea, IrConst::_16(dst.0 as u16), REG_A,  -1, 0);
			}

			// ld [hl+], a (0x22)
			// ld [hl-], a (0x32)
			(LD, &[pm @ (IndHlPlus | IndHlMinus), Srg(A)]) => { // no flag changes
				b.store_ind(ea, HL, REG_A,  0, -1);
				b.inc_hl   (ea, pm == IndHlPlus);
			}

			// ld a, [hl+] (0x2A)
			// ld a, [hl-] (0x3A)
			(LD, &[Srg(A), pm @ (IndHlPlus | IndHlMinus)]) => { // no flag changes
				b.load_ind(ea, A, HL,  0);
				b.inc_hl  (ea, pm == IndHlPlus);
			}

			// ld [hl], n (0x36)
			(LD, &[IndReg(HL), Op2]) => { // no flag changes
				let Operand::UImm(src) = i.ops()[1] else { panic!() };
				b.store_ind(ea, HL, IrConst::_8(src as u8),  0, 1);
			}

			// ld [nn], sp (0x08)
			(LD, &[IndOp, Srg(SP)]) => { // no flag changes
				let Operand::Mem(dst, _) = i.ops()[0] else { panic!() };

				// split it into two 8-bit stores, little-endian
				b.ilo  (ea, REG_Z, REG_SP,                      -1, -1);
				b.store(ea, IrConst::_16(dst.0 as u16), REG_Z,   0, -1);
				b.ihi  (ea, REG_W, REG_SP,                      -1, -1);
				// since "dst+1" isn't what they wrote in the operand, we don't associate
				// the IR operand with it; only on the first store.
				b.store(ea, IrConst::_16((dst.0 + 1) as u16), REG_W,  -1, -1);
			}

			// ld a, [0xFF00 + n] (0xF0)
			(LDH, [Srg(A), IndOp]) => { // no flag changes
				let Operand::Mem(src, _) = i.ops()[0] else { panic!() };
				b.load(ea, REG_A, IrConst::_16(src.0 as u16),  -1, 0);
			}
			// ld a, [0xFF00 + c] (0xF2)
			(LDH, [Srg(A), IndReg(C)]) => { // no flag changes
				b.izxt (ea, REG_WZ_TMP, REG_C,                            -1, -1);
				b.iuadd(ea, REG_WZ_TMP, REG_WZ_TMP, IrConst::_16(0xFF00), -1, -1, -1);
				b.load (ea, REG_A,      REG_WZ_TMP,                       -1, 0);
			}
			// ld [0xFF00 + n], a (0xE0)
			(LDH, [IndOp, Srg(A)]) => { // no flag changes
				let Operand::Mem(dst, _) = i.ops()[0] else { panic!() };
				b.store(ea, IrConst::_16(dst.0 as u16), REG_A,  -1, 0);
			}
			// ld [0xFF00 + c], a (0xE2)
			(LDH, [IndReg(C), Srg(A)]) => { // no flag changes
				b.izxt (ea, REG_WZ_TMP, REG_C,                            -1, -1);
				b.iuadd(ea, REG_WZ_TMP, REG_WZ_TMP, IrConst::_16(0xFF00), -1, -1, -1);
				b.store(ea, REG_WZ_TMP, REG_A,                             0, -1);
			}

			// push bc (0xC5)
			// push de (0xD5)
			// push hl (0xE5)
			(PUSH, &[Srg(reg @ (BC | DE | HL))]) => { // no flag changes
				b.push16(ea, IrReg::from(reg.hi()), IrReg::from(reg.lo()));
			}

			// push af (0xF5)
			(PUSH, [Srg(AF)]) => { // no flag changes
				b.merge_flags(ea, REG_Z);
				b.push16     (ea, REG_A, REG_Z);
			}

			// pop bc (0xC1)
			// pop de (0xD1)
			// pop hl (0xE1)
			(POP, &[Srg(reg @ (BC | DE | HL))]) => { // no flag changes
				b.pop16(ea, IrReg::from(reg.hi()), IrReg::from(reg.lo()));
			}

			// pop af (0xF1)
			(POP, [Srg(AF)]) => { // {Z*, N*, H*, C*}
				b.pop16        (ea, REG_A, REG_Z);
				b.extract_flags(ea, REG_Z)
			}

			_ => panic!("IR unimplemented: {:?}", self),
		}
	}
}
