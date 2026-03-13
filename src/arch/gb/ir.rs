//! IR compiler for Game Boy/LR35902/SM83.

use crate::arch::{ IIrCompiler };
// use crate::program::{ MemIndir };
use crate::ir::{ IrReg, IrConst, IrSrc, IrBuilder };

use super::*;

// ------------------------------------------------------------------------------------------------
// GBIrCompiler
// ------------------------------------------------------------------------------------------------

pub(crate) struct GBIrCompiler;

impl IIrCompiler for GBIrCompiler {
	fn build_ir(&self, i: &Instruction, target: Option<EA>, b: &mut IrBuilder) {
		match i.bytes() {
			&[0xCB, byte2, ..] => build_ir(&lookup_desc_cb(byte2), i, target, b),
			&[byte1, ..]       => build_ir(&lookup_desc(byte1).expect("ono"), i, target, b),
			_                  => unreachable!(),
		}
	}

	fn arg_regs     (&self) -> &'static [IrReg] { ARG_REGS }
	fn return_regs  (&self) -> &'static [IrReg] { RETURN_REGS }
	fn stack_ptr_reg(&self) -> IrReg            { REG_SP }
}

// ------------------------------------------------------------------------------------------------
// IR regs
// ------------------------------------------------------------------------------------------------

const REG_A:  IrReg = IrReg::reg8 (0);
const REG_B:  IrReg = IrReg::reg8 (1);
const REG_C:  IrReg = IrReg::reg8 (2);
const REG_D:  IrReg = IrReg::reg8 (3);
const REG_E:  IrReg = IrReg::reg8 (4);
const REG_H:  IrReg = IrReg::reg8 (5);
const REG_L:  IrReg = IrReg::reg8 (6);
const REG_CF: IrReg = IrReg::reg8 (7);  // 4 Carry
const REG_HF: IrReg = IrReg::reg8 (8);  // 5 Half-carry (BCD)
const REG_NF: IrReg = IrReg::reg8 (9);  // 6 Subtraction (BCD)
const REG_ZF: IrReg = IrReg::reg8 (10); // 7 Zero
const REG_SP: IrReg = IrReg::reg16(11);
const REG_W:  IrReg = IrReg::reg8 (13); // 8-bit temporary
const REG_X:  IrReg = IrReg::reg8 (14); // 8-bit temporary
const REG_Y:  IrReg = IrReg::reg8 (15); // 8-bit temporary
const REG_Z:  IrReg = IrReg::reg8 (16); // 8-bit temporary
const REG_BC: IrReg = IrReg::reg16(17); // 16-bit temporary
const REG_DE: IrReg = IrReg::reg16(19); // 16-bit temporary
const REG_HL: IrReg = IrReg::reg16(21); // 16-bit temporary
const REG_WZ: IrReg = IrReg::reg16(23); // 16-bit temporary

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

// ------------------------------------------------------------------------------------------------
// Register and flag handling
// ------------------------------------------------------------------------------------------------

impl IrBuilder {
	/// Pair the constituent registers of a paired register into its corresponding `REG_XX`.
	/// Named `rr` to match the ISA docs. Returns the temporary register.
	fn rr(&mut self, ea: EA, reg: Reg) -> IrReg {
		match reg {
			Reg::BC => { self.ipair(ea, REG_BC, REG_B, REG_C,  -1, -1, -1); REG_BC }
			Reg::DE => { self.ipair(ea, REG_DE, REG_D, REG_E,  -1, -1, -1); REG_DE }
			Reg::HL => { self.ipair(ea, REG_HL, REG_H, REG_L,  -1, -1, -1); REG_HL }
			_ => panic!("given something other than a paired register"),
		}
	}

	/// Combine all the flag registers into an 8-bit value in `dst`.
	fn combine_flags(&mut self, ea: EA, dst: IrReg) {
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

	/// Set the Z flag to whether or not `reg == 0`.
	fn z_(&mut self, ea: EA, reg: impl Into<IrReg>, regn: i8) -> &mut Self {
		let reg = reg.into();
		self.ieq(ea, REG_ZF, reg, IrConst::_8(0),  -1, regn, -1);
		self
	}

	/// Set the N flag to a given value.
	fn nx(&mut self, ea: EA, src: impl Into<IrSrc>, srcn: i8) -> &mut Self {
		let src = src.into();
		self.assign(ea, REG_NF, src,  -1, srcn);
		self
	}

	/// Set the C flag to the carry out of unsigned `src1 + src2`.
	fn cx(&mut self, ea: EA, src1: impl Into<IrSrc>, src2: impl Into<IrSrc>, src1n: i8, src2n: i8)
	-> &mut Self {
		self.icarry(ea, REG_CF, src1.into(), src2.into(),  -1, src1n, src2n);
		self
	}

	/// Set the C flag to the carry out of unsigned `src1 + src2 + C`.
	fn cxc(&mut self, ea: EA, src1: impl Into<IrSrc>, src2: impl Into<IrSrc>, src1n: i8, src2n: i8)
	-> &mut Self {
		self.icarryc(ea, REG_CF, src1.into(), src2.into(), REG_CF,  -1, src1n, src2n, -1);
		self
	}

	/// Set the Z flag to 0.
	fn z0(&mut self, ea: EA) -> &mut Self { self.assign(ea, REG_ZF, IrConst::_8(0), -1, -1); self }
	/// Set the N flag to 0.
	fn n0(&mut self, ea: EA) -> &mut Self { self.assign(ea, REG_NF, IrConst::_8(0), -1, -1); self }
	/// Set the H flag to 0.
	fn h0(&mut self, ea: EA) -> &mut Self { self.assign(ea, REG_HF, IrConst::_8(0), -1, -1); self }
	/// Set the C flag to 0.
	fn c0(&mut self, ea: EA) -> &mut Self { self.assign(ea, REG_CF, IrConst::_8(0), -1, -1); self }

	/// Set the N flag to 1.
	fn n1(&mut self, ea: EA) -> &mut Self { self.assign(ea, REG_NF, IrConst::_8(1), -1, -1); self }
	/// Set the H flag to 1.
	fn h1(&mut self, ea: EA) -> &mut Self { self.assign(ea, REG_HF, IrConst::_8(1), -1, -1); self }
	/// Set the C flag to 1.
	fn c1(&mut self, ea: EA) -> &mut Self { self.assign(ea, REG_CF, IrConst::_8(1), -1, -1); self }
}

// ------------------------------------------------------------------------------------------------
// Memory
// ------------------------------------------------------------------------------------------------

impl IrBuilder {
	/// Push an 8-bit value `src` onto the stack.
	fn push8(&mut self, ea: EA, src: impl Into<IrSrc>) {
		// full stack convention - subtract before storing
		self.iusub(ea, REG_SP, REG_SP, IrConst::_16(1),  -1, -1, -1);
		self.store(ea, REG_SP, src,                      -1, -1);
	}

	/// Pop an 8-bit value off the stack into `dst`.
	fn pop8(&mut self, ea: EA, dst: impl Into<IrReg>) {
		let dst = dst.into();
		// full stack convention - load before adding
		self.load (ea, dst,    REG_SP,                   -1, -1);
		self.iuadd(ea, REG_SP, REG_SP, IrConst::_16(1),  -1, -1, -1);
	}

	/// Push a 16-bit value onto the stack as two 8-bit halves, pushing the high half first so that
	/// the resultant value is little-endian in memory.
	fn push16(&mut self, ea: EA, srchi: impl Into<IrSrc>, srclo: impl Into<IrSrc>) {
		self.push8(ea, srchi);
		self.push8(ea, srclo);
	}

	/// Pop a 16-bit value off the stack as two 8-bit halves into `dstlo` and `dsthi`.
	fn pop16(&mut self, ea: EA, dsthi: impl Into<IrReg>, dstlo: impl Into<IrReg>) {
		self.pop8(ea, dstlo);
		self.pop8(ea, dsthi);
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
}

// ------------------------------------------------------------------------------------------------
// Control flow
// ------------------------------------------------------------------------------------------------

impl IrBuilder {
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
		self.pop16(ea, REG_W,  REG_Z);
		self.ipair(ea, REG_WZ, REG_W, REG_Z,  -1, -1, -1);
		self.ret  (ea, REG_WZ,                -1);
	}

	/// Evaluate the condition code `cc` and return a register which contains its truth value.
	fn cc(&mut self, ea: EA, cc: Cc) -> IrReg {
		match cc {
			Cc::C  => REG_CF,
			Cc::Z  => REG_ZF,
			Cc::NC => { self.bnot(ea, REG_Z, REG_CF, -1, -1); REG_Z }
			Cc::NZ => { self.bnot(ea, REG_Z, REG_ZF, -1, -1); REG_Z }
		}
	}

	/// Evaluate the logical inversion of the condition code `cc` and return a register which
	/// contains the inverted truth value.
	fn not_cc(&mut self, ea: EA, cc: Cc) -> IrReg {
		self.cc(ea, cc.not())
	}

	/// Do a conditional branch using the condition code `cc`.
	fn cc_branch(&mut self, ea: EA, cc: Cc, target: EA, targetn: i8) {
		let cond = self.cc(ea, cc);
		self.cbranch(ea, cond, target, -1, targetn);
	}
}

// ------------------------------------------------------------------------------------------------
// Computation
// ------------------------------------------------------------------------------------------------

/// Perform some read-modify-write operation using `[hl]` as the source/dest. `callback` is passed a
/// temporary register containing the 8-bit value loaded from `[hl]`; it must place the result back
/// into this same register, and it must not modify `REG_HL`.
fn hl_rmw(b: &mut IrBuilder, ea: EA, callback: impl Fn(&mut IrBuilder, IrReg), hln: i8) {
	b.load_ind(ea, REG_Z, Reg::HL,  hln);
	callback  (b,  REG_Z);
	b.store   (ea, REG_HL, REG_Z,   hln, -1);
}

impl IrBuilder {
	/// Shift the given `reg` left. The carry flag is set to the MSB of `reg`, and the zero flag is
	/// set if the result is 0. N and H flags are set to 0.
	fn sla(&mut self, ea: EA, reg: impl Into<IrReg>, regn: i8) {
		let reg = reg.into();
		self.ibit(ea, REG_CF, reg, IrConst::_8(7),    -1, regn, -1);
		self.ishl(ea, reg,    reg, IrConst::_8(1),  regn, regn, -1);
		self.z_  (ea, reg,                          regn)
		.n0      (ea)
		.h0      (ea);
	}

	/// Shift the given `reg` right arithmetic. The carry flag is set to the MSB of `reg`, and the
	/// zero flag is set if the result is 0. N and H flags are set to 0.
	fn sra(&mut self, ea: EA, reg: impl Into<IrReg>, regn: i8) {
		let reg = reg.into();
		self.ibit (ea, REG_CF, reg, IrConst::_8(0),    -1, regn, -1);
		self.isshr(ea, reg,    reg, IrConst::_8(1),  regn, regn, -1);
		self.z_   (ea, reg,                          regn)
		.n0       (ea)
		.h0       (ea);
	}

	/// Shift the given `reg` right logical. The carry flag is set to the MSB of `reg`, and the
	/// zero flag is set if the result is 0. N and H flags are set to 0.
	fn srl(&mut self, ea: EA, reg: impl Into<IrReg>, regn: i8) {
		let reg = reg.into();
		self.ibit (ea, REG_CF, reg, IrConst::_8(0),    -1, regn, -1);
		self.iushr(ea, reg,    reg, IrConst::_8(1),  regn, regn, -1);
		self.z_   (ea, reg,                          regn)
		.n0       (ea)
		.h0       (ea);
	}

	/// Rotate the given `reg` left. The carry flag is set to the MSB of `reg`, but otherwise does
	/// not participate. If `set_zero_flag`, the zero flag will be set if the result is 0;
	/// otherwise the zero flag will be set to 0 always. N and H flags are set to 0.
	fn rol(&mut self, ea: EA, reg: impl Into<IrReg>, set_zero_flag: bool, regn: i8) {
		let reg = reg.into();
		self.ibit(ea, REG_CF, reg, IrConst::_8(7),    -1, regn, -1);
		self.irol(ea, reg,    reg, IrConst::_8(1),  regn, regn, -1);
		self.n0  (ea)
		.h0      (ea);

		if set_zero_flag {
			self.z_(ea, reg, regn);
		} else {
			self.z0(ea);
		}
	}

	/// Rotate the given `reg` right. The carry flag is set to the MSB of `reg`, but otherwise does
	/// not participate. If `set_zero_flag`, the zero flag will be set if the result is 0;
	/// otherwise the zero flag will be set to 0 always. N and H flags are set to 0.
	fn ror(&mut self, ea: EA, reg: impl Into<IrReg>, set_zero_flag: bool, regn: i8) {
		let reg = reg.into();
		self.ibit(ea, REG_CF, reg, IrConst::_8(7),    -1, regn, -1);
		self.iror(ea, reg,    reg, IrConst::_8(1),  regn, regn, -1);
		self.n0  (ea)
		.h0      (ea);

		if set_zero_flag {
			self.z_(ea, reg, regn);
		} else {
			self.z0(ea);
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
		self.n0     (ea)
		.h0         (ea);

		if set_zero_flag {
			self.z_(ea, reg, regn);
		} else {
			self.z0(ea);
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
		self.n0     (ea)
		.h0         (ea);

		if set_zero_flag {
			self.z_(ea, reg, regn);
		} else {
			self.z0(ea);
		}
	}

	/// Swap the nybbles of the given register. The zero flag is set according to whether the result
	/// is zero, and the N, H, and C flags are all set to 0.
	fn swap(&mut self, ea: EA, reg: impl Into<IrReg>, regn: i8) {
		let reg = reg.into();
		self.irol(ea, reg, reg, IrConst::_8(4),  regn, regn, -1);
		self.z_  (ea, reg,                       regn)
		.n0      (ea)
		.h0      (ea)
		.c0      (ea);
	}

	/// Increment or decrement HL. Assumes HL has already been paired. Increments `REG_HL`, then
	/// extracts the components into `REG_H` and `REG_L`.
	fn inc_dec_hl(&mut self, ea: EA, plus: bool) {
		if plus {
			self.iuadd(ea, REG_HL, REG_HL, IrConst::ONE_16,  -1, -1, -1);
		} else {
			self.iusub(ea, REG_HL, REG_HL, IrConst::ONE_16,  -1, -1, -1);
		}

		self.ilo(ea, REG_L, REG_HL,  -1, -1);
		self.ihi(ea, REG_H, REG_HL,  -1, -1);
	}

	/// Perform an increment or decrement on `reg`. `delta == 1` increments, `delta == -1`
	/// decrements. If `change_flags`, the zero flag is set according to if `reg == 0` after the
	/// crement; N is set to 0 if `delta == 1` and 1 otherwise; and H is set according to the
	/// half-carry rules.
	fn inc_dec(&mut self, ea: EA, reg: impl Into<IrReg>, delta: isize, change_flags: bool) {
		let reg = reg.into();
		let nf = match delta {
			1  => { self.iuadd(ea, reg, reg, IrConst::with_size(reg.size(), 1),  -1, -1, -1); 0 }
			-1 => { self.iusub(ea, reg, reg, IrConst::with_size(reg.size(), 1),  -1, -1, -1); 1 }
			_  => panic!("bad delta"),
		};

		if change_flags {
			self.z_(ea, reg, -1);
			self.nx(ea, IrConst::_8(nf), -1);
			// TODO: half-carry
			self.h0(ea);
		}
	}

	/// Add paired register `reg` onto `HL` and update flags. Pairs both `HL` and `reg`, and
	/// extracts results into `REG_H` and `REG_L` afterwards.
	fn add_hl_rr(&mut self, ea: EA, reg: Reg) {
		let src = self.rr(ea, reg);
		self.rr    (ea, Reg::HL);
		// TODO: half-carry
		self.h0    (ea);
		self.cx    (ea,         REG_HL, src,     -1, -1);
		self.iuadd (ea, REG_HL, REG_HL, src, -1, -1, -1);
		self.n0    (ea);
		self.ihi   (ea, REG_H,  REG_HL,      -1, -1);
		self.ilo   (ea, REG_L,  REG_HL,      -1, -1);
	}

	/// Add `REG_SP + val` (written `sp + e` in ISA docs), put result into `dst`, and update flags.
	fn add_sp_e(&mut self, ea: EA, dst: IrReg, val: i64, valn: i8) {
		// it adds the sign-extended operand to SP, as if it were unsigned.
		let val = IrConst::_16((val as u64) as u16);
		// TODO: half-carry (and carry; HF = carrybits.3, CF = carrybits.7)
		self.h0   (ea);
		self.c0   (ea);
		self.iuadd(ea, dst, REG_SP, val,  -1, -1, valn);
		self.z0   (ea);
		self.n0   (ea);
	}

	/// Add `src` onto `REG_A` and update flags.
	fn add_a(&mut self, ea: EA, src: impl Into<IrSrc>, srcn: i8) {
		let src = src.into();
		// TODO: half-carry
		self.h0   (ea);
		self.cx   (ea,         REG_A, src,      -1, srcn);
		self.iuadd(ea, REG_A,  REG_A, src,  -1, -1, srcn);
		self.n0   (ea);
		self.z_   (ea, REG_A,               -1);
	}

	/// Add `src` onto `REG_A` with carry and update flags.
	fn adc_a(&mut self, ea: EA, src: impl Into<IrSrc>, srcn: i8) {
		let src = src.into();
		// TODO: half-carry
		self.h0    (ea);
		self.cxc   (ea,         REG_A, src,          -1, srcn);
		self.iuaddc(ea, REG_A,  REG_A, src, REG_CF,  -1, -1, srcn, -1);
		self.n0    (ea);
		self.z_    (ea, REG_A,                       -1);
	}
}

// ------------------------------------------------------------------------------------------------
// Computation
// ------------------------------------------------------------------------------------------------

fn build_ir(desc: &InstDesc, i: &Instruction, target: Option<EA>, b: &mut IrBuilder) {
	use { MetaOp::*, SynOp::*, Reg::* };

	let ea = i.ea();

	match (desc.meta_op(), desc.syn_ops()) {
		(UNK,  &[]) => { panic!("what the hell is an unknown instruction doing in a BB?"); }

		// for all these, have to emit *something* to avoid empty IR BBs.
		(NOP,  &[]) => { b.nop(ea); } // no flag changes
		(DI,   &[]) => { b.nop(ea); } // no flag changes
		(EI,   &[]) => { b.nop(ea); } // no flag changes
		(HALT, &[]) => { b.nop(ea); } // no flag changes
		(STOP, &[]) => { b.nop(ea); } // no flag changes

		// ------------------------------------------------------------------------------------
		// Computation

		// add hl, rr
		(ADD, &[Srg(HL), Srg(reg)]) => { // {Z-, N0, H*, C*}
			b.add_hl_rr(ea, reg);
		}
		// add sp, e
		(ADD, [Srg(SP), Op]) => { // {Z0, N0, H*, C*}
			let Operand::SImm(val) = i.ops()[0] else { panic!() };
			b.add_sp_e(ea, REG_SP, val, 0);
		}

		// add r
		(ADD, &[Srg(A), Srg(reg)]) => { // {Z*, N0, H*, C*}
			b.add_a(ea, IrReg::from(reg), -1);
		}
		// add [hl]
		(ADD, [Srg(A), IndReg(HL)]) => { // {Z*, N0, H*, C*}
			b.load_ind(ea, REG_Z, HL,   0);
			b.add_a   (ea, REG_Z,      -1);
		}
		// add n
		(ADD, [Srg(A), Op]) => { // {Z*, N0, H*, C*}
			let Operand::UImm(val) = i.ops()[0] else { panic!() };
			b.add_a(ea, IrConst::_8(val as u8), -1);
		}

		// adc r
		(ADC, &[Srg(A), Srg(reg)]) => { // {Z*, N0, H*, C*}
			b.adc_a(ea, IrReg::from(reg), -1);
		}
		// adc [hl]
		(ADC, [Srg(A), IndReg(HL)]) => { // {Z*, N0, H*, C*}
			b.load_ind(ea, REG_Z, HL,   0);
			b.adc_a   (ea, REG_Z,      -1);
		}
		// adc n
		(ADC, [Srg(A), Op]) => { // {Z*, N0, H*, C*}
			let Operand::UImm(val) = i.ops()[0] else { panic!() };
			b.adc_a(ea, IrConst::_8(val as u8), -1);
		}

		// sub r
		(SUB, &[Srg(A), Srg(_reg)]) => { // {Z*, N1, H*, C*}
			b.nop(ea); // TODO
		}
		// sub [hl]
		(SUB, [Srg(A), IndReg(HL)]) => { // {Z*, N1, H*, C*}
			b.nop(ea); // TODO
		}
		// sub n
		(SUB, [Srg(A), Op]) => { // {Z*, N1, H*, C*}
			b.nop(ea); // TODO
		}

		// sbc r
		(SBC, &[Srg(A), Srg(_reg)]) => { // {Z*, N1, H*, C*}
			b.nop(ea); // TODO
		}
		// sbc [hl]
		(SBC, [Srg(A), IndReg(HL)]) => { // {Z*, N1, H*, C*}
			b.nop(ea); // TODO
		}
		// sbc n
		(SBC, [Srg(A), Op]) => { // {Z*, N1, H*, C*}
			b.nop(ea); // TODO
		}

		// and r
		(AND, &[Srg(A), Srg(_reg)]) => { // {Z*, N0, H1, C0}
			b.nop(ea); // TODO
		}
		// and [hl]
		(AND, [Srg(A), IndReg(HL)]) => { // {Z*, N0, H1, C0}
			b.nop(ea); // TODO
		}
		// and n
		(AND, [Srg(A), Op]) => { // {Z*, N0, H1, C0}
			b.nop(ea); // TODO
		}

		// or r
		(OR, &[Srg(A), Srg(_reg)]) => { // {Z*, N0, H0, C0}
			b.nop(ea); // TODO
		}
		// or [hl]
		(OR, [Srg(A), IndReg(HL)]) => { // {Z*, N0, H0, C0}
			b.nop(ea); // TODO
		}
		// or n
		(OR, [Srg(A), Op]) => { // {Z*, N0, H0, C0}
			b.nop(ea); // TODO
		}

		// xor r
		(XOR, &[Srg(A), Srg(_reg)]) => { // {Z*, N0, H0, C0}
			b.nop(ea); // TODO
		}
		// xor [hl]
		(XOR, [Srg(A), IndReg(HL)]) => { // {Z*, N0, H0, C0}
			b.nop(ea); // TODO
		}
		// xor n
		(XOR, [Srg(A), Op]) => { // {Z*, N0, H0, C0}
			b.nop(ea); // TODO
		}

		// cp r
		(CP, &[Srg(A), Srg(_reg)]) => { // {Z*, N1, H*, C*}
			b.nop(ea); // TODO
		}
		// cp [hl]
		(CP, [Srg(A), IndReg(HL)]) => { // {Z*, N1, H*, C*}
			b.nop(ea); // TODO
		}
		// cp n
		(CP, [Srg(A), Op]) => { // {Z*, N1, H*, C*}
			b.nop(ea); // TODO
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
			b.n1  (ea)
			.h1   (ea);
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
			b.cx     (ea,     A, Z,         -1, -1);
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
			b.n0 (ea)
			.h0  (ea)
			.bnot(ea, REG_CF, REG_CF,  -1, -1);
		}
		(SCF, []) => { // {Z-, N0, H0, C1}
			b.n0(ea)
			.h0 (ea)
			.c1 (ea);
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
			b.ibranch(ea, REG_HL, 0);
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
			b.assign(ea, REG_SP, REG_HL,  -1, -1);
		}

		// ld r, r (many, many opcodes in [0x40 .. 0x7F] range)
		(LD, &[Srg(dst), Srg(src)]) => { // no flag changes
			b.assign(ea, dst.into(), IrReg::from(src),  -1, -1);
		}

		// ld hl, sp+e (0xF8)
		(LD, &[Srg(HL), SpPlusOp]) => { // {Z0, N0, H]*, C*}
			let Operand::SImm(val) = i.ops()[0] else { panic!() };
			b.add_sp_e(ea, REG_HL, val, 0);
			b.ilo     (ea, REG_L,  REG_HL,            -1, -1);
			b.ihi     (ea, REG_H,  REG_HL,            -1, -1);
		}

		// ld rr, nn (0x01, 0x11, 0x21)
		(LD, &[Srg(dst @ (BC | DE | HL)), Op]) => { // no flag changes
			let Operand::UImm(val) = i.ops()[0] else { panic!() };
			b.assign(ea, REG_WZ, IrConst::_16(val as u16),  -1,  0);
			b.ihi   (ea, dst.hi().into(), REG_WZ,           -1, -1);
			b.ilo   (ea, dst.lo().into(), REG_WZ,           -1, -1);
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
			b.store_ind (ea, HL, REG_A,  0, -1);
			b.inc_dec_hl(ea, pm == IndHlPlus);
		}

		// ld a, [hl+] (0x2A)
		// ld a, [hl-] (0x3A)
		(LD, &[Srg(A), pm @ (IndHlPlus | IndHlMinus)]) => { // no flag changes
			b.load_ind  (ea, A, HL,  0);
			b.inc_dec_hl(ea, pm == IndHlPlus);
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
			b.izxt (ea, REG_WZ, REG_C,                         -1, -1);
			b.iuadd(ea, REG_WZ, REG_WZ, IrConst::_16(0xFF00),  -1, -1, -1);
			b.load (ea, REG_A,      REG_WZ,                    -1, 0);
		}
		// ld [0xFF00 + n], a (0xE0)
		(LDH, [IndOp, Srg(A)]) => { // no flag changes
			let Operand::Mem(dst, _) = i.ops()[0] else { panic!() };
			b.store(ea, IrConst::_16(dst.0 as u16), REG_A,  -1, 0);
		}
		// ld [0xFF00 + c], a (0xE2)
		(LDH, [IndReg(C), Srg(A)]) => { // no flag changes
			b.izxt (ea, REG_WZ, REG_C,                         -1, -1);
			b.iuadd(ea, REG_WZ, REG_WZ, IrConst::_16(0xFF00),  -1, -1, -1);
			b.store(ea, REG_WZ, REG_A,                          0, -1);
		}

		// push bc (0xC5)
		// push de (0xD5)
		// push hl (0xE5)
		(PUSH, &[Srg(reg @ (BC | DE | HL))]) => { // no flag changes
			b.push16(ea, IrReg::from(reg.hi()), IrReg::from(reg.lo()));
		}

		// push af (0xF5)
		(PUSH, [Srg(AF)]) => { // no flag changes
			b.combine_flags(ea, REG_Z);
			b.push16       (ea, REG_A, REG_Z);
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

		_ => panic!("IR unimplemented: {:?}", desc),
	}
}
