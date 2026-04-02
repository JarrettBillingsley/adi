//! IR compiler for Game Boy/LR35902/SM83.

use crate::arch::{ IIrCompiler };
use crate::program::{ BBTerm };
use crate::ir::{ IrReg, IrConst, IrBuilder, BuildReg, BuildSrc, BuildEA };

use super::*;

// ------------------------------------------------------------------------------------------------
// GBIrCompiler
// ------------------------------------------------------------------------------------------------

pub(crate) struct GBIrCompiler;

impl IIrCompiler for GBIrCompiler {
	fn build_ir(&self, b: &mut IrBuilder) {
		match *b.inst().bytes() {
			[0xCB, byte2, ..] => build_ir(lookup_desc_cb(byte2), None, b),
			[byte1, ..]       => build_ir(lookup_desc(byte1).unwrap(), None, b),
			_                 => unreachable!(),
		}
	}

	fn build_ir_term(&self, b: &mut IrBuilder, term: &BBTerm) {
		match *b.inst().bytes() {
			[0xCB, byte2, ..] => build_ir(lookup_desc_cb(byte2), Some(term), b),
			[byte1, ..]       => build_ir(lookup_desc(byte1).unwrap(), Some(term), b),
			_                 => unreachable!(),
		}
	}

	fn arch_regs(&self) -> &'static [IrReg] {
		ARCH_REGS
	}

	fn stack_ptr_reg(&self) -> IrReg {
		REG_SP
	}

	fn reg_name(&self, offset: u8) -> &'static str {
		match offset {
			x if x == REG_A.offset()  => "a",
			x if x == REG_B.offset()  => "b",
			x if x == REG_C.offset()  => "c",
			x if x == REG_D.offset()  => "d",
			x if x == REG_E.offset()  => "e",
			x if x == REG_H.offset()  => "h",
			x if x == REG_L.offset()  => "l",
			x if x == REG_CF.offset() => "cf",
			x if x == REG_HF.offset() => "hf",
			x if x == REG_NF.offset() => "nf",
			x if x == REG_ZF.offset() => "zf",
			x if x == REG_SP.offset() => "sp",
			x if x == REG_W.offset()  => "w",
			x if x == REG_X.offset()  => "x",
			x if x == REG_Y.offset()  => "y",
			x if x == REG_Z.offset()  => "z",
			x if x == REG_BC.offset() => "bc",
			x if x == REG_DE.offset() => "de",
			x if x == REG_HL.offset() => "hl",
			x if x == REG_WZ.offset() => "wz",
			_ => panic!(),
		}
	}
}

// ------------------------------------------------------------------------------------------------
// Common constants
// ------------------------------------------------------------------------------------------------

const fn c8(val: u64) -> IrConst { IrConst::_8(val as u8) }
const C0_8: IrConst = c8(0);
const C1_8: IrConst = c8(1);
const C4_8: IrConst = c8(4);
const C5_8: IrConst = c8(5);
const C6_8: IrConst = c8(6);
const C7_8: IrConst = c8(7);

const C1_16: IrConst = IrConst::_16(1);
const CFF00_16: IrConst = IrConst::_16(0xFF00);

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

static ARCH_REGS: &[IrReg] =
	&[REG_A, REG_B, REG_C, REG_D, REG_E, REG_H, REG_L, REG_CF, REG_HF, REG_NF, REG_ZF];

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

impl IrBuilder<'_> {
	/// Pair the constituent registers of a paired register into its corresponding `REG_XX`.
	/// Named `rr` to match the ISA docs. Returns the temporary register.
	fn rr(&mut self, reg: Reg) -> IrReg {
		match reg {
			Reg::BC => { self.pair(REG_BC, REG_B, REG_C); REG_BC }
			Reg::DE => { self.pair(REG_DE, REG_D, REG_E); REG_DE }
			Reg::HL => { self.pair(REG_HL, REG_H, REG_L); REG_HL }
			_ => panic!("given something other than a paired register"),
		}
	}

	/// Set the Z flag to whether or not `reg == 0`.
	fn z_(&mut self, reg: impl Into<BuildReg>) -> &mut Self {
		self.eq(REG_ZF, reg.into(), C0_8)
	}

	/// Set the N flag to a given value.
	fn nx(&mut self, src: impl Into<BuildSrc>) -> &mut Self {
		self.mov(REG_NF, src)
	}

	/// Set the C flag to the carry out of unsigned `src1 + src2`.
	fn c_(&mut self, src1: impl Into<BuildSrc>, src2: impl Into<BuildSrc>)
	-> &mut Self {
		self.ucarry(REG_CF, src1, src2)
	}

	/// Set the C flag to the carry out of unsigned `src1 + src2 + C`.
	fn c_c(&mut self, src1: impl Into<BuildSrc>, src2: impl Into<BuildSrc>)
	-> &mut Self {
		self.ucarryc(REG_CF, src1, src2, REG_CF)
	}

	/// Set the C flag to the carry out of unsigned `src1 - src2`.
	fn c_b(&mut self, src1: impl Into<BuildSrc>, src2: impl Into<BuildSrc>)
	-> &mut Self {
		self.sborrow(REG_CF, src1, src2)
	}

	/// Set the C flag to the carry out of unsigned `src1 - src2 - C`.
	fn c_bc(&mut self, src1: impl Into<BuildSrc>, src2: impl Into<BuildSrc>)
	-> &mut Self {
		self.sborrowb(REG_CF, src1, src2, REG_CF)
	}

	/// Sets the C flag to bit `bitn` of register `src`.
	fn cbit(&mut self, src: impl Into<BuildReg>, bitn: u8) -> &mut Self {
		self.bit(REG_CF, src.into(), c8(bitn as u64))
	}

	/// Sets the h flag to bit `bitn` of register `src`.
	fn hbit(&mut self, src: IrReg, bitn: u8) -> &mut Self {
		self.bit(REG_HF, src, c8(bitn as u64))
	}

	/// Set the Z flag to 0.
	fn z0(&mut self) -> &mut Self { self.mov(REG_ZF, C0_8) }
	/// Set the N flag to 0.
	fn n0(&mut self) -> &mut Self { self.mov(REG_NF, C0_8) }
	/// Set the H flag to 0.
	fn h0(&mut self) -> &mut Self { self.mov(REG_HF, C0_8) }
	/// Set the C flag to 0.
	fn c0(&mut self) -> &mut Self { self.mov(REG_CF, C0_8) }

	/// Set the N flag to 1.
	fn n1(&mut self) -> &mut Self { self.mov(REG_NF, C1_8) }
	/// Set the H flag to 1.
	fn h1(&mut self) -> &mut Self { self.mov(REG_HF, C1_8) }
	/// Set the C flag to 1.
	fn c1(&mut self) -> &mut Self { self.mov(REG_CF, C1_8) }

	/// Combine all the flag registers into an 8-bit value in `dst`.
	fn combine_flags(&mut self, dst: IrReg) -> &mut Self {
		self
		.mov  (dst, C0_8)
		.bset(dst, dst, C4_8, REG_CF)
		.bset(dst, dst, C5_8, REG_HF)
		.bset(dst, dst, C6_8, REG_NF)
		.bset(dst, dst, C7_8, REG_ZF)
	}

	/// Extracts all the flag values from `src` into the flag registers.
	fn extract_flags(&mut self, src: IrReg) -> &mut Self {
		self
		.cbit(        src, 4)
		.hbit(        src, 5)
		.bit(REG_NF, src, C6_8)
		.bit(REG_ZF, src, C7_8)
	}
}

// ------------------------------------------------------------------------------------------------
// Memory
// ------------------------------------------------------------------------------------------------

impl IrBuilder<'_> {
	/// Push an 8-bit value `src` onto the stack.
	fn push8(&mut self, src: impl Into<BuildSrc>) -> &mut Self {
		// full stack convention - subtract before storing
		self
		.sub(REG_SP, REG_SP, C1_16)
		.store(REG_SP, src)
	}

	/// Pop an 8-bit value off the stack into `dst`.
	fn pop8(&mut self, dst: impl Into<BuildReg>) -> &mut Self {
		// full stack convention - load before adding
		self
		.load (dst,    REG_SP)
		.add(REG_SP, REG_SP, C1_16)
	}

	/// Push a 16-bit value onto the stack as two 8-bit halves, pushing the high half first so that
	/// the resultant value is little-endian in memory.
	fn push16(&mut self, srchi: impl Into<BuildSrc>, srclo: impl Into<BuildSrc>) -> &mut Self {
		self
		.push8(srchi)
		.push8(srclo)
	}

	/// Pop a 16-bit value off the stack as two 8-bit halves into `dstlo` and `dsthi`.
	fn pop16(&mut self, dsthi: impl Into<IrReg>, dstlo: impl Into<BuildReg>) -> &mut Self {
		self
		.pop8(dstlo)
		.pop8(dsthi)
	}

	/// Load indirect, using one of the paired registers as the source address.
	fn load_ind(&mut self, dst: impl Into<BuildReg>, src: (Reg, i8)) -> &mut Self {
		let src = (self.rr(src.0), src.1);
		self.load(dst, src)
	}

	/// Store indirect, using one of the paired registers as the destination address.
	fn store_ind(&mut self, dst: (Reg, i8), src: impl Into<BuildSrc>)
	-> &mut Self {
		let dst = (self.rr(dst.0), dst.1);
		self.store(dst, src)
	}
}

// ------------------------------------------------------------------------------------------------
// Control flow
// ------------------------------------------------------------------------------------------------

impl IrBuilder<'_> {
	/// Push the return address to the stack.
	fn push_return_addr(&mut self, ret_addr: VA) -> &mut Self {
		// push hi then lo
		self
		.push8(c8(ret_addr.0 >> 8  ))
		.push8(c8(ret_addr.0 & 0xFF))
	}

	/// Push `ret_addr` to the stack, and then call `dst`.
	fn call_(&mut self, ret_addr: VA, dst: impl Into<BuildEA>, cont: EA) -> &mut Self {
		self
		.push_return_addr(ret_addr)
		.call            (dst, cont)
	}

	/// Pop the return address and `ret` to it.
	fn return_(&mut self) -> &mut Self {
		self
		.pop16(REG_W,  REG_Z)
		.pair(REG_WZ, REG_W, REG_Z)
		.ret  (REG_WZ)
	}

	/// Evaluate the condition code `cc` and return a register which contains its truth value.
	fn cc(&mut self, cc: Cc) -> IrReg {
		match cc {
			Cc::C  => REG_CF,
			Cc::Z  => REG_ZF,
			Cc::NC => { self.bnot(REG_Z, REG_CF); REG_Z }
			Cc::NZ => { self.bnot(REG_Z, REG_ZF); REG_Z }
		}
	}

	/// Evaluate the logical inversion of the condition code `cc` and return a register which
	/// contains the inverted truth value.
	fn not_cc(&mut self, cc: Cc) -> IrReg {
		self.cc(cc.not())
	}

	/// Do a conditional branch using the condition code `cc`.
	fn cc_branch(&mut self, cc: Cc, dst: impl Into<BuildEA>, cont: EA) -> &mut Self {
		let cond = self.cc(cc);
		self.cbranch(cond, dst, cont)
	}
}

// ------------------------------------------------------------------------------------------------
// Computation
// ------------------------------------------------------------------------------------------------

impl IrBuilder<'_> {
	/// Begin some read-modify-write operation using `[hl]` as the source/dest. The returned `IrReg`
	/// holds the value loaded from `[hl]`. You must put the result back into the same register.
	fn hl_rmw_start(&mut self, hln: i8) -> IrReg {
		self.load_ind(REG_Z, (Reg::HL, hln));
		REG_Z
	}

	/// End some read-modify-write operation using `[hl]` as the source/dest.
	fn hl_rmw_end(&mut self, hln: i8) -> &mut Self {
		self.store((REG_HL, hln), REG_Z)
	}

	/// Shift the given `reg` left. The carry flag is set to the MSB of `reg`, and the zero flag is
	/// set if the result is 0. N and H flags are set to 0.
	fn sla(&mut self, reg: impl Into<BuildReg>) -> &mut Self {
		let reg = reg.into();
		self
		.cbit(     reg, 7)
		.shl(reg, reg, C1_8)
		.z_  (reg)
		.n0  ()
		.h0  ()
	}

	/// Shift the given `reg` right arithmetic. The carry flag is set to the MSB of `reg`, and the
	/// zero flag is set if the result is 0. N and H flags are set to 0.
	fn sra(&mut self, reg: impl Into<BuildReg>) -> &mut Self {
		let reg = reg.into();
		self
		.cbit (     reg, 0)
		.sshr(reg, reg, C1_8)
		.z_   (reg)
		.n0   ()
		.h0   ()
	}

	/// Shift the given `reg` right logical. The carry flag is set to the MSB of `reg`, and the
	/// zero flag is set if the result is 0. N and H flags are set to 0.
	fn srl(&mut self, reg: impl Into<BuildReg>) -> &mut Self {
		let reg = reg.into();
		self
		.cbit (     reg, 0)
		.ushr(reg, reg, C1_8)
		.z_   (reg)
		.n0   ()
		.h0   ()
	}

	/// Rotate the given `reg` left. The carry flag is set to the MSB of `reg`, but otherwise does
	/// not participate. If `set_zero_flag`, the zero flag will be set if the result is 0;
	/// otherwise the zero flag will be set to 0 always. N and H flags are set to 0.
	fn rol_(&mut self, reg: impl Into<BuildReg>, set_zero_flag: bool) -> &mut Self {
		let reg = reg.into();
		self
		.cbit(     reg, 7)
		.rol(reg, reg, C1_8)
		.n0  ()
		.h0  ();

		if set_zero_flag {
			self.z_(reg)
		} else {
			self.z0()
		}
	}

	/// Rotate the given `reg` right. The carry flag is set to the MSB of `reg`, but otherwise does
	/// not participate. If `set_zero_flag`, the zero flag will be set if the result is 0;
	/// otherwise the zero flag will be set to 0 always. N and H flags are set to 0.
	fn ror_(&mut self, reg: impl Into<BuildReg>, set_zero_flag: bool) -> &mut Self {
		let reg = reg.into();
		self
		.cbit(     reg, 7)
		.ror(reg, reg, C1_8)
		.n0  ()
		.h0  ();

		if set_zero_flag {
			self.z_(reg)
		} else {
			self.z0()
		}
	}

	/// Rotate the given `reg` left through the carry flag. If `set_zero_flag`, the zero flag will
	/// be set if the result is 0; otherwise the zero flag will be set to 0 always. N and H flags
	/// are set to 0.
	fn rolc(&mut self, reg: impl Into<BuildReg>, set_zero_flag: bool) -> &mut Self {
		let reg = reg.into();
		self
		.mov  (REG_Z, REG_CF)
		.cbit (       reg, 7)
		.rol (reg,   reg, C1_8)
		.bset(reg,   reg, C0_8, REG_Z)
		.n0   ()
		.h0   ();

		if set_zero_flag {
			self.z_(reg)
		} else {
			self.z0()
		}
	}

	/// Rotate the given `reg` right through the carry flag. If `set_zero_flag`, the zero flag will
	/// be set if the result is 0; otherwise the zero flag will be set to 0 always. N and H flags
	/// are set to 0.
	fn rorc(&mut self, reg: impl Into<BuildReg>, set_zero_flag: bool) -> &mut Self {
		let reg = reg.into();
		self
		.mov  (REG_Z, REG_CF)
		.cbit (       reg, 0)
		.ror (reg,   reg, C1_8)
		.bset(reg,   reg, C7_8, REG_Z)
		.n0   ()
		.h0   ();

		if set_zero_flag {
			self.z_(reg)
		} else {
			self.z0()
		}
	}

	/// Swap the nybbles of the given register. The zero flag is set according to whether the result
	/// is zero, and the N, H, and C flags are all set to 0.
	fn swap(&mut self, reg: impl Into<BuildReg>) -> &mut Self {
		let reg = reg.into();
		self
		.rol(reg, reg, C4_8)
		.z_  (reg)
		.n0  ()
		.h0  ()
		.c0  ()
	}

	/// Increment or decrement HL. Assumes HL has already been paired. Increments `REG_HL`, then
	/// extracts the components into `REG_H` and `REG_L`.
	fn inc_dec_hl(&mut self, plus: bool) -> &mut Self {
		if plus {
			self.add(REG_HL, REG_HL, C1_16);
		} else {
			self.sub(REG_HL, REG_HL, C1_16);
		}

		self
		.lo(REG_L, REG_HL)
		.hi(REG_H, REG_HL)
	}

	/// Perform an increment or decrement on `reg`. `delta == 1` increments, `delta == -1`
	/// decrements. If `change_flags`, the zero flag is set according to if `reg == 0` after the
	/// crement; N is set to 0 if `delta == 1` and 1 otherwise; and H is set according to the
	/// half-carry rules.
	fn inc_dec(&mut self, reg: impl Into<IrReg>, delta: isize, change_flags: bool)
	-> &mut Self {
		let reg = reg.into();
		let one = IrConst::with_size(reg.size(), 1);

		let nf = match delta {
			1  => {
				if change_flags {
					self
					.carries(REG_W, reg,   one)
					.hbit    (       REG_W, 3);
				}
				self.add(reg, reg, one);
				C0_8 // nf
			}
			-1 => {
				if change_flags {
					self
					.borrows(REG_W, REG_A, one)
					.hbit    (       REG_W, 3);
				}
				self.sub(reg, reg, one);
				C1_8 // nf
			}
			_  => panic!("bad delta"),
		};

		if change_flags {
			self
			.z_(reg)
			.nx( nf);
		}

		self
	}

	/// Add paired register `reg` onto `HL` and update flags. Pairs both `HL` and `reg`, and
	/// extracts results into `REG_H` and `REG_L` afterwards.
	fn add_hl_rr(&mut self, reg: Reg) -> &mut Self {
		let src = self.rr(reg);
		self.rr(Reg::HL);
		self
		.carries(REG_WZ, REG_HL, src)
		.cbit    (        REG_WZ, 15)
		.hbit    (        REG_WZ, 11)
		.add   (REG_HL, REG_HL, src)
		.n0      ()
		.hi     (REG_H,  REG_HL)
		.lo     (REG_L,  REG_HL)
	}

	/// Do `dst = REG_SP + val` (written `sp + e` in ISA docs), and update flags.
	fn add_sp_e(&mut self, dst: IrReg, val: (i64, i8)) -> &mut Self {
		// it adds the sign-extended operand to SP, as if it were unsigned.
		let val = (IrConst::_16((val.0 as u64) as u16), val.1);
		self
		.carries(REG_WZ, REG_SP, val)
		.cbit    (        REG_WZ, 7)
		.hbit    (        REG_WZ, 3)
		.add   (dst,    REG_SP, val)
		.z0      ()
		.n0      ()
	}

	/// Do `REG_A = REG_A + src` and update flags.
	fn add_a(&mut self, src: impl Into<BuildSrc>) -> &mut Self {
		let src = src.into();
		self
		.c_      (       REG_A, src)
		.carries(REG_W, REG_A, src)
		.hbit    (       REG_W, 3)
		.add   (REG_A, REG_A, src)
		.n0      ()
		.z_      (REG_A)
	}

	/// Do `REG_A = REG_A + src + REG_CF` and update flags.
	fn adc_a(&mut self, src: impl Into<BuildSrc>) -> &mut Self {
		let src = src.into();
		self
		.c_c      (       REG_A, src)
		.carriesc(REG_W, REG_A, src, REG_CF)
		.hbit     (       REG_W, 3)
		.addc   (REG_A, REG_A, src, REG_CF)
		.n0       ()
		.z_       (REG_A)
	}

	/// Do `dst = REG_A - src` and update flags.
	fn sub_(&mut self, dst: IrReg, src: impl Into<BuildSrc>) -> &mut Self {
		let src = src.into();
		self
		.c_b     (       REG_A, src)
		.borrows(REG_W, REG_A, src)
		.hbit    (       REG_W, 3)
		.sub   (dst,   REG_A, src)
		.n1      ()
		.z_      (dst)
	}

	/// Do `REG_A = REG_A - src - REG_CF` and update flags.
	fn sbc_a(&mut self, src: impl Into<BuildSrc>) -> &mut Self {
		let src = src.into();
		self
		.c_bc     (       REG_A, src)
		.borrowsb(REG_W, REG_A, src, REG_CF)
		.hbit     (       REG_W, 3)
		.subb   (REG_A, REG_A, src, REG_CF)
		.n1       ()
		.z_       (REG_A)
	}

	/// Do `REG_A = REG_A & src` and update flags.
	fn and_a(&mut self, src: impl Into<BuildSrc>) -> &mut Self {
		self
		.iand(REG_A, REG_A, src)
		.n0  ()
		.h1  ()
		.c0  ()
		.z_  (REG_A)
	}

	/// Do `REG_A = REG_A | src` and update flags.
	fn or_a(&mut self, src: impl Into<BuildSrc>) -> &mut Self {
		self.ior(REG_A, REG_A, src);
		self.n0 ()
		.h0     ()
		.c0     ()
		.z_     (REG_A)
	}

	/// Do `REG_A = REG_A ^ src` and update flags.
	fn xor_a(&mut self, src: impl Into<BuildSrc>) -> &mut Self {
		self.ixor(REG_A, REG_A, src);
		self.n0  ()
		.h0      ()
		.c0      ()
		.z_      (REG_A)
	}
}

// ------------------------------------------------------------------------------------------------
// Computation
// ------------------------------------------------------------------------------------------------

fn build_ir<'i>(desc: &InstDesc, term: Option<&BBTerm>, b: &mut IrBuilder<'i>) {
	use { MetaOp::*, SynOp::*, Reg::* };

	match (desc.meta_op(), desc.syn_ops()) {
		(UNK,  &[]) => { panic!("what the hell is an unknown instruction doing in a BB?"); }

		// for all these, have to emit *something* to avoid empty IR BBs.
		(NOP,  &[]) => { b.nop(); } // no flag changes
		(DI,   &[]) => { b.nop(); } // no flag changes
		(EI,   &[]) => { b.nop(); } // no flag changes
		(STOP, &[]) => { b.nop(); } // no flag changes

		(HALT, &[]) => { b.halt(); } // no flag changes

		// ------------------------------------------------------------------------------------
		// Computation

		// add hl, rr
		(ADD, &[Srg(HL), Srg(reg)]) => { // {Z-, N0, H*, C*}
			b.add_hl_rr(reg);
		}
		// add sp, e
		(ADD, [Srg(SP), Op]) => { // {Z0, N0, H*, C*}
			let Operand::SImm(val) = b.inst().ops()[0] else { panic!() };
			b.add_sp_e(REG_SP, (val, 0));
		}

		// add r
		(ADD, &[Srg(A), Srg(reg)]) => { // {Z*, N0, H*, C*}
			b.add_a(IrReg::from(reg));
		}
		// add [hl]
		(ADD, [Srg(A), IndReg(HL)]) => { // {Z*, N0, H*, C*}
			b
			.load_ind(REG_Z, (HL, 0))
			.add_a   (REG_Z);
		}
		// add n
		(ADD, [Srg(A), Op]) => { // {Z*, N0, H*, C*}
			let Operand::UImm(val) = b.inst().ops()[0] else { panic!() };
			b.add_a(c8(val));
		}

		// adc r
		(ADC, &[Srg(A), Srg(reg)]) => { // {Z*, N0, H*, C*}
			b.adc_a(IrReg::from(reg));
		}
		// adc [hl]
		(ADC, [Srg(A), IndReg(HL)]) => { // {Z*, N0, H*, C*}
			b
			.load_ind(REG_Z, (HL, 0))
			.adc_a   (REG_Z);
		}
		// adc n
		(ADC, [Srg(A), Op]) => { // {Z*, N0, H*, C*}
			let Operand::UImm(val) = b.inst().ops()[0] else { panic!() };
			b.adc_a(c8(val));
		}

		// sub r
		(SUB, &[Srg(A), Srg(reg)]) => { // {Z*, N1, H*, C*}
			b.sub_(REG_A, IrReg::from(reg));
		}
		// sub [hl]
		(SUB, [Srg(A), IndReg(HL)]) => { // {Z*, N1, H*, C*}
			b
			.load_ind(REG_Z, (HL, 0))
			.sub_    (REG_A, REG_Z);
		}
		// sub n
		(SUB, [Srg(A), Op]) => { // {Z*, N1, H*, C*}
			let Operand::UImm(val) = b.inst().ops()[0] else { panic!() };
			b.sub_(REG_A, c8(val));
		}

		// cp r
		(CP, &[Srg(A), Srg(reg)]) => { // {Z*, N1, H*, C*}
			b.sub_(REG_W, IrReg::from(reg));
		}
		// cp [hl]
		(CP, [Srg(A), IndReg(HL)]) => { // {Z*, N1, H*, C*}
			b
			.load_ind(REG_Z, (HL, 0))
			.sub_    (REG_W, REG_Z);
		}
		// cp n
		(CP, [Srg(A), Op]) => { // {Z*, N1, H*, C*}
			let Operand::UImm(val) = b.inst().ops()[0] else { panic!() };
			b.sub_(REG_W, c8(val));
		}

		// sbc r
		(SBC, &[Srg(A), Srg(reg)]) => { // {Z*, N1, H*, C*}
			b.sbc_a(IrReg::from(reg));
		}
		// sbc [hl]
		(SBC, [Srg(A), IndReg(HL)]) => { // {Z*, N1, H*, C*}
			b
			.load_ind(REG_Z, (HL, 0))
			.sbc_a   (REG_Z);
		}
		// sbc n
		(SBC, [Srg(A), Op]) => { // {Z*, N1, H*, C*}
			let Operand::UImm(val) = b.inst().ops()[0] else { panic!() };
			b.sbc_a(c8(val));
		}

		// and r
		(AND, &[Srg(A), Srg(reg)]) => { // {Z*, N0, H1, C0}
			b.and_a(IrReg::from(reg));
		}
		// and [hl]
		(AND, [Srg(A), IndReg(HL)]) => { // {Z*, N0, H1, C0}
			b
			.load_ind(REG_Z, (HL, 0))
			.and_a   (REG_Z);
		}
		// and n
		(AND, [Srg(A), Op]) => { // {Z*, N0, H1, C0}
			let Operand::UImm(val) = b.inst().ops()[0] else { panic!() };
			b.and_a(c8(val));
		}

		// or r
		(OR, &[Srg(A), Srg(reg)]) => { // {Z*, N0, H0, C0}
			b.or_a(IrReg::from(reg));
		}
		// or [hl]
		(OR, [Srg(A), IndReg(HL)]) => { // {Z*, N0, H0, C0}
			b
			.load_ind(REG_Z, (HL, 0))
			.or_a    (REG_Z);
		}
		// or n
		(OR, [Srg(A), Op]) => { // {Z*, N0, H0, C0}
			let Operand::UImm(val) = b.inst().ops()[0] else { panic!() };
			b.or_a(c8(val));
		}

		// xor r
		(XOR, &[Srg(A), Srg(reg)]) => { // {Z*, N0, H0, C0}
			b.xor_a(IrReg::from(reg));
		}
		// xor [hl]
		(XOR, [Srg(A), IndReg(HL)]) => { // {Z*, N0, H0, C0}
			b
			.load_ind(REG_Z, (HL, 0))
			.xor_a   (REG_Z);
		}
		// xor n
		(XOR, [Srg(A), Op]) => { // {Z*, N0, H0, C0}
			let Operand::UImm(val) = b.inst().ops()[0] else { panic!() };
			b.xor_a(c8(val));
		}

		// inc bc, inc de, inc hl
		(INC, &[Srg(reg @ (BC | DE | HL))]) => { // no flag changes
			let tmp_reg = b.rr(reg);
			b
			.inc_dec(tmp_reg, 1, false)
			.hi    (reg.hi(), tmp_reg)
			.lo    (reg.lo(), tmp_reg);
		}

		// inc sp
		(INC, [Srg(SP)]) => { // no flag changes
			b.inc_dec(REG_SP, 1, false);
		}

		// inc r
		(INC, &[Srg(reg)]) => { // {Z*, N0, H*, C-}
			let reg = IrReg::from(reg);
			b.inc_dec(reg, 1, true);
		}

		// inc [hl]
		(INC, [IndReg(HL)]) => {  // {Z*, N0, H*, C-}
			let reg = b.hl_rmw_start(0); b.inc_dec(reg, 1, true); b.hl_rmw_end(0);
		}

		// dec bc, inc de, inc hl
		(DEC, &[Srg(reg @ (BC | DE | HL))]) => { // no flag changes
			let tmp_reg = b.rr(reg);
			b
			.inc_dec(tmp_reg, -1, false)
			.hi    (reg.hi(), tmp_reg)
			.lo    (reg.lo(), tmp_reg);
		}

		// dec sp
		(DEC, [Srg(SP)]) => { // no flag changes
			b.inc_dec(REG_SP, -1, false);
		}

		// dec r
		(DEC, &[Srg(reg)]) => { // {Z*, N0, H*, C-}
			let reg = IrReg::from(reg);
			b.inc_dec(reg, -1, true);
		}

		// dec [hl]
		(DEC, [IndReg(HL)]) => {  // {Z*, N0, H*, C-}
			let reg = b.hl_rmw_start(0); b.inc_dec(reg, -1, true); b.hl_rmw_end(0);
		}

		// cpl a
		(CPL, [Srg(A)]) => { // {Z-, N1, H1, C-}
			b
			.inot(REG_A, REG_A)
			.n1  ()
			.h1  ();
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
				REG_NF as NF, REG_HF as HF, REG_CF as CF, REG_ZF as ZF,
				C0_8 as C0, C4_8 as C4, C6_8 as C6 };
			const C9:    IrConst = c8(9);
			const C0XF:  IrConst = c8(0x99);
			const C0X99: IrConst = c8(0x99);
			const CNEG6: IrConst = c8(-6i8 as u8 as u64);

			// Z = subtraction adjustment { 0x00, -0x06, -0x60, -0x66 }
			b
			.band (Z, NF, HF)   // Z.0 = NF & HF
			.band (X, NF, CF)   // X   = NF & CF
			.bset(Z, Z, C4, X) // Z.4 = NF & CF
			.mul (Z, Z, CNEG6) // Z   = Z * -6

			// W = addition adjustment { 0x00, 0x06, 0x60, 0x66 }
			.inot (X, NF)       // X   = !NF
			.iand (Y, A, C0XF)  // Y   = A & 0xF
			.ugt (Y, Y, C9)    // Y   = A & 0xF > 9
			.bor  (Y, Y, HF)    // Y   = HF | (A & 0xF > 9)
			.band (W, X, Y)     // W.0 = !NF & (HF | (A & 0xF > 9))
			.ugt (Y, A, C0X99) // Y   = A > 0x99
			.bor  (Y, Y, CF)    // Y   = CF | (A > 0x99)
			.band (Y, Y, X)     // Y   = !NF & (CF | (A > 0x99))
			.bset(W, W, C4, Y) // W.4 = !NF & (CF | (A > 0x99))
			.mul (W, W, C6)    // W   = W * 6

			// because the above two values were calculated based on NF and !NF, either they are
			// both 0 or exactly one is 0. so adding them together has the effect of choosing
			// between them.

			// Z = adjustment
			.add(Z, Z, W)     // Z = NF ? Z : W (effectively)

			// now we can do the actual addition and set the flags
			.c_   (    A, Z)
			.add(A,  A, Z)
			.eq  (ZF, A, C0)
			.mov  (HF, C0);
		}

		// ------------------------------------------------------------------------------------
		// Bitwise

		// {Z0, N0, H0, C*}
		(RLA,  []) => { b.rolc(REG_A, false); }
		(RLCA, []) => { b.rol_(REG_A, false); }
		(RRA,  []) => { b.rorc(REG_A, false); }
		(RRCA, []) => { b.ror_(REG_A, false); }

		// {Z*, N0, H0, C*}
		(SLA, &[Srg(HL)])  => { let reg = b.hl_rmw_start(0); b.sla( reg    ); b.hl_rmw_end(0); }
		(SLA, &[Srg(reg)]) => {                              b.sla((reg, 0)); }
		(SRA, &[Srg(HL)])  => { let reg = b.hl_rmw_start(0); b.sra( reg    ); b.hl_rmw_end(0); }
		(SRA, &[Srg(reg)]) => {                              b.sra((reg, 0)); }
		(SRL, &[Srg(HL)])  => { let reg = b.hl_rmw_start(0); b.srl( reg    ); b.hl_rmw_end(0); }
		(SRL, &[Srg(reg)]) => {                              b.srl((reg, 0)); }

		(RL,  &[Srg(HL)])  => { let reg = b.hl_rmw_start(0); b.rolc( reg,     true); b.hl_rmw_end(0); }
		(RL,  &[Srg(reg)]) => {                              b.rolc((reg, 0), true);                  }
		(RLC, &[Srg(HL)])  => { let reg = b.hl_rmw_start(0); b.rol_( reg,     true); b.hl_rmw_end(0); }
		(RLC, &[Srg(reg)]) => {                              b.rol_((reg, 0), true);                  }
		(RR,  &[Srg(HL)])  => { let reg = b.hl_rmw_start(0); b.rorc( reg,     true); b.hl_rmw_end(0); }
		(RR,  &[Srg(reg)]) => {                              b.rorc((reg, 0), true);                  }
		(RRC, &[Srg(HL)])  => { let reg = b.hl_rmw_start(0); b.ror_( reg,     true); b.hl_rmw_end(0); }
		(RRC, &[Srg(reg)]) => {                              b.ror_((reg, 0), true);                  }

		// {Z*, N0, H0, C0}
		(SWAP, &[Srg(reg)])   => {                              b.swap(reg);                  }
		(SWAP, &[IndReg(HL)]) => { let reg = b.hl_rmw_start(0); b.swap(reg); b.hl_rmw_end(0); }

		// {Z*, N0, H1, C-}
		(BIT, &[Op, Srg(reg)]) => {
			let Operand::UImm(bit) = b.inst().ops()[0] else { panic!() };
			b.bit(REG_ZF, IrReg::from(reg), c8(bit));
		}
		(BIT, [Op, IndReg(HL)]) => {
			let Operand::UImm(bit) = b.inst().ops()[0] else { panic!() };
			// operand 0 is the bit number, operand 1 is [hl]
			b
			.load_ind(REG_Z,  (HL, 1))
			.bit    (REG_ZF, REG_Z, c8(bit));
		}

		// no flag changes
		(RES, &[Op, Srg(reg)]) => {
			let Operand::UImm(bit) = b.inst().ops()[0] else { panic!() };
			let reg = IrReg::from(reg);
			b.bset(reg, reg, c8(bit), C0_8);
		}
		(RES, [Op, IndReg(HL)]) => {
			let Operand::UImm(bit) = b.inst().ops()[0] else { panic!() };
			// operand 0 is the bit number, operand 1 is [hl]
			let reg = b.hl_rmw_start(1); b.bset(reg, reg, c8(bit), C0_8); b.hl_rmw_end(1);
		}

		// no flag changes
		(SET, &[Op, Srg(reg)]) => {
			let Operand::UImm(bit) = b.inst().ops()[0] else { panic!() };
			let reg = IrReg::from(reg);
			b.bset(reg, reg, c8(bit), C1_8);
		}
		(SET, [Op, IndReg(HL)]) => {
			let Operand::UImm(bit) = b.inst().ops()[0] else { panic!() };
			// operand 0 is the bit number, operand 1 is [hl]
			let reg = b.hl_rmw_start(1); b.bset(reg, reg, c8(bit), C1_8); b.hl_rmw_end(1);
		}

		// ------------------------------------------------------------------------------------
		// Flag manipulation

		(CCF, []) => { // {Z-, N0, H0, C*}
			b
			.n0  ()
			.h0  ()
			.bnot(REG_CF, REG_CF);
		}
		(SCF, []) => { // {Z-, N0, H0, C1}
			b
			.n0()
			.h0()
			.c1();
		}

		// ------------------------------------------------------------------------------------
		// Control flow

		// no flag changes
		(JP, [Op]) |
		(JR, [Op]) => {
			let term = term.unwrap();
			let dst  = term.one_explicit_successor().unwrap();
			b.branch((dst, 0));
		}

		(JP, &[Cc(cond), Op]) |
		(JR, &[Cc(cond), Op]) => {
			let term = term.unwrap();
			let dst  = term.one_explicit_successor().unwrap();
			let cont = term.continuation_successor().unwrap();
			b.cc_branch(cond, (dst, 0), cont);
		}

		(JP, [Srg(HL)]) => {
			b.rr     (HL);
			b.ibranch((REG_HL, 0));
		}

		(CALL, [Op]) |
		(RST,  [Op]) => {
			let term = term.unwrap();
			let dst  = term.one_explicit_successor().unwrap();
			let cont = term.continuation_successor().unwrap();
			b.call_(b.inst().next_va(), (dst, 0), cont);
		}
		(CALL, &[Cc(cond), Op]) => {
			let term     = term.unwrap();
			let dst      = term.one_explicit_successor().unwrap();
			let cont     = term.continuation_successor().unwrap();
			let ra       = b.inst().next_va();
			let not_cond = b.not_cc(cond);
			b
			.cbranch_and_split(not_cond, cont)
			.push_return_addr (ra)
			.call             ((dst, 0), cont);
		}

		(RETI, []) |
		(RET,  []) => { b.return_(); }
		(RET,  &[Cc(cond)]) => {
			let cont     = term.unwrap().continuation_successor().unwrap();
			let not_cond = b.not_cc(cond);
			b
			.cbranch_and_split(not_cond, cont)
			.return_();
		}

		// ------------------------------------------------------------------------------------
		// Data transfer

		// ld sp, hl (0xF9)
		(LD, &[Srg(SP), Srg(HL)]) => { // no flag changes
			b.rr (HL);
			b.mov(REG_SP, REG_HL);
		}

		// ld r, r (many, many opcodes in [0x40 .. 0x7F] range)
		(LD, &[Srg(dst), Srg(src)]) => { // no flag changes
			b.mov(dst, IrReg::from(src));
		}

		// ld hl, sp+e (0xF8)
		(LD, &[Srg(HL), SpPlusOp]) => { // {Z0, N0, H]*, C*}
			let Operand::SImm(val) = b.inst().ops()[0] else { panic!() };
			b
			.add_sp_e(REG_HL, (val, 0))
			.lo     (REG_L,  REG_HL)
			.hi     (REG_H,  REG_HL);
		}

		// ld rr, nn (0x01, 0x11, 0x21)
		(LD, &[Srg(dst @ (BC | DE | HL)), Op]) => { // no flag changes
			let Operand::UImm(val) = b.inst().ops()[0] else { panic!() };
			let val = IrConst::_16(val as u16);
			// seems silly to do this, but it's to preserve the original source operand in the IR,
			// for later tracing back and marking this operand as a reference
			b
			.mov(REG_WZ,   (val, 0))
			.hi(dst.hi(), REG_WZ)
			.lo(dst.lo(), REG_WZ);
		}

		// ld sp, nn (0x31) (same as above but SP is represented differently)
		(LD, &[Srg(SP), Op]) => { // no flag changes
			let Operand::UImm(val) = b.inst().ops()[0] else { panic!() };
			b.mov(REG_SP, (IrConst::_16(val as u16), 0));
		}

		// ld r, n (various)
		(LD, &[Srg(dst), Op]) => { // no flag changes
			let Operand::UImm(val) = b.inst().ops()[0] else { panic!() };
			b.mov(dst, (c8(val), 0));
		}

		// ld r, [rr] (various)
		(LD, &[Srg(dst), IndReg(src @ (BC | DE | HL))]) => { // no flag changes
			b.load_ind(dst, (src, 0));
		}

		// ld [rr], r (various)
		(LD, &[IndReg(dst @ (BC | DE | HL)), Srg(src)]) => { // no flag changes
			b.store_ind((dst, 0), IrReg::from(src));
		}

		// ld a, [nn] (0xFA)
		(LD, &[Srg(A), IndOp]) => { // no flag changes
			let Operand::Mem(src, _) = b.inst().ops()[0] else { panic!() };
			b.load(REG_A, (IrConst::_16(src.0 as u16), 0));
		}

		// ld [nn], a (0xEA)
		(LD, &[IndOp, Srg(A)]) => { // no flag changes
			let Operand::Mem(dst, _) = b.inst().ops()[0] else { panic!() };
			b.store((IrConst::_16(dst.0 as u16), 0), REG_A);
		}

		// ld [hl+], a (0x22)
		// ld [hl-], a (0x32)
		(LD, &[pm @ (IndHlPlus | IndHlMinus), Srg(A)]) => { // no flag changes
			b
			.store_ind ((HL, -1), REG_A)
			.inc_dec_hl(pm == IndHlPlus);
		}

		// ld a, [hl+] (0x2A)
		// ld a, [hl-] (0x3A)
		(LD, &[Srg(A), pm @ (IndHlPlus | IndHlMinus)]) => { // no flag changes
			b
			.load_ind  (A, (HL, -1))
			.inc_dec_hl(pm == IndHlPlus);
		}

		// ld [hl], n (0x36)
		(LD, &[IndReg(HL), Op2]) => { // no flag changes
			let Operand::UImm(src) = b.inst().ops()[1] else { panic!() };
			b.store_ind((HL, 0), (c8(src), 1));
		}

		// ld [nn], sp (0x08)
		(LD, &[IndOp, Srg(SP)]) => { // no flag changes
			let Operand::Mem(dst, _) = b.inst().ops()[0] else { panic!() };
			let dst0 = IrConst::_16(dst.0 as u16);
			let dst1 = IrConst::_16((dst.0 + 1) as u16);

			// split it into two 8-bit stores, little-endian
			b
			.lo  (REG_Z,     REG_SP)
			.store((dst0, 0), REG_Z)
			.hi  (REG_W,     REG_SP)
			// since "dst+1" isn't what they wrote in the operand, we don't associate
			// the IR operand with it; only on the first store.
			.store(dst1,      REG_W);
		}

		// ld a, [0xFF00 + n] (0xF0)
		(LDH, [Srg(A), IndOp]) => { // no flag changes
			let Operand::Mem(src, _) = b.inst().ops()[0] else { panic!() };
			b.load(REG_A, (IrConst::_16(src.0 as u16), 0));
		}
		// ld a, [0xFF00 + c] (0xF2)
		(LDH, [Srg(A), IndReg(C)]) => { // no flag changes
			b
			.zxt (REG_WZ, REG_C)
			.add (REG_WZ, REG_WZ, CFF00_16)
			.load(REG_A,  (REG_WZ, 0));
		}
		// ld [0xFF00 + n], a (0xE0)
		(LDH, [IndOp, Srg(A)]) => { // no flag changes
			let Operand::Mem(dst, _) = b.inst().ops()[0] else { panic!() };
			b.store((IrConst::_16(dst.0 as u16), 0), REG_A);
		}
		// ld [0xFF00 + c], a (0xE2)
		(LDH, [IndReg(C), Srg(A)]) => { // no flag changes
			b
			.zxt  (REG_WZ,      REG_C)
			.add  (REG_WZ,      REG_WZ, CFF00_16)
			.store((REG_WZ, 0), REG_A);
		}

		// push bc (0xC5)
		// push de (0xD5)
		// push hl (0xE5)
		(PUSH, &[Srg(reg @ (BC | DE | HL))]) => { // no flag changes
			b.push16(IrReg::from(reg.hi()), IrReg::from(reg.lo()));
		}

		// push af (0xF5)
		(PUSH, [Srg(AF)]) => { // no flag changes
			b
			.combine_flags(REG_Z)
			.push16       (REG_A, REG_Z);
		}

		// pop bc (0xC1)
		// pop de (0xD1)
		// pop hl (0xE1)
		(POP, &[Srg(reg @ (BC | DE | HL))]) => { // no flag changes
			b.pop16(IrReg::from(reg.hi()), IrReg::from(reg.lo()));
		}

		// pop af (0xF1)
		(POP, [Srg(AF)]) => { // {Z*, N*, H*, C*}
			b
			.pop16        (REG_A, REG_Z)
			.extract_flags(REG_Z);
		}

		_ => panic!("IR unimplemented: {:?}", desc),
	}
}
