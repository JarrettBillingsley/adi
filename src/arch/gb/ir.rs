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
	fn build_ir(&self, i: &Instruction, target: Option<EA>, next: Option<EA>, b: &mut IrBuilder) {
		b.set_ea(i.ea());
		match i.bytes() {
			&[0xCB, byte2, ..] => build_ir(&lookup_desc_cb(byte2), i, target, next, b),
			&[byte1, ..]       => build_ir(&lookup_desc(byte1).unwrap(), i, target, next, b),
			_                  => unreachable!(),
		}
	}

	fn arg_regs     (&self) -> &'static [IrReg] { ARG_REGS }
	fn return_regs  (&self) -> &'static [IrReg] { RETURN_REGS }
	fn stack_ptr_reg(&self) -> IrReg            { REG_SP }
}

// ------------------------------------------------------------------------------------------------
// Common constants
// ------------------------------------------------------------------------------------------------

const C0_8: IrConst = IrConst::_8(0);
const C1_8: IrConst = IrConst::_8(1);
const C4_8: IrConst = IrConst::_8(4);
const C5_8: IrConst = IrConst::_8(5);
const C6_8: IrConst = IrConst::_8(6);
const C7_8: IrConst = IrConst::_8(7);

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
	fn rr(&mut self, reg: Reg) -> IrReg {
		match reg {
			Reg::BC => { self.ipair(REG_BC, REG_B, REG_C,  -1, -1, -1); REG_BC }
			Reg::DE => { self.ipair(REG_DE, REG_D, REG_E,  -1, -1, -1); REG_DE }
			Reg::HL => { self.ipair(REG_HL, REG_H, REG_L,  -1, -1, -1); REG_HL }
			_ => panic!("given something other than a paired register"),
		}
	}

	/// Set the Z flag to whether or not `reg == 0`.
	fn z_(&mut self, reg: impl Into<IrReg>, regn: i8) -> &mut Self {
		let reg = reg.into();
		self.ieq(REG_ZF, reg, C0_8,  -1, regn, -1)
	}

	/// Set the N flag to a given value.
	fn nx(&mut self, src: impl Into<IrSrc>, srcn: i8) -> &mut Self {
		let src = src.into();
		self.mov(REG_NF, src,  -1, srcn)
	}

	/// Set the C flag to the carry out of unsigned `src1 + src2`.
	fn c_(&mut self, src1: impl Into<IrSrc>, src2: impl Into<IrSrc>, src1n: i8, src2n: i8)
	-> &mut Self {
		self.iucarry(REG_CF, src1.into(), src2.into(),  -1, src1n, src2n)
	}

	/// Set the C flag to the carry out of unsigned `src1 + src2 + C`.
	fn c_c(&mut self, src1: impl Into<IrSrc>, src2: impl Into<IrSrc>, src1n: i8, src2n: i8)
	-> &mut Self {
		self.iucarryc(REG_CF, src1.into(), src2.into(), REG_CF,  -1, src1n, src2n, -1)
	}

	/// Set the C flag to the carry out of unsigned `src1 - src2`.
	fn c_b(&mut self, src1: impl Into<IrSrc>, src2: impl Into<IrSrc>, src1n: i8, src2n: i8)
	-> &mut Self {
		self.isborrow(REG_CF, src1.into(), src2.into(),  -1, src1n, src2n)
	}

	/// Set the C flag to the carry out of unsigned `src1 - src2 - C`.
	fn c_bc(&mut self, src1: impl Into<IrSrc>, src2: impl Into<IrSrc>, src1n: i8, src2n: i8)
	-> &mut Self {
		self.isborrowb(REG_CF, src1.into(), src2.into(), REG_CF,  -1, src1n, src2n, -1)
	}

	/// Sets the C flag to bit `bitn` of register `src`.
	fn cbit(&mut self, src: IrReg, bitn: u8, srcn: i8) -> &mut Self {
		self.ibit(REG_CF, src, IrConst::_8(bitn),  -1, srcn, -1)
	}

	/// Sets the h flag to bit `bitn` of register `src`.
	fn hbit(&mut self, src: IrReg, bitn: u8) -> &mut Self {
		self.ibit(REG_HF, src, IrConst::_8(bitn),  -1, -1, -1)
	}

	/// Set the Z flag to 0.
	fn z0(&mut self) -> &mut Self { self.mov(REG_ZF, C0_8, -1, -1) }
	/// Set the N flag to 0.
	fn n0(&mut self) -> &mut Self { self.mov(REG_NF, C0_8, -1, -1) }
	/// Set the H flag to 0.
	fn h0(&mut self) -> &mut Self { self.mov(REG_HF, C0_8, -1, -1) }
	/// Set the C flag to 0.
	fn c0(&mut self) -> &mut Self { self.mov(REG_CF, C0_8, -1, -1) }

	/// Set the N flag to 1.
	fn n1(&mut self) -> &mut Self { self.mov(REG_NF, C1_8, -1, -1) }
	/// Set the H flag to 1.
	fn h1(&mut self) -> &mut Self { self.mov(REG_HF, C1_8, -1, -1) }
	/// Set the C flag to 1.
	fn c1(&mut self) -> &mut Self { self.mov(REG_CF, C1_8, -1, -1) }

	/// Combine all the flag registers into an 8-bit value in `dst`.
	fn combine_flags(&mut self, dst: IrReg) -> &mut Self {
		self
		.mov  (dst, C0_8,                -1, -1        )
		.ibset(dst,  dst, C4_8, REG_CF,  -1, -1, -1, -1)
		.ibset(dst,  dst, C5_8, REG_HF,  -1, -1, -1, -1)
		.ibset(dst,  dst, C6_8, REG_NF,  -1, -1, -1, -1)
		.ibset(dst,  dst, C7_8, REG_ZF,  -1, -1, -1, -1)
	}

	/// Extracts all the flag values from `src` into the flag registers.
	fn extract_flags(&mut self, src: IrReg) -> &mut Self {
		self
		.cbit(        src,    4,      -1    )
		.hbit(        src,    5             )
		.ibit(REG_NF, src, C6_8,  -1, -1, -1)
		.ibit(REG_ZF, src, C7_8,  -1, -1, -1)
	}
}

// ------------------------------------------------------------------------------------------------
// Memory
// ------------------------------------------------------------------------------------------------

impl IrBuilder {
	/// Push an 8-bit value `src` onto the stack.
	fn push8(&mut self, src: impl Into<IrSrc>) -> &mut Self {
		// full stack convention - subtract before storing
		self
		.iusub(REG_SP, REG_SP, C1_16,  -1, -1, -1)
		.store(REG_SP,    src,         -1, -1    )
	}

	/// Pop an 8-bit value off the stack into `dst`.
	fn pop8(&mut self, dst: impl Into<IrReg>) -> &mut Self {
		let dst = dst.into();
		// full stack convention - load before adding
		self
		.load (dst,    REG_SP,         -1, -1    )
		.iuadd(REG_SP, REG_SP, C1_16,  -1, -1, -1)
	}

	/// Push a 16-bit value onto the stack as two 8-bit halves, pushing the high half first so that
	/// the resultant value is little-endian in memory.
	fn push16(&mut self, srchi: impl Into<IrSrc>, srclo: impl Into<IrSrc>) -> &mut Self {
		self
		.push8(srchi)
		.push8(srclo)
	}

	/// Pop a 16-bit value off the stack as two 8-bit halves into `dstlo` and `dsthi`.
	fn pop16(&mut self, dsthi: impl Into<IrReg>, dstlo: impl Into<IrReg>) -> &mut Self {
		self
		.pop8(dstlo)
		.pop8(dsthi)
	}

	/// Load indirect, using one of the paired registers as the source address.
	fn load_ind(&mut self, dst: impl Into<IrReg>, src: Reg, srcn: i8) -> &mut Self {
		let src = self.rr(src);
		self.load(dst.into(), src,  -1, srcn)
	}

	/// Store indirect, using one of the paired registers as the destination address.
	fn store_ind(&mut self, dst: Reg, src: impl Into<IrSrc>, dstn: i8, srcn: i8)
	-> &mut Self {
		let dst = self.rr(dst);
		self.store(dst, src.into(),  dstn, srcn)
	}
}

// ------------------------------------------------------------------------------------------------
// Control flow
// ------------------------------------------------------------------------------------------------

impl IrBuilder {
	/// Push the return address to the stack.
	fn push_return_addr(&mut self, ret_addr: VA) -> &mut Self {
		let ret_addr = ret_addr.0 as u16;
		// push hi then lo
		self
		.push8(IrConst::_8((ret_addr >> 8  ) as u8))
		.push8(IrConst::_8((ret_addr & 0xFF) as u8))
	}

	/// Push `ret_addr` to the stack, and then call `target`.
	fn call_(&mut self, ret_addr: VA, target: EA, targetn: i8) -> &mut Self {
		self
		.push_return_addr(ret_addr       )
		.call            (target, targetn)
	}

	/// Pop the return address and `ret` to it.
	fn return_(&mut self) -> &mut Self {
		self
		.pop16(REG_W,  REG_Z                    )
		.ipair(REG_WZ, REG_W, REG_Z,  -1, -1, -1)
		.ret  (REG_WZ,                    -1    )
	}

	/// Evaluate the condition code `cc` and return a register which contains its truth value.
	fn cc(&mut self, cc: Cc) -> IrReg {
		match cc {
			Cc::C  => REG_CF,
			Cc::Z  => REG_ZF,
			Cc::NC => { self.bnot(REG_Z, REG_CF, -1, -1); REG_Z }
			Cc::NZ => { self.bnot(REG_Z, REG_ZF, -1, -1); REG_Z }
		}
	}

	/// Evaluate the logical inversion of the condition code `cc` and return a register which
	/// contains the inverted truth value.
	fn not_cc(&mut self, cc: Cc) -> IrReg {
		self.cc(cc.not())
	}

	/// Do a conditional branch using the condition code `cc`.
	fn cc_branch(&mut self, cc: Cc, target: EA, targetn: i8) -> &mut Self {
		let cond = self.cc(cc);
		self.cbranch(cond, target, -1, targetn)
	}
}

// ------------------------------------------------------------------------------------------------
// Computation
// ------------------------------------------------------------------------------------------------

/// Perform some read-modify-write operation using `[hl]` as the source/dest. `callback` is passed a
/// temporary register containing the 8-bit value loaded from `[hl]`; it must place the result back
/// into this same register, and it must not modify `REG_HL`.
fn hl_rmw(b: &mut IrBuilder, hln: i8, callback: impl Fn(&mut IrBuilder, IrReg) -> &mut IrBuilder) {
	b.load_ind( REG_Z, Reg::HL,  hln    );
	callback  (     b, REG_Z            );
	b.store   (REG_HL, REG_Z,    hln, -1);
}

impl IrBuilder {
	/// Shift the given `reg` left. The carry flag is set to the MSB of `reg`, and the zero flag is
	/// set if the result is 0. N and H flags are set to 0.
	fn sla(&mut self, reg: impl Into<IrReg>, regn: i8) -> &mut Self {
		let reg = reg.into();
		self
		.cbit(     reg,    7,        regn    )
		.ishl(reg, reg, C1_8,  regn, regn, -1)
		.z_  (reg,                   regn    )
		.n0  ()
		.h0  ()
	}

	/// Shift the given `reg` right arithmetic. The carry flag is set to the MSB of `reg`, and the
	/// zero flag is set if the result is 0. N and H flags are set to 0.
	fn sra(&mut self, reg: impl Into<IrReg>, regn: i8) -> &mut Self {
		let reg = reg.into();
		self
		.cbit (     reg,    0,        regn    )
		.isshr(reg, reg, C1_8,  regn, regn, -1)
		.z_   (reg,                   regn    )
		.n0   ()
		.h0   ()
	}

	/// Shift the given `reg` right logical. The carry flag is set to the MSB of `reg`, and the
	/// zero flag is set if the result is 0. N and H flags are set to 0.
	fn srl(&mut self, reg: impl Into<IrReg>, regn: i8) -> &mut Self {
		let reg = reg.into();
		self
		.cbit (     reg,    0,        regn    )
		.iushr(reg, reg, C1_8,  regn, regn, -1)
		.z_   (reg,                   regn    )
		.n0   ()
		.h0   ()
	}

	/// Rotate the given `reg` left. The carry flag is set to the MSB of `reg`, but otherwise does
	/// not participate. If `set_zero_flag`, the zero flag will be set if the result is 0;
	/// otherwise the zero flag will be set to 0 always. N and H flags are set to 0.
	fn rol(&mut self, reg: impl Into<IrReg>, set_zero_flag: bool, regn: i8) -> &mut Self {
		let reg = reg.into();
		self
		.cbit(     reg,    7,        regn    )
		.irol(reg, reg, C1_8,  regn, regn, -1)
		.n0  ()
		.h0  ();

		if set_zero_flag {
			self.z_(reg, regn)
		} else {
			self.z0()
		}
	}

	/// Rotate the given `reg` right. The carry flag is set to the MSB of `reg`, but otherwise does
	/// not participate. If `set_zero_flag`, the zero flag will be set if the result is 0;
	/// otherwise the zero flag will be set to 0 always. N and H flags are set to 0.
	fn ror(&mut self, reg: impl Into<IrReg>, set_zero_flag: bool, regn: i8) -> &mut Self {
		let reg = reg.into();
		self
		.cbit(     reg,    7,        regn    )
		.iror(reg, reg, C1_8,  regn, regn, -1)
		.n0  ()
		.h0  ();

		if set_zero_flag {
			self.z_(reg, regn)
		} else {
			self.z0()
		}
	}

	/// Rotate the given `reg` left through the carry flag. If `set_zero_flag`, the zero flag will
	/// be set if the result is 0; otherwise the zero flag will be set to 0 always. N and H flags
	/// are set to 0.
	fn rolc(&mut self, reg: impl Into<IrReg>, set_zero_flag: bool, regn: i8) -> &mut Self {
		let reg = reg.into();
		self
		.mov  (REG_Z, REG_CF,                 -1,   -1        )
		.cbit (          reg,    7,               regn        )
		.irol (  reg,    reg, C1_8,         regn, regn, -1    )
		.ibset(  reg,    reg, C0_8, REG_Z,  regn, regn, -1, -1)
		.n0   ()
		.h0   ();

		if set_zero_flag {
			self.z_(reg, regn)
		} else {
			self.z0()
		}
	}

	/// Rotate the given `reg` right through the carry flag. If `set_zero_flag`, the zero flag will
	/// be set if the result is 0; otherwise the zero flag will be set to 0 always. N and H flags
	/// are set to 0.
	fn rorc(&mut self, reg: impl Into<IrReg>, set_zero_flag: bool, regn: i8) -> &mut Self {
		let reg = reg.into();
		self
		.mov  (REG_Z, REG_CF,                 -1,   -1        )
		.cbit (          reg,    0,               regn        )
		.iror (  reg,    reg, C1_8,         regn, regn, -1    )
		.ibset(  reg,    reg, C7_8, REG_Z,  regn, regn, -1, -1)
		.n0   ()
		.h0   ();

		if set_zero_flag {
			self.z_(reg, regn)
		} else {
			self.z0()
		}
	}

	/// Swap the nybbles of the given register. The zero flag is set according to whether the result
	/// is zero, and the N, H, and C flags are all set to 0.
	fn swap(&mut self, reg: impl Into<IrReg>, regn: i8) -> &mut Self {
		let reg = reg.into();
		self
		.irol(reg, reg, C4_8,  regn, regn, -1)
		.z_  (reg,                   regn    )
		.n0  ()
		.h0  ()
		.c0  ()
	}

	/// Increment or decrement HL. Assumes HL has already been paired. Increments `REG_HL`, then
	/// extracts the components into `REG_H` and `REG_L`.
	fn inc_dec_hl(&mut self, plus: bool) -> &mut Self {
		if plus {
			self.iuadd(REG_HL, REG_HL, C1_16,  -1, -1, -1);
		} else {
			self.iusub(REG_HL, REG_HL, C1_16,  -1, -1, -1);
		}

		self
		.ilo(REG_L, REG_HL,  -1, -1)
		.ihi(REG_H, REG_HL,  -1, -1)
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
					.icarries(REG_W,   reg, one,  -1, -1, -1)
					.hbit    (       REG_W,   3             );
				}
				self.iuadd(reg, reg, one,  -1, -1, -1);
				C0_8 // nf
			}
			-1 => {
				if change_flags {
					self
					.iborrows(REG_W, REG_A, one,  -1, -1, -1)
					.hbit    (       REG_W,   3             );
				}
				self.iusub(reg, reg, one,  -1, -1, -1);
				C1_8 // nf
			}
			_  => panic!("bad delta"),
		};

		if change_flags {
			self
			.z_(reg, -1)
			.nx( nf, -1);
		}

		self
	}

	/// Add paired register `reg` onto `HL` and update flags. Pairs both `HL` and `reg`, and
	/// extracts results into `REG_H` and `REG_L` afterwards.
	fn add_hl_rr(&mut self, reg: Reg) -> &mut Self {
		let src = self.rr(reg);
		self.rr(Reg::HL);
		self
		.icarries(REG_WZ, REG_HL, src,   1, -1, -1)
		.cbit    (        REG_WZ,  15,      -1    )
		.hbit    (        REG_WZ,  11             )
		.iuadd   (REG_HL, REG_HL, src,  -1, -1, -1)
		.n0      ()
		.ihi     (REG_H,  REG_HL,       -1, -1    )
		.ilo     (REG_L,  REG_HL,       -1, -1    )
	}

	/// Do `dst = REG_SP + val` (written `sp + e` in ISA docs), and update flags.
	fn add_sp_e(&mut self, dst: IrReg, val: i64, valn: i8) -> &mut Self {
		// it adds the sign-extended operand to SP, as if it were unsigned.
		let val = IrConst::_16((val as u64) as u16);
		self
		.icarries(REG_WZ, REG_SP, val,  -1, -1, -1  )
		.cbit    (        REG_WZ,   7,      -1      )
		.hbit    (        REG_WZ,   3               )
		.iuadd   (dst,    REG_SP, val,  -1, -1, valn)
		.z0      ()
		.n0      ()
	}

	/// Do `REG_A = REG_A + src` and update flags.
	fn add_a(&mut self, src: impl Into<IrSrc>, srcn: i8) -> &mut Self {
		let src = src.into();
		self
		.c_      (        REG_A, src,      -1, srcn)
		.icarries(REG_W,  REG_A, src,  -1, -1, srcn)
		.hbit    (        REG_W,   3               )
		.iuadd   (REG_A,  REG_A, src,  -1, -1, srcn)
		.n0      ()
		.z_      (REG_A,                   -1      )
	}

	/// Do `REG_A = REG_A + src + REG_CF` and update flags.
	fn adc_a(&mut self, src: impl Into<IrSrc>, srcn: i8) -> &mut Self {
		let src = src.into();
		self
		.c_c      (        REG_A, src,              -1, srcn    )
		.icarriesc(REG_W,  REG_A, src, REG_CF,  -1, -1, srcn, -1)
		.hbit     (        REG_W,   3                           )
		.iuaddc   (REG_A,  REG_A, src, REG_CF,  -1, -1, srcn, -1)
		.n0       ()
		.z_       (REG_A,                           -1          )
	}

	/// Do `dst = REG_A - src` and update flags.
	fn sub_(&mut self, dst: IrReg, src: impl Into<IrSrc>, srcn: i8) -> &mut Self {
		let src = src.into();
		self
		.c_b     (        REG_A, src,      -1, srcn)
		.iborrows(REG_W,  REG_A, src,  -1, -1, srcn)
		.hbit    (        REG_W,   3               )
		.iusub   (dst,    REG_A, src,  -1, -1, srcn)
		.n1      ()
		.z_      (dst,                     -1      )
	}

	/// Do `REG_A = REG_A - src - REG_CF` and update flags.
	fn sbc_a(&mut self, src: impl Into<IrSrc>, srcn: i8) -> &mut Self {
		let src = src.into();
		self
		.c_bc     (        REG_A, src,              -1, srcn    )
		.iborrowsb(REG_W,  REG_A, src, REG_CF,  -1, -1, srcn, -1)
		.hbit     (        REG_W,   3                           )
		.iusubb   (REG_A,  REG_A, src, REG_CF,  -1, -1, srcn, -1)
		.n1       ()
		.z_       (REG_A,                           -1          )
	}

	/// Do `REG_A = REG_A & src` and update flags.
	fn and_a(&mut self, src: impl Into<IrSrc>, srcn: i8) -> &mut Self {
		let src = src.into();

		self
		.iand(REG_A, REG_A, src,  -1, -1, srcn)
		.n0  ()
		.h1  ()
		.c0  ()
		.z_  (REG_A,                  -1      )
	}

	/// Do `REG_A = REG_A | src` and update flags.
	fn or_a(&mut self, src: impl Into<IrSrc>, srcn: i8) -> &mut Self {
		let src = src.into();

		self.ior(REG_A, REG_A, src,  -1, -1, srcn);
		self.n0 ()
		.h0     ()
		.c0     ()
		.z_     (REG_A,                  -1      )
	}

	/// Do `REG_A = REG_A ^ src` and update flags.
	fn xor_a(&mut self, src: impl Into<IrSrc>, srcn: i8) -> &mut Self {
		let src = src.into();

		self.ixor(REG_A, REG_A, src,  -1, -1, srcn);
		self.n0  ()
		.h0      ()
		.c0      ()
		.z_      (REG_A,                  -1      )
	}
}

// ------------------------------------------------------------------------------------------------
// Computation
// ------------------------------------------------------------------------------------------------

fn build_ir(desc: &InstDesc, i: &Instruction, target: Option<EA>, next: Option<EA>,
b: &mut IrBuilder) {
	use { MetaOp::*, SynOp::*, Reg::* };

	match (desc.meta_op(), desc.syn_ops()) {
		(UNK,  &[]) => { panic!("what the hell is an unknown instruction doing in a BB?"); }

		// for all these, have to emit *something* to avoid empty IR BBs.
		(NOP,  &[]) => { b.nop(); } // no flag changes
		(DI,   &[]) => { b.nop(); } // no flag changes
		(EI,   &[]) => { b.nop(); } // no flag changes
		(HALT, &[]) => { b.nop(); } // no flag changes
		(STOP, &[]) => { b.nop(); } // no flag changes

		// ------------------------------------------------------------------------------------
		// Computation

		// add hl, rr
		(ADD, &[Srg(HL), Srg(reg)]) => { // {Z-, N0, H*, C*}
			b.add_hl_rr(reg);
		}
		// add sp, e
		(ADD, [Srg(SP), Op]) => { // {Z0, N0, H*, C*}
			let Operand::SImm(val) = i.ops()[0] else { panic!() };
			b.add_sp_e(REG_SP, val, 0);
		}

		// add r
		(ADD, &[Srg(A), Srg(reg)]) => { // {Z*, N0, H*, C*}
			b.add_a(IrReg::from(reg), -1);
		}
		// add [hl]
		(ADD, [Srg(A), IndReg(HL)]) => { // {Z*, N0, H*, C*}
			b
			.load_ind(REG_Z, HL,   0)
			.add_a   (REG_Z,      -1);
		}
		// add n
		(ADD, [Srg(A), Op]) => { // {Z*, N0, H*, C*}
			let Operand::UImm(val) = i.ops()[0] else { panic!() };
			b.add_a(IrConst::_8(val as u8), -1);
		}

		// adc r
		(ADC, &[Srg(A), Srg(reg)]) => { // {Z*, N0, H*, C*}
			b.adc_a(IrReg::from(reg), -1);
		}
		// adc [hl]
		(ADC, [Srg(A), IndReg(HL)]) => { // {Z*, N0, H*, C*}
			b
			.load_ind(REG_Z, HL,   0)
			.adc_a   (REG_Z,      -1);
		}
		// adc n
		(ADC, [Srg(A), Op]) => { // {Z*, N0, H*, C*}
			let Operand::UImm(val) = i.ops()[0] else { panic!() };
			b.adc_a(IrConst::_8(val as u8), -1);
		}

		// sub r
		(SUB, &[Srg(A), Srg(reg)]) => { // {Z*, N1, H*, C*}
			b.sub_(REG_A, IrReg::from(reg), -1);
		}
		// sub [hl]
		(SUB, [Srg(A), IndReg(HL)]) => { // {Z*, N1, H*, C*}
			b
			.load_ind(REG_Z,    HL,   0)
			.sub_    (REG_A, REG_Z,  -1);
		}
		// sub n
		(SUB, [Srg(A), Op]) => { // {Z*, N1, H*, C*}
			let Operand::UImm(val) = i.ops()[0] else { panic!() };
			b.sub_(REG_A, IrConst::_8(val as u8), -1);
		}

		// cp r
		(CP, &[Srg(A), Srg(reg)]) => { // {Z*, N1, H*, C*}
			b.sub_(REG_W, IrReg::from(reg), -1);
		}
		// cp [hl]
		(CP, [Srg(A), IndReg(HL)]) => { // {Z*, N1, H*, C*}
			b
			.load_ind(REG_Z,    HL,   0)
			.sub_    (REG_W, REG_Z,  -1);
		}
		// cp n
		(CP, [Srg(A), Op]) => { // {Z*, N1, H*, C*}
			let Operand::UImm(val) = i.ops()[0] else { panic!() };
			b.sub_(REG_W, IrConst::_8(val as u8), -1);
		}

		// sbc r
		(SBC, &[Srg(A), Srg(reg)]) => { // {Z*, N1, H*, C*}
			b.sbc_a(IrReg::from(reg), -1);
		}
		// sbc [hl]
		(SBC, [Srg(A), IndReg(HL)]) => { // {Z*, N1, H*, C*}
			b
			.load_ind(REG_Z, HL,   0)
			.sbc_a   (REG_Z,      -1);
		}
		// sbc n
		(SBC, [Srg(A), Op]) => { // {Z*, N1, H*, C*}
			let Operand::UImm(val) = i.ops()[0] else { panic!() };
			b.sbc_a(IrConst::_8(val as u8), -1);
		}

		// and r
		(AND, &[Srg(A), Srg(reg)]) => { // {Z*, N0, H1, C0}
			b.and_a(IrReg::from(reg), -1);
		}
		// and [hl]
		(AND, [Srg(A), IndReg(HL)]) => { // {Z*, N0, H1, C0}
			b
			.load_ind(REG_Z, HL,   0)
			.and_a   (REG_Z,      -1);
		}
		// and n
		(AND, [Srg(A), Op]) => { // {Z*, N0, H1, C0}
			let Operand::UImm(val) = i.ops()[0] else { panic!() };
			b.and_a(IrConst::_8(val as u8), -1);
		}

		// or r
		(OR, &[Srg(A), Srg(reg)]) => { // {Z*, N0, H0, C0}
			b.or_a(IrReg::from(reg), -1);
		}
		// or [hl]
		(OR, [Srg(A), IndReg(HL)]) => { // {Z*, N0, H0, C0}
			b
			.load_ind(REG_Z, HL,   0)
			.or_a    (REG_Z,      -1);
		}
		// or n
		(OR, [Srg(A), Op]) => { // {Z*, N0, H0, C0}
			let Operand::UImm(val) = i.ops()[0] else { panic!() };
			b.or_a(IrConst::_8(val as u8), -1);
		}

		// xor r
		(XOR, &[Srg(A), Srg(reg)]) => { // {Z*, N0, H0, C0}
			b.xor_a(IrReg::from(reg), -1);
		}
		// xor [hl]
		(XOR, [Srg(A), IndReg(HL)]) => { // {Z*, N0, H0, C0}
			b
			.load_ind(REG_Z, HL,   0)
			.xor_a   (REG_Z,      -1);
		}
		// xor n
		(XOR, [Srg(A), Op]) => { // {Z*, N0, H0, C0}
			let Operand::UImm(val) = i.ops()[0] else { panic!() };
			b.xor_a(IrConst::_8(val as u8), -1);
		}

		// inc bc, inc de, inc hl
		(INC, &[Srg(reg @ (BC | DE | HL))]) => { // no flag changes
			let tmp_reg = b.rr(reg);
			b
			.inc_dec(        tmp_reg,       1, false)
			.ihi    (reg.hi().into(), tmp_reg,  -1, -1)
			.ilo    (reg.lo().into(), tmp_reg,  -1, -1);
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
			hl_rmw(b, 0, |b, reg| b.inc_dec(reg, 1, true));
		}

		// dec bc, inc de, inc hl
		(DEC, &[Srg(reg @ (BC | DE | HL))]) => { // no flag changes
			let tmp_reg = b.rr(reg);
			b
			.inc_dec(        tmp_reg,      -1, false)
			.ihi    (reg.hi().into(), tmp_reg,  -1, -1)
			.ilo    (reg.lo().into(), tmp_reg,  -1, -1);
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
			hl_rmw(b, 0, |b, reg| b.inc_dec(reg, -1, true));
		}

		// cpl a
		(CPL, [Srg(A)]) => { // {Z-, N1, H1, C-}
			b
			.inot(REG_A, REG_A,  -1, -1)
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
				REG_NF as NF, REG_HF as HF, REG_CF as CF, REG_ZF as ZF };
			const C9:    IrConst = IrConst::_8(9);
			const C0XF:  IrConst = IrConst::_8(0x99);
			const C0X99: IrConst = IrConst::_8(0x99);
			const CNEG6: IrConst = IrConst::_8(-6i8 as u8);

			// Z = subtraction adjustment { 0x00, -0x06, -0x60, -0x66 }
			b
			.band (Z, NF, HF,      -1, -1, -1    ) // Z.0 = NF & HF
			.band (X, NF, CF,      -1, -1, -1    ) // X   = NF & CF
			.ibset(Z, Z, C4_8, X,  -1, -1, -1, -1) // Z.4 = NF & CF
			.imul (Z, Z, CNEG6,    -1, -1, -1    ) // Z   = Z * -6

			// W = addition adjustment { 0x00, 0x06, 0x60, 0x66 }
			.inot (X, NF,          -1, -1        ) // X   = !NF
			.iand (Y, A, C0XF,     -1, -1, -1    ) // Y   = A & 0xF
			.iugt (Y, Y, C9,       -1, -1, -1    ) // Y   = A & 0xF > 9
			.bor  (Y, Y, HF,       -1, -1, -1    ) // Y   = HF | (A & 0xF > 9)
			.band (W, X, Y,        -1, -1, -1    ) // W.0 = !NF & (HF | (A & 0xF > 9))
			.iugt (Y, A, C0X99,    -1, -1, -1    ) // Y   = A > 0x99
			.bor  (Y, Y, CF,       -1, -1, -1    ) // Y   = CF | (A > 0x99)
			.band (Y, Y, X,        -1, -1, -1    ) // Y   = !NF & (CF | (A > 0x99))
			.ibset(W, W, C4_8, Y,  -1, -1, -1, -1) // W.4 = !NF & (CF | (A > 0x99))
			.imul (W, W, C6_8,     -1, -1, -1    ) // W   = W * 6

			// because the above two values were calculated based on NF and !NF, either they are
			// both 0 or exactly one is 0. so adding them together has the effect of choosing
			// between them.

			// Z = adjustment
			.iuadd(Z, Z, W,  -1, -1, -1) // Z = NF ? Z : W (effectively)

			// now we can do the actual addition and set the flags
			.c_   (    A, Z,         -1, -1)
			.iuadd(A,  A, Z,     -1, -1, -1)
			.ieq  (ZF, A, C0_8,  -1, -1, -1)
			.mov  (HF, C0_8,     -1, -1    );
		}

		// ------------------------------------------------------------------------------------
		// Bitwise

		// {Z0, N0, H0, C*}
		(RLA,  []) => { b.rolc(REG_A, false, -1); }
		(RLCA, []) => { b.rol (REG_A, false, -1); }
		(RRA,  []) => { b.rorc(REG_A, false, -1); }
		(RRCA, []) => { b.ror (REG_A, false, -1); }

		// {Z*, N0, H0, C*}
		(SLA, &[Srg(HL)])  => { hl_rmw(b, 0, |b, reg| b.sla(reg, -1)); }
		(SLA, &[Srg(reg)]) => {                       b.sla(reg,  0);  }
		(SRA, &[Srg(HL)])  => { hl_rmw(b, 0, |b, reg| b.sra(reg, -1)); }
		(SRA, &[Srg(reg)]) => {                       b.sra(reg,  0);  }
		(SRL, &[Srg(HL)])  => { hl_rmw(b, 0, |b, reg| b.srl(reg, -1)); }
		(SRL, &[Srg(reg)]) => {                       b.srl(reg,  0);  }

		(RL,  &[Srg(HL)])  => { hl_rmw(b, 0, |b, reg| b.rolc(reg, true, -1)); }
		(RL,  &[Srg(reg)]) => {                       b.rolc(reg, true,  0);  }
		(RLC, &[Srg(HL)])  => { hl_rmw(b, 0, |b, reg| b.rol (reg, true, -1)); }
		(RLC, &[Srg(reg)]) => {                       b.rol (reg, true,  0);  }
		(RR,  &[Srg(HL)])  => { hl_rmw(b, 0, |b, reg| b.rorc(reg, true, -1)); }
		(RR,  &[Srg(reg)]) => {                       b.rorc(reg, true,  0);  }
		(RRC, &[Srg(HL)])  => { hl_rmw(b, 0, |b, reg| b.ror (reg, true, -1)); }
		(RRC, &[Srg(reg)]) => {                       b.ror (reg, true,  0);  }

		// {Z*, N0, H0, C0}
		(SWAP, &[Srg(reg)])   => {                       b.swap(reg, -1);  }
		(SWAP, &[IndReg(HL)]) => { hl_rmw(b, 0, |b, reg| b.swap(reg, -1)); }

		// {Z*, N0, H1, C-}
		(BIT, &[Op, Srg(reg)]) => {
			let Operand::UImm(bit) = i.ops()[0] else { panic!() };
			let bit = IrConst::_8(bit as u8);
			let reg = IrReg::from(reg);
			b.ibit(REG_ZF, reg, bit, -1, -1, -1);
		}
		(BIT, [Op, IndReg(HL)]) => {
			let Operand::UImm(bit) = i.ops()[0] else { panic!() };
			let bit = IrConst::_8(bit as u8);
			// operand 0 is the bit number, operand 1 is [hl]
			b
			.load_ind( REG_Z,    HL,           1    )
			.ibit    (REG_ZF, REG_Z, bit, -1, -1, -1);
		}

		// no flag changes
		(RES, &[Op, Srg(reg)]) => {
			let Operand::UImm(bit) = i.ops()[0] else { panic!() };
			let bit = IrConst::_8(bit as u8);
			let reg = IrReg::from(reg);
			b.ibset(reg, reg, bit, C0_8,  -1, -1, -1, -1);
		}
		(RES, [Op, IndReg(HL)]) => {
			let Operand::UImm(bit) = i.ops()[0] else { panic!() };
			let bit = IrConst::_8(bit as u8);
			// operand 0 is the bit number, operand 1 is [hl]
			hl_rmw(b, 1, |b, reg| b.ibset(reg, reg, bit, C0_8, -1, -1, -1, -1));
		}

		// no flag changes
		(SET, &[Op, Srg(reg)]) => {
			let Operand::UImm(bit) = i.ops()[0] else { panic!() };
			let bit = IrConst::_8(bit as u8);
			let reg = IrReg::from(reg);
			b.ibset(reg, reg, bit, C1_8,  -1, -1, -1, -1);
		}
		(SET, [Op, IndReg(HL)]) => {
			let Operand::UImm(bit) = i.ops()[0] else { panic!() };
			let bit = IrConst::_8(bit as u8);
			// operand 0 is the bit number, operand 1 is [hl]
			hl_rmw(b, 1, |b, reg| b.ibset(reg, reg, bit, C1_8, -1, -1, -1, -1));
		}

		// ------------------------------------------------------------------------------------
		// Flag manipulation

		(CCF, []) => { // {Z-, N0, H0, C*}
			b
			.n0  ()
			.h0  ()
			.bnot(REG_CF, REG_CF,  -1, -1);
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
		(JP, [Op]) => { b.branch(target.unwrap(), 0); }
		(JR, [Op]) => { b.branch(target.unwrap(), 0); }

		(JP, &[Cc(cond), Op]) => { b.cc_branch(cond, target.unwrap(), 0); }
		(JR, &[Cc(cond), Op]) => { b.cc_branch(cond, target.unwrap(), 0); }

		(JP, [Srg(HL)]) => {
			b.rr     (HL);
			b.ibranch(REG_HL, 0);
		}

		(CALL, [Op]) => { b.call_(i.next_va(), target.unwrap(), 0); }
		(RST,  [Op]) => { b.call_(i.next_va(), target.unwrap(), 0); }
		(CALL, &[Cc(cond), Op]) => {
			let cond = b.not_cc(cond);
			b
			.cbranch_and_split(       cond, next.unwrap(),  -1, -1)
			.push_return_addr ( i.next_va()                       )
			.call             (           target.unwrap(),       0);
		}

		(RETI, []) => { b.return_(); }
		(RET,  []) => { b.return_(); }
		(RET,  &[Cc(cond)]) => {
			let cond = b.not_cc(cond);
			b
			.cbranch_and_split(cond, next.unwrap(),  -1, -1)
			.return_();
		}

		// ------------------------------------------------------------------------------------
		// Data transfer

		// ld sp, hl (0xF9)
		(LD, &[Srg(SP), Srg(HL)]) => { // no flag changes
			b.rr (HL);
			b.mov(REG_SP, REG_HL,  -1, -1);
		}

		// ld r, r (many, many opcodes in [0x40 .. 0x7F] range)
		(LD, &[Srg(dst), Srg(src)]) => { // no flag changes
			b.mov(dst.into(), IrReg::from(src),  -1, -1);
		}

		// ld hl, sp+e (0xF8)
		(LD, &[Srg(HL), SpPlusOp]) => { // {Z0, N0, H]*, C*}
			let Operand::SImm(val) = i.ops()[0] else { panic!() };
			b
			.add_sp_e(REG_HL,     val,   0    )
			.ilo     ( REG_L,  REG_HL,  -1, -1)
			.ihi     ( REG_H,  REG_HL,  -1, -1);
		}

		// ld rr, nn (0x01, 0x11, 0x21)
		(LD, &[Srg(dst @ (BC | DE | HL)), Op]) => { // no flag changes
			let Operand::UImm(val) = i.ops()[0] else { panic!() };
			let val = IrConst::_16(val as u16);
			// seems silly to do this, but it's to preserve the original source operand in the IR,
			// for later tracing back and marking this operand as a reference
			b
			.mov(         REG_WZ,    val,  -1,  0)
			.ihi(dst.hi().into(), REG_WZ,  -1, -1)
			.ilo(dst.lo().into(), REG_WZ,  -1, -1);
		}

		// ld sp, nn (0x31) (same as above but SP is represented differently)
		(LD, &[Srg(SP), Op]) => { // no flag changes
			let Operand::UImm(val) = i.ops()[0] else { panic!() };
			b.mov(REG_SP, IrConst::_16(val as u16),  -1,  0);
		}

		// ld r, n (various)
		(LD, &[Srg(dst), Op]) => { // no flag changes
			let Operand::UImm(val) = i.ops()[0] else { panic!() };
			b.mov(dst.into(), IrConst::_8(val as u8),  -1, 0);
		}

		// ld r, [rr] (various)
		(LD, &[Srg(dst), IndReg(src @ (BC | DE | HL))]) => { // no flag changes
			b.load_ind(dst, src,  -1);
		}

		// ld [rr], r (various)
		(LD, &[IndReg(dst @ (BC | DE | HL)), Srg(src)]) => { // no flag changes
			b.store_ind(dst, IrReg::from(src),  -1, -1);
		}

		// ld a, [nn] (0xFA)
		(LD, &[Srg(A), IndOp]) => { // no flag changes
			let Operand::Mem(src, _) = i.ops()[0] else { panic!() };
			b.load(REG_A, IrConst::_16(src.0 as u16),  -1, 0);
		}

		// ld [nn], a (0xEA)
		(LD, &[IndOp, Srg(A)]) => { // no flag changes
			let Operand::Mem(dst, _) = i.ops()[0] else { panic!() };
			b.store(IrConst::_16(dst.0 as u16), REG_A,  0, -1);
		}

		// ld [hl+], a (0x22)
		// ld [hl-], a (0x32)
		(LD, &[pm @ (IndHlPlus | IndHlMinus), Srg(A)]) => { // no flag changes
			b
			.store_ind (HL, REG_A,  -1, -1)
			.inc_dec_hl(pm == IndHlPlus);
		}

		// ld a, [hl+] (0x2A)
		// ld a, [hl-] (0x3A)
		(LD, &[Srg(A), pm @ (IndHlPlus | IndHlMinus)]) => { // no flag changes
			b
			.load_ind  (A, HL,  -1)
			.inc_dec_hl(pm == IndHlPlus);
		}

		// ld [hl], n (0x36)
		(LD, &[IndReg(HL), Op2]) => { // no flag changes
			let Operand::UImm(src) = i.ops()[1] else { panic!() };
			b.store_ind(HL, IrConst::_8(src as u8),  0, 1);
		}

		// ld [nn], sp (0x08)
		(LD, &[IndOp, Srg(SP)]) => { // no flag changes
			let Operand::Mem(dst, _) = i.ops()[0] else { panic!() };
			let dst0 = IrConst::_16(dst.0 as u16);
			let dst1 = IrConst::_16((dst.0 + 1) as u16);

			// split it into two 8-bit stores, little-endian
			b
			.ilo  (REG_Z, REG_SP,  -1, -1)
			.store( dst0,  REG_Z,   0, -1)
			.ihi  (REG_W, REG_SP,  -1, -1)
			// since "dst+1" isn't what they wrote in the operand, we don't associate
			// the IR operand with it; only on the first store.
			.store( dst1, REG_W,  -1, -1);
		}

		// ld a, [0xFF00 + n] (0xF0)
		(LDH, [Srg(A), IndOp]) => { // no flag changes
			let Operand::Mem(src, _) = i.ops()[0] else { panic!() };
			b.load(REG_A, IrConst::_16(src.0 as u16),  -1, 0);
		}
		// ld a, [0xFF00 + c] (0xF2)
		(LDH, [Srg(A), IndReg(C)]) => { // no flag changes
			b
			.izxt (REG_WZ,  REG_C,            -1, -1    )
			.iuadd(REG_WZ, REG_WZ, CFF00_16,  -1, -1, -1)
			.load ( REG_A, REG_WZ,             0, -1    );
		}
		// ld [0xFF00 + n], a (0xE0)
		(LDH, [IndOp, Srg(A)]) => { // no flag changes
			let Operand::Mem(dst, _) = i.ops()[0] else { panic!() };
			b.store(IrConst::_16(dst.0 as u16), REG_A,  0, -1);
		}
		// ld [0xFF00 + c], a (0xE2)
		(LDH, [IndReg(C), Srg(A)]) => { // no flag changes
			b
			.izxt (REG_WZ,  REG_C,            -1, -1    )
			.iuadd(REG_WZ, REG_WZ, CFF00_16,  -1, -1, -1)
			.store(REG_WZ,  REG_A,             0, -1    );
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
			.combine_flags(REG_Z       )
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
			.extract_flags(REG_Z       );
		}

		_ => panic!("IR unimplemented: {:?}", desc),
	}
}
