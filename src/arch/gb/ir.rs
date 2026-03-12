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

fn reg_to_ir_reg(reg: u8) -> IrReg {
	match Reg::from(reg) {
		Reg::A  => REG_A,
		Reg::B  => REG_B,
		Reg::C  => REG_C,
		Reg::D  => REG_D,
		Reg::E  => REG_E,
		Reg::H  => REG_H,
		Reg::L  => REG_L,
		Reg::SP => REG_SP,
		_ => panic!(),
	}
}

impl InstDesc {
	// /// Do an addition and set flags according to the result.
	// fn add_(&self, dst: IrReg, src: impl Into<IrSrc>, dstn: i8, srcn: i8, i: &Instruction,
	// b: &mut IrBuilder) {
	// 	let ea = i.ea();
	// 	let src = src.into();

	// 	b.iuadd (ea, dst,    dst, src,               dstn, dstn, srcn);
	// 	b.icarry(ea, REG_CF, dst, src,               -1, -1, -1);
	// 	b.ieq   (ea, REG_ZF, dst, IrConst::ZERO_8,   -1, -1, -1);
	// }

	fn cc(&self, cc: Cc, i: &Instruction, b: &mut IrBuilder) -> IrReg {
		match cc {
			Cc::C  => REG_CF,
			Cc::Z  => REG_ZF,
			Cc::NC => {
				b.bnot(i.ea(), REG_Z, REG_CF, -1, -1);
				REG_Z
			}
			Cc::NZ => {
				b.bnot(i.ea(), REG_Z, REG_ZF, -1, -1);
				REG_Z
			}
		}
	}

	fn not_cc(&self, cc: Cc, i: &Instruction, b: &mut IrBuilder) -> IrReg {
		match cc {
			Cc::C  => self.cc(Cc::NC, i, b),
			Cc::Z  => self.cc(Cc::NZ, i, b),
			Cc::NC => self.cc(Cc::C,  i, b),
			Cc::NZ => self.cc(Cc::Z,  i, b),
		}
	}

	fn hl(&self, i: &Instruction, b: &mut IrBuilder) {
		b.ipair(i.ea(), REG_HL_TMP, REG_H, REG_L,  -1, -1, -1);
	}

	fn wz(&self, i: &Instruction, b: &mut IrBuilder) {
		b.ipair(i.ea(), REG_WZ_TMP, REG_W, REG_Z,  -1, -1, -1);
	}

	/// Push an 8-bit value `src` onto the stack.
	fn push8(&self, src: impl Into<IrSrc>, i: &Instruction, b: &mut IrBuilder) {
		let ea = i.ea();
		// full stack convention - subtract before storing
		b.iusub(ea, REG_SP, REG_SP, IrConst::_16(1),  -1, -1, -1);
		b.store(ea, REG_SP, src,                      -1, -1);
	}

	/// Pop an 8-bit value off the stack into `dst`.
	fn pop8(&self, dst: IrReg, i: &Instruction, b: &mut IrBuilder) {
		let ea = i.ea();
		// full stack convention - load before adding
		b.load (ea, dst,    REG_SP,                   -1, -1);
		b.iuadd(ea, REG_SP, REG_SP, IrConst::_16(1),  -1, -1, -1);
	}

	/// Pop a 16-bit value off the stack as two 8-bit halves into `dstlo` and `dsthi`.
	fn pop16(&self, dsthi: IrReg, dstlo: IrReg, i: &Instruction, b: &mut IrBuilder) {
		self.pop8(dstlo, i, b);
		self.pop8(dsthi, i, b);
	}

	/// Pop a value into the WZ register and pair it, so it's ready to use as REG_WZ_TMP.
	fn pop_wz(&self, i: &Instruction, b: &mut IrBuilder) {
		self.pop16(REG_W, REG_Z, i, b);
		self.wz(i, b);
	}

	/// Push the return address to the stack.
	fn push_return_addr(&self, i: &Instruction, b: &mut IrBuilder) {
	 	let ret_addr = i.next_va().0 as u16;
		// push hi then lo
		self.push8(IrConst::_8((ret_addr >> 8  ) as u8), i, b);
		self.push8(IrConst::_8((ret_addr & 0xFF) as u8), i, b);
	}

	fn return_(&self, i: &Instruction, b: &mut IrBuilder) {
		self.pop_wz(i, b);
		b.ret(i.ea(), REG_WZ_TMP, -1);
	}

	pub(super) fn build_ir(&self, i: &Instruction, target: Option<EA>, b: &mut IrBuilder) {
		use MetaOp::*;

		let ea = i.ea();

		match self.meta_op() {
			UNK => { panic!("what the hell is an unknown instruction doing in a BB?"); }

			// for all these, have to emit *something* to avoid empty IR BBs.
			NOP  => { b.nop(ea); } // no flag changes
			DI   => { b.nop(ea); } // no flag changes
			EI   => { b.nop(ea); } // no flag changes
			HALT => { b.nop(ea); } // no flag changes
			STOP => { b.nop(ea); } // no flag changes

			// ------------------------------------------------------------------------------------
			// Computation

			ADD => {
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
			ADC => {
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
			SUB => {
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
			SBC => {
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
			AND => {
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
			OR => {
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
			XOR => {
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
			CP => {
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
			INC => {
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
			DEC => {
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
			CPL => {
				b.nop(ea); // TODO
				// {Z-, N1, H1, C-} 0x2F (cpl)
			}
			DA => {
				b.nop(ea); // TODO
				// {Z*, N-, H0, C*} 0x27 (da a)
			}

			// ------------------------------------------------------------------------------------
			// Bitwise

			SLA => {
				b.nop(ea); // TODO
				// r8 <<= 1
					// {Z*, N0, H0, C*} 0xCB_{0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x27} (sla r)
					// InstDesc(0xCB_20, SLA,  &[Srg(B)],                 Other,  Imp),
					// ...

				// [hl] <<= 1
					// {Z*, N0, H0, C*} 0xCB_26 (sla [hl])
					// InstDesc(0xCB_26, SLA,  &[IndReg(HL)],             Other,  Ind(HL, RW)),
			}
			SRA => {
				b.nop(ea); // TODO
				// r8 >>= 1
					// {Z*, N0, H0, C*} 0xCB_{0x28, 0x29, 0x2A, 0x2B, 0x2C, 0x2D, 0x2F} (sra r)
					// InstDesc(0xCB_28, SRA,  &[Srg(B)],                 Other,  Imp),
					// ...

				// [hl] >>= 1
					// {Z*, N0, H0, C*} 0xCB_2E (sra [hl])
					// InstDesc(0xCB_2E, SRA,  &[IndReg(HL)],             Other,  Ind(HL, RW)),
			}
			SRL => {
				b.nop(ea); // TODO
				// r8 >>>= 1
					// {Z*, N0, H0, C*} 0xCB_{0x38, 0x39, 0x3A, 0x3B, 0x3C, 0x3D, 0x3F} (srl r)
					// InstDesc(0xCB_38, SRL,  &[Srg(B)],                 Other,  Imp),
					// ...

				// [hl] >>>= 1
					// {Z*, N0, H0, C*} 0xCB_3E (srl [hl])
					// InstDesc(0xCB_3E, SRL,  &[IndReg(HL)],             Other,  Ind(HL, RW)),
			}
			RLA => {
				b.nop(ea); // TODO
				// {Z0, N0, H0, C*} 0x17 (rla)
			}
			RL => {
				b.nop(ea); // TODO
				// rol(r8)
					// {Z*, N0, H0, C*} 0xCB_{0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x17} (rl r)
					// InstDesc(   0x17, RL,   &[Srg(A)],                 Other,  Imp),
					// InstDesc(0xCB_17, RL,   &[Srg(A)],                 Other,  Imp),
					// ...

				// rol([hl])
					// {Z*, N0, H0, C*} 0xCB_16 (rl [hl])
					// InstDesc(0xCB_16, RL,   &[IndReg(HL)],             Other,  Ind(HL, RW)),
			}
			RLCA => {
				b.nop(ea); // TODO
				// {Z0, N0, H0, C*} 0x07 (rlca)
			}
			RLC => {
				b.nop(ea); // TODO
				// rolc(r8)
					// {Z*, N0, H0, C*} 0xCB_{0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x07} (rlc r)
					// InstDesc(   0x07, RLC,  &[Srg(A)],                 Other,  Imp),
					// InstDesc(0xCB_07, RLC,  &[Srg(A)],                 Other,  Imp),
					// ...

				// rolc([hl])
					// {Z*, N0, H0, C*} 0xCB_06 (rlc [hl])
					// InstDesc(0xCB_06, RLC,  &[IndReg(HL)],             Other,  Ind(HL, RW)),
			}
			RRA => {
				b.nop(ea); // TODO
				// {Z0, N0, H0, C*} 0x1F (rra)
			}
			RR => {
				b.nop(ea); // TODO
				// ror(r8)
					// {Z*, N0, H0, C*} 0xCB_{0x18, 0x19, 0x1A, 0x1B, 0x1C, 0x1D, 0x1F} (rr r)
					// InstDesc(   0x1F, RR,   &[Srg(A)],                 Other,  Imp),
					// InstDesc(0xCB_1F, RR,   &[Srg(A)],                 Other,  Imp),
					// ...

				// ror([hl])
					// {Z*, N0, H0, C*} 0xCB_1E (rr [hl])
					// InstDesc(0xCB_1E, RR,   &[IndReg(HL)],             Other,  Ind(HL, RW)),
			}
			RRCA => {
				b.nop(ea); // TODO
				// {Z0, N0, H0, C*} 0x0F (rrca)
			}
			RRC => {
				b.nop(ea); // TODO
				// rorc(r8)
					// {Z*, N0, H0, C*} 0xCB_{0x08, 0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x0F} (rrc r)
					// InstDesc(   0x0F, RRC,  &[Srg(A)],                 Other,  Imp),
					// InstDesc(0xCB_0F, RRC,  &[Srg(A)],                 Other,  Imp),
					// ...

				// rorc([hl])
					// {Z*, N0, H0, C*} 0xCB_0E (rrc [hl])
					// InstDesc(0xCB_0E, RRC,  &[IndReg(HL)],             Other,  Ind(HL, RW)),
			}
			SWAP => {
				b.nop(ea); // TODO
				// swap(r8)
					// {Z*, N0, H0, C0} 0xCB_{0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x37} (swap r)
					// InstDesc(0xCB_30, SWAP, &[Srg(B)],                 Other,  Imp),
					// ...

				// swap([hl])
					// {Z*, N0, H0, C0} 0xCB_36 (swap [hl])
					// InstDesc(0xCB_36, SWAP, &[IndReg(HL)],             Other,  Ind(HL, RW)),
			}
			BIT => {
				b.nop(ea); // TODO
				// zf <- r8.n
					// {Z*, N0, H1, C-} 0xCB_{{4,5,6,7}{^6,E}}} (bit n, r)
					// InstDesc(0xCB_40, BIT,  &[Op, Srg(B)],             Other,  Imp),
					// ...

				// zf <- [hl].n
					// {Z*, N0, H1, C-} 0xCB_{{4,5,6,7}{6,E}}} (bit n, [hl])
					// InstDesc(0xCB_46, BIT,  &[Op, IndReg(HL)],         Other,  Ind(HL, R)),
			}
			RES => {
				b.nop(ea); // TODO
				// r8.n <- 0
					// no flag changes
					// InstDesc(0xCB_80, RES,  &[Op, Srg(B)],             Other,  Imp),
					// ...

				// [hl].n <- 0
					// no flag changes
					// InstDesc(0xCB_86, RES,  &[Op, IndReg(HL)],         Other,  Ind(HL, RW)),
			}
			SET => {
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

			CCF => {
				b.nop(ea); // TODO
				// {Z-, N0, H0, C*} 0x3F (ccf)
			}
			SCF => {
				b.nop(ea); // TODO
				// {Z-, N0, H0, C1} 0x37 (scf)
			}

			// ------------------------------------------------------------------------------------
			// Control flow

			JP => {
				// no flag changes
				match self.syn_ops()[0] {
					SynOp::Op => {
						// jp nn
						b.branch(ea, target.unwrap(), 0);
					}
					SynOp::Srg(Reg::HL) => {
						// jp hl
						self.hl(i, b);
						b.ibranch(ea, REG_HL_TMP, 0);
					}
					SynOp::Cc(cond) => {
						// jp cc, nn
						let cond = self.cc(cond, i, b);
						b.cbranch(ea, cond, target.unwrap(), -1, 0);
					}
					_ => panic!(),
				}
			}
			JR => {
				// no flag changes
				match self.syn_ops()[0] {
					SynOp::Op => {
						// jr e
						b.branch(ea, target.unwrap(), 0);
					}
					SynOp::Cc(cond) => {
						// jr cc, e
						let cond = self.cc(cond, i, b);
						b.cbranch(ea, cond, target.unwrap(), -1, 0);
					}
					_ => panic!()
				}
			}
			CALL => {
				// no flag changes
				match self.syn_ops()[0] {
					SynOp::Op => {
						// call nn
						self.push_return_addr(i, b);
						b.call(ea, target.unwrap(), 0);
					}
					SynOp::Cc(cond) => {
						// call cc, nn
						let cond = self.not_cc(cond, i, b);
						b.cbranch_and_split(ea, cond, i.next_ea(), -1, -1);
						self.push_return_addr(i, b);
						b.call             (ea, target.unwrap(),   0);
					}
					_ => panic!()
				}
			}
			RET => {
				// no flag changes
				match self.syn_ops().get(0) {
					None => {
						// ret
						self.return_(i, b);
					}
					Some(SynOp::Cc(cond)) => {
						// ret cc
						let cond = self.not_cc(*cond, i, b);
						b.cbranch_and_split(ea, cond, i.next_ea(), -1, -1);
						self.return_(i, b);
					}
					_ => panic!()
				}
			}
			RST => {
				// no flag changes
				// rst n
				self.push_return_addr(i, b);
				b.call(ea, target.unwrap(), 0);
			}
			RETI => {
				// no flag changes
				// reti
				self.return_(i, b);
			}

			// ------------------------------------------------------------------------------------
			// Data transfer

			LD => {
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
			LDH => {
				b.nop(ea); // TODO
				// no flag changes

				// [MEM] <- REG
					// InstDesc(   0xE0, LDH,  &[IndOp, Srg(A)],          Other,  AddHi(W)),
					// InstDesc(   0xE2, LDH,  &[IndReg(C), Srg(A)],      Other,  IndHi(W)),

				// REG <- [MEM]
					// InstDesc(   0xF0, LDH,  &[Srg(A), IndOp],          Other,  AddHi(R)),
					// InstDesc(   0xF2, LDH,  &[Srg(A), IndReg(C)],      Other,  IndHi(R)),
			}
			PUSH => {
				b.nop(ea); // TODO
				// no flag changes
				// InstDesc(   0xC5, PUSH, &[Srg(BC)],                Other,  Ind(SP, W)),
				// ...
			}
			POP => {
				b.nop(ea); // TODO
				// no flag changes EXCEPT for 0xF1
				// {Z*, N*, H*, C*} 0xF1 (pop af)

				// InstDesc(   0xC1, POP,  &[Srg(BC)],                Other,  Ind(SP, R)),
				// ...
			}
		}
	}
}
