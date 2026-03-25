//! IR compiler for MOS 65xx.
//!
//! Referenced from Mesen 2 source code (Mesen2/Core/NES/NesCpu.cpp/h) for accuracy.

use crate::arch::{ IIrCompiler };
use crate::program::{ MemIndir, BBTerm };
use crate::ir::{ IrReg, IrConst, IrSrc, IrBuilder, BuildReg };

use super::*;

// ------------------------------------------------------------------------------------------------
// IR
// ------------------------------------------------------------------------------------------------

pub(crate) struct Mos65xxIrCompiler;

impl IIrCompiler for Mos65xxIrCompiler {
	fn build_ir(&self, b: &mut IrBuilder) {
		lookup_desc(b.inst().bytes()[0]).build_ir(None, b);
	}

	fn build_ir_term(&self, b: &mut IrBuilder, term: &BBTerm) {
		lookup_desc(b.inst().bytes()[0]).build_ir(Some(term), b);
	}

	fn arg_regs(&self) -> &'static [IrReg] {
		ARG_REGS
	}

	fn return_regs(&self) -> &'static [IrReg] {
		RETURN_REGS
	}

	fn stack_ptr_reg(&self) -> IrReg {
		REG_S
	}
}

const REG_A:  IrReg = IrReg::reg8(0);
const REG_X:  IrReg = IrReg::reg8(1);
const REG_Y:  IrReg = IrReg::reg8(2);
const REG_S:  IrReg = IrReg::reg8(3);
const REG_CF: IrReg = IrReg::reg8(4);  // 0 Carry
const REG_ZF: IrReg = IrReg::reg8(5);  // 1 Zero
const REG_IF: IrReg = IrReg::reg8(6);  // 2 Interrupt
const REG_DF: IrReg = IrReg::reg8(7);  // 3 Decimal
// bit 4 is the break flag. it isn't actually a register, but appears when flags are pushed.
// bit 5 is reserved. it isn't actually a register, but appears as a 1 when flags are pushed.
const REG_VF: IrReg = IrReg::reg8(10); // 6 oVerflow
const REG_NF: IrReg = IrReg::reg8(11); // 7 Negative

const REG_TMP1:  IrReg = IrReg::reg8(12);  // 8-bit temporary
const REG_TMP2:  IrReg = IrReg::reg8(13);  // 8-bit temporary
const REG_TMP16: IrReg = IrReg::reg16(15); // 16-bit temporary
const REG_TMP16_2: IrReg = IrReg::reg16(17); // 16-bit temporary

static ARG_REGS: &[IrReg] =
	&[REG_A, REG_X, REG_Y,        REG_CF, REG_ZF, REG_IF, REG_DF, REG_VF, REG_NF];

static RETURN_REGS: &[IrReg] =
	&[REG_A, REG_X, REG_Y, REG_S, REG_CF, REG_ZF, REG_IF, REG_DF, REG_VF, REG_NF];

fn reg_to_ir_reg(reg: u8) -> IrReg {
	match Reg::from(reg) {
		Reg::A => REG_A,
		Reg::X => REG_X,
		Reg::Y => REG_Y,
		Reg::S => REG_S,
		Reg::P => panic!(),
	}
}

impl InstDesc {
	/// Gets the operand, but doesn't *access* it. The caller is responsible for that. (That is, for
	/// memory operands, gives the final effective address, but doesn't load from/store into it.
	/// Some kinds of memory operands require multiple memory accesses, though.)
	///
	/// May affect REG_TMP1, REG_TMP2, REG_TMP16, and REG_TMP16_2.
	///
	/// Returned source is either a constant, REG_TMP2 (for zero-page addresses), or REG_TMP16.
	///
	/// Panics if called on an instruction with implicit addressing. Caller is responsible for that.
	fn get_operand(&self, b: &mut IrBuilder) -> IrSrc {
		use AddrMode::*;
		use MemIndir::*;

		match self.addr_mode {
			IMP => { panic!("get_operand shouldn't be called on instructions with no operand"); }
			// Immediate (1 byte), e.g. `lda #$30`.
			IMM => {
				let Operand::UImm(val) = b.inst().ops()[0] else { panic!() };
				IrConst::_8(val as u8).into()
			}
			// Zero-page absolute (1 byte), e.g. `lda $10`.
			ZPG => {
				// R|W|RW
				let Operand::Mem(va, _) = b.inst().ops()[0] else { panic!() };
				IrConst::_16(va.0 as u16).into()
			}
			// Zero-page, X- or Y-indexed (1 byte), e.g. `lda $80,X`.
			// Never crosses out of page 0, so high byte of address is always 0.
			ZPX | ZPY => {
				// ZPX: R|W|RW
				// ZPY: R|W
				let Operand::Indir(RegDisp { reg, disp }, _) = b.inst().ops()[0] else { panic!() };

				// using tmp2 here so that resulting address is in the range [0, 255].
				// tmp2 = reg + disp
				b.add(REG_TMP2, reg_to_ir_reg(reg), (IrConst::_8(disp as u8), 0));
				REG_TMP2.into()
			}
			// Absolute (2 bytes), e.g. `lda $8040`.
			ABS => {
				// R|W|RW
				let Operand::Mem(va, _) = b.inst().ops()[0] else { panic!() };
				IrConst::_16(va.0 as u16).into()
			}
			// Absolute, X- or Y-indexed (2 bytes), e.g. `lda $8040,X`
			// *Can* cross page boundaries, so does a full 16-bit add.
			ABX | ABY => {
				// ABX: R|W|RW
				// ABY: R|W
				let Operand::Indir(RegDisp { reg, disp }, _) = b.inst().ops()[0] else { panic!() };

				// tmp16 = zxt(reg)
				// tmp16 += disp
				b.zxt( REG_TMP16, reg_to_ir_reg(reg));
				b.add(REG_TMP16, REG_TMP16, (IrConst::_16(disp as u16), 0));
				REG_TMP16.into()
			}
			// Indirect (2 bytes); used only for indirect jump i.e. `jmp ($2000)`.
			IND => {
				// R
				let Operand::Mem(va, _) = b.inst().ops()[0] else { panic!() };
				let va = va.0 as u16;

				// TODO: this addressing mode may or may not cross page boundaries depending on CPU
				// revision. NMOS versions don't, CMOS versions do. The NES uses the older NMOS
				// behavior so that's what I'm implementing for now.

				if (va & 0xFF) == 0xFF {
					// have to do SILLY STUFF; the high byte's address is byte 0 on the same page
					// as the low byte's address.

					// tmp2  = lo
					// tmp1  = hi
					// tmp16 = tmp1:tmp2
					b.load (REG_TMP2,  IrConst::_16(va));
					b.load (REG_TMP1,  IrConst::_16(va & 0xFF00));
					b.pair(REG_TMP16, REG_TMP1, REG_TMP2);
				} else {
					// ez mode

					// tmp16 = *va
					b.load(REG_TMP16, (IrConst::_16(va), 0));
				}
				REG_TMP16.into()
			}
			// "Indexed Indirect" - double-indirect zero-page X-indexed (1 byte), e.g. `lda
			//  ($10,X)`. Loads a 2-byte address from `X + offset`, then accesses the byte at that
			//  address.
			//
			//  The `X + disp` step never crosses out of page 0, and neither does accessing the 2
			//  bytes of the loaded address; so if `X + disp` == 0x00FF, the 2 address bytes will
			//  be loaded from `0x00FF` and `0x0000`.
			IZX => {
				// R
				let Operand::Indir(RegDisp { disp, .. }, _) = b.inst().ops()[0] else { panic!() };

				// tmp2 = X + disp  (wraps at 8 bits)
				b.add(REG_TMP2, REG_X, (IrConst::_8(disp as u8), 0));

				// now tmp2 points to a location in the zero page, but we don't statically know
				// whether tmp2 + 1 would wrap around or not, so we have to do it the Long Way.

				// tmp1 = tmp2 + 1  (wraps at 8 bits)
				b.add(REG_TMP1, REG_TMP2, IrConst::ONE_8);

				// tmp2 = *tmp2  (lo)
				// tmp1 = *tmp1  (hi)
				// tmp16 = tmp1:tmp2
				b.load (REG_TMP2,  REG_TMP2);
				b.load (REG_TMP1,  REG_TMP1);
				b.pair(REG_TMP16, REG_TMP1, REG_TMP2);
				REG_TMP16.into()
			}
			// "Indirect Indexed" - double-indirect zero-page Y-indexed (1 byte), e.g. `lda
			//  ($10),Y`. Loads a 2-byte address from `offset`, then accesses the byte at
			//  `Y + address`.
			//
			//  The initial zero-page access never crosses of page 0, but the subsequent "addr + Y"
			//  does a full 16-bit add.
			IZY => {
				// R
				let Operand::Mem(va, _) = b.inst().ops()[0] else { panic!() };
				let va = va.0;

				if va == 0xFF {
					// wrap that address

					// tmp2  = *0x00FF  (lo)
					// tmp1  = *0x0000  (hi)
					// tmp16 = tmp1:tmp2
					b.load (REG_TMP2,  (IrConst::_8(0xFF), 0));
					b.load (REG_TMP1,  (IrConst::ZERO_8, 0));
					b.pair(REG_TMP16, REG_TMP1, REG_TMP2);
				} else {
					// tmp16 = *va
					b.load(REG_TMP16, (IrConst::_8(va as u8), 0));
				}

				// tmp16_2 = zxt(Y)
				// tmp16 = tmp16 + tmp16_2
				b.zxt( REG_TMP16_2, REG_Y);
				b.add(REG_TMP16, REG_TMP16, REG_TMP16_2);
				REG_TMP16.into()
			}
			// PC-relative (1 byte), e.g. `bcc whatever`.
			// Signed offset added to PC (+2 for size of branch instruction).
			REL => {
				panic!("get_operand shouldn't be called on instructions with control flow targets");
				// Target
				// let Operand::Mem(va, _) = b.inst().ops()[0] else { panic!() };
				// IrConst::_16(va.0 as u16).into()
			}
			// Alias for `ABS` but for `jmp`/`jsr` instructions, to distinguish their operand types.
			LAB => {
				panic!("get_operand shouldn't be called on instructions with control flow targets");
				// Target
				// let Operand::Mem(va, _) = b.inst().ops()[0] else { panic!() };
				// IrConst::_16(va.0 as u16).into()
			}
		}
	}

	/// Gets the actual value of the operand, performing a load if needed, and places the value
	/// into `dst`.
	fn get_operand_value_into(&self, dst: IrReg, b: &mut IrBuilder) {

		use AddrMode::*;

		match self.addr_mode {
			IMP | REL | LAB | IND => {
				panic!("get_operand_value_into should not be called on this");
			}
			IMM => {
				// just a constant, assign it
				let val = self.get_operand(b);
				b.mov(dst, (val, 0));
			}
			ZPG | ABS => {
				// needs a load, and that load references the operand
				let addr = self.get_operand(b);
				b.load(dst, (addr, 0));
			}
			ZPX | ZPY | ABX | ABY | IZX | IZY => {
				// needs a load, but that load does *not* reference the operand
				let addr = self.get_operand(b);
				b.load(dst, addr);
			}
		}
	}

	/// Push an 8-bit value `src` onto the stack.
	fn push8(&self, src: impl Into<IrSrc>, b: &mut IrBuilder) {
		// empty stack convention - store before subtracting
		b.pair(REG_TMP16, IrConst::ONE_8, REG_S);
		b.store(REG_TMP16, src);
		b.sub(REG_S, REG_S, IrConst::_8(1));
	}

	/// Pop an 8-bit value off the stack into `dst`.
	fn pop8(&self, dst: IrReg, b: &mut IrBuilder) {
		// empty stack convention - add before loading
		b.add(REG_S, REG_S, IrConst::_8(1));
		b.pair(REG_TMP16, IrConst::ONE_8, REG_S);
		b.load (dst, REG_TMP16);
	}

	/// Push the return address to the stack. It's always the instruction's address + 2, despite the
	/// address *actually* being 3 bytes away. This is fixed by `rts` which adds 1 to the pulled
	/// address.
	fn push_return_addr(&self, b: &mut IrBuilder) {
	 	let ret_addr = (b.inst().va().0 + 2) as u16;
		// push hi then lo
		self.push8(IrConst::_8((ret_addr >> 8  ) as u8), b);
		self.push8(IrConst::_8((ret_addr & 0xFF) as u8), b);
	}

	/// Pop the return address from the stack and `ret` to it. If `add_1` is true, adds 1 to the
	/// popped address (`rts` does this, but `rti` does not).
	fn return_(&self, b: &mut IrBuilder, add_1: bool) {
		// pop lo then hi
		self.pop8(REG_TMP2, b);
		self.pop8(REG_TMP1, b);
		b.pair(REG_TMP16, REG_TMP1, REG_TMP2);

		if add_1 {
			b.add(REG_TMP16, REG_TMP16, IrConst::ONE_16);
		}

		b.ret  (REG_TMP16);
	}

	/// Combine all flags IR regs into a single 8-bit value and push it.
	///
	/// Changes REG_TMP1.
	fn push_flags(&self, b: &mut IrBuilder) {
		// the values of bits 4 and 5 (Break and Reserved) are always 1 when pushed.
		b.mov  (REG_TMP1, IrConst::_8(0b0011_0000));
		b.bset(REG_TMP1, REG_TMP1, IrConst::_8(0), REG_CF);
		b.bset(REG_TMP1, REG_TMP1, IrConst::_8(1), REG_ZF);
		b.bset(REG_TMP1, REG_TMP1, IrConst::_8(2), REG_IF);
		b.bset(REG_TMP1, REG_TMP1, IrConst::_8(3), REG_DF);
		b.bset(REG_TMP1, REG_TMP1, IrConst::_8(6), REG_VF);
		b.bset(REG_TMP1, REG_TMP1, IrConst::_8(7), REG_NF);
		self.push8(REG_TMP1, b);
	}

	/// Pop a value off the stack and split it into the various IR flag regs.
	///
	/// Changes REG_TMP1.
	fn pop_flags(&self, b: &mut IrBuilder) {
		self.pop8(REG_TMP1, b);
		b.bit(REG_CF, REG_TMP1, IrConst::_8(0));
		b.bit(REG_ZF, REG_TMP1, IrConst::_8(1));
		b.bit(REG_IF, REG_TMP1, IrConst::_8(2));
		b.bit(REG_DF, REG_TMP1, IrConst::_8(3));
		b.bit(REG_VF, REG_TMP1, IrConst::_8(6));
		b.bit(REG_NF, REG_TMP1, IrConst::_8(7));
	}

	/// Do a comparison and set flags according to the result.
	fn cmp(&self, src1: IrReg, src2: impl Into<BuildReg>, b: &mut IrBuilder) {
		let src2 = src2.into();
		b.sub(REG_TMP1, src1, src2);
		// CF = not(src1 <u src2)
		b.ult (REG_CF,   src1, src2);
		b.bnot (REG_CF,   REG_CF);
		self.set_nz(REG_TMP1, b);
	}

	/// Do an addition and set flags according to the result.
	fn add_(&self, src: impl Into<IrSrc>, srcn: i8, b: &mut IrBuilder) {
		let src = src.into();
		b.scarryc(REG_VF, REG_A, src, REG_CF);
		b.ucarryc(REG_CF, REG_A, src, REG_CF);
		b.addc  (REG_A,  REG_A, (src, srcn), REG_CF);
		self.set_nz(REG_A, b);
	}

	/// Sets the Negative and Zero flags based on the value of the given `reg`.
	fn set_nz(&self, reg: IrReg, b: &mut IrBuilder) {
		b.slt(REG_NF, reg, IrConst::ZERO_8);
		b.eq (REG_ZF, reg, IrConst::ZERO_8);
	}

	/// Sets the Carry flag to 1 if MSB of `reg` is 1.
	fn set_c(&self, reg: IrReg, b: &mut IrBuilder) {
		b.slt(REG_CF, reg, IrConst::ZERO_8);
	}

	// TODO: are dummy reads/writes worth implementing? at least on the NES there seems to be only
	// *two* game which rely on a single dummy read, but apparently it's more common on other
	// platforms like the C64.
	//
	// NES games which rely on dummy read done by `sta $4000,X` to acknowledge pending APU IRQs
	// - Cobra Triangle
	// - Ironsword: Wizards and Warriors II
	pub(super) fn build_ir(&self, term: Option<&BBTerm>, b: &mut IrBuilder) {
		use MetaOp::*;

		match self.meta_op {
			UNK => { panic!("what the hell is an unknown instruction doing in a BB?"); }

			HLT => {
				b.halt();
			}

			// NOPs
			NOP | DOP => { // no flags changed
				// have to emit *something* or else we can end up with empty IR BBs.
				b.nop();
			}

			// ------------------------------------------------------------------------------------
			// Computation

			// Addition/subtraction ALU
			ADC => { // NZCV
				if self.addr_mode == AddrMode::IMM {
					let val = self.get_operand(b);
					self.add_(val, 0, b);
				} else {
					self.get_operand_value_into(REG_TMP1, b);
					self.add_(REG_TMP1, -1, b);
				}
			}
			SBC => { // NZCV
				if self.addr_mode == AddrMode::IMM {
					let val = self.get_operand(b);
					b.inot(REG_TMP1, val);
					self.add_(REG_TMP1, 0, b);
				} else {
					self.get_operand_value_into(REG_TMP1, b);
					b.inot(REG_TMP1, REG_TMP1);
					self.add_(REG_TMP1, -1, b);
				}
			}

			// 'crements
			DEC => { // NZ
				let addr = self.get_operand(b);
				b.load (REG_TMP1, (addr, 0));
				b.sub(REG_TMP1, REG_TMP1, IrConst::ONE_8);
				b.store((addr, 0), REG_TMP1);
				self.set_nz(REG_TMP1, b);
			}
			DEX => { // NZ
				b.sub(REG_X, REG_X, IrConst::ONE_8);
				self.set_nz(REG_X, b);
			}
			DEY => { // NZ
				b.sub(REG_Y, REG_Y, IrConst::ONE_8);
				self.set_nz(REG_Y, b);
			}
			INC => { // NZ
				let addr = self.get_operand(b);
				b.load (REG_TMP1, (addr, 0));
				b.add(REG_TMP1, REG_TMP1, IrConst::ONE_8);
				b.store((addr, 0), REG_TMP1);
				self.set_nz(REG_TMP1, b);
			}
			INX => { // NZ
				b.add(REG_X, REG_X, IrConst::ONE_8);
				self.set_nz(REG_X, b);
			}
			INY => { // NZ
				b.add(REG_Y, REG_Y, IrConst::ONE_8);
				self.set_nz(REG_Y, b);
			}

			// Bitwise ALU
			AND => { // NZ
				if self.addr_mode == AddrMode::IMM {
					let src = self.get_operand(b);
					b.iand(REG_A, REG_A, (src, 0));
				} else {
					self.get_operand_value_into(REG_TMP1, b);
					b.iand(REG_A, REG_A, REG_TMP1);
				}
				self.set_nz(REG_A, b);
			}
			ORA => { // NZ
				if self.addr_mode == AddrMode::IMM {
					let src = self.get_operand(b);
					b.ior(REG_A, REG_A, (src, 0));
				} else {
					self.get_operand_value_into(REG_TMP1, b);
					b.ior(REG_A, REG_A, REG_TMP1);
				}
				self.set_nz(REG_A, b);
			}
			EOR => { // NZ
				if self.addr_mode == AddrMode::IMM {
					let src = self.get_operand(b);
					b.ixor(REG_A, REG_A, (src, 0));
				} else {
					self.get_operand_value_into(REG_TMP1, b);
					b.ixor(REG_A, REG_A, REG_TMP1);
				}
				self.set_nz(REG_A, b);
			}
			BIT => { // NZV (NF = mem.7, VF = mem.6, ZF = whether A&op is 0)
				self.get_operand_value_into(REG_TMP1, b);
				b.bit(REG_NF,   REG_TMP1, IrConst::_8(7));
				b.bit(REG_VF,   REG_TMP1, IrConst::_8(6));
				b.iand(REG_TMP1, REG_TMP1, REG_A);
				b.eq (REG_ZF,   REG_TMP1, IrConst::ZERO_8);
			}

			// Comparisons
			CMP => { // NZC
				let opn = if self.addr_mode == AddrMode::IMM { 0 } else { -1 };
				self.get_operand_value_into(REG_TMP1, b);
				self.cmp(REG_A, (REG_TMP1, opn), b);
			}
			CPX => { // NZC
				let opn = if self.addr_mode == AddrMode::IMM { 0 } else { -1 };
				self.get_operand_value_into(REG_TMP1, b);
				self.cmp(REG_X, (REG_TMP1, opn), b);
			}
			CPY => { // NZC
				let opn = if self.addr_mode == AddrMode::IMM { 0 } else { -1 };
				self.get_operand_value_into(REG_TMP1, b);
				self.cmp(REG_Y, (REG_TMP1, opn), b);
			}

			// Shifts and rotates
			// TODO: BOY this stuff is begging to be abstracted
			ASL => { // NZC
				let addr = self.get_operand(b);
				b.load (REG_TMP1, (addr, 0));

				// if the number is "negative" then the MSB is 1 so set the carry flag
				self.set_c(REG_TMP1, b);
				b.shl(REG_TMP1, REG_TMP1, IrConst::ONE_8);
				self.set_nz(REG_TMP1, b);

				b.store((addr, 0), REG_TMP1);
			}
			ASLA => { // NZC
				self.set_c(REG_A, b);
				b.shl(REG_A, REG_A, IrConst::ONE_8);
				self.set_nz(REG_A, b);
			}
			LSR => { // NZC (NF = 0, hardcoded)
				let addr = self.get_operand(b);
				b.load (REG_TMP1, (addr, 0));

				// cf = (tmp1 & 1) (i.e. if LSB is 1, set CF)
				b.iand (REG_CF,   REG_TMP1, IrConst::ONE_8);
				b.ushr(REG_TMP1, REG_TMP1, IrConst::ONE_8);
				self.set_nz(REG_TMP1, b);

				b.store((addr, 0), REG_TMP1);
			}
			LSRA => { // NZC (NF = 0, hardcoded)
				// cf = (A & 1) (i.e. if LSB is 1, set CF)
				b.iand (REG_CF, REG_A, IrConst::ONE_8);
				b.ushr(REG_A,  REG_A, IrConst::ONE_8);
				self.set_nz(REG_A, b);
			}
			ROL => { // NZC
				let addr = self.get_operand(b);
				b.load (REG_TMP1, (addr, 0));

				b.mov(REG_TMP2, REG_CF);                     // tmp2 = cf
				self.set_c(REG_TMP1, b);                                 // cf = ( 0)
				b.shl(REG_TMP1, REG_TMP1, IrConst::ONE_8); // a = a << 1
				b.ior (REG_TMP1, REG_TMP1, REG_TMP2); // a = a | tmp2
				self.set_nz(REG_TMP1, b);

				b.store((addr, 0), REG_TMP1);
			}
			ROLA => { // NZC
				b.mov(REG_TMP2, REG_CF);               // tmp2 = cf
				self.set_c(REG_A, b);                              // cf = ( 0)
				b.shl(REG_A, REG_A, IrConst::ONE_8); // a = a << 1
				b.ior (REG_A, REG_A, REG_TMP2); // a = a | tmp2
				self.set_nz(REG_A, b);
			}
			ROR => { // NZC
				let addr = self.get_operand(b);
				b.load (REG_TMP1, (addr, 0));

				b.mov(REG_TMP2, REG_CF);     // tmp2 = cf
				b.iand  (REG_CF,   REG_TMP1, IrConst::ONE_8); // cf = (tmp1 & 1)
				b.ushr (REG_TMP1, REG_TMP1, IrConst::ONE_8); // a = a << 1
				// TODO: do this with bit set/get IR instructions
				b.shl  (REG_TMP2, REG_TMP2, IrConst::_8(7)); // tmp2 <<= 7
				b.ior   (REG_TMP1, REG_TMP1, REG_TMP2); // a = a | tmp2
				self.set_nz(REG_TMP1, b);

				b.store((addr, 0), REG_TMP1);
			}
			RORA  => { // NZC
				b.mov(REG_TMP2, REG_CF);     // tmp2 = cf
				b.iand  (REG_CF,   REG_A,    IrConst::ONE_8); // cf = (A & 1)
				b.ushr (REG_A,    REG_A,    IrConst::ONE_8); // a = a << 1
				// TODO: do this with bit set/get IR instructions
				b.shl  (REG_TMP2, REG_TMP2, IrConst::_8(7)); // tmp2 <<= 7
				b.ior   (REG_A,    REG_A,    REG_TMP2); // a = a | tmp2
				self.set_nz(REG_A, b);
			}

			// ------------------------------------------------------------------------------------
			// Flag manipulation

			CLC => { b.mov(REG_CF, IrConst::ZERO_8); }
			CLD => { b.mov(REG_DF, IrConst::ZERO_8); }
			CLI => { b.mov(REG_IF, IrConst::ZERO_8); }
			CLV => { b.mov(REG_VF, IrConst::ZERO_8); }
			SEC => { b.mov(REG_CF, IrConst::ONE_8); }
			SED => { b.mov(REG_DF, IrConst::ONE_8); }
			SEI => { b.mov(REG_IF, IrConst::ONE_8); }

			// ------------------------------------------------------------------------------------
			// Control flow

			// Jump, call, return, break
			JMP => { // no flags changed
				match self.addr_mode {
					AddrMode::LAB => {
						let dst = term.unwrap().one_explicit_successor().unwrap();
						b.branch((dst, 0));
					}
					AddrMode::IND => {
						let dst_ind = self.get_operand(b);
						b.ibranch(dst_ind);
					}
					_ => panic!(),
				}
			}
			JSR => { // no flags changed
				let term = term.unwrap();
				let dst = term.one_explicit_successor().unwrap();
				let cont = term.continuation_successor().unwrap();
				self.push_return_addr(b);
				b.call((dst, 0), cont);
			}
			RTS => { // no flags changed
				self.return_(b, true);
			}
			BRK => { // IF = 1
				self.push_return_addr(b);
				// pushed flags include break flag set to 1, which is what we want
				self.push_flags(b);
				b.mov    (REG_IF,    IrConst::ONE_8); // set IF
				b.load   (REG_TMP16, IrConst::_16(VEC_IRQ)); // read IRQ vector
				b.ibranch(REG_TMP16);     // jump to it
			}
			RTI => { // flags set from stack
				self.pop_flags(b);
				self.return_(b, false);
			}

			// Branches
			BCC => { // no flags changed
				let term = term.unwrap();
				let dst = term.one_explicit_successor().unwrap();
				let cont = term.continuation_successor().unwrap();
				b.bnot   (REG_TMP1, REG_CF);
				b.cbranch(REG_TMP1, (dst, 0), cont);
			}
			BCS => { // no flags changed
				let term = term.unwrap();
				let dst = term.one_explicit_successor().unwrap();
				let cont = term.continuation_successor().unwrap();
				b.cbranch(REG_CF,   (dst, 0), cont);
			}
			BNE => { // no flags changed
				let term = term.unwrap();
				let dst = term.one_explicit_successor().unwrap();
				let cont = term.continuation_successor().unwrap();
				b.bnot   (REG_TMP1, REG_ZF);
				b.cbranch(REG_TMP1, (dst, 0), cont);
			}
			BEQ => { // no flags changed
				let term = term.unwrap();
				let dst = term.one_explicit_successor().unwrap();
				let cont = term.continuation_successor().unwrap();
				b.cbranch(REG_ZF,   (dst, 0), cont);
			}
			BPL => { // no flags changed
				let term = term.unwrap();
				let dst = term.one_explicit_successor().unwrap();
				let cont = term.continuation_successor().unwrap();
				b.bnot   (REG_TMP1, REG_NF);
				b.cbranch(REG_TMP1, (dst, 0), cont);
			}
			BMI => { // no flags changed
				let term = term.unwrap();
				let dst = term.one_explicit_successor().unwrap();
				let cont = term.continuation_successor().unwrap();
				b.cbranch(REG_NF,   (dst, 0), cont);
			}
			BVC => { // no flags changed
				let term = term.unwrap();
				let dst = term.one_explicit_successor().unwrap();
				let cont = term.continuation_successor().unwrap();
				b.bnot   (REG_TMP1, REG_VF);
				b.cbranch(REG_TMP1, (dst, 0), cont);
			}
			BVS => { // no flags changed
				let term = term.unwrap();
				let dst = term.one_explicit_successor().unwrap();
				let cont = term.continuation_successor().unwrap();
				b.cbranch(REG_VF,   (dst, 0), cont);
			}

			// ------------------------------------------------------------------------------------
			// Data transfer

			// Loads and sores
			LDA | LDAI => { // NZ
				self.get_operand_value_into(REG_A, b);
				self.set_nz(REG_A, b);
			}
			LDX | LDXI => { // NZ
				self.get_operand_value_into(REG_X, b);
				self.set_nz(REG_X, b);
			}
			LDY | LDYI => { // NZ
				self.get_operand_value_into(REG_Y, b);
				self.set_nz(REG_Y, b);
			}
			STA => { // no flags changed
				let addr = self.get_operand(b);
				b.store((addr, 0), REG_A);
			}
			STX => { // no flags changed
				let addr = self.get_operand(b);
				b.store((addr, 0), REG_X);
			}
			STY => { // no flags changed
				let addr = self.get_operand(b);
				b.store((addr, 0), REG_Y);
			}

			// Pushes and pops
			PHA => { // no flags changed
				self.push8(REG_A, b);
			}
			PHP => { // no flags changed
				self.push_flags(b);
			}
			PLA => { // NZ
				self.pop8  (REG_A, b);
				self.set_nz(REG_A, b);
			}
			PLP => { // flags set from stack
				self.pop_flags(b);
			}

			// Transfers
			TAX => { // NZ
				b.mov(REG_X, REG_A);
				self.set_nz(REG_X, b);
			}
			TAY => { // NZ
				b.mov(REG_Y, REG_A);
				self.set_nz(REG_Y, b);
			}
			TSX => { // NZ
				b.mov(REG_X, REG_S);
				self.set_nz(REG_X, b);
			}
			TXA => { // NZ
				b.mov(REG_A, REG_X);
				self.set_nz(REG_A, b);
			}
			TXS => {  // no flags changed
				b.mov(REG_S, REG_X);
			}
			TYA => { // NZ
				b.mov(REG_A, REG_Y);
				self.set_nz(REG_A, b);
			}
		}
	}
}