//! IR compiler for MOS 65xx.
//!
//! Referenced from Mesen 2 source code (Mesen2/Core/NES/NesCpu.cpp/h) for accuracy.

use crate::arch::{ IIrCompiler };
use crate::program::{ MemIndir };
use crate::ir::{ IrReg, IrConst, IrSrc, IrBuilder };

use super::*;

// ------------------------------------------------------------------------------------------------
// IR
// ------------------------------------------------------------------------------------------------

pub(crate) struct Mos65xxIrCompiler;

impl IIrCompiler for Mos65xxIrCompiler {
	fn build_ir(&self, i: &Instruction, target: Option<EA>, b: &mut IrBuilder) {
		lookup_desc(i.bytes()[0]).build_ir(i, target, b);
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
	/// May affect REG_TMP1, REG_TMP2, or REG_TMP16.
	///
	/// Returned source is either a constant, REG_TMP2 (for zero-page addresses), or REG_TMP16.
	///
	/// Panics if called on an instruction with implicit addressing. Caller is responsible for that.
	fn get_operand(&self, i: &Instruction, b: &mut IrBuilder) -> IrSrc {
		let ea = i.ea();
		use AddrMode::*;
		use MemIndir::*;

		match self.addr_mode {
			IMP => { panic!("get_operand shouldn't be called on instructions with no operand"); }
			// Immediate (1 byte), e.g. `lda #$30`.
			IMM => {
				let Operand::UImm(val, _) = i.ops()[0] else { panic!() };
				IrConst::_8(val as u8).into()
			}
			// Zero-page absolute (1 byte), e.g. `lda $10`.
			ZPG => {
				// R|W|RW
				let Operand::Mem(va, _) = i.ops()[0] else { panic!() };
				IrConst::_16(va.0 as u16).into()
			}
			// Zero-page, X- or Y-indexed (1 byte), e.g. `lda $80,X`.
			// Never crosses out of page 0, so high byte of address is always 0.
			ZPX | ZPY => {
				// ZPX: R|W|RW
				// ZPY: R|W
				let Operand::Indir(RegDisp { reg, disp }, _) = i.ops()[0] else { panic!() };

				// using tmp2 here so that resulting address is in the range [0, 255].
				// tmp2 = reg + disp
				b.iuadd(ea, REG_TMP2, reg_to_ir_reg(reg), IrConst::_8(disp as u8), -1, -1,  0);
				REG_TMP2.into()
			}
			// Absolute (2 bytes), e.g. `lda $8040`.
			ABS => {
				// R|W|RW
				let Operand::Mem(va, _) = i.ops()[0] else { panic!() };
				IrConst::_16(va.0 as u16).into()
			}
			// Absolute, X- or Y-indexed (2 bytes), e.g. `lda $8040,X`
			// *Can* cross page boundaries, so does a full 16-bit add.
			ABX | ABY => {
				// ABX: R|W|RW
				// ABY: R|W
				let Operand::Indir(RegDisp { reg, disp }, _) = i.ops()[0] else { panic!() };

				// tmp16 = reg + disp
				b.iuadd(ea, REG_TMP16, reg_to_ir_reg(reg), IrConst::_16(disp as u16), -1, -1, 0);
				REG_TMP16.into()
			}
			// Indirect (2 bytes); used only for indirect jump i.e. `jmp ($2000)`.
			IND => {
				// R
				let Operand::Mem(va, _) = i.ops()[0] else { panic!() };
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
					b.load (ea, REG_TMP2,  IrConst::_16(va),          -1, -1);
					b.load (ea, REG_TMP1,  IrConst::_16(va & 0xFF00), -1, -1);
					b.ipair(ea, REG_TMP16, REG_TMP1, REG_TMP2,        -1, -1, -1);
				} else {
					// ez mode

					// tmp16 = *va
					b.load(ea, REG_TMP16, IrConst::_16(va), -1, 0);
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
				let Operand::Indir(RegDisp { disp, .. }, _) = i.ops()[0] else { panic!() };

				// tmp2 = X + disp  (wraps at 8 bits)
				b.iuadd(ea, REG_TMP2, REG_X, IrConst::_8(disp as u8), -1, -1,  0);

				// now tmp2 points to a location in the zero page, but we don't statically know
				// whether tmp2 + 1 would wrap around or not, so we have to do it the Long Way.

				// tmp1 = tmp2 + 1  (wraps at 8 bits)
				b.iuadd(ea, REG_TMP1, REG_TMP2, IrConst::ONE_8, -1, -1, -1);

				// tmp2 = *tmp2  (lo)
				// tmp1 = *tmp1  (hi)
				// tmp16 = tmp1:tmp2
				b.load (ea, REG_TMP2,  REG_TMP2,           -1, -1);
				b.load (ea, REG_TMP1,  REG_TMP1,           -1, -1);
				b.ipair(ea, REG_TMP16, REG_TMP1, REG_TMP2, -1, -1, -1);
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
				let Operand::Mem(va, _) = i.ops()[0] else { panic!() };
				let va = va.0;

				if va == 0xFF {
					// wrap that address

					// tmp2  = *0x00FF  (lo)
					// tmp1  = *0x0000  (hi)
					// tmp16 = tmp1:tmp2
					b.load (ea, REG_TMP2,  IrConst::_8(0xFF),  -1, 0);
					b.load (ea, REG_TMP1,  IrConst::ZERO_8,    -1, 0);
					b.ipair(ea, REG_TMP16, REG_TMP1, REG_TMP2, -1, -1, -1);
				} else {
					// tmp16 = *va
					b.load(ea, REG_TMP16, IrConst::_8(va as u8), -1, 0);
				}

				// tmp16 = tmp16 + Y
				b.iuadd(ea, REG_TMP16, REG_TMP16, REG_Y, -1, -1, -1);
				REG_TMP16.into()
			}
			// PC-relative (1 byte), e.g. `bcc whatever`.
			// Signed offset added to PC (+2 for size of branch instruction).
			REL => {
				panic!("get_operand shouldn't be called on instructions with control flow targets");
				// Target
				// let Operand::Mem(va, _) = i.ops()[0] else { panic!() };
				// IrConst::_16(va.0 as u16).into()
			}
			// Alias for `ABS` but for `jmp`/`jsr` instructions, to distinguish their operand types.
			LAB => {
				panic!("get_operand shouldn't be called on instructions with control flow targets");
				// Target
				// let Operand::Mem(va, _) = i.ops()[0] else { panic!() };
				// IrConst::_16(va.0 as u16).into()
			}
		}
	}

	/// Gets the actual value of the operand, performing a load if needed, and places the value
	/// into `dst`.
	fn get_operand_value_into(&self, dst: IrReg, i: &Instruction, b: &mut IrBuilder) {
		let ea = i.ea();

		use AddrMode::*;

		match self.addr_mode {
			IMP | REL | LAB | IND => {
				panic!("get_operand_value_into should not be called on this");
			}
			IMM => {
				// just a constant, assign it
				let val = self.get_operand(i, b);
				b.assign(ea, dst, val, -1, 0);
			}
			ZPG | ABS => {
				// needs a load, and that load references the operand
				let addr = self.get_operand(i, b);
				b.load(ea, dst, addr, -1, 0);
			}
			ZPX | ZPY | ABX | ABY | IZX | IZY => {
				// needs a load, but that load does *not* reference the operand
				let addr = self.get_operand(i, b);
				b.load(ea, dst, addr, -1, -1);
			}
		}
	}

	/// Push an 8-bit value `src` onto the stack.
	fn push8(&self, src: impl Into<IrSrc>, i: &Instruction, b: &mut IrBuilder) {
		let ea = i.ea();
		// empty stack convention - store before subtracting
		b.ipair(ea, REG_TMP16, IrConst::ONE_8, REG_S, -1, -1, -1);
		b.store(ea, REG_TMP16, src,                   -1, -1);
		b.iusub(ea, REG_S, REG_S, IrConst::_8(1),     -1, -1, -1);
	}

	/// Pop an 8-bit value off the stack into `dst`.
	fn pop8(&self, dst: IrReg, i: &Instruction, b: &mut IrBuilder) {
		let ea = i.ea();
		// empty stack convention - add before loading
		b.iuadd(ea, REG_S, REG_S, IrConst::_8(1),     -1, -1, -1);
		b.ipair(ea, REG_TMP16, IrConst::ONE_8, REG_S, -1, -1, -1);
		b.load (ea, dst, REG_TMP16,                   -1, -1);
	}

	/// Push the return address to the stack. It's always the instruction's address + 2.
	fn push_return_addr(&self, i: &Instruction, b: &mut IrBuilder) {
	 	let ret_addr = (i.va().0 + 2) as u16;
		// push hi then lo
		self.push8(IrConst::_8((ret_addr >> 8  ) as u8), i, b);
		self.push8(IrConst::_8((ret_addr & 0xFF) as u8), i, b);
	}

	/// Pop the return address from the stack and `ret` to it.
	fn return_(&self, i: &Instruction, b: &mut IrBuilder) {
		let ea = i.ea();
		// pop lo then hi
		self.pop8(REG_TMP2, i, b);
		self.pop8(REG_TMP1, i, b);
		b.ipair(ea, REG_TMP16, REG_TMP1, REG_TMP2, -1, -1, -1);
		b.ret  (ea, REG_TMP16,                     -1);
	}

	/// Combine all flags IR regs into a single 8-bit value and push it.
	///
	/// Changes REG_TMP1.
	fn push_flags(&self, i: &Instruction, b: &mut IrBuilder) {
		// TODO: need bit set/get IR instructions...

		// 0 Carry
		// 1 Zero
		// 2 Interrupt
		// 3 Decimal
		// the values of bits 4 and 5 (Break and Reserved) are always 1 when pushed.
		// 6 oVerflow
		// 7 Negative

		b.assign(i.ea(), REG_TMP1, IrConst::ZERO_8, -1, -1);
		self.push8(REG_TMP1, i, b);
	}

	/// Pop a value off the stack and split it into the various IR flag regs.
	///
	/// Changes REG_TMP1.
	fn pop_flags(&self, i: &Instruction, b: &mut IrBuilder) {
		let ea = i.ea();
		self.pop8(REG_TMP1, i, b);

		// TODO: need bit set/get IR instructions...

		// temporarily assigning from TMP1 to prevent constprop from thinking these are constants
		b.assign(ea, REG_CF, REG_TMP1, -1, -1); // 0 Carry
		b.assign(ea, REG_ZF, REG_TMP1, -1, -1); // 1 Zero
		b.assign(ea, REG_IF, REG_TMP1, -1, -1); // 2 Interrupt
		b.assign(ea, REG_DF, REG_TMP1, -1, -1); // 3 Decimal
		b.assign(ea, REG_VF, REG_TMP1, -1, -1); // 6 oVerflow
		b.assign(ea, REG_NF, REG_TMP1, -1, -1); // 7 Negative
	}

	/// Do a comparison and set flags according to the result.
	fn cmp(&self, src1: IrReg, src2: IrReg, src2n: i8, i: &Instruction, b: &mut IrBuilder) {
		let ea = i.ea();
		b.iusub(ea, REG_TMP1, src1, src2, -1, -1, src2n);
		// CF = not(src1 <u src2)
		b.iult (ea, REG_CF,   src1, src2, -1, -1, -1);
		b.bnot (ea, REG_CF,   REG_CF,     -1, -1);
		self.set_nz(REG_TMP1, i, b);
	}

	/// Do an addition and set flags according to the result.
	fn add_(&self, src: impl Into<IrSrc>, srcn: i8, i: &Instruction, b: &mut IrBuilder) {
		let ea = i.ea();
		let src = src.into();

		b.iuaddc  (ea, REG_A,  REG_A, src, REG_CF, -1, -1, srcn, -1);
		b.iscarryc(ea, REG_VF, REG_A, src, REG_CF, -1, -1,   -1, -1);
		b.icarryc (ea, REG_CF, REG_A, src, REG_CF, -1, -1,   -1, -1);
		self.set_nz(REG_A, i, b);
	}

	/// Sets the Negative and Zero flags based on the value of the given `reg`.
	fn set_nz(&self, reg: IrReg, i: &Instruction, b: &mut IrBuilder) {
		let ea = i.ea();
		b.islt(ea, REG_NF, reg, IrConst::ZERO_8,   -1, -1, -1);
		b.ieq (ea, REG_ZF, reg, IrConst::ZERO_8,   -1, -1, -1);
	}

	/// Sets the Carry flag to 1 if MSB of `reg` is 1.
	fn set_c(&self, reg: IrReg, i: &Instruction, b: &mut IrBuilder) {
		b.islt(i.ea(), REG_CF, reg, IrConst::ZERO_8,   -1, -1, -1);
	}

	// TODO: are dummy reads/writes worth implementing? at least on the NES there seems to be only
	// *two* game which rely on a single dummy read, but apparently it's more common on other
	// platforms like the C64.
	//
	// NES games which rely on dummy read done by `sta $4000,X` to acknowledge pending APU IRQs
	// - Cobra Triangle
	// - Ironsword: Wizards and Warriors II
	pub(super) fn build_ir(&self, i: &Instruction, target: Option<EA>, b: &mut IrBuilder) {
		use MetaOp::*;

		let ea = i.ea();

		match self.meta_op {
			UNK => { panic!("what the hell is an unknown instruction doing in a BB?"); }

			// NOPs
			NOP | DOP => { // no flags changed
				// uhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh nothing.
				// really these only matter for cycle counting in emulation but...
			}

			// ------------------------------------------------------------------------------------
			// Computation

			// Addition/subtraction ALU
			ADC => { // NZCV
				if self.addr_mode == AddrMode::IMM {
					let val = self.get_operand(i, b);
					self.add_(val, 0, i, b);
				} else {
					self.get_operand_value_into(REG_TMP1, i, b);
					self.add_(REG_TMP1, -1, i, b);
				}
			}
			SBC => { // NZCV
				if self.addr_mode == AddrMode::IMM {
					let val = self.get_operand(i, b);
					b.inot(ea, REG_TMP1, val, -1, -1);
					self.add_(REG_TMP1, 0, i, b);
				} else {
					self.get_operand_value_into(REG_TMP1, i, b);
					b.inot(ea, REG_TMP1, REG_TMP1, -1, -1);
					self.add_(REG_TMP1, -1, i, b);
				}
			}

			// 'crements
			DEC => { // NZ
				let addr = self.get_operand(i, b);
				b.load (ea, REG_TMP1, addr,                     -1, 0);
				b.iusub(ea, REG_TMP1, REG_TMP1, IrConst::ONE_8, -1, -1, -1);
				b.store(ea, REG_TMP1, addr,                     -1, 0);
				self.set_nz(REG_TMP1, i, b);
			}
			DEX => { // NZ
				b.iusub(ea, REG_X, REG_X, IrConst::ONE_8, -1, -1, -1);
				self.set_nz(REG_X, i, b);
			}
			DEY => { // NZ
				b.iusub(ea, REG_Y, REG_Y, IrConst::ONE_8, -1, -1, -1);
				self.set_nz(REG_Y, i, b);
			}
			INC => { // NZ
				let addr = self.get_operand(i, b);
				b.load (ea, REG_TMP1, addr,                     -1, 0);
				b.iuadd(ea, REG_TMP1, REG_TMP1, IrConst::ONE_8, -1, -1, -1);
				b.store(ea, REG_TMP1, addr,                     -1, 0);
				self.set_nz(REG_TMP1, i, b);
			}
			INX => { // NZ
				b.iuadd(ea, REG_X, REG_X, IrConst::ONE_8, -1, -1, -1);
				self.set_nz(REG_X, i, b);
			}
			INY => { // NZ
				b.iuadd(ea, REG_Y, REG_Y, IrConst::ONE_8, -1, -1, -1);
				self.set_nz(REG_Y, i, b);
			}

			// Bitwise ALU
			AND => { // NZ
				let src = self.get_operand(i, b);
				b.iand(ea, REG_A, REG_A, src, -1, -1, 0);
				self.set_nz(REG_A, i, b);
			}
			ORA => { // NZ
				let src = self.get_operand(i, b);
				b.ior(ea, REG_A, REG_A, src, -1, -1, 0);
				self.set_nz(REG_A, i, b);
			}
			EOR => { // NZ
				let src = self.get_operand(i, b);
				b.ixor(ea, REG_A, REG_A, src, -1, -1, 0);
				self.set_nz(REG_A, i, b);
			}
			BIT => { // NZV (NF = mem.7, VF = mem.6, ZF = whether A&op is 0)
				self.get_operand_value_into(REG_TMP1, i, b);

				// TODO: need bit set/get IR instructions...
				// temporarily doing this to prevent constprop from thinking these are constants
				b.assign(ea, REG_NF,   REG_TMP1,                  -1, -1);
				b.assign(ea, REG_VF,   REG_TMP1,                  -1, -1);

				b.iand  (ea, REG_TMP1, REG_TMP1, REG_A,           -1, -1, -1);
				b.ieq   (ea, REG_ZF,   REG_TMP1, IrConst::ZERO_8, -1, -1, -1);
			}

			// Comparisons
			CMP => { // NZC
				let opn = if self.addr_mode == AddrMode::IMM { 0 } else { -1 };
				self.get_operand_value_into(REG_TMP1, i, b);
				self.cmp(REG_A, REG_TMP1, opn, i, b);
			}
			CPX => { // NZC
				let opn = if self.addr_mode == AddrMode::IMM { 0 } else { -1 };
				self.get_operand_value_into(REG_TMP1, i, b);
				self.cmp(REG_X, REG_TMP1, opn, i, b);
			}
			CPY => { // NZC
				let opn = if self.addr_mode == AddrMode::IMM { 0 } else { -1 };
				self.get_operand_value_into(REG_TMP1, i, b);
				self.cmp(REG_Y, REG_TMP1, opn, i, b);
			}

			// Shifts and rotates
			// TODO: BOY this stuff is begging to be abstracted
			ASL => { // NZC
				let addr = self.get_operand(i, b);
				b.load (ea, REG_TMP1, addr,                     -1, 0);

				// if the number is "negative" then the MSB is 1 so set the carry flag
				self.set_c(REG_TMP1, i, b);
				b.ishl(ea, REG_TMP1, REG_TMP1, IrConst::ONE_8,  -1, -1, -1);
				self.set_nz(REG_TMP1, i, b);

				b.store(ea, REG_TMP1, addr,                     -1, 0);
			}
			ASLA => { // NZC
				self.set_c(REG_A, i, b);
				b.ishl(ea, REG_A, REG_A, IrConst::ONE_8, -1, -1, -1);
				self.set_nz(REG_A, i, b);
			}
			LSR => { // NZC (NF = 0, hardcoded)
				let addr = self.get_operand(i, b);
				b.load (ea, REG_TMP1, addr, -1, 0);

				// cf = (tmp1 & 1) (i.e. if LSB is 1, set CF)
				b.iand (ea, REG_CF,   REG_TMP1, IrConst::ONE_8, -1, -1, -1);
				b.iushr(ea, REG_TMP1, REG_TMP1, IrConst::ONE_8, -1, -1, -1);
				self.set_nz(REG_TMP1, i, b);

				b.store(ea, REG_TMP1, addr, -1, 0);
			}
			LSRA => { // NZC (NF = 0, hardcoded)
				// cf = (A & 1) (i.e. if LSB is 1, set CF)
				b.iand (ea, REG_CF, REG_A, IrConst::ONE_8, -1, -1, -1);
				b.iushr(ea, REG_A,  REG_A, IrConst::ONE_8, -1, -1, -1);
				self.set_nz(REG_A, i, b);
			}
			ROL => { // NZC
				let addr = self.get_operand(i, b);
				b.load (ea, REG_TMP1, addr, -1, 0);

				b.assign(ea, REG_TMP2, REG_CF, -1, -1);                     // tmp2 = cf
				self.set_c(REG_TMP1, i, b);                                 // cf = (a < 0)
				b.ishl(ea, REG_TMP1, REG_TMP1, IrConst::ONE_8, -1, -1, -1); // a = a << 1
				b.ior (ea, REG_TMP1, REG_TMP1, REG_TMP2,       -1, -1, -1); // a = a | tmp2
				self.set_nz(REG_TMP1, i, b);

				b.store(ea, REG_TMP1, addr, -1, 0);
			}
			ROLA => { // NZC
				b.assign(ea, REG_TMP2, REG_CF, -1, -1);               // tmp2 = cf
				self.set_c(REG_A, i, b);                              // cf = (a < 0)
				b.ishl(ea, REG_A, REG_A, IrConst::ONE_8, -1, -1, -1); // a = a << 1
				b.ior (ea, REG_A, REG_A, REG_TMP2,       -1, -1, -1); // a = a | tmp2
				self.set_nz(REG_A, i, b);
			}
			ROR => { // NZC
				let addr = self.get_operand(i, b);
				b.load (ea, REG_TMP1, addr, -1, 0);

				b.assign(ea, REG_TMP2, REG_CF,                   -1, -1);     // tmp2 = cf
				b.iand  (ea, REG_CF,   REG_TMP1, IrConst::ONE_8, -1, -1, -1); // cf = (tmp1 & 1)
				b.iushr (ea, REG_TMP1, REG_TMP1, IrConst::ONE_8, -1, -1, -1); // a = a << 1
				// TODO: do this with bit set/get IR instructions
				b.ishl  (ea, REG_TMP2, REG_TMP2, IrConst::_8(7), -1, -1, -1); // tmp2 <<= 7
				b.ior   (ea, REG_TMP1, REG_TMP1, REG_TMP2,       -1, -1, -1); // a = a | tmp2
				self.set_nz(REG_TMP1, i, b);

				b.store(ea, REG_TMP1, addr, -1, 0);
			}
			RORA  => { // NZC
				b.assign(ea, REG_TMP2, REG_CF,                   -1, -1);     // tmp2 = cf
				b.iand  (ea, REG_CF,   REG_A,    IrConst::ONE_8, -1, -1, -1); // cf = (A & 1)
				b.iushr (ea, REG_A,    REG_A,    IrConst::ONE_8, -1, -1, -1); // a = a << 1
				// TODO: do this with bit set/get IR instructions
				b.ishl  (ea, REG_TMP2, REG_TMP2, IrConst::_8(7), -1, -1, -1); // tmp2 <<= 7
				b.ior   (ea, REG_A,    REG_A,    REG_TMP2,       -1, -1, -1); // a = a | tmp2
				self.set_nz(REG_A, i, b);
			}

			// ------------------------------------------------------------------------------------
			// Flag manipulation

			CLC => { b.assign(ea, REG_CF, IrConst::ZERO_8, -1, -1); }
			CLD => { b.assign(ea, REG_DF, IrConst::ZERO_8, -1, -1); }
			CLI => { b.assign(ea, REG_IF, IrConst::ZERO_8, -1, -1); }
			CLV => { b.assign(ea, REG_VF, IrConst::ZERO_8, -1, -1); }
			SEC => { b.assign(ea, REG_CF, IrConst::ONE_8,  -1, -1); }
			SED => { b.assign(ea, REG_DF, IrConst::ONE_8,  -1, -1); }
			SEI => { b.assign(ea, REG_IF, IrConst::ONE_8,  -1, -1); }

			// ------------------------------------------------------------------------------------
			// Control flow

			// Jump, call, return, break
			JMP => { // no flags changed
				match self.addr_mode {
					AddrMode::LAB => {
						b.branch(ea, target.unwrap(), 0);
					}
					AddrMode::IND => {
						let target_ind = self.get_operand(i, b);
						b.ibranch(ea, target_ind, -1);
					}
					_ => panic!(),
				}
			}
			JSR => { // no flags changed
				self.push_return_addr(i, b);
				b.call(ea, target.unwrap(), 0);
			}
			RTS => { // no flags changed
				self.return_(i, b);
			}
			BRK => { // IF = 1
				self.push_return_addr(i, b);
				self.push_flags(i, b);
				b.assign (ea, REG_IF,    IrConst::ONE_8,        -1, -1); // set IF
				b.load   (ea, REG_TMP16, IrConst::_16(VEC_IRQ), -1, -1); // read IRQ vector
				b.ibranch(ea, REG_TMP16,                        -1);     // jump to it
			}
			RTI => { // flags set from stack
				self.pop_flags(i, b);
				self.return_(i, b);
			}

			// Branches
			BCC => { // no flags changed
				b.bnot   (ea, REG_TMP1, REG_CF,          -1, -1);
				b.cbranch(ea, REG_TMP1, target.unwrap(), -1, 0);
			}
			BCS => { // no flags changed
				b.cbranch(ea, REG_CF,   target.unwrap(), -1, 0);
			}
			BNE => { // no flags changed
				b.bnot   (ea, REG_TMP1, REG_ZF,          -1, -1);
				b.cbranch(ea, REG_TMP1, target.unwrap(), -1, 0);
			}
			BEQ => { // no flags changed
				b.cbranch(ea, REG_ZF,   target.unwrap(), -1, 0);
			}
			BPL => { // no flags changed
				b.bnot   (ea, REG_TMP1, REG_NF,          -1, -1);
				b.cbranch(ea, REG_TMP1, target.unwrap(), -1, 0);
			}
			BMI => { // no flags changed
				b.cbranch(ea, REG_NF,   target.unwrap(), -1, 0);
			}
			BVC => { // no flags changed
				b.bnot   (ea, REG_TMP1, REG_VF,          -1, -1);
				b.cbranch(ea, REG_TMP1, target.unwrap(), -1, 0);
			}
			BVS => { // no flags changed
				b.cbranch(ea, REG_VF,   target.unwrap(), -1, 0);
			}

			// ------------------------------------------------------------------------------------
			// Data transfer

			// Loads and sores
			LDA | LDAI => { // NZ
				self.get_operand_value_into(REG_A, i, b);
				self.set_nz(REG_A, i, b);
			}
			LDX | LDXI => { // NZ
				self.get_operand_value_into(REG_X, i, b);
				self.set_nz(REG_X, i, b);
			}
			LDY | LDYI => { // NZ
				self.get_operand_value_into(REG_Y, i, b);
				self.set_nz(REG_Y, i, b);
			}
			STA => { // no flags changed
				let addr = self.get_operand(i, b);
				b.store(ea, addr, REG_A, 0, -1);
			}
			STX => { // no flags changed
				let addr = self.get_operand(i, b);
				b.store(ea, addr, REG_X, 0, -1);
			}
			STY => { // no flags changed
				let addr = self.get_operand(i, b);
				b.store(ea, addr, REG_Y, 0, -1);
			}

			// Pushes and pops
			PHA => { // no flags changed
				self.push8(REG_A, i, b);
			}
			PHP => { // no flags changed
				self.push_flags(i, b);
			}
			PLA => { // NZ
				self.pop8  (REG_A, i, b);
				self.set_nz(REG_A, i, b);
			}
			PLP => { // flags set from stack
				self.pop_flags(i, b);
			}

			// Transfers
			TAX => { // NZ
				b.assign(ea, REG_X, REG_A, -1, -1);
				self.set_nz(REG_X, i, b);
			}
			TAY => { // NZ
				b.assign(ea, REG_Y, REG_A, -1, -1);
				self.set_nz(REG_Y, i, b);
			}
			TSX => { // NZ
				b.assign(ea, REG_X, REG_S, -1, -1);
				self.set_nz(REG_X, i, b);
			}
			TXA => { // NZ
				b.assign(ea, REG_A, REG_X, -1, -1);
				self.set_nz(REG_A, i, b);
			}
			TXS => {  // no flags changed
				b.assign(ea, REG_S, REG_X, -1, -1);
			}
			TYA => { // NZ
				b.assign(ea, REG_A, REG_Y, -1, -1);
				self.set_nz(REG_A, i, b);
			}
		}
	}
}