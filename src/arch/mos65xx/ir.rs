
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
const REG_CF: IrReg = IrReg::reg8(4);  // Carry
const REG_ZF: IrReg = IrReg::reg8(5);  // Zero
const REG_IF: IrReg = IrReg::reg8(6);  // Interrupt
const REG_DF: IrReg = IrReg::reg8(7);  // Decimal
const REG_BF: IrReg = IrReg::reg8(8);  // Break
const REG_RF: IrReg = IrReg::reg8(9);  // Reserved
const REG_OF: IrReg = IrReg::reg8(10); // Overflow
const REG_NF: IrReg = IrReg::reg8(11); // Negative

const REG_TMP1:  IrReg = IrReg::reg8(12);  // 8-bit temporary
const REG_TMP2:  IrReg = IrReg::reg8(13);  // 8-bit temporary
// const REG_TMP3:  IrReg = IrReg::reg8(14);  // 8-bit temporary
const REG_TMP16: IrReg = IrReg::reg16(15); // 16-bit temporary

static ARG_REGS: &[IrReg] =
	&[REG_A, REG_X, REG_Y,        REG_CF, REG_ZF, REG_IF, REG_DF, REG_BF, REG_RF, REG_OF, REG_NF];

static RETURN_REGS: &[IrReg] =
	&[REG_A, REG_X, REG_Y, REG_S, REG_CF, REG_ZF, REG_IF, REG_DF, REG_BF, REG_RF, REG_OF, REG_NF];

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
					// tmp16 = *tmp16
					b.load (ea, REG_TMP2,  IrConst::_16(va),          -1, -1);
					b.load (ea, REG_TMP1,  IrConst::_16(va & 0xFF00), -1, -1);
					b.ipair(ea, REG_TMP16, REG_TMP1, REG_TMP2,        -1, -1, -1);
					b.load (ea, REG_TMP16, REG_TMP16,                 -1, 0);
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

				// tmp1 = *tmp1  (hi)
				// tmp2 = *tmp2  (lo)
				// tmp16 = tmp1:tmp2
				// tmp16 = *tmp16
				b.load (ea, REG_TMP1,  REG_TMP1,           -1, -1);
				b.load (ea, REG_TMP2,  REG_TMP2,           -1, -1);
				b.ipair(ea, REG_TMP16, REG_TMP1, REG_TMP2, -1, -1, -1);
				b.load (ea, REG_TMP16, REG_TMP16,          -1, 0);
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
				// Target
				let Operand::Mem(va, _) = i.ops()[0] else { panic!() };
				IrConst::_16(va.0 as u16).into()
			}
			// Alias for `ABS` but for `jmp`/`jsr` instructions, to distinguish their operand types.
			LAB => {
				// Target
				let Operand::Mem(va, _) = i.ops()[0] else { panic!() };
				IrConst::_16(va.0 as u16).into()
			}
		}
	}

	pub(super) fn build_ir(&self, i: &Instruction, target: Option<EA>, b: &mut IrBuilder) {
		use MetaOp::*;

		let ea = i.ea();

		match self.meta_op {
			UNK => { panic!("what the hell is an unknown instruction doing in a BB?"); }
			ADC => {

			}
			AND => {

			}
			ASLA => {

			}
			ASL => {

			}
			BCC => {

			}
			BCS => {

			}
			BEQ => {

			}
			BIT => {

			}
			BMI => {

			}
			BNE => {

			}
			BPL => {

			}
			BRK => {

			}
			BVC => {

			}
			BVS => {

			}
			CLC => {

			}
			CLD => {

			}
			CLI => {

			}
			CLV => {

			}
			CMP => {

			}
			CPX => {

			}
			CPY => {

			}
			DEC => {

			}
			DEX => {

			}
			DEY => {

			}
			EOR => {

			}
			INC => {

			}
			INX => {

			}
			INY => {

			}
			JMP => {

			}
			JSR => {

			}
			LDA => {

			}
			LDAI => {

			}
			LDX => {

			}
			LDXI => {

			}
			LDY => {

			}
			LDYI => {

			}
			LSRA => {

			}
			LSR => {

			}
			NOP | DOP => {

			}
			ORA => {

			}
			PHA => {

			}
			PHP => {

			}
			PLA => {

			}
			PLP => {

			}
			ROLA => {

			}
			ROL => {

			}
			RORA => {

			}
			ROR => {

			}
			RTI => {

			}
			RTS => {

			}
			SBC => {

			}
			SEC => {

			}
			SED => {

			}
			SEI => {

			}
			STA => {

			}
			STX => {

			}
			STY => {

			}
			TAX => {

			}
			TAY => {

			}
			TSX => {

			}
			TXA => {

			}
			TXS => {

			}
			TYA => {

			}
		}
	}
}