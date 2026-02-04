
use crate::program::{ MemIndir, Operand };
use crate::memory::{ MemAccess, VA };
use super::{ InstDesc, Opcode, MetaOp, AddrMode, Reg };

// ------------------------------------------------------------------------------------------------
// InstDesc table
// ------------------------------------------------------------------------------------------------

pub(super) fn lookup_desc(opcode: u8) -> InstDesc {
	INST_DESCS[opcode as usize]
}

use Opcode::*;
use MetaOp::*;
use AddrMode::*;
use MemAccess::{ R, W, RW, Target };
use MemIndir::*;

const OP_INVALID: InstDesc =
	InstDesc { opcode: INVALID, meta_op: UNK, addr_mode: IMP, access: None };

fn direct_r (a: u64) -> Operand { Operand::Mem(VA(a as usize), R) }
fn direct_w (a: u64) -> Operand { Operand::Mem(VA(a as usize), W) }
fn direct_rw(a: u64) -> Operand { Operand::Mem(VA(a as usize), RW) }
fn target   (a: u64) -> Operand { Operand::Mem(VA(a as usize), Target) }
fn indirx_r (a: u64) -> Operand {
	Operand::Indir(RegDisp { reg: Reg::X as u8, disp: a as i64 }, R)
}
fn indirx_w (a: u64) -> Operand {
	Operand::Indir(RegDisp { reg: Reg::X as u8, disp: a as i64 }, W)
}
fn indirx_rw(a: u64) -> Operand {
	Operand::Indir(RegDisp { reg: Reg::X as u8, disp: a as i64 }, RW)
}
fn indiry_r (a: u64) -> Operand {
	Operand::Indir(RegDisp { reg: Reg::Y as u8, disp: a as i64 }, R)
}
fn indiry_w (a: u64) -> Operand {
	Operand::Indir(RegDisp { reg: Reg::Y as u8, disp: a as i64 }, W)
}

const INST_DESCS: &[InstDesc] = &[
	// 0x00
	InstDesc { opcode: BRK_IMM, meta_op: BRK,  addr_mode: IMM, access: None },
	InstDesc { opcode: ORA_IZX, meta_op: ORA,  addr_mode: IZX, access: Some(indirx_r) },
	OP_INVALID,
	OP_INVALID,
	InstDesc { opcode: DOP_04,  meta_op: DOP,  addr_mode: IMM, access: None },
	InstDesc { opcode: ORA_ZPG, meta_op: ORA,  addr_mode: ZPG, access: Some(direct_r) },
	InstDesc { opcode: ASL_ZPG, meta_op: ASL,  addr_mode: ZPG, access: Some(direct_rw) },
	OP_INVALID,
	InstDesc { opcode: PHP_IMP, meta_op: PHP,  addr_mode: IMP, access: None },
	InstDesc { opcode: ORA_IMM, meta_op: ORA,  addr_mode: IMM, access: None },
	InstDesc { opcode: ASL_IMP, meta_op: ASLA, addr_mode: IMP, access: None },
	OP_INVALID,
	OP_INVALID,
	InstDesc { opcode: ORA_ABS, meta_op: ORA,  addr_mode: ABS, access: Some(direct_r) },
	InstDesc { opcode: ASL_ABS, meta_op: ASL,  addr_mode: ABS, access: Some(direct_rw) },
	OP_INVALID,

	// 0x10
	InstDesc { opcode: BPL_REL, meta_op: BPL,  addr_mode: REL, access: Some(target) },
	InstDesc { opcode: ORA_IZY, meta_op: ORA,  addr_mode: IZY, access: Some(direct_r) },
	OP_INVALID,
	OP_INVALID,
	InstDesc { opcode: DOP_14,  meta_op: DOP,  addr_mode: IMM, access: None },
	InstDesc { opcode: ORA_ZPX, meta_op: ORA,  addr_mode: ZPX, access: Some(indirx_r) },
	InstDesc { opcode: ASL_ZPX, meta_op: ASL,  addr_mode: ZPX, access: Some(indirx_rw) },
	OP_INVALID,
	InstDesc { opcode: CLC_IMP, meta_op: CLC,  addr_mode: IMP, access: None },
	InstDesc { opcode: ORA_ABY, meta_op: ORA,  addr_mode: ABY, access: Some(indiry_r) },
	OP_INVALID,
	OP_INVALID,
	OP_INVALID,
	InstDesc { opcode: ORA_ABX, meta_op: ORA,  addr_mode: ABX, access: Some(indirx_r) },
	InstDesc { opcode: ASL_ABX, meta_op: ASL,  addr_mode: ABX, access: Some(indirx_rw) },
	OP_INVALID,

	// 0x20
	InstDesc { opcode: JSR_LAB, meta_op: JSR,  addr_mode: LAB, access: Some(target) },
	InstDesc { opcode: AND_IZX, meta_op: AND,  addr_mode: IZX, access: Some(indirx_r) },
	OP_INVALID,
	OP_INVALID,
	InstDesc { opcode: BIT_ZPG, meta_op: BIT,  addr_mode: ZPG, access: Some(direct_r) },
	InstDesc { opcode: AND_ZPG, meta_op: AND,  addr_mode: ZPG, access: Some(direct_r) },
	InstDesc { opcode: ROL_ZPG, meta_op: ROL,  addr_mode: ZPG, access: Some(direct_rw) },
	OP_INVALID,
	InstDesc { opcode: PLP_IMP, meta_op: PLP,  addr_mode: IMP, access: None },
	InstDesc { opcode: AND_IMM, meta_op: AND,  addr_mode: IMM, access: None },
	InstDesc { opcode: ROL_IMP, meta_op: ROLA, addr_mode: IMP, access: None },
	OP_INVALID,
	InstDesc { opcode: BIT_ABS, meta_op: BIT,  addr_mode: ABS, access: Some(direct_r) },
	InstDesc { opcode: AND_ABS, meta_op: AND,  addr_mode: ABS, access: Some(direct_r) },
	InstDesc { opcode: ROL_ABS, meta_op: ROL,  addr_mode: ABS, access: Some(direct_rw) },
	OP_INVALID,

	// 0x30
	InstDesc { opcode: BMI_REL, meta_op: BMI,  addr_mode: REL, access: Some(target) },
	InstDesc { opcode: AND_IZY, meta_op: AND,  addr_mode: IZY, access: Some(direct_r) },
	OP_INVALID,
	OP_INVALID,
	InstDesc { opcode: DOP_34,  meta_op: DOP,  addr_mode: IMM, access: None },
	InstDesc { opcode: AND_ZPX, meta_op: AND,  addr_mode: ZPX, access: Some(indirx_r) },
	InstDesc { opcode: ROL_ZPX, meta_op: ROL,  addr_mode: ZPX, access: Some(indirx_rw) },
	OP_INVALID,
	InstDesc { opcode: SEC_IMP, meta_op: SEC,  addr_mode: IMP, access: None },
	InstDesc { opcode: AND_ABY, meta_op: AND,  addr_mode: ABY, access: Some(indiry_r) },
	OP_INVALID,
	OP_INVALID,
	OP_INVALID,
	InstDesc { opcode: AND_ABX, meta_op: AND,  addr_mode: ABX, access: Some(indirx_r) },
	InstDesc { opcode: ROL_ABX, meta_op: ROL,  addr_mode: ABX, access: Some(indirx_rw) },
	OP_INVALID,

	// 0x40
	InstDesc { opcode: RTI_IMP, meta_op: RTI,  addr_mode: IMP, access: None },
	InstDesc { opcode: EOR_IZX, meta_op: EOR,  addr_mode: IZX, access: Some(indirx_r) },
	OP_INVALID,
	OP_INVALID,
	InstDesc { opcode: DOP_44,  meta_op: DOP,  addr_mode: IMM, access: None },
	InstDesc { opcode: EOR_ZPG, meta_op: EOR,  addr_mode: ZPG, access: Some(direct_r) },
	InstDesc { opcode: LSR_ZPG, meta_op: LSR,  addr_mode: ZPG, access: Some(direct_rw) },
	OP_INVALID,
	InstDesc { opcode: PHA_IMP, meta_op: PHA,  addr_mode: IMP, access: None },
	InstDesc { opcode: EOR_IMM, meta_op: EOR,  addr_mode: IMM, access: None },
	InstDesc { opcode: LSR_IMP, meta_op: LSRA, addr_mode: IMP, access: None },
	OP_INVALID,
	InstDesc { opcode: JMP_LAB, meta_op: JMP,  addr_mode: LAB, access: Some(target) },
	InstDesc { opcode: EOR_ABS, meta_op: EOR,  addr_mode: ABS, access: Some(direct_r) },
	InstDesc { opcode: LSR_ABS, meta_op: LSR,  addr_mode: ABS, access: Some(direct_rw) },
	OP_INVALID,

	// 0x50
	InstDesc { opcode: BVC_REL, meta_op: BVC,  addr_mode: REL, access: Some(target) },
	InstDesc { opcode: EOR_IZY, meta_op: EOR,  addr_mode: IZY, access: Some(direct_r) },
	OP_INVALID,
	OP_INVALID,
	InstDesc { opcode: DOP_54,  meta_op: DOP,  addr_mode: IMM, access: None },
	InstDesc { opcode: EOR_ZPX, meta_op: EOR,  addr_mode: ZPX, access: Some(indirx_r) },
	InstDesc { opcode: LSR_ZPX, meta_op: LSR,  addr_mode: ZPX, access: Some(indirx_rw) },
	OP_INVALID,
	InstDesc { opcode: CLI_IMP, meta_op: CLI,  addr_mode: IMP, access: None },
	InstDesc { opcode: EOR_ABY, meta_op: EOR,  addr_mode: ABY, access: Some(indiry_r) },
	OP_INVALID,
	OP_INVALID,
	OP_INVALID,
	InstDesc { opcode: EOR_ABX, meta_op: EOR,  addr_mode: ABX, access: Some(indirx_r) },
	InstDesc { opcode: LSR_ABX, meta_op: LSR,  addr_mode: ABX, access: Some(indirx_rw) },
	OP_INVALID,

	// 0x60
	InstDesc { opcode: RTS_IMP, meta_op: RTS,  addr_mode: IMP, access: None },
	InstDesc { opcode: ADC_IZX, meta_op: ADC,  addr_mode: IZX, access: Some(indirx_r) },
	OP_INVALID,
	OP_INVALID,
	InstDesc { opcode: DOP_64,  meta_op: DOP,  addr_mode: IMM, access: None },
	InstDesc { opcode: ADC_ZPG, meta_op: ADC,  addr_mode: ZPG, access: Some(direct_r) },
	InstDesc { opcode: ROR_ZPG, meta_op: ROR,  addr_mode: ZPG, access: Some(direct_rw) },
	OP_INVALID,
	InstDesc { opcode: PLA_IMP, meta_op: PLA,  addr_mode: IMP, access: None },
	InstDesc { opcode: ADC_IMM, meta_op: ADC,  addr_mode: IMM, access: None },
	InstDesc { opcode: ROR_IMP, meta_op: RORA, addr_mode: IMP, access: None },
	OP_INVALID,
	// TODO: has an implicit Target
	InstDesc { opcode: JMP_IND, meta_op: JMP,  addr_mode: IND, access: Some(direct_r) },
	InstDesc { opcode: ADC_ABS, meta_op: ADC,  addr_mode: ABS, access: Some(direct_r) },
	InstDesc { opcode: ROR_ABS, meta_op: ROR,  addr_mode: ABS, access: Some(direct_rw) },
	OP_INVALID,

	// 0x70
	InstDesc { opcode: BVS_REL, meta_op: BVS,  addr_mode: REL, access: Some(target) },
	InstDesc { opcode: ADC_IZY, meta_op: ADC,  addr_mode: IZY, access: Some(direct_r) },
	OP_INVALID,
	OP_INVALID,
	InstDesc { opcode: DOP_74,  meta_op: DOP,  addr_mode: IMM, access: None },
	InstDesc { opcode: ADC_ZPX, meta_op: ADC,  addr_mode: ZPX, access: Some(indirx_r) },
	InstDesc { opcode: ROR_ZPX, meta_op: ROR,  addr_mode: ZPX, access: Some(indirx_rw) },
	OP_INVALID,
	InstDesc { opcode: SEI_IMP, meta_op: SEI,  addr_mode: IMP, access: None },
	InstDesc { opcode: ADC_ABY, meta_op: ADC,  addr_mode: ABY, access: Some(indiry_r) },
	OP_INVALID,
	OP_INVALID,
	OP_INVALID,
	InstDesc { opcode: ADC_ABX, meta_op: ADC,  addr_mode: ABX, access: Some(indirx_r) },
	InstDesc { opcode: ROR_ABX, meta_op: ROR,  addr_mode: ABX, access: Some(indirx_rw) },
	OP_INVALID,

	// 0x80
	InstDesc { opcode: DOP_80,  meta_op: DOP,  addr_mode: IMM, access: None },
	// TODO: implicit indirect write
	InstDesc { opcode: STA_IZX, meta_op: STA,  addr_mode: IZX, access: Some(indirx_r) },
	InstDesc { opcode: DOP_82,  meta_op: DOP,  addr_mode: IMM, access: None },
	OP_INVALID,
	InstDesc { opcode: STY_ZPG, meta_op: STY,  addr_mode: ZPG, access: Some(direct_w) },
	InstDesc { opcode: STA_ZPG, meta_op: STA,  addr_mode: ZPG, access: Some(direct_w) },
	InstDesc { opcode: STX_ZPG, meta_op: STX,  addr_mode: ZPG, access: Some(direct_w) },
	OP_INVALID,
	InstDesc { opcode: DEY_IMP, meta_op: DEY,  addr_mode: IMP, access: None },
	InstDesc { opcode: DOP_89,  meta_op: DOP,  addr_mode: IMM, access: None },
	InstDesc { opcode: TXA_IMP, meta_op: TXA,  addr_mode: IMP, access: None },
	OP_INVALID,
	InstDesc { opcode: STY_ABS, meta_op: STY,  addr_mode: ABS, access: Some(direct_w) },
	InstDesc { opcode: STA_ABS, meta_op: STA,  addr_mode: ABS, access: Some(direct_w) },
	InstDesc { opcode: STX_ABS, meta_op: STX,  addr_mode: ABS, access: Some(direct_w) },
	OP_INVALID,

	// 0x90
	InstDesc { opcode: BCC_REL, meta_op: BCC,  addr_mode: REL, access: Some(target) },
	// TODO: implicit indirect write
	InstDesc { opcode: STA_IZY, meta_op: STA,  addr_mode: IZY, access: Some(direct_r) },
	OP_INVALID,
	OP_INVALID,
	InstDesc { opcode: STY_ZPX, meta_op: STY,  addr_mode: ZPX, access: Some(indirx_w) },
	InstDesc { opcode: STA_ZPX, meta_op: STA,  addr_mode: ZPX, access: Some(indirx_w) },
	InstDesc { opcode: STX_ZPY, meta_op: STX,  addr_mode: ZPY, access: Some(indiry_w) },
	OP_INVALID,
	InstDesc { opcode: TYA_IMP, meta_op: TYA,  addr_mode: IMP, access: None },
	InstDesc { opcode: STA_ABY, meta_op: STA,  addr_mode: ABY, access: Some(indiry_w) },
	InstDesc { opcode: TXS_IMP, meta_op: TXS,  addr_mode: IMP, access: None },
	OP_INVALID,
	OP_INVALID,
	InstDesc { opcode: STA_ABX, meta_op: STA,  addr_mode: ABX, access: Some(indirx_w) },
	OP_INVALID,
	OP_INVALID,

	// 0xA0
	InstDesc { opcode: LDY_IMM, meta_op: LDYI, addr_mode: IMM, access: None },
	InstDesc { opcode: LDA_IZX, meta_op: LDA,  addr_mode: IZX, access: Some(indirx_r) },
	InstDesc { opcode: LDX_IMM, meta_op: LDXI, addr_mode: IMM, access: None },
	OP_INVALID,
	InstDesc { opcode: LDY_ZPG, meta_op: LDY,  addr_mode: ZPG, access: Some(direct_r) },
	InstDesc { opcode: LDA_ZPG, meta_op: LDA,  addr_mode: ZPG, access: Some(direct_r) },
	InstDesc { opcode: LDX_ZPG, meta_op: LDX,  addr_mode: ZPG, access: Some(direct_r) },
	OP_INVALID,
	InstDesc { opcode: TAY_IMP, meta_op: TAY,  addr_mode: IMP, access: None },
	InstDesc { opcode: LDA_IMM, meta_op: LDAI, addr_mode: IMM, access: None },
	InstDesc { opcode: TAX_IMP, meta_op: TAX,  addr_mode: IMP, access: None },
	OP_INVALID,
	InstDesc { opcode: LDY_ABS, meta_op: LDY,  addr_mode: ABS, access: Some(direct_r) },
	InstDesc { opcode: LDA_ABS, meta_op: LDA,  addr_mode: ABS, access: Some(direct_r) },
	InstDesc { opcode: LDX_ABS, meta_op: LDX,  addr_mode: ABS, access: Some(direct_r) },
	OP_INVALID,

	// 0xB0
	InstDesc { opcode: BCS_REL, meta_op: BCS,  addr_mode: REL, access: Some(target) },
	InstDesc { opcode: LDA_IZY, meta_op: LDA,  addr_mode: IZY, access: Some(direct_r) },
	OP_INVALID,
	OP_INVALID,
	InstDesc { opcode: LDY_ZPX, meta_op: LDY,  addr_mode: ZPX, access: Some(indirx_r) },
	InstDesc { opcode: LDA_ZPX, meta_op: LDA,  addr_mode: ZPX, access: Some(indirx_r) },
	InstDesc { opcode: LDX_ZPY, meta_op: LDX,  addr_mode: ZPY, access: Some(indiry_r) },
	OP_INVALID,
	InstDesc { opcode: CLV_IMP, meta_op: CLV,  addr_mode: IMP, access: None },
	InstDesc { opcode: LDA_ABY, meta_op: LDA,  addr_mode: ABY, access: Some(indiry_r) },
	InstDesc { opcode: TSX_IMP, meta_op: TSX,  addr_mode: IMP, access: None },
	OP_INVALID,
	InstDesc { opcode: LDY_ABX, meta_op: LDY,  addr_mode: ABX, access: Some(indirx_r) },
	InstDesc { opcode: LDA_ABX, meta_op: LDA,  addr_mode: ABX, access: Some(indirx_r) },
	InstDesc { opcode: LDX_ABY, meta_op: LDX,  addr_mode: ABY, access: Some(indiry_r) },
	OP_INVALID,

	// 0xC0
	InstDesc { opcode: CPY_IMM, meta_op: CPY,  addr_mode: IMM, access: None },
	InstDesc { opcode: CMP_IZX, meta_op: CMP,  addr_mode: IZX, access: Some(indirx_r) },
	InstDesc { opcode: DOP_C2,  meta_op: DOP,  addr_mode: IMM, access: None },
	OP_INVALID,
	InstDesc { opcode: CPY_ZPG, meta_op: CPY,  addr_mode: ZPG, access: Some(direct_r) },
	InstDesc { opcode: CMP_ZPG, meta_op: CMP,  addr_mode: ZPG, access: Some(direct_r) },
	InstDesc { opcode: DEC_ZPG, meta_op: DEC,  addr_mode: ZPG, access: Some(direct_rw) },
	OP_INVALID,
	InstDesc { opcode: INY_IMP, meta_op: INY,  addr_mode: IMP, access: None },
	InstDesc { opcode: CMP_IMM, meta_op: CMP,  addr_mode: IMM, access: None },
	InstDesc { opcode: DEX_IMP, meta_op: DEX,  addr_mode: IMP, access: None },
	OP_INVALID,
	InstDesc { opcode: CPY_ABS, meta_op: CPY,  addr_mode: ABS, access: Some(direct_r) },
	InstDesc { opcode: CMP_ABS, meta_op: CMP,  addr_mode: ABS, access: Some(direct_r) },
	InstDesc { opcode: DEC_ABS, meta_op: DEC,  addr_mode: ABS, access: Some(direct_rw) },
	OP_INVALID,

	// 0xD0
	InstDesc { opcode: BNE_REL, meta_op: BNE,  addr_mode: REL, access: Some(target) },
	InstDesc { opcode: CMP_IZY, meta_op: CMP,  addr_mode: IZY, access: Some(direct_r) },
	OP_INVALID,
	OP_INVALID,
	InstDesc { opcode: DOP_D4,  meta_op: DOP,  addr_mode: IMM, access: None },
	InstDesc { opcode: CMP_ZPX, meta_op: CMP,  addr_mode: ZPX, access: Some(indirx_r) },
	InstDesc { opcode: DEC_ZPX, meta_op: DEC,  addr_mode: ZPX, access: Some(indirx_rw) },
	OP_INVALID,
	InstDesc { opcode: CLD_IMP, meta_op: CLD,  addr_mode: IMP, access: None },
	InstDesc { opcode: CMP_ABY, meta_op: CMP,  addr_mode: ABY, access: Some(indiry_r) },
	OP_INVALID,
	OP_INVALID,
	OP_INVALID,
	InstDesc { opcode: CMP_ABX, meta_op: CMP,  addr_mode: ABX, access: Some(indirx_r) },
	InstDesc { opcode: DEC_ABX, meta_op: DEC,  addr_mode: ABX, access: Some(indirx_rw) },
	OP_INVALID,

	// 0xE0
	InstDesc { opcode: CPX_IMM, meta_op: CPX,  addr_mode: IMM, access: None },
	InstDesc { opcode: SBC_IZX, meta_op: SBC,  addr_mode: IZX, access: Some(indirx_r) },
	InstDesc { opcode: DOP_E2,  meta_op: DOP,  addr_mode: IMM, access: None },
	OP_INVALID,
	InstDesc { opcode: CPX_ZPG, meta_op: CPX,  addr_mode: ZPG, access: Some(direct_r) },
	InstDesc { opcode: SBC_ZPG, meta_op: SBC,  addr_mode: ZPG, access: Some(direct_r) },
	InstDesc { opcode: INC_ZPG, meta_op: INC,  addr_mode: ZPG, access: Some(direct_rw) },
	OP_INVALID,
	InstDesc { opcode: INX_IMP, meta_op: INX,  addr_mode: IMP, access: None },
	InstDesc { opcode: SBC_IMM, meta_op: SBC,  addr_mode: IMM, access: None },
	InstDesc { opcode: NOP_IMP, meta_op: NOP,  addr_mode: IMP, access: None },
	OP_INVALID,
	InstDesc { opcode: CPX_ABS, meta_op: CPX,  addr_mode: ABS, access: Some(direct_r) },
	InstDesc { opcode: SBC_ABS, meta_op: SBC,  addr_mode: ABS, access: Some(direct_r) },
	InstDesc { opcode: INC_ABS, meta_op: INC,  addr_mode: ABS, access: Some(direct_rw) },
	OP_INVALID,

	// 0xF0
	InstDesc { opcode: BEQ_REL, meta_op: BEQ,  addr_mode: REL, access: Some(target) },
	InstDesc { opcode: SBC_IZY, meta_op: SBC,  addr_mode: IZY, access: Some(direct_r) },
	OP_INVALID,
	OP_INVALID,
	InstDesc { opcode: DOP_F4,  meta_op: DOP,  addr_mode: IMM, access: None },
	InstDesc { opcode: SBC_ZPX, meta_op: SBC,  addr_mode: ZPX, access: Some(indirx_r) },
	InstDesc { opcode: INC_ZPX, meta_op: INC,  addr_mode: ZPX, access: Some(indirx_rw) },
	OP_INVALID,
	InstDesc { opcode: SED_IMP, meta_op: SED,  addr_mode: IMP, access: None },
	InstDesc { opcode: SBC_ABY, meta_op: SBC,  addr_mode: ABY, access: Some(indiry_r) },
	OP_INVALID,
	OP_INVALID,
	OP_INVALID,
	InstDesc { opcode: SBC_ABX, meta_op: SBC,  addr_mode: ABX, access: Some(indirx_r) },
	InstDesc { opcode: INC_ABX, meta_op: INC,  addr_mode: ABX, access: Some(indirx_rw) },
	OP_INVALID,
];