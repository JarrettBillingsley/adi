use super::*;
use super::descs::*;
use super::opcodes::*;
use super::types::*;

#[test]
fn opcode_lookup() {
	assert_eq!(lookup_desc(Opcode::BRK_IMP as u8).meta_op, MetaOp::BRK);
	assert_eq!(lookup_desc(Opcode::ORA_IZX as u8).meta_op, MetaOp::ORA);
	assert_eq!(lookup_desc(Opcode::INC_ABX as u8).meta_op, MetaOp::INC);
	assert_eq!(lookup_desc(0x72                 ).meta_op, MetaOp::UNK);
	assert_eq!(lookup_desc(0xFF                 ).meta_op, MetaOp::UNK);
}

#[test]
fn mnemonics() {
	assert_eq!(MetaOp::BRK.mnemonic(SyntaxFlavor::Old),  "brk");
	assert_eq!(MetaOp::BRK.mnemonic(SyntaxFlavor::New),  "brk");
	assert_eq!(MetaOp::LDA.mnemonic(SyntaxFlavor::Old),  "lda");
	assert_eq!(MetaOp::LDA.mnemonic(SyntaxFlavor::New),  "ld  a,");
	assert_eq!(MetaOp::LDAI.mnemonic(SyntaxFlavor::Old), "lda");
	assert_eq!(MetaOp::LDAI.mnemonic(SyntaxFlavor::New), "li  a,");
	assert_eq!(MetaOp::UNK.mnemonic(SyntaxFlavor::Old),  "???");
	assert_eq!(MetaOp::UNK.mnemonic(SyntaxFlavor::New),  "???");
}

/*fn make_instr(va: usize, opcode: u8, ops: &[Operand]) -> Instruction {
	let desc = lookup_desc(opcode);
	let size = desc.addr_mode.op_bytes() + 1;
	let mut operands = Operands::new();
	for op in ops { operands.push(*op); }
	Instruction::new(VAddr(va), desc, size, operands)
} */

fn disas(va: usize, img: &[u8]) -> Instruction {
	let va = VAddr(va);
	match Disassembler.disas_instr(img, 0, va) {
		Ok(inst) => inst,
		Err(..)  => panic!()
	}
}

fn check_disas(va: usize, img: &[u8], meta_op: MetaOp, op: Option<Operand>) {
	let va = VAddr(va);
	match Disassembler.disas_instr(img, 0, va) {
		Ok(inst) => {
			assert_eq!(inst.va, va);
			assert_eq!(inst.desc.meta_op, meta_op);
			assert_eq!(inst.op, op);
		}

		Err(e) => {
			panic!("failed to disassemble: {}", e);
		}
	}
}

fn check_fail(va: usize, img: &[u8], expected: DisasError) {
	let va = VAddr(va);
	match Disassembler.disas_instr(img, 0, va) {
		Ok(inst) => {
			panic!("should have failed disassembling {:?}, but got {:?}", img, inst);
		}

		Err(e) => {
			assert_eq!(e, expected);
		}
	}
}

#[test]
fn disasm_success() {
	use MetaOp::*;
	use Operand::*;
	use MemAccess::*;
	use Opcode::*;

	check_disas(0, &[BRK_IMP as u8],               BRK,  None);
	check_disas(0, &[LDA_IMM as u8, 0xEF],         LDAI, Some(Imm(0xEF)));
	check_disas(0, &[ADC_ABS as u8, 0x56, 0x34],   ADC,  Some(Mem(0x3456, Read)));
	check_disas(0, &[STY_ZPG as u8, 0x33],         STY,  Some(Mem(0x0033, Write)));
	check_disas(0, &[ASL_ZPG as u8, 0x99],         ASL,  Some(Mem(0x0099, Rmw)));
	check_disas(0, &[ROL_ABS as u8, 0xAA, 0x99],   ROL,  Some(Mem(0x99AA, Offset)));
	check_disas(0, &[JMP_LAB as u8, 0xFE, 0xFF],   JMP,  Some(Mem(0xFFFE, Target)));
	check_disas(0, &[JMP_IND as u8, 0xFE, 0xFF],   JMP,  Some(Mem(0xFFFE, Read)));
	check_disas(3, &[BCC_REL as u8, 10],           BCC,  Some(Mem(3 + 10 + 2, Target)));
	check_disas(8, &[BCC_REL as u8, (-5i8) as u8], BCC,  Some(Mem(8 - 5 + 2,  Target)));
}

#[test]
fn disasm_failure() {
	use Opcode::*;

	// offset == end of image
	check_fail(0, &[], DisasError::out_of_bytes(0, VAddr(0), 1, 0));

	// bad opcode
	check_fail(0, &[0xCB], DisasError::unknown_instruction(0, VAddr(0)));
	check_fail(0, &[0xFF], DisasError::unknown_instruction(0, VAddr(0)));

	// 1 operand byte
	check_fail(0, &[LDA_IMM as u8], DisasError::out_of_bytes(0, VAddr(0), 2, 1));

	// 2 operand bytes
	check_fail(0, &[JMP_LAB as u8], DisasError::out_of_bytes(0, VAddr(0), 3, 1));
	check_fail(0, &[JMP_LAB as u8, 0x00], DisasError::out_of_bytes(0, VAddr(0), 3, 2));
}

struct DummyLookup;

impl NameLookupTrait for DummyLookup {
	fn lookup(&self, addr: VAddr) -> Option<String> {
		match addr.0 {
			0x0030 => Some("v_ztable".into()),
			0xBEEF => Some("beefmaster".into()),
			0xFFFC => Some("VEC_RESET".into()),
			_ => None,
		}
	}
}

#[test]
fn printing() {
	use Opcode::*;

	let tests: &[(Instruction, &str, &str)] = &[
		(disas(0, &[BRK_IMP as u8]),             "brk ",            "brk "                  ),
		(disas(0, &[LDA_IMM as u8, 0xEF]),       "lda #$EF",        "li  a, 0xEF"           ),
		(disas(0, &[ADC_ABS as u8, 0x56, 0x34]), "adc $3456",       "adc a, [0x3456]"       ),
		(disas(0, &[STY_ZPG as u8, 0x33]),       "sty $33",         "st  y, [0x33]"         ),
		(disas(0, &[ASL_ZPG as u8, 0x99]),       "asl $99",         "shl [0x99]"            ),
		(disas(0, &[ROL_ABS as u8, 0xEF, 0xBE]), "rol beefmaster",  "rol [beefmaster]"      ),
		(disas(0, &[JMP_LAB as u8, 0xFE, 0xFF]), "jmp $FFFE",       "jmp 0xFFFE"            ),
		(disas(0, &[JMP_IND as u8, 0xFC, 0xFF]), "jmp (VEC_RESET)", "jmp [VEC_RESET]"       ),
		(disas(3, &[BCC_REL as u8, 10]),         "bcc $000F",       "bcc 0x000F"            ),
		(disas(8, &[BCC_REL as u8, 0xF2]),       "bcc VEC_RESET",   "bcc VEC_RESET"         ),
		(disas(0, &[ORA_ZPX as u8, 0x30]),       "ora v_ztable,x",  "or  a, [v_ztable + x]" ),
		(disas(0, &[LDX_ZPY as u8, 0x40]),       "ldx $40,y",       "ld  x, [0x40 + y]"     ),
		(disas(0, &[LDY_ABX as u8, 0x50, 0x60]), "ldy $6050,x",     "ld  y, [0x6050 + x]"   ),
		(disas(0, &[CMP_ABY as u8, 0x30, 0x00]), "cmp v_ztable,y",  "cmp a, [v_ztable + y]" ),
		(disas(0, &[SBC_IZX as u8, 0x10]),       "sbc ($10,x)",     "sbc a, [[0x10 + x]]"   ),
		(disas(0, &[EOR_IZY as u8, 0x90]),       "eor ($90),y",     "xor a, [[0x90] + y]"   ),
	];

	let old = Printer::new(SyntaxFlavor::Old);
	let new = Printer::new(SyntaxFlavor::New);
	let l = &DummyLookup;

	for (i, o, n) in tests {
		assert_eq!(old.fmt_instr(&i, l), *o);
		assert_eq!(new.fmt_instr(&i, l), *n);
	}
}