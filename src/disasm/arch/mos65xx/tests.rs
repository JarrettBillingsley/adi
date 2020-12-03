use super::*;
use super::opcode_table::*;
use super::types::*;

#[test]
fn opcode_lookup() {
	assert_eq!(lookup_opcode(0x00).meta_op, MetaOp::BRK);
	assert_eq!(lookup_opcode(0x01).meta_op, MetaOp::ORA);
	assert_eq!(lookup_opcode(0xFE).meta_op, MetaOp::INC);
	assert_eq!(lookup_opcode(0x72).meta_op, MetaOp::UNK);
	assert_eq!(lookup_opcode(0xFF).meta_op, MetaOp::UNK);
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
	let opcode = lookup_opcode(opcode);
	let size = opcode.addr_mode.op_bytes() + 1;
	let mut operands = Operands::new();
	for op in ops { operands.push(*op); }
	Instruction::new(VAddr(va), opcode, size, operands)
} */

fn disas(va: usize, img: &[u8]) -> Instruction {
	let va = VAddr(va);
	match Disassembler.disas_instr(img, 0, va) {
		Ok(inst) => inst,
		Err(..)  => panic!()
	}
}

fn check_disas(va: usize, img: &[u8], meta_op: MetaOp, ops: &[Operand]) {
	let va = VAddr(va);
	match Disassembler.disas_instr(img, 0, va) {
		Ok(inst) => {
			let mut operands = Operands::new();
			for op in ops { operands.push(*op); }

			assert_eq!(inst.va, va);
			assert_eq!(inst.opcode.meta_op, meta_op);
			assert_eq!(inst.ops, operands);
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

	check_disas(0, &[0x00],               BRK,  &[]);
	check_disas(0, &[0xA9, 0xEF],         LDAI, &[Imm(0xEF)]);
	check_disas(0, &[0x6D, 0x56, 0x34],   ADC,  &[Mem(0x3456, Read)]);
	check_disas(0, &[0x84, 0x33],         STY,  &[Mem(0x0033, Write)]);
	check_disas(0, &[0x06, 0x99],         ASL,  &[Mem(0x0099, Rmw)]);
	check_disas(0, &[0x2E, 0xAA, 0x99],   ROL,  &[Mem(0x99AA, Offset)]);
	check_disas(0, &[0x4C, 0xFE, 0xFF],   JMP,  &[Mem(0xFFFE, Target)]);
	check_disas(0, &[0x6C, 0xFE, 0xFF],   JMP,  &[Mem(0xFFFE, Read)]);
	check_disas(3, &[0x90, 10],           BCC,  &[Mem(3 + 10 + 2, Target)]);
	check_disas(8, &[0x90, (-5i8) as u8], BCC,  &[Mem(8 - 5 + 2,  Target)]);
}

#[test]
fn disasm_failure() {
	// offset == end of image
	check_fail(0, &[], out_of_bytes(0, VAddr(0), 1, 0));

	// bad opcode
	check_fail(0, &[0xCB], unknown_instruction(0, VAddr(0)));
	check_fail(0, &[0xFF], unknown_instruction(0, VAddr(0)));

	// 1 operand byte
	check_fail(0, &[0xA9], out_of_bytes(0, VAddr(0), 2, 1));

	// 2 operand bytes
	check_fail(0, &[0x4C], out_of_bytes(0, VAddr(0), 3, 1));
	check_fail(0, &[0x4C, 0x00], out_of_bytes(0, VAddr(0), 3, 2));
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
	let tests: &[(Instruction, &str, &str)] = &[
		(disas(0, &[0x00]),             "brk ",            "brk "                  ),
		(disas(0, &[0xA9, 0xEF]),       "lda #$EF",        "li  a, 0xEF"           ),
		(disas(0, &[0x6D, 0x56, 0x34]), "adc $3456",       "adc a, [0x3456]"       ),
		(disas(0, &[0x84, 0x33]),       "sty $33",         "st  y, [0x33]"         ),
		(disas(0, &[0x06, 0x99]),       "asl $99",         "shl [0x99]"            ),
		(disas(0, &[0x2E, 0xEF, 0xBE]), "rol beefmaster",  "rol [beefmaster]"      ),
		(disas(0, &[0x4C, 0xFE, 0xFF]), "jmp $FFFE",       "jmp 0xFFFE"            ),
		(disas(0, &[0x6C, 0xFC, 0xFF]), "jmp (VEC_RESET)", "jmp [VEC_RESET]"       ),
		(disas(3, &[0x90, 10]),         "bcc $000F",       "bcc 0x000F"            ),
		(disas(8, &[0x90, 0xF2]),       "bcc VEC_RESET",   "bcc VEC_RESET"         ),
		(disas(0, &[0x15, 0x30]),       "ora v_ztable,x",  "or  a, [v_ztable + x]" ),
		(disas(0, &[0xB6, 0x40]),       "ldx $40,y",       "ld  x, [0x40 + y]"     ),
		(disas(0, &[0xBC, 0x50, 0x60]), "ldy $6050,x",     "ld  y, [0x6050 + x]"   ),
		(disas(0, &[0xD9, 0x30, 0x00]), "cmp v_ztable,y",  "cmp a, [v_ztable + y]" ),
		(disas(0, &[0xE1, 0x10]),       "sbc ($10,x)",     "sbc a, [[0x10 + x]]"   ),
		(disas(0, &[0x51, 0x90]),       "eor ($90),y",     "xor a, [[0x90] + y]"   ),
	];

	let old = Printer::new(SyntaxFlavor::Old);
	let new = Printer::new(SyntaxFlavor::New);
	let l = &DummyLookup;

	for (i, o, n) in tests {
		assert_eq!(old.fmt_instr(&i, l), *o);
		assert_eq!(new.fmt_instr(&i, l), *n);
	}
}