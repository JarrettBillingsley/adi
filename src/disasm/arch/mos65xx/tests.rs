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
	assert_eq!(MetaOp::BRK.mnemonic_old(),  "brk");
	assert_eq!(MetaOp::BRK.mnemonic_new(),  "brk");
	assert_eq!(MetaOp::LDA.mnemonic_old(),  "lda");
	assert_eq!(MetaOp::LDA.mnemonic_new(),  "ld  a,");
	assert_eq!(MetaOp::LDAI.mnemonic_old(), "lda");
	assert_eq!(MetaOp::LDAI.mnemonic_new(), "li  a,");
	assert_eq!(MetaOp::UNK.mnemonic_old(),  "???");
	assert_eq!(MetaOp::UNK.mnemonic_new(),  "???");
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