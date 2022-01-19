#![allow(unused_imports)]

use crate::memory::{ MmuState, SegId, EA, VA };
use crate::arch::{ DisasError, INameLookup, Disassembler, IDisassembler, IPrinter };
use crate::program::{ MemAccess, MemIndir, Instruction, Operand };

use super::{
	MetaOp,
	GBDisassembler,
	GBPrinter,
	Reg,
	lookup_desc,
	lookup_desc_cb,
};

#[test]
fn opcode_lookup() {
	assert_eq!(lookup_desc(0x09).unwrap().meta_op(), MetaOp::ADD);
	assert_eq!(lookup_desc(0x76).unwrap().meta_op(), MetaOp::HALT);
	assert_eq!(lookup_desc(0xD3), None);
	assert_eq!(lookup_desc(0xFC), None);
	assert_eq!(lookup_desc_cb(0x00).meta_op(), MetaOp::RLC);
	assert_eq!(lookup_desc_cb(0x33).meta_op(), MetaOp::SWAP);
}

#[test]
fn mnemonics() {
	assert_eq!(MetaOp::EI.mnemonic(),   "ei");
	assert_eq!(MetaOp::STOP.mnemonic(), "stop");
	assert_eq!(MetaOp::SCF.mnemonic(),  "scf");
	assert_eq!(MetaOp::UNK.mnemonic(),  "???");
}

fn disas(va: usize, img: &[u8]) -> Instruction {
	let ea = EA::new(SegId(0), va);
	let va = VA(va);
	let state = MmuState::default();
	match GBDisassembler.disas_inst(img, state, va, ea) {
		Ok(inst) => inst,
		Err(..)  => panic!()
	}
}

fn check_disas(va: usize, img: &[u8], meta_op: MetaOp, ops: &[Operand]) {
	let ea = EA::new(SegId(0), va);
	let va = VA(va);
	let state = MmuState::default();
	match GBDisassembler.disas_inst(img, state, va, ea) {
		Ok(inst) => {
			assert_eq!(inst.va(), va);
			assert_eq!(lookup_desc(inst.bytes()[0]).unwrap().meta_op(), meta_op);
			assert_eq!(inst.ops().len(), ops.len());

			for (op, expected) in inst.ops().iter().zip(ops.iter()) {
				assert_eq!(op, expected);
			}
		}

		Err(e) => {
			panic!("failed to disassemble: {}", e);
		}
	}
}

fn check_fail(va: usize, img: &[u8], expected: DisasError) {
	let ea = EA::new(SegId(0), va);
	let va = VA(va);
	let state = MmuState::default();
	match GBDisassembler.disas_inst(img, state, va, ea) {
		Ok(inst) => {
			panic!("should have failed disassembling {:?}, but got {:?}", img, inst);
		}

		Err(e) => {
			assert_eq!(e, expected);
		}
	}
}

fn indr(reg: Reg, acc: MemAccess) -> Operand {
	Operand::Indir(MemIndir::Reg { reg: reg as u8 }, acc)
}

fn indrd(reg: Reg, disp: i64, acc: MemAccess) -> Operand {
	Operand::Indir(MemIndir::RegDisp { reg: reg as u8, disp }, acc)
}

fn mem(addr: usize, acc: MemAccess) -> Operand {
	Operand::Mem(VA(addr), acc)
}

fn uimm(val: usize) -> Operand {
	Operand::UImm(val as u64, None)
}

fn simm(val: isize) -> Operand {
	Operand::SImm(val as i64, None)
}

#[test]
fn disasm_success() {
	use MetaOp::*;
	use MemAccess::*;

	check_disas(0, &[0x00],               NOP,  &[]                            ); // Imp
	check_disas(0, &[0xCF],               RST,  &[mem(0x0008, Target)]         );
	check_disas(0, &[0x10, 0x00],         STOP, &[]                            ); // Dummy
	check_disas(0, &[0x16, 0xFE],         LD,   &[uimm(0xFE)]                  ); // UImm8
	check_disas(0, &[0x01, 0xAD, 0xDE],   LD,   &[uimm(0xDEAD)]                ); // Imm16
	check_disas(0, &[0xE8, 0x13],         ADD,  &[simm(0x13)]                  ); // SImm8
	check_disas(0, &[0xE8, 0xFE],         ADD,  &[simm(-2)]                    );
	check_disas(0, &[0xF8, 0x13],         LD,   &[indrd(Reg::SP, 0x13, R)]     ); // SPImm
	check_disas(0, &[0xF8, 0xFE],         LD,   &[indrd(Reg::SP,   -2, R)]     );
	check_disas(0, &[0xF0, 0x34],         LDH,  &[mem(0xFF34, R)]              ); // AddHi
	check_disas(0, &[0xE0, 0x34],         LDH,  &[mem(0xFF34, W)]              );
	check_disas(0, &[0xF2],               LDH,  &[indrd(Reg::C, 0xFF00, R)]    ); // IndHi
	check_disas(0, &[0xE2],               LDH,  &[indrd(Reg::C, 0xFF00, W)]    );
	check_disas(0, &[0x02],               LD,   &[indr(Reg::BC, W)]            ); // Ind
	check_disas(0, &[0x36, 0x69],         LD,   &[indr(Reg::HL, W), uimm(0x69)]); // LdHlImm
	check_disas(0, &[0xC2, 0x34, 0x12],   JP,   &[mem(0x1234, Target)]         ); // Rel
	check_disas(3, &[0x18, 10],           JR,   &[mem(3 + 10 + 2, Target)]     );
	check_disas(8, &[0x20, (-5i8) as u8], JR,   &[mem(8 - 5 + 2, Target)]      );
	check_disas(0, &[0xFA, 0x34, 0x12],   LD,   &[mem(0x1234, R)]              ); // Add16
	check_disas(0, &[0x08, 0x34, 0x12],   LD,   &[mem(0x1234, W)]              );
	check_disas(0, &[0xCD, 0x34, 0x12],   CALL, &[mem(0x1234, Target)]         );
}

#[test]
fn disasm_failure() {
	let ea = EA::new(SegId(0), 0);

	check_fail(0, &[],           DisasError::out_of_bytes(VA(0), ea, 1, 0));
	check_fail(0, &[0xCB],       DisasError::out_of_bytes(VA(0), ea, 2, 1));
	check_fail(0, &[0xFD],       DisasError::unknown_instruction(VA(0), ea));
	check_fail(0, &[0x3E],       DisasError::out_of_bytes(VA(0), ea, 2, 1));
	check_fail(0, &[0x01],       DisasError::out_of_bytes(VA(0), ea, 3, 1));
	check_fail(0, &[0x01, 0x00], DisasError::out_of_bytes(VA(0), ea, 3, 2));
}

struct DummyLookup;

impl INameLookup for DummyLookup {
	fn lookup(&self, _state: MmuState, addr: VA) -> Option<String> {
		match addr.0 {
			0x0100 => Some("CartHeader".into()),
			0xBEEF => Some("beefmaster".into()),
			0xFF40 => Some("LCDC".into()),
			0xFFFC => Some("v_hram".into()),
			_      => None,
		}
	}
}

#[test]
fn printing() {
	let tests: &[(Instruction, &str)] = &[
		(disas(0, &[0xFB]),             "ei "),             // Imp
		(disas(0, &[0x03]),             "inc bc"),
		(disas(0, &[0x0D]),             "dec c"),
		(disas(0, &[0xCF]),             "rst 0x08"),
		(disas(0, &[0x10, 0x00]),       "stop "),           // Dummy
		(disas(0, &[0x02]),             "ld [bc], a"),      // Ind
		(disas(0, &[0x32]),             "ld [hl-], a"),
		(disas(0, &[0xE2]),             "ldh [c], a"),      // IndHi
		(disas(0, &[0x36, 7]),          "ld [hl], 7"),      // LdHlImm
		(disas(0, &[0x06, 0xEF]),       "ld b, 0xEF"),      // UImm8
		(disas(0, &[0x01, 0x0D, 0xF0]), "ld bc, 0xF00D"),   // Imm16
		(disas(0, &[0xE8, 0x04]),       "add sp, 4"),       // SImm8
		(disas(0, &[0xE8, 0xFC]),       "add sp, -4"),
		(disas(0, &[0xE8, 0x7F]),       "add sp, 0x7F"),
		(disas(0, &[0xE8, 0x80]),       "add sp, -0x80"),
		(disas(0xCE, &[0x18, 0x30]),    "jr CartHeader"),   // Rel
		(disas(0, &[0x18, 0x30]),       "jr 0x0032"),
		(disas(0, &[0x20, 0x30]),       "jr nz, 0x0032"),
		(disas(0, &[0x08, 0xFC, 0xFF]), "ld [v_hram], sp"), // Add16
		(disas(0, &[0x08, 0xFD, 0xFF]), "ld [0xFFFD], sp"),
		(disas(0, &[0xCD, 0xEF, 0xBE]), "call beefmaster"),

	];

	let print = GBPrinter::new();
	let l = &DummyLookup;
	let state = MmuState::default();

	for (i, exp) in tests {
		assert_eq!(print.fmt_instr(&i, state, l), *exp);
	}
}

#[test]
fn disasm_range() {
	let code = &[
		0xFB,             // ei
		0x03,             // inc bc
		0x0D,             // dec c
		0xCF,             // rst 0x08
		0x10, 0x00,       // stop
		0x02,             // ld [bc], a
		0x32,             // ld [hl-], a
		0xE2,             // ldh [c], a
		0x36, 7,          // ld [hl], 7
		0x06, 0xEF,       // ld b, 0xEF
		0x01, 0x0D, 0xF0, // ld bc, 0xF00D
		0xE8, 0x04,       // add sp, 4
		0xE8, 0xFC,       // add sp, -4
		0xE8, 0x7F,       // add sp, 0x7F
		0xE8, 0x80,       // add sp, -0x80
		0x18, 0x30,       // jr 0x004A
		0x18, 0x30,       // jr 0x004C
		0x20, 0x30,       // jr nz, 0x004E
		0x08, 0xFC, 0xFF, // ld [v_hram], sp
		0x08, 0xFD, 0xFF, // ld [0xFFFD], sp
		0xCD, 0xEF, 0xBE, // call beefmaster
	];

	let expected = &[
		"ei ",
		"inc bc",
		"dec c",
		"rst 0x08",
		"stop ",
		"ld [bc], a",
		"ld [hl-], a",
		"ldh [c], a",
		"ld [hl], 7",
		"ld b, 0xEF",
		"ld bc, 0xF00D",
		"add sp, 4",
		"add sp, -4",
		"add sp, 0x7F",
		"add sp, -0x80",
		"jr 0x004A",
		"jr 0x004C",
		"jr nz, 0x004E",
		"ld [v_hram], sp",
		"ld [0xFFFD], sp",
		"call beefmaster",
	];

	let p = GBPrinter::new();
	let state = MmuState::default();
	let dis: Disassembler = GBDisassembler.into();
	let mut iter = dis.disas_all(code, state, VA(0), EA::new(SegId(0), 0));
	let mut output = Vec::new();

	for inst in &mut iter {
		output.push(p.fmt_instr(&inst, state, &DummyLookup));
	}

	assert!(!iter.has_err());
	assert_eq!(expected.len(), output.len());

	for (exp, out) in expected.iter().zip(output.iter()) {
		assert_eq!(exp, out);
	}
}