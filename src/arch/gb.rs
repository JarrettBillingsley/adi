//! The Game Boy architecture, known as LR35902 and (more recently) Sharp SM83.
//!
//! It is *NOT* a Z80. It's a cousin of the Z80, much closer to the Intel 8080.
//! It borrows the `0xCB`-prefixed bit manipulation instructions from the Z80, but
//! that's about it. No second register set, no index registers, no other prefixes.
//! Also, it drops the IO ports/instructions entirely, and adds a "zero-page-like"
//! addressing mode for accessing `0xFF00..0xFFFF` where memory-mapped IO resides.

use crate::program::{
	MemAccess,
	MemIndir,
	Operand,
	Instruction,
};
use crate::arch::{
	DisasError, DisasResult,
	Printer, IPrinter, PrinterCtx, FmtResult,
	Disassembler, IDisassembler,
	INameLookup,
	IArchitecture,
};
use crate::memory::{ MmuState, Endian, EA, VA };

// ------------------------------------------------------------------------------------------------
// Sub-modules
// ------------------------------------------------------------------------------------------------

mod descs;
#[cfg(test)]
mod tests;

use descs::{ lookup_desc, lookup_desc_cb, Reg, GBOpKind, InstDesc };

#[cfg(test)]
use descs::{ MetaOp };

// ------------------------------------------------------------------------------------------------
// Disassembler
// ------------------------------------------------------------------------------------------------

pub struct GBDisassembler;

impl IDisassembler for GBDisassembler {
	fn disas_instr(&self, img: &[u8], _state: MmuState, va: VA, ea: EA)
	-> DisasResult<Instruction> {
		// do we have enough bytes?
		if img.is_empty() {
			return Err(DisasError::out_of_bytes(va, ea, 1, 0));
		}

		// is the opcode OK?
		let desc = if img[0] == 0xCB {
			if img.len() == 1 {
				return Err(DisasError::out_of_bytes(va, ea, 2, 1));
			}

			lookup_desc_cb(img[1])
		} else if let Some(desc) = lookup_desc(img[0]) {
			desc
		} else {
			log::trace!("ran into opcode 0x{:02X}", img[0]);
			return Err(DisasError::unknown_instruction(va, ea));
		};

		// do we have enough bytes for the operand?
		let inst_size = desc.inst_size();

		if inst_size > img.len() {
			return Err(DisasError::out_of_bytes(va, ea, inst_size, img.len()));
		}

		// okay cool, let's decode
		let bytes = &img[0 .. inst_size];
		let mut ops = [Operand::Reg(0), Operand::Reg(0)];
		let (num_ops, target) = decode_operands(desc, va, bytes, &mut ops);
		Ok(Instruction::new(va, ea, desc.kind(), target, &ops[0 .. num_ops], bytes))
	}
}

fn rdisp(reg: Reg, disp: i64) -> MemIndir {
	MemIndir::RegDisp { reg: reg as u8, disp }
}

fn decode_operands(desc: InstDesc, va: VA, img: &[u8], ops: &mut [Operand; 2])
-> (usize, Option<VA>) {
	use GBOpKind::*;
	use Operand::{ UImm, SImm, Indir, Mem };
	use MemAccess::*;

	match desc.op_kind() {
		Imp => {
			if let Some(addr) = desc.rst_target() {
				let addr = VA(addr as usize);
				ops[0] = Mem(addr, Target);
				(1, Some(addr))
			} else {
				(0, None)
			}
		}
		Dummy     => (0, None),
		UImm8     => { ops[0] = UImm(img[1] as u64, None);                          (1, None) }
		Imm16     => { ops[0] = UImm((img[2] as u64) << 8 | (img[1] as u64), None); (1, None) }
		SImm8     => { ops[0] = SImm(img[1] as i8 as i64, None);                    (1, None) }
		SPImm     => { ops[0] = Indir(rdisp(Reg::SP, img[1] as i8 as i64), R);      (1, None) }
		AddHi(a)  => { ops[0] = Mem(VA(0xFF00 + (img[1] as usize)), a);             (1, None) }
		IndHi(a)  => { ops[0] = Indir(rdisp(Reg::C, 0xFF00), a);                    (1, None) }
		Ind(r, a) => { ops[0] = Indir(MemIndir::Reg { reg: r as u8 }, a);           (1, None) }

		LdHlImm   => {
			ops[0] = Indir(MemIndir::Reg { reg: Reg::HL as u8 }, W);
			ops[1] = UImm(img[1] as u64, None);
			(2, None)
		}

		Rel => {
			let addr = 2 + (va.0 as isize) + (img[1] as i8 as isize);
			let addr = VA(addr as usize);
			ops[0] = Mem(addr, Target);
			(1, Some(addr))
		}

		Add16(a) => {
			let addr = (img[2] as usize) << 8 | (img[1] as usize);
			let addr = VA(addr as usize);
			ops[0] = Mem(addr, a);

			if a == Target {
				(1, Some(addr))
			} else {
				(1, None)
			}
		}
	}
}

// ------------------------------------------------------------------------------------------------
// GBPrinter
// ------------------------------------------------------------------------------------------------

#[derive(Debug, Copy, Clone)]
pub struct GBPrinter;

impl GBPrinter {
	pub fn new() -> Self {
		Self { }
	}

	fn fmt_uimm(self, imm: u64) -> String {
		if imm < 0x10 { format!("{}",  imm) } else { format!("0x{:X}", imm) }
	}

	fn fmt_simm(self, imm: i64) -> String {
		if imm.abs() < 0x10 {
			format!("{}",  imm)
		} else {
			let sign = if imm < 0 { "-" } else { "" };
			format!("{}0x{:X}", sign, imm.abs())
		}
	}

	fn fmt_addr(self, addr: u64, state: MmuState, l: &impl INameLookup) -> String {
		match l.lookup(state, VA(addr as usize)) {
			Some(name) => name,
			None       => format!("0x{:04X}", addr),
		}
	}

	fn lookup_desc(self, bytes: &[u8]) -> InstDesc {
		if bytes[0] == 0xCB {
			lookup_desc_cb(bytes[1])
		} else {
			lookup_desc(bytes[0]).expect("ono")
		}
	}
}

impl IPrinter for GBPrinter {
	fn mnemonic_max_len(&self) -> usize {
		3
	}

	fn get_mnemonic(&self, i: &Instruction) -> String {
		self.lookup_desc(i.bytes()).mnemonic().into()
	}

	fn fmt_operands(&self, i: &Instruction, state: MmuState, l: &impl INameLookup) -> String {
		use GBOpKind::*;

		let desc = self.lookup_desc(i.bytes());
		let templ = desc.op_templ();

		match desc.op_kind() {
			Imp | Dummy | Ind(..) | IndHi(..) =>
				templ.into(),
			LdHlImm =>
				templ.replace("{}", &self.fmt_uimm(i.ops().last().unwrap().uimm())),
			UImm8 | Imm16 =>
				templ.replace("{}", &self.fmt_uimm(i.ops().first().unwrap().uimm())),
			SImm8 =>
				templ.replace("{}", &self.fmt_simm(i.ops().first().unwrap().simm())),
			Rel | Add16(..) | AddHi(..) =>
				templ.replace("{}",
					&self.fmt_addr(i.ops().first().unwrap().addr().0 as u64, state, l)),
			SPImm => {
				let disp = match i.ops().first().unwrap() {
					Operand::Indir(MemIndir::RegDisp { disp, .. }, ..) => *disp,
					_ => unreachable!(),
				};

				if disp < 0 {
					templ.replace("{}", &format!(" - {}", self.fmt_uimm(-disp as u64)))
				} else {
					templ.replace("{}", &format!(" + {}", self.fmt_uimm(disp as u64)))
				}
			}
		}
	}

	fn print_register(&self, ctx: &mut PrinterCtx, r: u8) -> FmtResult {
		ctx.style_register(&|ctx| ctx.write_str(Reg::register_names()[r as usize]))
	}
}

// ------------------------------------------------------------------------------------------------
// Architecture
// ------------------------------------------------------------------------------------------------

pub struct GBArchitecture;

impl IArchitecture for GBArchitecture {
	fn endianness      (&self) -> Endian       { Endian::Little }
	fn addr_bits       (&self) -> usize        { 16 }
	fn new_disassembler(&self) -> Disassembler { GBDisassembler.into() }
	fn new_printer     (&self) -> Printer      { GBPrinter::new().into() }
}
