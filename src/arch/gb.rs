//! The Game Boy architecture, known as LR35902 and (more recently) Sharp SM83.
//!
//! It is *NOT* a Z80. It's a cousin of the Z80, much closer to the Intel 8080.
//! It borrows the `0xCB`-prefixed bit manipulation instructions from the Z80, but
//! that's about it. No second register set, no index registers, no other prefixes.
//! Also, it drops the IO ports/instructions entirely, and adds a "zero-page-like"
//! addressing mode for accessing `0xFF00..0xFFFF` where memory-mapped IO resides.

use crate::{ Offs };
use crate::program::{
	MemIndir,
	Operand,
	Instruction,
};
use crate::arch::{
	DisasError, DisasResult,
	Printer, IPrinter, PrinterCtx, FmtResult,
	Disassembler, IDisassembler,
	IArchitecture,
	IrCompiler,
};
use crate::memory::{ MmuState, Endian, EA, VA, MemAccess };

// ------------------------------------------------------------------------------------------------
// Sub-modules
// ------------------------------------------------------------------------------------------------

mod descs;
mod ir;
#[cfg(test)]
mod tests;

use descs::{ lookup_desc, lookup_desc_cb, Reg, GBOpKind, InstDesc, SynOp, Cc };
use descs::{ MetaOp };
pub(crate) use ir::{ GBIrCompiler };

// ------------------------------------------------------------------------------------------------
// Disassembler
// ------------------------------------------------------------------------------------------------

pub struct GBDisassembler;

impl IDisassembler for GBDisassembler {
	fn disas_inst(&self, img: &[u8], _state: MmuState, va: VA, ea: EA)
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
			log::debug!("ran into opcode 0x{:02X}", img[0]);
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

/// decode operands into `ops`. returns (number of operands, control flow target)
fn decode_operands(desc: &InstDesc, va: VA, img: &[u8], ops: &mut [Operand; 2])
-> (usize, Option<VA>) {
	use Operand::{ UImm, SImm, Indir, Mem };
	use MemAccess::{ W, Target };

	// comments show grouping by number and kind of operands
	match desc.op_kind() {
		// []
		GBOpKind::Dummy | GBOpKind::Imp => (0, None),

		// [UImm]
		GBOpKind::UImm8 => {
			ops[0] = UImm(img[1] as u64);
			(1, None)
		}
		GBOpKind::Imm16 => {
			ops[0] = UImm((img[2] as u64) << 8 | (img[1] as u64));
			(1, None)
		}
		GBOpKind::Bit(bitn) => {
			ops[0] = UImm(bitn as u64);
			(1, None)
		}

		// [SImm]
		GBOpKind::SImm8 => {
			ops[0] = SImm(img[1] as i8 as i64);
			(1, None)
		}

		// [Mem]
		GBOpKind::Rel => {
			let addr = 2 + (va.0 as isize) + (img[1] as i8 as isize);
			let addr = VA(addr as usize as Offs);
			ops[0] = Mem(addr, Target);
			(1, Some(addr))
		}
		GBOpKind::Add16(a) => {
			let addr = (img[2] as usize) << 8 | (img[1] as usize);
			let addr = VA(addr as Offs);
			ops[0] = Mem(addr, a);
			(1, if a == Target { Some(addr) } else { None })
		}
		GBOpKind::AddHi(a) => {
			ops[0] = Mem(VA(0xFF00 + (img[1] as Offs)), a);
			(1, None)
		}
		GBOpKind::Rst(addr) => {
			let addr = VA(addr as Offs);
			ops[0] = Mem(addr, Target);
			(1, Some(addr))
		}

		// [Indir(Reg | RegDisp)]
		GBOpKind::Ind(r, a) => {
			ops[0] = Indir(MemIndir::Reg { reg: r as u8 }, a);
			(1, None)
		}
		GBOpKind::IndHi(a) => {
			ops[0] = Indir(MemIndir::RegDisp { reg: Reg::C as u8, disp: 0xFF00 }, a);
			(1, None)
		}

		// [Indir(Reg), UImm]
		GBOpKind::LdHlImm => {
			ops[0] = Indir(MemIndir::Reg { reg: Reg::HL as u8 }, W);
			ops[1] = UImm(img[1] as u64);
			(2, None)
		}

		// [UImm, Indir(Reg)]
		GBOpKind::BitInd(bitn, a) => {
			ops[0] = UImm(bitn as u64);
			ops[1] = Indir(MemIndir::Reg { reg: Reg::HL as u8 }, a);
			(2, None)
		}
	}
}

// ------------------------------------------------------------------------------------------------
// GBPrinter
// ------------------------------------------------------------------------------------------------

#[derive(Debug, Copy, Clone)]
pub struct GBPrinter;

impl GBPrinter {
	#[allow(clippy::new_without_default)]
	pub fn new() -> Self {
		Self { }
	}

	fn lookup_desc(self, bytes: &[u8]) -> &'static InstDesc {
		match *bytes {
			[0xCB, byte2, ..] => lookup_desc_cb(byte2),
			[byte1, ..]       => lookup_desc(byte1).expect("ono"),
			_                 => unreachable!(),
		}
	}
}

impl IPrinter for GBPrinter {
	// --------------------------------------------------------------------------------------------
	// Required methods

	fn get_mnemonic(&self, i: &Instruction) -> String {
		self.lookup_desc(i.bytes()).mnemonic().into()
	}

	fn print_register(&self, ctx: &mut PrinterCtx, r: u8) -> FmtResult {
		ctx.style_register(&|ctx| ctx.write_str(Reg::register_names()[r as usize]))
	}

	fn print_indir_reg(&self, ctx: &mut PrinterCtx, reg: u8) -> FmtResult {
		self.print_register(ctx, reg)
	}

	// only called for `[0xFF00 + C]` style instructions
	fn print_indir_reg_disp(&self, ctx: &mut PrinterCtx, reg: u8, disp: i64) -> FmtResult {
		self.print_int_no_radix(ctx, disp)?;
		ctx.write_str(" + ")?;
		self.print_register(ctx, reg)
	}

	fn print_raw_va(&self, ctx: &mut PrinterCtx, va: VA) -> FmtResult {
		ctx.style_number(&|ctx| write!(ctx, "0x{:04X}", va))
	}

	// --------------------------------------------------------------------------------------------
	// Provided method overrides

	fn mnemonic_max_len(&self) -> usize {
		4
	}

	fn print_operands(&self, ctx: &mut PrinterCtx) -> FmtResult {
		let desc = self.lookup_desc(ctx.get_inst().bytes());

		for (i, syn_op) in desc.syn_ops().iter().enumerate() {
			if i > 0 {
				ctx.write_str(", ")?;
			}

			// comments show which kinds of instructions they appear on, and what the operands
			// for those instructions are.
			match syn_op {
				SynOp::Op => {
					// Uimm8, Imm16, Bit => [UImm]
					// SImm8             => [SImm]
					// Add16, Rel, Rst   => [Mem]
					// BitInd            => [UImm, Indir(Reg)]
					self.print_operand(ctx, 0)?;
					// operand 1 of BitInd is handled by the SynOp::IndReg case below
				}
				SynOp::Op2 => {
					// LdHlImm => [Indir(Reg), UImm]
					self.print_operand(ctx, 1)?;
				}
				SynOp::IndOp => {
					// Add16, AddHi => [Mem]
					ctx.write_char('[')?;
					// print_operand will print out its opinfo so we don't have to do it here.
					self.print_operand(ctx, 0)?;
					ctx.write_char(']')?;
				}
				SynOp::SpPlusOp => {
					// SImm8 => [SImm]
					ctx.write_str("sp + ")?;
					self.print_operand(ctx, 0)?;
				}
				SynOp::Srg(r) => {
					self.print_register(ctx, *r as u8)?;
				}
				SynOp::IndReg(_) => {
					// Ind     => [Indir(Reg)]
					// LdHlImm => [Indir(Reg), UImm]
					// IndHi   => [Indir(RegDisp)]
					// BitInd  => [UImm, Indir(Reg)]

					match ctx.get_inst().ops() {
						[Operand::Indir(MemIndir::Reg { reg }, _), ..] => {
							ctx.write_char('[')?;
							self.print_indir_reg(ctx, *reg)?;
							self.print_mem_opinfo(ctx, 0)?;
							ctx.write_char(']')?;
						}
						[Operand::Indir(MemIndir::RegDisp { reg, disp }, _), ..] => {
							ctx.write_char('[')?;
							self.print_indir_reg_disp(ctx, *reg, *disp)?;
							self.print_mem_opinfo(ctx, 0)?;
							ctx.write_char(']')?;
						}
						[_, Operand::Indir(MemIndir::Reg { reg }, _)] => {
							ctx.write_char('[')?;
							self.print_register(ctx, *reg)?;
							self.print_mem_opinfo(ctx, 1)?;
							ctx.write_char(']')?;
						}
						_ => panic!("{:?}", ctx.get_inst()),
					}
				}
				SynOp::IndHlPlus => {
					// Ind => [Indir(Reg)]
					ctx.write_char('[')?;
					self.print_register(ctx, Reg::HL as u8)?;
					ctx.write_char('+')?;
					self.print_mem_opinfo(ctx, 0)?;
					ctx.write_char(']')?;
				}
				SynOp::IndHlMinus => {
					// Ind => [Indir(Reg)]
					ctx.write_char('[')?;
					self.print_register(ctx, Reg::HL as u8)?;
					ctx.write_char('-')?;
					self.print_mem_opinfo(ctx, 0)?;
					ctx.write_char(']')?;
				}
				SynOp::Cc(c) => {
					match c {
						Cc::C  => ctx.write_str("c")?,
						Cc::NC => ctx.write_str("nc")?,
						Cc::Z  => ctx.write_str("z")?,
						Cc::NZ => ctx.write_str("nz")?,
					}
				}
			}
		}

		Ok(())
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
	fn new_ir_compiler (&self) -> IrCompiler   { GBIrCompiler.into() }
}
