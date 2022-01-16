
use enum_dispatch::enum_dispatch;

use crate::ir::{ IrBuilder, IrReg };
use crate::memory::{ Endian, MmuState, EA, VA };
use crate::program::{ Instruction };

// ------------------------------------------------------------------------------------------------
// Sub-modules
// ------------------------------------------------------------------------------------------------

pub mod gb;
pub mod mos65xx;
pub mod toy;
pub mod error;

pub use error::*;

// ------------------------------------------------------------------------------------------------
// IDisassembler
// ------------------------------------------------------------------------------------------------

use gb::{ GBDisassembler };
use mos65xx::{ Mos65xxDisassembler };
use toy::{ ToyDisassembler };

#[enum_dispatch]
pub enum Disassembler {
	GBDisassembler,
	Mos65xxDisassembler,
	ToyDisassembler,
}

impl Disassembler {
	/// Iterator over all instructions in a slice, where the first one has the given VA.
	pub fn disas_all<'dis, 'img>(&'dis self, img: &'img [u8], state: MmuState, va: VA, ea: EA)
	-> DisasAll<'dis, 'img> {
		DisasAll::new(self, img, state, va, ea)
	}
}

#[enum_dispatch(Disassembler)]
/// Trait for disassemblers.
pub trait IDisassembler : Sized {
	/// Disassemble a single instruction from `img` with the given VA and EA.
	/// Returns the disassembled instruction and the new MMU state (which will be in effect
	/// on the *next* instruction).
	fn disas_instr(&self, img: &[u8], state: MmuState, va: VA, ea: EA)
	-> DisasResult<Instruction>;

	// --------------------------------------------------------------------------------------------
	// Provided methods
}

/// Iterator type. Also lets you find out *why* iteration stopped, like:
///
/// ```ignore
/// let mut iter = dis.disas_all(image, va);
/// for inst in &mut iter {
///     // blah blah
/// }
///
/// if let Some(err) = iter.err() {
///     // do stuff with err and iter.err_offset()/err_va()/err_ea()
/// }
/// ```
pub struct DisasAll<'dis, 'img> {
	disas: &'dis Disassembler,
	img:   &'img [u8],
	state: MmuState,
	va:    VA,
	ea:    EA,
	offs:  usize,
	err:   Option<DisasError>,
}

impl<'dis, 'img> DisasAll<'dis, 'img> {
	fn new(disas: &'dis Disassembler, img: &'img [u8], state: MmuState, va: VA, ea: EA)
	-> Self {
		Self { disas, img, state, va, ea, offs: 0, err: None }
	}

	/// If iteration stopped because of an error, returns that error.
	pub fn err(&self) -> Option<DisasError> {
		self.err
	}

	/// Whether or not iteration stopped because of an error.
	pub fn has_err(&self) -> bool {
		self.err().is_some()
	}

	/// The offset into the slice where an error occurred, if any.
	pub fn err_offset(&self) -> usize {
		self.offs
	}

	/// The VA where an error occurred, if any.
	pub fn err_va(&self) -> VA {
		self.va
	}

	/// The EA where an error occurred, if any.
	pub fn err_ea(&self) -> EA {
		self.ea
	}

	pub fn skip_it(&mut self) {
		self.va += 1;
		self.ea += 1;
		self.offs += 1;
		self.err = None;
	}
}

impl<'dis, 'img> Iterator for DisasAll<'dis, 'img> {
	type Item = Instruction;

	fn next(&mut self) -> Option<Self::Item> {
		if self.offs == self.img.len() {
			// don't want to produce an error when successfully disassembling all instructions
			None
		} else {
			match self.disas.disas_instr(&self.img[self.offs ..], self.state, self.va, self.ea) {
				Ok(inst) => {
					let size = inst.size();
					self.va += size;
					self.ea += size;
					self.offs += size;
					Some(inst)
				}

				Err(e) => {
					self.err = Some(e);
					None
				}
			}
		}
	}
}

// ------------------------------------------------------------------------------------------------
// IPrintStyler, NullPrintStyler
// ------------------------------------------------------------------------------------------------

/// Trait for applying styling to the output of a [`Printer`]. There are multiple styles which
/// each have `begin_` and `end_` methods; they all take a [`std::fmt::Write`] which is the
/// writer associated with a [`PrinterCtx`]. Implementors *may* write things to the writer (e.g.
/// an HTML styler might write HTML tags to it), or they may ignore it completely and do styling
/// some other way (e.g. a GUI may create widgets for each style or something).
pub trait IPrintStyler {
	/// Begin outputting an instruction mnemonic.
	fn begin_mnemonic(&mut self, writer: &mut dyn FmtWrite);
	///
	fn end_mnemonic(&mut self, writer: &mut dyn FmtWrite);

	/// Begin outputting a register name.
	fn begin_register(&mut self, writer: &mut dyn FmtWrite);
	///
	fn end_register(&mut self, writer: &mut dyn FmtWrite);

	/// Begin outputting a number literal.
	fn begin_number(&mut self, writer: &mut dyn FmtWrite);
	///
	fn end_number(&mut self, writer: &mut dyn FmtWrite);

	/// Begin outputting a meaningful symbol (e.g. braces around address calculations, arithmetic
	/// operators).
	fn begin_symbol(&mut self, writer: &mut dyn FmtWrite);
	///
	fn end_symbol(&mut self, writer: &mut dyn FmtWrite);

	/// Begin outputting a string or character literal.
	fn begin_string(&mut self, writer: &mut dyn FmtWrite);
	///
	fn end_string(&mut self, writer: &mut dyn FmtWrite);

	/// Begin outputting a comment.
	fn begin_comment(&mut self, writer: &mut dyn FmtWrite);
	///
	fn end_comment(&mut self, writer: &mut dyn FmtWrite);
}

/// Dummy print styler that ignores all styling commands.
struct NullPrintStyler;

impl IPrintStyler for NullPrintStyler {
	fn begin_mnemonic(&mut self, _writer: &mut dyn FmtWrite) {}
	fn end_mnemonic(&mut self, _writer: &mut dyn FmtWrite) {}
	fn begin_register(&mut self, _writer: &mut dyn FmtWrite) {}
	fn end_register(&mut self, _writer: &mut dyn FmtWrite) {}
	fn begin_number(&mut self, _writer: &mut dyn FmtWrite) {}
	fn end_number(&mut self, _writer: &mut dyn FmtWrite) {}
	fn begin_symbol(&mut self, _writer: &mut dyn FmtWrite) {}
	fn end_symbol(&mut self, _writer: &mut dyn FmtWrite) {}
	fn begin_string(&mut self, _writer: &mut dyn FmtWrite) {}
	fn end_string(&mut self, _writer: &mut dyn FmtWrite) {}
	fn begin_comment(&mut self, _writer: &mut dyn FmtWrite) {}
	fn end_comment(&mut self, _writer: &mut dyn FmtWrite) {}
}

// ------------------------------------------------------------------------------------------------
// PrinterCtx
// ------------------------------------------------------------------------------------------------

use std::fmt::{ Write as FmtWrite, Error as FmtError, Arguments as FmtArguments };
pub type FmtResult = Result<(), FmtError>;

use crate::{ Program, IPlatform, Radix, Operand };

pub struct PrinterCtx<'p, 'i, 'w, 's> {
	prog:      &'p Program,
	inst:      &'i Instruction,
	mmu_state: MmuState,
	writer:    &'w mut dyn FmtWrite,
	styler:    &'s mut dyn IPrintStyler,
}

impl<'p, 'i, 'w, 's> PrinterCtx<'p, 'i, 'w, 's> {
	pub fn new(
		prog: &'p Program,
		inst: &'i Instruction,
		mmu_state: MmuState,
		writer: &'w mut dyn FmtWrite,
		styler: &'s mut dyn IPrintStyler,
	) -> Self {
		Self { prog, inst, mmu_state, writer, styler }
	}

	// --------------------------------------------------------------------------------------------
	// Misc methods

	pub fn num_ops(&self) -> usize {
		self.inst.num_ops()
	}

	// the 'i on the return value is C R U C I A L for borrow checking; otherwise,
	// you cannot get an operand and use the write_ methods simultaneously.
	pub fn get_op(&self, i: usize) -> &'i Operand {
		self.inst.get_op(i)
	}

	// again with the return value lifetime
	pub fn get_inst(&self) -> &'i Instruction {
		self.inst
	}

	pub fn register_name(&self, r: u8) -> &'static str {
		self.prog.plat().arch().register_names()[r as usize]
	}

	pub fn name_of_va(&self, va: VA) -> String {
		self.prog.name_of_va(self.mmu_state, va)
	}

	// --------------------------------------------------------------------------------------------
	// Style methods

	pub fn style_mnemonic(&mut self, f: &dyn Fn(&mut PrinterCtx) -> FmtResult) -> FmtResult {
		self.styler.begin_mnemonic(self.writer);
		f(self)?;
		self.styler.end_mnemonic(self.writer);
		Ok(())
	}

	pub fn style_register(&mut self, f: &dyn Fn(&mut PrinterCtx) -> FmtResult) -> FmtResult {
		self.styler.begin_register(self.writer);
		f(self)?;
		self.styler.end_register(self.writer);
		Ok(())
	}

	pub fn style_number(&mut self, f: &dyn Fn(&mut PrinterCtx) -> FmtResult) -> FmtResult {
		self.styler.begin_number(self.writer);
		f(self)?;
		self.styler.end_number(self.writer);
		Ok(())
	}

	pub fn style_symbol(&mut self, f: &dyn Fn(&mut PrinterCtx) -> FmtResult) -> FmtResult {
		self.styler.begin_symbol(self.writer);
		f(self)?;
		self.styler.end_symbol(self.writer);
		Ok(())
	}

	pub fn style_string(&mut self, f: &dyn Fn(&mut PrinterCtx) -> FmtResult) -> FmtResult {
		self.styler.begin_string(self.writer);
		f(self)?;
		self.styler.end_string(self.writer);
		Ok(())
	}

	pub fn style_comment(&mut self, f: &dyn Fn(&mut PrinterCtx) -> FmtResult) -> FmtResult {
		self.styler.begin_comment(self.writer);
		f(self)?;
		self.styler.end_comment(self.writer);
		Ok(())
	}

	// --------------------------------------------------------------------------------------------
	// std::fmt::Write methods

	pub fn write_str(&mut self, s: &str) -> FmtResult {
		self.writer.write_str(s)
	}

	pub fn write_char(&mut self, c: char) -> FmtResult {
		self.writer.write_char(c)
	}

	/// Because of this method, you can use Rust's built-in `write!()` macro on a `PrinterCtx`
	/// object, like `write!(ctx, "x {}", y)`.
	pub fn write_fmt(&mut self, args: FmtArguments<'_>) -> FmtResult {
		self.writer.write_fmt(args)
	}
}

// ------------------------------------------------------------------------------------------------
// IPrinter
// ------------------------------------------------------------------------------------------------

use gb::{ GBPrinter };
use mos65xx::{ Mos65xxPrinter };
use toy::{ ToyPrinter };

#[enum_dispatch]
pub enum Printer {
	GBPrinter,
	Mos65xxPrinter,
	ToyPrinter,
}

/// Trait for instruction printers.
#[enum_dispatch(Printer)]
pub trait IPrinter {
	/// The length (in characters) of the *longest* instruction mnemonic for this architecture.
	/// This is used to output padding spaces to align operands. Defaults to 8. If set to less
	/// than the actual longest mnemonic, operands will be misaligned in the output.
	/// DO NOT IMPLEMENT THIS METHOD IN ANYTHING OTHER THAN CONSTANT TIME.
	/// (This is not an associated constant because `enum_dispatch` doesn't support those.)
	fn mnemonic_max_len(&self) -> usize {
		8
	}

	/// Give a string representation of an instruction's mnemonic.
	fn get_mnemonic(&self, i: &Instruction) -> String;

	/// Give a string representation of an instruction's operands.
	fn fmt_operands(&self, i: &Instruction, state: MmuState, l: &impl INameLookup) -> String;

	/// Prints an indirect memory access where the address is specified by a register.
	/// It's up to the architecture what this will look like.
	fn print_indir_reg(&self, ctx: &mut PrinterCtx, reg: u8) -> FmtResult {
		// TODO: these should be implemented by implementors
		let _ = (ctx, reg);
		Ok(())
	}

	/// Prints an indirect memory access where the address is specified by a register plus a
	/// displacement. It's up to the architecture what this will look like.
	fn print_indir_reg_disp(&self, ctx: &mut PrinterCtx, reg: u8, disp: i64) -> FmtResult {
		// TODO: these should be implemented by implementors
		let _ = (ctx, reg, disp);
		Ok(())
	}

	// --------------------------------------------------------------------------------------------
	// Provided methods

	/// Give a string representation of an instruction.
	fn fmt_instr(&self, i: &Instruction, state: MmuState, l: &impl INameLookup) -> String {
		format!("{} {}", self.get_mnemonic(i), self.fmt_operands(i, state, l))
	}

	/// Prints the instruction associated with `ctx`. Inserts whitespace padding after
	/// the mnemonic based on what [`mnemonic_max_len`] returns.
	fn print_instr(&self, ctx: &mut PrinterCtx) -> FmtResult {
		let width = self.mnemonic_max_len();
		let mnem = self.get_mnemonic(ctx.get_inst());

		write!(ctx, "{mnem:width$} ")?;
		self.print_operands(ctx)
	}

	/// Prints all operands of the instruction associated with `ctx`, comma-separated.
	/// This is only called by [`print_instr`], so if you override that method (maybe
	/// because your architecture does not fill in all operands), this may go unused.
	fn print_operands(&self, ctx: &mut PrinterCtx) -> FmtResult {
		for i in 0 .. ctx.num_ops() {
			if i > 0 {
				ctx.write_str(", ")?;
			}

			self.print_operand(ctx, i)?;
		}

		Ok(())
	}

	/// Prints the `i`th operand of the instruction associated with `ctx`. This dispatches
	/// to the appropriate `print_` method based on the operand's type.
	fn print_operand(&self, ctx: &mut PrinterCtx, i: usize) -> FmtResult {
		use crate::Operand::*;
		use crate::MemIndir::{ self, RegDisp };

		match ctx.get_op(i) {
			Reg(r)                          => self.print_register(ctx, *r),
			UImm(imm, None)                 => self.print_uint_no_radix(ctx, *imm),
			UImm(imm, Some(Radix::Bin))     => self.print_uint_bin(ctx, *imm),
			UImm(imm, Some(Radix::Dec))     => self.print_uint_dec(ctx, *imm),
			UImm(imm, Some(Radix::Hex))     => self.print_uint_hex(ctx, *imm),
			SImm(imm, None)                 => self.print_int_no_radix(ctx, *imm),
			SImm(imm, Some(Radix::Bin))     => self.print_int_bin(ctx, *imm),
			SImm(imm, Some(Radix::Dec))     => self.print_int_dec(ctx, *imm),
			SImm(imm, Some(Radix::Hex))     => self.print_int_hex(ctx, *imm),
			Mem(addr, _)                    => self.print_va(ctx, *addr),
			Indir(MemIndir::Reg { reg }, _) => self.print_indir_reg(ctx, *reg),
			Indir(RegDisp { reg, disp }, _) => self.print_indir_reg_disp(ctx, *reg, *disp),
		}
	}

	/// Prints the name of a register.
	fn print_register(&self, ctx: &mut PrinterCtx, r: u8) -> FmtResult {
		ctx.style_register(&|ctx| ctx.write_str(ctx.register_name(r)))
	}

	/// Prints an unsigned integer in decimal (base 10).
	fn print_uint_dec(&self, ctx: &mut PrinterCtx, val: u64) -> FmtResult {
		ctx.style_number(&|ctx| write!(ctx, "{}", val))
	}

	/// Prints an unsigned integer in hexadecimal (base 16). Defaults to prepending
	/// the value with `0x` and uses uppercase hex digits.
	fn print_uint_hex(&self, ctx: &mut PrinterCtx, val: u64) -> FmtResult {
		ctx.style_number(&|ctx| write!(ctx, "0x{:X}", val))
	}

	/// Prints an unsigned integer in binary (base 2). Defaults to prepending the
	/// value with `0b`.
	fn print_uint_bin(&self, ctx: &mut PrinterCtx, val: u64) -> FmtResult {
		ctx.style_number(&|ctx| write!(ctx, "0b{:b}", val))
	}

	/// Prints an unsigned integer with no specified radix. Defaults to using decimal for
	/// numbers under 16, and hexadecimal otherwise.
	fn print_uint_no_radix(&self, ctx: &mut PrinterCtx, val: u64) -> FmtResult {
		if val < 0x10 {
			self.print_uint_dec(ctx, val)
		} else {
			self.print_uint_hex(ctx, val)
		}
	}

	/// Prints a signed integer in decimal (base 10).
	fn print_int_dec(&self, ctx: &mut PrinterCtx, val: i64) -> FmtResult {
		ctx.style_number(&|ctx| write!(ctx, "{}", val))
	}

	/// Prints a signed integer in hexadecimal (base 16). Defaults to prepending
	/// the value with `0x` and uses uppercase hex digits. Negative numbers are displayed
	/// like `-0x12` for `-18`, rather than in 2's complement.
	fn print_int_hex(&self, ctx: &mut PrinterCtx, val: i64) -> FmtResult {
		if val < 0 {
			ctx.style_symbol(&|ctx| ctx.write_char('-'))?;
		}

		ctx.style_number(&|ctx| write!(ctx, "0x{:X}", val.abs()))
	}

	/// Prints a signed integer in binary (base 2). Defaults to prepending the
	/// value with `0b`. Negative numbers are displayed like `-0b101` for `-5`, rather than
	/// in 2's complement.
	fn print_int_bin(&self, ctx: &mut PrinterCtx, val: i64) -> FmtResult {
		if val < 0 {
			ctx.style_symbol(&|ctx| ctx.write_char('-'))?;
		}

		ctx.style_number(&|ctx| write!(ctx, "0b{:b}", val.abs()))
	}

	/// Prints a signed integer with no specified radix. Defaults to using decimal for
	/// numbers under 16, and hexadecimal otherwise.
	fn print_int_no_radix(&self, ctx: &mut PrinterCtx, val: i64) -> FmtResult {
		if val.abs() < 0x10 {
			self.print_int_dec(ctx, val)
		} else {
			self.print_int_hex(ctx, val)
		}
	}

	/// Prints the name of thing to which a virtual address refers, based on the MMU state
	/// associated with `ctx`.
	fn print_va(&self, ctx: &mut PrinterCtx, va: VA) -> FmtResult {
		ctx.write_str(&ctx.name_of_va(va))
	}
}

/// Trait to abstract the process of looking up names of addresses.
pub trait INameLookup {
	fn lookup(&self, state: MmuState, addr: VA) -> Option<String>;
}

/// A dummy struct that implements `INameLookup` whose `lookup` method always returns `None`.
pub struct NullLookup;

impl INameLookup for NullLookup {
	fn lookup(&self, _state: MmuState, _addr: VA) -> Option<String> {
		None
	}
}

// ------------------------------------------------------------------------------------------------
// IIrCompiler
// ------------------------------------------------------------------------------------------------

use toy::{ ToyIrCompiler };

#[enum_dispatch]
pub(crate) enum IrCompiler {
	ToyIrCompiler,
}

/// Trait for IR Compilers.
#[enum_dispatch(IrCompiler)]
pub(crate) trait IIrCompiler: Sized + Sync + Send {
	/// Given an instruction, an optional control flow target, and an [`IrBuilder`], convert the
	/// instruction into a sequence of IR instructions.
	fn to_ir(&self, i: &Instruction, target: Option<EA>, b: &mut IrBuilder);

	/// Give a set of registers which can be used to pass arguments.
	fn arg_regs(&self) -> &'static [IrReg];

	/// Give a set of registers which can be used as return values.
	fn return_regs(&self) -> &'static [IrReg];

	/// Give the register which represents the stack pointer.
	fn stack_ptr_reg(&self) -> IrReg;
}

// ------------------------------------------------------------------------------------------------
// IArchitecture
// ------------------------------------------------------------------------------------------------

use gb::{ GBArchitecture };
use mos65xx::{ Mos65xxArchitecture };
use toy::{ ToyArchitecture };

#[enum_dispatch]
pub enum Architecture {
	GBArchitecture,
	Mos65xxArchitecture,
	ToyArchitecture,
}

#[enum_dispatch(Architecture)]
pub(crate) trait IArchitecture: Sized + Sync + Send {
	/// The system's endianness.
	fn endianness(&self) -> Endian;
	/// How many bits in an address.
	fn addr_bits(&self) -> usize;
	/// The names of the registers, in the order that the architecture numbers them in operands.
	/// The signature of this method more or less forces you to return a static/const array, which
	/// you should do. This method should be very fast as it is called often.
	fn register_names(&self) -> &'static [&'static str];
	/// Construct a new disassembler.
	fn new_disassembler(&self) -> Disassembler;
	/// Construct a new printer.
	fn new_printer(&self) -> Printer;

	// TODO: this is just a temporary implementation until all arches have their own
	/// Construct a new IR compiler.
	fn new_ir_compiler(&self) -> IrCompiler {
		ToyIrCompiler.into()
	}
}