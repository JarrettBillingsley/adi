
use std::fmt::{
	Write as FmtWrite,
	Error as FmtError,
	Arguments as FmtArguments,
};

use enum_dispatch::enum_dispatch;

use crate::{ MmuState, VA, Instruction, Radix, Operand };

// ------------------------------------------------------------------------------------------------
// PrintStyle
// ------------------------------------------------------------------------------------------------

/// Styles used when outputting instructions. This enum is marked non-exhaustive as more
/// styles may be added in the future.
#[non_exhaustive]
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum PrintStyle {
	Mnemonic,
	Register,
	Number,
	Symbol,
	String,
	Comment,
	Refname,
	Label,
	Operand(usize),
}

// ------------------------------------------------------------------------------------------------
// IPrintStyler
// ------------------------------------------------------------------------------------------------

/// Trait for applying styling to the output of a [`Printer`]. There are multiple styles which
/// each have `begin_` and `end_` methods; they all take a [`std::fmt::Write`] which is the
/// writer associated with a [`PrinterCtx`]. Implementors *may* write things to the writer (e.g.
/// an HTML styler might write HTML tags to it), or they may ignore it completely and do styling
/// some other way (e.g. a GUI may create widgets for each style or something).
pub trait IPrintStyler {
	/// Begin some style of text.
	fn begin(&mut self, style: PrintStyle, writer: &mut dyn FmtWrite);
	/// End some style of text.
	fn end(&mut self, style: PrintStyle, writer: &mut dyn FmtWrite);
}

// ------------------------------------------------------------------------------------------------
// NullPrintStyler
// ------------------------------------------------------------------------------------------------

/// Dummy print styler that ignores all styling commands.
pub struct NullPrintStyler;

impl IPrintStyler for NullPrintStyler {
	fn begin(&mut self, _style: PrintStyle, _writer: &mut dyn FmtWrite) {}
	fn end(&mut self, _style: PrintStyle, _writer: &mut dyn FmtWrite) {}
}

// ------------------------------------------------------------------------------------------------
// INameLookup
// ------------------------------------------------------------------------------------------------

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
// PrinterCtx
// ------------------------------------------------------------------------------------------------

pub type FmtResult = Result<(), FmtError>;

pub struct PrinterCtx<'i, 'l, 'w, 's> {
	inst:      &'i Instruction,
	mmu_state: MmuState,
	lookup:    &'l dyn INameLookup,
	writer:    &'w mut dyn FmtWrite,
	styler:    &'s mut dyn IPrintStyler,
}

impl<'i, 'l, 'w, 's> PrinterCtx<'i, 'l, 'w, 's> {
	pub fn new(
		inst: &'i Instruction,
		mmu_state: MmuState,
		lookup: &'l dyn INameLookup,
		writer: &'w mut dyn FmtWrite,
		styler: &'s mut dyn IPrintStyler,
	) -> Self {
		Self { inst, mmu_state, lookup, writer, styler }
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

	pub fn name_of_va(&self, va: VA) -> Option<String> {
		self.lookup.lookup(self.mmu_state, va)
	}

	// --------------------------------------------------------------------------------------------
	// Style methods

	/// General-purpose method for styling some output. Use like:
	///
	/// ```ignore
	/// ctx.style(PrintStyle::Number, &|ctx| {
	///     // output stuff with ctx here
	/// })
	/// ```
	///
	/// There are shortcut methods for various styles as well.
	pub fn style(&mut self, style: PrintStyle, f: &dyn Fn(&mut PrinterCtx) -> FmtResult)
	-> FmtResult {
		self.styler.begin(style, self.writer);
		f(self)?;
		self.styler.end(style, self.writer);
		Ok(())
	}

	/// Short for `ctx.style(PrintStyle::Mnemonic, &whatever)`. Same goes for the rest.
	pub fn style_mnemonic(&mut self, f: &dyn Fn(&mut PrinterCtx) -> FmtResult) -> FmtResult {
		self.style(PrintStyle::Mnemonic, f)
	}

	///
	pub fn style_register(&mut self, f: &dyn Fn(&mut PrinterCtx) -> FmtResult) -> FmtResult {
		self.style(PrintStyle::Register, f)
	}

	///
	pub fn style_number(&mut self, f: &dyn Fn(&mut PrinterCtx) -> FmtResult) -> FmtResult {
		self.style(PrintStyle::Number, f)
	}

	///
	pub fn style_symbol(&mut self, f: &dyn Fn(&mut PrinterCtx) -> FmtResult) -> FmtResult {
		self.style(PrintStyle::Symbol, f)
	}

	///
	pub fn style_string(&mut self, f: &dyn Fn(&mut PrinterCtx) -> FmtResult) -> FmtResult {
		self.style(PrintStyle::String, f)
	}

	///
	pub fn style_comment(&mut self, f: &dyn Fn(&mut PrinterCtx) -> FmtResult) -> FmtResult {
		self.style(PrintStyle::Comment, f)
	}

	///
	pub fn style_refname(&mut self, f: &dyn Fn(&mut PrinterCtx) -> FmtResult) -> FmtResult {
		self.style(PrintStyle::Refname, f)
	}

	///
	pub fn style_label(&mut self, f: &dyn Fn(&mut PrinterCtx) -> FmtResult) -> FmtResult {
		self.style(PrintStyle::Label, f)
	}

	///
	pub fn style_operand(&mut self, i: usize, f: &dyn Fn(&mut PrinterCtx) -> FmtResult)
	-> FmtResult {
		self.style(PrintStyle::Operand(i), f)
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

use crate::arch::gb::{ GBPrinter };
use crate::arch::mos65xx::{ Mos65xxPrinter };
use crate::arch::toy::{ ToyPrinter };

#[enum_dispatch]
pub enum Printer {
	GBPrinter,
	Mos65xxPrinter,
	ToyPrinter,
}

/// Trait for instruction printers.
#[enum_dispatch(Printer)]
pub trait IPrinter {
	/// Give a string representation of an instruction's mnemonic.
	fn get_mnemonic(&self, i: &Instruction) -> String;

	/// Give a string representation of an instruction's operands.
	fn fmt_operands(&self, i: &Instruction, state: MmuState, l: &impl INameLookup) -> String;

	/// Prints the name of a register. Be sure to use `ctx.style_register` for proper
	/// output styling.
	fn print_register(&self, ctx: &mut PrinterCtx, r: u8) -> FmtResult;

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

	/// This should print a virtual address in hexadecimal using `ctx.style_number`, with
	/// however many digits are appropriate for this architecture.
	fn print_raw_va(&self, ctx: &mut PrinterCtx, va: VA) -> FmtResult;

	// --------------------------------------------------------------------------------------------
	// Provided methods

	/// The length (in characters) of the *longest* instruction mnemonic for this architecture.
	/// This is used to output padding spaces to align operands. Defaults to 8. If set to less
	/// than the actual longest mnemonic, operands will be misaligned in the output.
	///
	/// **DO NOT IMPLEMENT THIS METHOD IN ANYTHING OTHER THAN CONSTANT TIME.**
	/// This would be an associated constant, but `enum_dispatch` doesn't support those.
	fn mnemonic_max_len(&self) -> usize {
		8
	}

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
	/// to the appropriate `print_` method based on the operand's type. It also calls
	/// `ctx.style_operand` around everything.
	fn print_operand(&self, ctx: &mut PrinterCtx, i: usize) -> FmtResult {
		ctx.style_operand(i, &|ctx| {
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
		})
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

	/// If the referenced `va` has a name (which is discovered through `ctx`), prints that;
	/// otherwise, prints the raw address (with [`print_raw_va`]).
	fn print_va(&self, ctx: &mut PrinterCtx, va: VA) -> FmtResult {
		if let Some(name) = ctx.name_of_va(va) {
			ctx.style_refname(&|ctx| ctx.write_str(&name))
		} else {
			self.print_raw_va(ctx, va)
		}
	}
}