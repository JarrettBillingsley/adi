
use std::fmt::{
	Write as FmtWrite,
	Error as FmtError,
	Arguments as FmtArguments,
};

use enum_dispatch::enum_dispatch;

use crate::{ MmuState, VA, EA, Instruction, Radix, Operand, OpInfo };

/// Convenient alias for the `Result` type used by `std::fmt::Write`'s methods, and by extension
/// many of the printing methods in this library.
pub type FmtResult = Result<(), FmtError>;

// ------------------------------------------------------------------------------------------------
// PrintStyle
// ------------------------------------------------------------------------------------------------

/// Styles used when outputting instructions. This enum is marked non-exhaustive as more
/// styles may be added in the future. What each kind is for is documented below.
#[non_exhaustive]
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum PrintStyle {
	/// Instruction mnemonic (name).
	Mnemonic,
	/// Register name.
	Register,
	/// Number literals (of any base).
	Number,
	/// Meaningful symbols, like arithmetic operators.
	Symbol,
	/// String and character literals.
	String,
	/// Comments.
	Comment,
	/// Names that reference something else; that is, names that appear as operands.
	Refname,
	/// Labels that define a new name.
	Label,
	/// An instruction operand; the `usize` is its index in the instruction, and can be used
	/// in e.g. GUIs to make them interactive.
	Operand(usize),
}

// ------------------------------------------------------------------------------------------------
// IPrintOutput
// ------------------------------------------------------------------------------------------------

/// Trait for objects that are used as the output of printing operations. This is a supertrait
/// of [`std::fmt::Write`] meaning it inherits those three methods for outputting plain text.
/// It also adds two more methods used for applying styling (and other interesting metadata) to
/// the text being output.
pub trait IPrintOutput: FmtWrite {
	/// Begin some style of text.
	fn begin(&mut self, style: PrintStyle) -> FmtResult;
	/// End some style of text.
	fn end(&mut self, style: PrintStyle) -> FmtResult;
}

// ------------------------------------------------------------------------------------------------
// FmtWritePrintOutput
// ------------------------------------------------------------------------------------------------

/// Print output that wraps a [`std::fmt::Write`] trait object and ignores styling commands.
/// Useful if you want to, say, output to a `String` (which implements that trait).
pub struct FmtWritePrintOutput<'w>(pub &'w mut dyn FmtWrite);

impl FmtWrite for FmtWritePrintOutput<'_> {
	fn write_str(&mut self, s: &str) -> FmtResult {
		self.0.write_str(s)
	}
}

impl IPrintOutput for FmtWritePrintOutput<'_> {
	fn begin(&mut self, _style: PrintStyle) -> FmtResult { Ok(()) }
	fn end(&mut self, _style: PrintStyle) -> FmtResult { Ok(()) }
}

// ------------------------------------------------------------------------------------------------
// ConsolePrintOutput
// ------------------------------------------------------------------------------------------------

/// Outputs to the console with `print!()`. Ignores styling. Not recommended for more than
/// simple tests.
pub struct ConsolePrintOutput;

impl FmtWrite for ConsolePrintOutput {
	fn write_str(&mut self, s: &str) -> FmtResult {
		print!("{}", s);
		Ok(())
	}
}

impl IPrintOutput for ConsolePrintOutput {
	fn begin(&mut self, _style: PrintStyle) -> FmtResult { Ok(()) }
	fn end(&mut self, _style: PrintStyle) -> FmtResult { Ok(()) }
}

// ------------------------------------------------------------------------------------------------
// AnsiConsolePrintOutput
// ------------------------------------------------------------------------------------------------

/// Outputs to the console with `print!()`. Does simple styling with hardcoded ANSI colors. Not
/// recommended for more than simple tests.
pub struct AnsiConsolePrintOutput;

impl FmtWrite for AnsiConsolePrintOutput {
	fn write_str(&mut self, s: &str) -> FmtResult { print!("{}", s); Ok(()) }
	fn write_char(&mut self, c: char) -> FmtResult { print!("{}", c); Ok(()) }
	fn write_fmt(&mut self, args: FmtArguments<'_>) -> FmtResult { print!("{}", args); Ok(()) }
}

impl IPrintOutput for AnsiConsolePrintOutput {
	fn begin(&mut self, style: PrintStyle) -> FmtResult {
		match style {
			PrintStyle::Mnemonic => self.write_str("\x1b[31m"),
			PrintStyle::Number   => self.write_str("\x1b[32m"),
			PrintStyle::String   => self.write_str("\x1b[38;2;255;127;0m"),
			PrintStyle::Comment  => self.write_str("\x1b[32m"),
			PrintStyle::Label    => self.write_str("\x1b[38;2;127;63;0m"),
			PrintStyle::Refname  => self.write_str("\x1b[4m"),
			_ => Ok(()),
		}
	}
	fn end(&mut self, style: PrintStyle) -> FmtResult {
		match style {
			PrintStyle::Mnemonic |
			PrintStyle::Number |
			PrintStyle::String |
			PrintStyle::Comment |
			PrintStyle::Label |
			PrintStyle::Refname => self.write_str("\x1b[0m"),
			_ => Ok(()),
		}
	}
}

// ------------------------------------------------------------------------------------------------
// INameLookup
// ------------------------------------------------------------------------------------------------

/// Trait to abstract the process of looking up names of addresses.
pub trait INameLookup {
	fn lookup(&self, state: MmuState, addr: VA) -> Option<String>;
	fn lookup_ea(&self, ea: EA) -> String;
}

/// A dummy struct that implements `INameLookup` whose `lookup` method always returns `None`.
pub struct NullLookup;

impl INameLookup for NullLookup {
	fn lookup(&self, _state: MmuState, _addr: VA) -> Option<String> {
		None
	}

	fn lookup_ea(&self, ea: EA) -> String {
		format!("{:?}", ea)
	}
}

// ------------------------------------------------------------------------------------------------
// PrinterCtx
// ------------------------------------------------------------------------------------------------

/// A `PrinterCtx` holds onto objects relevant for printing out a single instruction. This is
/// passed as an argument to most methods of [`IPrinter`] and is how most instruction printing
/// is accomplished.
pub struct PrinterCtx<'i, 'l, 's> {
	inst:      &'i Instruction,
	mmu_state: MmuState,
	lookup:    &'l dyn INameLookup,
	output:    &'s mut dyn IPrintOutput,
}

impl<'i, 'l, 's> PrinterCtx<'i, 'l, 's> {
	pub fn new(
		inst: &'i Instruction,
		mmu_state: MmuState,
		lookup: &'l dyn INameLookup,
		output: &'s mut dyn IPrintOutput,
	) -> Self {
		Self { inst, mmu_state, lookup, output }
	}

	// --------------------------------------------------------------------------------------------
	// Misc methods

	/// How many operands the associated instruction has.
	pub fn num_ops(&self) -> usize {
		self.inst.num_ops()
	}

	/// Gets the `i`th operand of the associated instruction.
	// the 'i on the return value is C R U C I A L for borrow checking; otherwise,
	// you cannot get an operand and use the write_ methods simultaneously.
	pub fn get_op(&self, i: usize) -> &'i Operand {
		self.inst.get_op(i)
	}

	/// Gets info of the `i`th operand of the associated instruction.
	// again with the return value lifetime
	pub fn get_opinfo(&self, i: usize) -> &'i OpInfo {
		self.inst.get_opinfo(i)
	}

	/// Gets the associated instruction.
	// again with the return value lifetime
	pub fn get_inst(&self) -> &'i Instruction {
		self.inst
	}

	/// Given a virtual address, tries to get a name for it (using the associated MMU state).
	/// If no useful name exists, returns `None`.
	pub fn name_of_va(&self, va: VA) -> Option<String> {
		self.lookup.lookup(self.mmu_state, va)
	}

	/// Gets the name of an EA.
	pub fn name_of_ea(&self, ea: EA) -> String {
		self.lookup.lookup_ea(ea)
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
		self.output.begin(style)?;
		f(self)?;
		self.output.end(style)
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

	/// Write a string.
	pub fn write_str(&mut self, s: &str) -> FmtResult {
		self.output.write_str(s)
	}

	/// Write a character.
	pub fn write_char(&mut self, c: char) -> FmtResult {
		self.output.write_char(c)
	}

	/// Write formatted text.
	///
	/// Because of this method, you can use Rust's built-in `write!()` macro on a `PrinterCtx`
	/// object, like `write!(ctx, "x {}", y)`.
	pub fn write_fmt(&mut self, args: FmtArguments<'_>) -> FmtResult {
		self.output.write_fmt(args)
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

	/// Prints the name of a register. Be sure to use `ctx.style_register` for proper
	/// output styling.
	fn print_register(&self, ctx: &mut PrinterCtx, r: u8) -> FmtResult;

	/// Prints an indirect memory access where the address is specified by a register.
	/// It's up to the architecture what this will look like.
	fn print_indir_reg(&self, ctx: &mut PrinterCtx, reg: u8) -> FmtResult;

	/// Prints an indirect memory access where the address is specified by a register plus a
	/// displacement. It's up to the architecture what this will look like.
	fn print_indir_reg_disp(&self, ctx: &mut PrinterCtx, reg: u8, disp: i64) -> FmtResult;

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

	/// Prints the instruction associated with `ctx`. Inserts whitespace padding after
	/// the mnemonic based on what [`mnemonic_max_len`] returns.
	fn print_inst(&self, ctx: &mut PrinterCtx) -> FmtResult {
		let width = self.mnemonic_max_len();
		let mnem = self.get_mnemonic(ctx.get_inst());

		ctx.style_mnemonic(&|ctx| write!(ctx, "{mnem:width$} "))?;
		self.print_operands(ctx)
	}

	/// Prints all operands of the instruction associated with `ctx`, comma-separated.
	/// This is only called by [`print_inst`], so if you override that method (maybe
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
				Mem(va, _)                      => {
					self.print_mem_addr(ctx, *va)?;
					self.print_mem_opinfo(ctx, i)
				}
				Indir(MemIndir::Reg { reg }, _) => {
					self.print_indir_reg(ctx, *reg)?;
					self.print_mem_opinfo(ctx, i)
				}
				Indir(RegDisp { reg, disp }, _) => {
					self.print_indir_reg_disp(ctx, *reg, *disp)?;
					self.print_mem_opinfo(ctx, i)
				}
			}
		})
	}

	fn print_mem_opinfo(&self, ctx: &mut PrinterCtx, i: usize) -> FmtResult {
		match ctx.get_opinfo(i) {
			OpInfo::None => Ok(()),
			OpInfo::VARef { target, info: _ } => {
				ctx.style_symbol(&|ctx| write!(ctx, " => "))?;
				self.print_va(ctx, *target)
			}
			OpInfo::Ref { target, delta, info: _ } => {
				ctx.style_symbol(&|ctx| write!(ctx, " => "))?;
				self.print_ea(ctx, *target)?;

				if *delta != 0 {
					ctx.style_symbol(&|ctx| write!(ctx, " + "))?;
					self.print_int_no_radix(ctx, *delta as i64)?;
				}

				Ok(())
			}
		}
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

	/// Prints a memory address operand. By default, dispatches to [`print_va`], but you can
	/// override this to e.g. print brackets around the address to match the asm syntax.
	fn print_mem_addr(&self, ctx: &mut PrinterCtx, va: VA) -> FmtResult {
		self.print_va(ctx, va)
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

	/// If the referenced `ea` has a name (which is discovered through `ctx`), prints that;
	/// otherwise, prints the raw address (with [`print_raw_ea`]).
	fn print_ea(&self, ctx: &mut PrinterCtx, ea: EA) -> FmtResult {
		let name = ctx.name_of_ea(ea);
		ctx.style_refname(&|ctx| ctx.write_str(&name))
	}
}