
use enum_dispatch::enum_dispatch;

use crate::memory::{ Endian, IMemory, MmuState, Location, VA };
use crate::program::{ Instruction, BasicBlock };

// ------------------------------------------------------------------------------------------------
// Sub-modules
// ------------------------------------------------------------------------------------------------

pub mod mos65xx;
pub mod error;

pub use error::*;

// ------------------------------------------------------------------------------------------------
// IDisassembler
// ------------------------------------------------------------------------------------------------

use mos65xx::{ Mos65xxDisassembler };

#[enum_dispatch]
pub enum Disassembler {
	Mos65xxDisassembler,
}

impl Disassembler {
	/// Iterator over all instructions in a slice, where the first one has the given VA.
	pub fn disas_all<'dis, 'img>(&'dis self, img: &'img [u8], state: MmuState, va: VA, loc: Location)
	-> DisasAll<'dis, 'img> {
		DisasAll::new(self, img, state, va, loc)
	}
}

#[enum_dispatch(Disassembler)]
/// Trait for disassemblers.
pub trait IDisassembler : Sized {
	/// Disassemble a single instruction from `img` with the given VA and Location.
	/// Returns the disassembled instruction and the new MMU state (which will be in effect
	/// on the *next* instruction).
	fn disas_instr(&self, img: &[u8], state: MmuState, va: VA, loc: Location)
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
///     // do stuff with err and iter.err_offs()
/// }
/// ```
pub struct DisasAll<'dis, 'img> {
	disas: &'dis Disassembler,
	img:   &'img [u8],
	state: MmuState,
	va:    VA,
	loc:   Location,
	offs:  usize,
	err:   Option<DisasError>,
}

impl<'dis, 'img> DisasAll<'dis, 'img> {
	fn new(disas: &'dis Disassembler, img: &'img [u8], state: MmuState, va: VA, loc: Location)
	-> Self {
		Self { disas, img, state, va, loc, offs: 0, err: None }
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

	/// The Location where an error occurred, if any.
	pub fn err_loc(&self) -> Location {
		self.loc
	}

	pub fn skip_it(&mut self) {
		self.va += 1;
		self.loc += 1;
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
			match self.disas.disas_instr(&self.img[self.offs ..], self.state, self.va, self.loc) {
				Ok(inst) => {
					let size = inst.size();
					self.va += size;
					self.loc += size;
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
// IPrinter
// ------------------------------------------------------------------------------------------------

use mos65xx::{ Mos65xxPrinter };

#[enum_dispatch]
pub enum Printer {
	Mos65xxPrinter,
}

/// Trait for instruction printers.
#[enum_dispatch(Printer)]
pub trait IPrinter {
	/// Give a string representation of an instruction's mnemonic.
	fn fmt_mnemonic(&self, i: &Instruction) -> String;

	/// Give a string representation of an instruction's operands.
	fn fmt_operands(&self, i: &Instruction, state: MmuState, l: &impl INameLookup) -> String;

	// --------------------------------------------------------------------------------------------
	// Provided methods

	/// Give a string representation of an instruction.
	fn fmt_instr(&self, i: &Instruction, state: MmuState, l: &impl INameLookup) -> String {
		format!("{} {}", self.fmt_mnemonic(i), self.fmt_operands(i, state, l))
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
// IInterpreter
// ------------------------------------------------------------------------------------------------

use mos65xx::{ Mos65xxInterpreter };

#[enum_dispatch]
pub enum Interpreter {
	Mos65xxInterpreter,
}

#[enum_dispatch(Interpreter)]
pub trait IInterpreter: Sized + Sync + Send {
	fn reset(&mut self);

	// interprets the BB and returns the location of the successor to run, or None if
	// we hit the end
	fn interpret_bb(&mut self, mem: &dyn IMemory, bb: &BasicBlock) -> Option<Location>;
}

// ------------------------------------------------------------------------------------------------
// IArchitecture
// ------------------------------------------------------------------------------------------------

use mos65xx::{ Mos65xxArchitecture };

#[enum_dispatch]
pub enum Architecture {
	Mos65xxArchitecture,
}

#[enum_dispatch(Architecture)]
pub trait IArchitecture: Sized + Sync + Send {
	/// The system's endianness.
	fn endianness(&self) -> Endian;
	/// How many bits in an address.
	fn addr_bits(&self) -> usize;
	/// Construct a new disassembler.
	fn new_disassembler(&self) -> Disassembler;
	/// Construct a new printer.
	fn new_printer(&self) -> Printer;
	/// Construct a new interpreter.
	fn new_interpreter(&self) -> Interpreter;
}