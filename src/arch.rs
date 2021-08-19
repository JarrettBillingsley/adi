
use std::fmt::{ Debug, Formatter, Result as FmtResult };

use enum_dispatch::enum_dispatch;

use crate::memory::{ Endian, Memory, MmuState, EA, VA };
use crate::program::{ Instruction, BasicBlock };

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
///     // do stuff with err and iter.err_offs()
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
// Value, ValueKind
// ------------------------------------------------------------------------------------------------

#[derive(PartialEq, Eq, Copy, Clone)]
pub enum ValueKind {
	Indeterminate,
	Immediate,
	Computed,
	Loaded(Option<VA>), // the address it was loaded from, if known.
	Argument(usize),    // the "i'th" argument. (65xx uses Reg::* indexes)
	Return,             // came out of a function
}

impl Debug for ValueKind {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		use ValueKind::*;

		match self {
			Indeterminate   => write!(f, "???"),
			Immediate       => write!(f, "IMM"),
			Computed        => write!(f, "COMPUTED"),
			Loaded(None)    => write!(f, "LOAD(????)"),
			Loaded(Some(a)) => write!(f, "LOAD({:04X})", a.0),
			Argument(i)     => write!(f, "ARG({})", i),
			Return          => write!(f, "RETURNVAL"),
		}
	}
}

impl Default for ValueKind {
	fn default() -> Self { ValueKind::Indeterminate }
}

#[derive(PartialEq, Eq, Copy, Clone, Default)]
pub struct Value<T> {
	pub val:  T,
	pub kind: ValueKind,
}

impl<T: std::fmt::UpperHex> Debug for Value<T> {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		write!(f, "({:0width$X}, {:?})", self.val, self.kind,
			width = core::mem::size_of::<T>() * 2)
	}
}

impl<T> Value<T> {
	pub fn ind (val: T) -> Self                   { Self { val, kind: ValueKind::Indeterminate } }
	pub fn imm (val: T) -> Self                   { Self { val, kind: ValueKind::Immediate     } }
	pub fn comp(val: T) -> Self                   { Self { val, kind: ValueKind::Computed      } }
	pub fn load(val: T, addr: Option<VA>) -> Self { Self { val, kind: ValueKind::Loaded(addr)  } }
	pub fn arg (val: T, i: usize) -> Self         { Self { val, kind: ValueKind::Argument(i)   } }
	pub fn ret (val: T) -> Self                   { Self { val, kind: ValueKind::Return        } }
}

// ------------------------------------------------------------------------------------------------
// IInterpreter
// ------------------------------------------------------------------------------------------------

use gb::{ GBInterpreter };
use mos65xx::{ Mos65xxInterpreter };
use toy::{ ToyInterpreter };

#[enum_dispatch]
pub enum Interpreter {
	GBInterpreter,
	Mos65xxInterpreter,
	ToyInterpreter,
}

#[enum_dispatch(Interpreter)]
pub trait IInterpreter: Sized + Sync + Send {
	// reset all state.
	fn reset(&mut self);

	// interprets the BB and returns the EA of the successor to run, or None if
	// we hit the end. state is optional state to replace bb.mmu_state().
	fn interpret_bb(&mut self, mem: &Memory, bb: &BasicBlock, state: Option<MmuState>)
	-> Option<EA>;

	// the most recent MMU state change that was encountered when interpreting the last BB.
	fn last_mmu_state_change(&self) -> Option<(MmuState, ValueKind)>;
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