
use crate::memory::{ Endian, IMemory };
use crate::disasm::{ IInstruction, IDisassembler, IPrinter };

// ------------------------------------------------------------------------------------------------
// Sub-modules
// ------------------------------------------------------------------------------------------------

pub mod mos65xx;

// ------------------------------------------------------------------------------------------------
// IArchitecture
// ------------------------------------------------------------------------------------------------

pub trait IArchitecture: Sized + Sync + Send {
	/// Type for one instruction.
	type TInstruction: IInstruction;
	/// Type for the disassembler.
	type TDisassembler: IDisassembler<Self::TInstruction>;
	/// Type for the printer.
	type TPrinter: IPrinter<Self::TInstruction>;
	/// Type for the interpreter.
	type TInterpreter: IInterpreter<Self::TInstruction>;

	/// The system's endianness.
	fn endianness(&self) -> Endian;
	/// How many bits in an address.
	fn addr_bits(&self) -> usize;
	/// Construct a new disassembler.
	fn new_disassembler(&self) -> Self::TDisassembler;
	/// Construct a new printer.
	fn new_printer(&self) -> Self::TPrinter;
	/// Construct a new interpreter.
	fn new_interpreter(&self) -> Self::TInterpreter;
}

// ------------------------------------------------------------------------------------------------
// IInterpreter
// ------------------------------------------------------------------------------------------------

use crate::program::{ BasicBlock };
use crate::memory::{ Location };

pub trait IInterpreter<I: IInstruction>: Sized + Sync + Send {
	fn reset(&mut self);

	// interprets the BB and returns the location of the successor to run, or None if
	// we hit the end
	fn interpret_bb(&mut self, mem: &dyn IMemory, bb: &BasicBlock<I>) -> Option<Location>;
}