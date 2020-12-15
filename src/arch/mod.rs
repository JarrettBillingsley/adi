
use crate::memory::{ Endian };
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
	type TDisassembler: IDisassembler<TInstruction = Self::TInstruction>;

	/// Type for the printer.
	type TPrinter: IPrinter<TInstruction = Self::TInstruction>;

	/// The system's endianness.
	fn endianness(&self) -> Endian;

	/// How many bits in an address.
	fn addr_bits(&self) -> usize;

	/// Construct a new disassembler.
	fn new_disassembler(&self) -> Self::TDisassembler;

	/// Construct a new printer.
	fn new_printer(&self) -> Self::TPrinter;
}