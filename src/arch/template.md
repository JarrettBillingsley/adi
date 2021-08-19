
Here is a template for an empty architecture. GO NUTS

```rust
//! This is a toy architecture for an imaginary CPU. It exists to make it easier
//! to write tests. It is not stable and can change at any time.

use crate::program::{
	// MemAccess,
	// MemIndir,
	// Operand,
	Instruction,
	BasicBlock,
};
use crate::arch::{
	DisasError, DisasResult,
	Printer, IPrinter,
	Disassembler, IDisassembler,
	Interpreter,
	INameLookup,
	IArchitecture,
	IInterpreter, ValueKind,
};
use crate::memory::{ Memory, MmuState, Endian, EA, VA };

// ------------------------------------------------------------------------------------------------
// Disassembler
// ------------------------------------------------------------------------------------------------

pub struct ToyDisassembler;

impl IDisassembler for ToyDisassembler {
	fn disas_instr(&self, _img: &[u8], _state: MmuState, va: VA, ea: EA)
	-> DisasResult<Instruction> {
		Err(DisasError::unknown_instruction(va, ea))
	}
}

// ------------------------------------------------------------------------------------------------
// Printer
// ------------------------------------------------------------------------------------------------

#[derive(Debug, Copy, Clone)]
pub struct ToyPrinter;

impl ToyPrinter {
	pub fn new() -> Self {
		Self { }
	}
}

impl IPrinter for ToyPrinter {
	fn fmt_mnemonic(&self, _i: &Instruction) -> String {
		"".into()
	}

	fn fmt_operands(&self, _i: &Instruction, _state: MmuState, _l: &impl INameLookup) -> String {
		"".into()
	}
}

// ------------------------------------------------------------------------------------------------
// Interpreter (dummied out)
// ------------------------------------------------------------------------------------------------

pub struct ToyInterpreter;

impl ToyInterpreter {
	fn new() -> Self {
		Self {}
	}
}

impl IInterpreter for ToyInterpreter {
	fn reset(&mut self) {}

	fn interpret_bb(&mut self, _mem: &Memory, _bb: &BasicBlock, _state: Option<MmuState>)
	-> Option<EA> {
		None
	}

	fn last_mmu_state_change(&self) -> Option<(MmuState, ValueKind)> {
		None
	}
}

// ------------------------------------------------------------------------------------------------
// Architecture
// ------------------------------------------------------------------------------------------------

pub struct ToyArchitecture;

impl IArchitecture for ToyArchitecture {
	fn endianness      (&self) -> Endian       { Endian::Little }
	fn addr_bits       (&self) -> usize        { 16 }
	fn new_disassembler(&self) -> Disassembler { ToyDisassembler.into() }
	fn new_printer     (&self) -> Printer      { ToyPrinter::new().into() }
	fn new_interpreter (&self) -> Interpreter  { ToyInterpreter::new().into() }
}
```