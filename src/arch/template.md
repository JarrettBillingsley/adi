
Here is a template for an empty architecture. GO NUTS

```rust
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

pub struct ARCH_NAME_HERE_Disassembler;

impl IDisassembler for ARCH_NAME_HERE_Disassembler {
	fn disas_instr(&self, _img: &[u8], _state: MmuState, va: VA, ea: EA)
	-> DisasResult<Instruction> {
		Err(DisasError::unknown_instruction(va, ea))
	}
}

// ------------------------------------------------------------------------------------------------
// Printer
// ------------------------------------------------------------------------------------------------

#[derive(Debug, Copy, Clone)]
pub struct ARCH_NAME_HERE_Printer;

impl ARCH_NAME_HERE_Printer {
	pub fn new() -> Self {
		Self { }
	}
}

impl IPrinter for ARCH_NAME_HERE_Printer {
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

pub struct ARCH_NAME_HERE_Interpreter;

impl ARCH_NAME_HERE_Interpreter {
	fn new() -> Self {
		Self {}
	}
}

impl IInterpreter for ARCH_NAME_HERE_Interpreter {
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

pub struct ARCH_NAME_HERE_Architecture;

impl IArchitecture for ARCH_NAME_HERE_Architecture {
	fn endianness      (&self) -> Endian       { Endian::Little }
	fn addr_bits       (&self) -> usize        { 16 }
	fn new_disassembler(&self) -> Disassembler { ARCH_NAME_HERE_Disassembler.into() }
	fn new_printer     (&self) -> Printer      { ARCH_NAME_HERE_Printer::new().into() }
	fn new_interpreter (&self) -> Interpreter  { ARCH_NAME_HERE_Interpreter::new().into() }
}
```