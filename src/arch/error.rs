
use std::error::Error;

use parse_display::Display;

use crate::memory::{ Location, VA };

// ------------------------------------------------------------------------------------------------
// DisasErrorKind
// ------------------------------------------------------------------------------------------------

/// The kinds of disassembly errors.
#[derive(Debug, Display, PartialEq, Eq, Copy, Clone)]
pub enum DisasErrorKind {
	/// Unknown instruction - undefined opcode.
	#[display("unknown instruction")]
	UnknownInstruction,

	/// Ran out of bytes.
	#[display("out of bytes (expected {expected}, got {got})")]
	OutOfBytes { expected: usize, got: usize },
}

// ------------------------------------------------------------------------------------------------
// DisasError
// ------------------------------------------------------------------------------------------------

/// The disassembly error type.
#[derive(Debug, Display, PartialEq, Eq, Copy, Clone)]
#[display("disassembly error at VA 0x{va:08X}: {kind}")]
pub struct DisasError {
	/// VA passed to `disas_instr`.
	pub va:   VA,
	/// Location passed to `disas_instr`.
	pub loc:  Location,
	/// kind of error.
	pub kind: DisasErrorKind,
}

impl Error for DisasError {}

impl DisasError {
	/// Shorthand constructors.
	pub fn out_of_bytes(va: VA, loc: Location, expected: usize, got: usize) -> DisasError {
		DisasError { va, loc, kind: DisasErrorKind::OutOfBytes { expected, got } }
	}

	/// Ditto.
	pub fn unknown_instruction(va: VA, loc: Location) -> DisasError {
		DisasError { va, loc, kind: DisasErrorKind::UnknownInstruction }
	}
}

// ------------------------------------------------------------------------------------------------
// DisasResult
// ------------------------------------------------------------------------------------------------

/// Alias for a `Result` with a `DisasError` as its error type.
pub type DisasResult<T> = Result<T, DisasError>;