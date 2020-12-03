
use std::error::Error;
use std::fmt::{ Display, Formatter, Result as FmtResult };

use crate::memory::*;

// ------------------------------------------------------------------------------------------------
// DisasErrorKind
// ------------------------------------------------------------------------------------------------

/// The kinds of disassembly errors.
#[derive(Debug, PartialEq, Eq)]
pub enum DisasErrorKind {
	/// Unknown instruction - undefined opcode.
	UnknownInstruction,

	/// Ran out of bytes.
	OutOfBytes { expected: usize, got: usize },
}

impl Display for DisasErrorKind {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		use DisasErrorKind::*;

		match self {
			UnknownInstruction =>
				write!(f, "unknown instruction"),

			OutOfBytes { expected, got } =>
				write!(f, "out of bytes (expected {}, got {})", expected, got),
		}
	}
}

// ------------------------------------------------------------------------------------------------
// DisasError
// ------------------------------------------------------------------------------------------------

/// The disassembly error type.
#[derive(Debug, PartialEq, Eq)]
pub struct DisasError {
	/// VA passed to `disas_instr`.
	pub va:   VAddr,
	/// kind of error.
	pub kind: DisasErrorKind,
}

impl Display for DisasError {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		write!(f, "disassembly error at VA 0x{:08X}: {}", self.va, self.kind)
	}
}

impl Error for DisasError {}

impl DisasError {
	/// Shorthand constructors.
	pub fn out_of_bytes(va: VAddr, expected: usize, got: usize) -> DisasError {
		DisasError { va, kind: DisasErrorKind::OutOfBytes { expected, got } }
	}

	/// Ditto.
	pub fn unknown_instruction(va: VAddr) -> DisasError {
		DisasError { va, kind: DisasErrorKind::UnknownInstruction }
	}
}

// ------------------------------------------------------------------------------------------------
// DisasResult
// ------------------------------------------------------------------------------------------------

/// Alias for a `Result` with a `DisasError` as its error type.
pub type DisasResult<T> = Result<T, DisasError>;