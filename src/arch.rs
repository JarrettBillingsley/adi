
use enum_dispatch::enum_dispatch;

use crate::ir::{ IrBuilder, IrReg };
use crate::memory::{ Endian, MmuState, EA, VA };
use crate::program::{ Instruction };

// ------------------------------------------------------------------------------------------------
// Sub-modules
// ------------------------------------------------------------------------------------------------

pub mod gb;
pub mod mos65xx;
pub mod toy;
pub mod error;
pub mod print;

pub use error::*;
pub use print::*;

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
	fn disas_inst(&self, img: &[u8], state: MmuState, va: VA, ea: EA)
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
///     // do stuff with err and iter.err_offset()/err_va()/err_ea()
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
			match self.disas.disas_inst(&self.img[self.offs ..], self.state, self.va, self.ea) {
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
// IIrCompiler
// ------------------------------------------------------------------------------------------------

use toy::{ ToyIrCompiler };

#[enum_dispatch]
pub(crate) enum IrCompiler {
	ToyIrCompiler,
}

/// Trait for IR Compilers.
#[enum_dispatch(IrCompiler)]
pub(crate) trait IIrCompiler: Sized + Sync + Send {
	/// Given an instruction, an optional control flow target, and an [`IrBuilder`], convert the
	/// instruction into a sequence of IR instructions.
	fn to_ir(&self, i: &Instruction, target: Option<EA>, b: &mut IrBuilder);

	/// Give a set of registers which can be used to pass arguments.
	fn arg_regs(&self) -> &'static [IrReg];

	/// Give a set of registers which can be used as return values.
	fn return_regs(&self) -> &'static [IrReg];

	/// Give the register which represents the stack pointer.
	fn stack_ptr_reg(&self) -> IrReg;
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
pub(crate) trait IArchitecture: Sized + Sync + Send {
	/// The system's endianness.
	fn endianness(&self) -> Endian;
	/// How many bits in an address.
	fn addr_bits(&self) -> usize;
	/// Construct a new disassembler.
	fn new_disassembler(&self) -> Disassembler;
	/// Construct a new printer.
	fn new_printer(&self) -> Printer;

	// TODO: this is just a temporary implementation until all arches have their own
	/// Construct a new IR compiler.
	fn new_ir_compiler(&self) -> IrCompiler {
		ToyIrCompiler.into()
	}
}