
use delegate::delegate;
use enum_dispatch::enum_dispatch;

use crate::{ Offs };
use crate::ir::{ IrBuilder, IrReg, ValSize };
use crate::memory::{ Endian, MmuState, EA, VA };
use crate::program::{ Instruction, BBTerm, RegSet };

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
		self.va += 1u64;
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
					self.va += size as Offs;
					self.ea += size as Offs;
					self.offs += size;

					// terminate iteration if the instruction is a halt
					if inst.is_halt() {
						self.offs = self.img.len();
					}

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

use gb::{ GBIrCompiler };
use toy::{ ToyIrCompiler };
use mos65xx::{ Mos65xxIrCompiler };

#[enum_dispatch]
#[allow(clippy::enum_variant_names)]
pub(crate) enum IrCompiler {
	GBIrCompiler,
	ToyIrCompiler,
	Mos65xxIrCompiler,
}

/// Trait for IR Compilers.
#[enum_dispatch(IrCompiler)]
pub(crate) trait IIrCompiler: Sized + Sync + Send {
	/// Convert `b.inst()` into a sequence of IR instructions. This is called for all instructions
	/// other than the terminator.
	fn build_ir(&self, b: &mut IrBuilder);

	/// Convert the terminating instruction of a basic block into a sequence of IR instructions.
	/// `term` is the basic block's terminator, used to encode control flow targets.
	fn build_ir_term(&self, b: &mut IrBuilder, term: &BBTerm);

	/// Give a set of all architectural registers (that is, those which are programmer-accessible,
	/// not including registers internal to the IR). Do NOT include the stack pointer in this list;
	/// return it from `stack_ptr_reg` instead.
	fn arch_regs(&self) -> &'static [IrReg];

	/// Give the register which represents the stack pointer.
	fn stack_ptr_reg(&self) -> IrReg;

	/// Give the name of a register from its offset, or panic if the offset is invalid.
	fn reg_name(&self, offset: u8) -> &'static str;
}

// ------------------------------------------------------------------------------------------------
// IArchitecture
// ------------------------------------------------------------------------------------------------

use gb::{ GBArchitecture };
use mos65xx::{ Mos65xxArchitecture };
use toy::{ ToyArchitecture };

#[enum_dispatch]
#[derive(Clone, Copy)]
pub enum ArchitectureKind {
	GBArchitecture,
	Mos65xxArchitecture,
	ToyArchitecture,
}

#[enum_dispatch(ArchitectureKind)]
pub(crate) trait IArchitecture: Sized + Sync + Send + Clone + Copy {
	/// The system's endianness.
	fn endianness(&self) -> Endian;
	/// How many bits in an address.
	fn addr_bits(&self) -> usize;
	/// Construct a new disassembler.
	fn new_disassembler(&self) -> Disassembler;
	/// Construct a new printer.
	fn new_printer(&self) -> Printer;
	/// Construct a new IR compiler.
	fn new_ir_compiler(&self) -> IrCompiler;
}

pub struct Architecture {
	kind: ArchitectureKind,
	arch_regs: RegSet,
	reg_sizes: [ValSize; IrReg::MAX_NUM],
}

impl Architecture {
	pub(crate) fn new(kind: ArchitectureKind) -> Self {
		let mut arch_regs = RegSet::new();
		let mut reg_sizes = [ValSize::_8; IrReg::MAX_NUM];

		for reg in kind.new_ir_compiler().arch_regs() {
			arch_regs.insert(reg.offset());
			reg_sizes[reg.offset() as usize] = reg.size();
		}

		Self {
			kind,
			arch_regs,
			reg_sizes,
		}
	}

	delegate! {
		to self.kind {
			/// The system's endianness.
			pub(crate) fn endianness(&self) -> Endian;
			/// How many bits in an address.
			pub(crate) fn addr_bits(&self) -> usize;
			/// Construct a new disassembler.
			pub(crate) fn new_disassembler(&self) -> Disassembler;
			/// Construct a new printer.
			pub(crate) fn new_printer(&self) -> Printer;
			/// Construct a new IR compiler.
			pub(crate) fn new_ir_compiler(&self) -> IrCompiler;
		}
	}

	/// Returns the architectural registers (excluding the stack pointer) as a `RegSet`.
	pub(crate) fn arch_reg_set(&self) -> RegSet {
		self.arch_regs
	}

	/// Given an architectural register's offset, returns it as an `IrReg`. This is useful to go
	/// from register offsets (such as are given by `RegSet::iter()`) back to `IrReg`s.
	///
	/// Panics if `offset` is not the offset of one of the architectural registers.
	pub(crate) fn arch_ir_reg(&self, offset: u8) -> IrReg {
		assert!(self.arch_regs.contains(offset));
		IrReg::new(self.reg_sizes[offset as usize], offset)
	}
}