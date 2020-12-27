
use parse_display::Display;
use smallvec::{ SmallVec };

use crate::memory::{ MmuState, Location, VA };

// ------------------------------------------------------------------------------------------------
// Sub-modules
// ------------------------------------------------------------------------------------------------

pub mod error;

pub use error::*;

// ------------------------------------------------------------------------------------------------
// MemAccess
// ------------------------------------------------------------------------------------------------

/// How a memory operand is accessed by an instruction.
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum MemAccess {
	/// A read (load).
	Read,
	/// A write (store).
	Write,
	/// Read-modify-write (both read and write).
	Rmw,
	/// Used as an immediate (e.g. put into a register, or used as the base address in an EA).
	Offset,
	/// Used as the target of a jump or branch.
	Target,
}

// ------------------------------------------------------------------------------------------------
// Operand
// ------------------------------------------------------------------------------------------------

/// Instruction operands.
#[derive(Debug, Display, PartialEq, Eq, Copy, Clone)]
#[display("{:?}")]
pub enum Operand {
	/// A register. The interpretation of the number is up to the architecture.
	Reg(usize),

	/// An unsigned immediate that fits in 8 bits.
	UImm8(u8),

	/// A 16-bit memory address, along with what kind of access it is.
	Mem16(u16, MemAccess),
}

impl Operand {
	/// Does this refer to a register?
	pub fn is_reg(&self) -> bool { matches!(self, Operand::Reg(..)) }

	/// Is this an immediate value (but NOT a memory address)?
	pub fn is_imm(&self) -> bool { matches!(self, Operand::UImm8(..)) }

	/// How, if any way, does this operand access memory?
	pub fn access(&self) -> Option<MemAccess> {
		match self {
			Operand::Mem16(_, a) => Some(*a),
			_ => None,
		}
	}

	/// If `access` is Some (`is_mem` returns true), get the address it refers to; panics otherwise.
	pub fn addr(&self) -> VA {
		match self {
			Operand::Mem16(a, _) => VA(*a as usize),
			_ => panic!("not a memory operand"),
		}
	}

	/// If this is an immediate value, get it as an unsigned number; panics otherwise.
	pub fn uimm(&self) -> u64 {
		match self {
			Operand::UImm8(i) => *i as u64,
			_ => panic!("not an immediate operand"),
		}
	}

	/// If this is an immediate value, get it as a signed number; panics otherwise.
	pub fn simm(&self) -> i64 {
		match self {
			Operand::UImm8(i) => (*i as i8) as i64,
			_ => panic!("not an immediate operand"),
		}
	}

	/// Does this operand access memory?
	pub fn is_mem(&self) -> bool {
		self.access().is_some()
	}
}

// ------------------------------------------------------------------------------------------------
// InstructionKind
// ------------------------------------------------------------------------------------------------

#[derive(Debug, Display, PartialEq, Eq, Copy, Clone)]
/// What rough class of instruction one is.
pub enum InstructionKind {
	/// Invalid instruction.
	Invalid,
	/// Something else.
	Other,
	/// Control flow - function call.
	Call,
	/// Control flow - function return.
	Ret,
	/// Control flow - conditional jump/branch.
	Cond,
	/// Control flow - unconditional jump/branch.
	Uncond,
	/// Control flow - indirect jump/branch.
	Indir,
	/// Control flow - halt/stop.
	Halt,
}

impl InstructionKind {
	pub fn is_control(&self) -> bool {
		use InstructionKind::*;
		match self {
			Call | Ret | Cond | Uncond | Indir | Halt  => true,
			Invalid | Other => false,
		}
	}

	pub fn has_control_target(&self) -> bool {
		use InstructionKind::*;
		match self {
			Call | Cond | Uncond | Indir  => true,
			Ret | Halt | Invalid | Other => false,
		}
	}
}

// ------------------------------------------------------------------------------------------------
// InstructionKind
// ------------------------------------------------------------------------------------------------

const MAX_OPS:   usize = 3; // ? we'll see
const MAX_BYTES: usize = 3;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Instruction {
	pub(crate) va:     VA,
	pub(crate) loc:    Location,
	pub(crate) kind:   InstructionKind,
	pub(crate) target: Option<VA>,
	pub(crate) ops:    SmallVec<[Operand; MAX_OPS]>,
	pub(crate) bytes:  SmallVec<[u8; MAX_BYTES]>,
}

impl Instruction {
	#[allow(clippy::too_many_arguments)]
	pub(crate) fn new(va: VA, loc: Location, kind: InstructionKind,
	target: Option<VA>, ops: &[Operand], bytes: &[u8]) -> Self {
		Self {
			va,
			loc,
			kind,
			target,
			ops:   SmallVec::from_slice(ops),
			bytes: SmallVec::from_slice(bytes)
		}
	}

	/// Get Location.
	pub fn loc(&self) -> Location { self.loc }
	/// Get virtual address.
	pub fn va(&self) -> VA { self.va }
	/// Get the Location of the instruction after this one.
	pub fn next_loc(&self) -> Location { self.loc() + self.size() }
	/// Get the virtual address of the instruction after this one.
	pub fn next_va(&self) -> VA { self.va() + self.size() }
	/// Get size, in bytes.
	pub fn size(&self) -> usize { self.bytes.len() }
	/// The original bytes that this instruction was decoded from.
	pub fn bytes(&self) -> &[u8] { &self.bytes }
	/// How many operands it has.
	pub fn num_ops(&self) -> usize { self.ops.len() }
	/// Accessor for operands.
	pub fn get_op(&self, i: usize) -> &Operand { &self.ops[i] }
	/// If this is a control instruction, the target address of that control, if it has one.
	pub fn control_target(&self) -> Option<VA> { self.target }
	/// What kind of instruction is this?
	pub fn kind(&self) -> InstructionKind { self.kind }
	/// Is this a control flow instruction?
	pub fn is_control(&self) -> bool { self.kind().is_control() }
	/// Is this an invalid instruction?
	pub fn is_invalid(&self) -> bool { matches!(self.kind(), InstructionKind::Invalid) }
	/// Is this some other kind of instruction?
	pub fn is_other(&self) -> bool { matches!(self.kind(), InstructionKind::Other) }
	/// Is this a function call?
	pub fn is_call(&self) -> bool { matches!(self.kind(), InstructionKind::Call) }
	/// Is this a function return ?
	pub fn is_ret(&self) -> bool { matches!(self.kind(), InstructionKind::Ret) }
	/// Is this a conditional jump/branch?
	pub fn is_cond(&self) -> bool { matches!(self.kind(), InstructionKind::Cond) }
	/// Is this an unconditional jump/branch?
	pub fn is_uncond(&self) -> bool { matches!(self.kind(), InstructionKind::Uncond) }
	/// Is this an indirect jump/branch (i.e. through a register)?
	pub fn is_indir(&self) -> bool { matches!(self.kind(), InstructionKind::Indir) }
	/// Is this some kind of halt instruction from which there is no recovery?
	pub fn is_halt(&self) -> bool { matches!(self.kind(), InstructionKind::Halt) }
}

// ------------------------------------------------------------------------------------------------
// IDisassembler
// ------------------------------------------------------------------------------------------------

/// Trait for disassemblers.
pub trait IDisassembler : Sized {
	/// Disassemble a single instruction from `img` with the given VA and Location.
	/// Returns the disassembled instruction and the new MMU state (which will be in effect
	/// on the *next* instruction).
	fn disas_instr(&self, img: &[u8], state: MmuState, va: VA, loc: Location)
	-> DisasResult<Instruction>;

	// --------------------------------------------------------------------------------------------
	// Provided methods

	/// Iterator over all instructions in a slice, where the first one has the given VA.
	fn disas_all<'dis, 'img>(&'dis self, img: &'img [u8], state: MmuState, va: VA, loc: Location)
	-> DisasAll<'dis, 'img, Self> {
		DisasAll::new(self, img, state, va, loc)
	}
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
pub struct DisasAll<'dis, 'img, D: IDisassembler> {
	disas: &'dis D,
	img:   &'img [u8],
	state: MmuState,
	va:    VA,
	loc:   Location,
	offs:  usize,
	err:   Option<DisasError>,
}

impl<'dis, 'img, D: IDisassembler> DisasAll<'dis, 'img, D> {
	fn new(disas: &'dis D, img: &'img [u8], state: MmuState, va: VA, loc: Location) -> Self {
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

impl<'dis, 'img, D: IDisassembler> Iterator for DisasAll<'dis, 'img, D> {
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

/// Trait for instruction printers.
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