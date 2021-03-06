
use parse_display::Display;
use smallvec::{ SmallVec };

use crate::memory::{ Location, VA };

// ------------------------------------------------------------------------------------------------
// MemAccess
// ------------------------------------------------------------------------------------------------

/// How a memory operand is accessed.
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum MemAccess {
	/// A read (load).
	R,
	/// A write (store).
	W,
	/// Read *and* write.
	RW,
	/// Getting the address without accessing the data at it. (e.g. `lea`, `la`)
	Offset,
	/// Used as the target of a jump or branch.
	Target,
}

impl MemAccess {
	/// Does this read memory?
	pub fn reads_mem(&self) -> bool {
		use MemAccess::*;
		matches!(self, R | RW)
	}

	/// Does this write memory?
	pub fn writes_mem(&self) -> bool {
		use MemAccess::*;
		matches!(self, W | RW)
	}
}

// ------------------------------------------------------------------------------------------------
// MemIndir
// ------------------------------------------------------------------------------------------------

/// An indirect memory access.
/// In all variants, the interpretation of the register is up to the architecture.
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum MemIndir {
	/// The register holds the address.
	Reg        { reg: u8 },
	/// The address is `reg + disp`.
	RegDisp    { reg: u8, disp: i64 },
	/// x86-style `reg1 + reg2*scale + disp`.
	RegRegDisp { reg: u8, reg2: u8, scale: u8, disp: i64 },
}

impl MemIndir {
	/// The base (first) register.
	pub fn base_reg(&self) -> u8 {
		use MemIndir::*;
		match self {
			Reg { reg } | RegDisp { reg, .. } | RegRegDisp { reg, .. } => *reg
		}
	}

	/// The displacement. `MemIndir::Reg` has a displacement of 0.
	pub fn disp(&self) -> i64 {
		use MemIndir::*;
		match self {
			Reg { .. } => 0,
			RegDisp { disp, .. } | RegRegDisp { disp, .. } => *disp
		}
	}
}

// ------------------------------------------------------------------------------------------------
// Operand
// ------------------------------------------------------------------------------------------------

/// Instruction operands.
#[derive(Debug, Display, PartialEq, Eq, Copy, Clone)]
#[display("{:?}")]
pub enum Operand {
	/// A register. The interpretation of the number is up to the architecture.
	Reg(u64),
	/// An unsigned immediate.
	UImm(u64),
	/// A signed immediate.
	SImm(i64),
	/// A memory address, along with what kind of access it is.
	Mem(u64, MemAccess),
	/// An indirect memory access, where the address is not part of the instruction.
	Indir(MemIndir, MemAccess),
}

impl Operand {
	/// Does this refer to a register?
	pub fn is_reg(&self) -> bool {
		matches!(self, Operand::Reg(..))
	}

	/// Is this an immediate value (but NOT a memory address)?
	pub fn is_imm(&self) -> bool {
		matches!(self, Operand::UImm(..) | Operand::SImm(..))
	}

	/// Does this operand access memory?
	pub fn is_mem(&self) -> bool {
		self.access().is_some()
	}

	/// Does this operand have a hard-coded address?
	pub fn has_addr(&self) -> bool {
		matches!(self, Operand::Mem(..))
	}

	/// How, if any way, does this operand access memory?
	pub fn access(&self) -> Option<MemAccess> {
		use Operand::*;
		match self {
			Mem(_, a) | Indir(_, a) => Some(*a),
			_                       => None,
		}
	}

	/// If `access` is Some (`is_mem` returns true), get the address it refers to; panics otherwise.
	pub fn addr(&self) -> VA {
		match self {
			Operand::Mem(a, _) => VA(*a as usize),
			_ => panic!("not a memory operand"),
		}
	}

	/// If this is an unsigned immediate value, get it as an unsigned number; panics otherwise.
	pub fn uimm(&self) -> u64 {
		match self {
			Operand::UImm(i) => *i,
			_ => panic!("not an immediate operand"),
		}
	}

	/// If this is a signed immediate value, get it as a signed number; panics otherwise.
	pub fn simm(&self) -> i64 {
		match self {
			Operand::SImm(i) => *i,
			_ => panic!("not an immediate operand"),
		}
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
const MAX_BYTES: usize = 8;

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
	/// The array of operands.
	pub fn ops(&self) -> &[Operand] { &self.ops }
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

