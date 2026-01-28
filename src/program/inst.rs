
use parse_display::Display;

use crate::memory::{ EA, VA };
use crate::program::{ Radix };

// ------------------------------------------------------------------------------------------------
// MemAccess
// ------------------------------------------------------------------------------------------------

const R_BIT: u8 = 1;
const W_BIT: u8 = 2;
const O_BIT: u8 = 4;
const T_BIT: u8 = 8;

/// How a memory operand is accessed. This kind of looks/works like bitflags, but for reasons
/// of ergonomics, `bitflags` is not used. (`bitflags` prevents `use`-ing the names within.)
#[repr(u8)]
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum MemAccess {
	/// A read (load).
	R = R_BIT,
	/// A write (store).
	W = W_BIT,
	/// Read and write.
	RW = R_BIT | W_BIT,
	/// Getting the address without accessing the data at it. (e.g. `lea`, `la`)
	Offset = O_BIT,
	/// Read and offset.
	RO = R_BIT | O_BIT,
	/// Write and offset.
	WO = W_BIT | O_BIT,
	/// Read, write, and offset.
	RWO = R_BIT | W_BIT | O_BIT,
	/// Used as the target of a jump or branch.
	Target = T_BIT,
	/// Read and target.
	RT = R_BIT | T_BIT,
	/// Write and target.
	WT = W_BIT | T_BIT,
	/// Read, write, and target.
	RWT = R_BIT | W_BIT | T_BIT,
	/// Offset and target.
	OT = O_BIT | T_BIT,
	/// Read, offset, and target.
	ROT = R_BIT | O_BIT | T_BIT,
	/// Write, offset, and target.
	WOT = W_BIT | O_BIT | T_BIT,
	/// Read, write, offset, and target.
	RWOT = R_BIT | W_BIT | O_BIT | T_BIT,
}

impl MemAccess {
	/// Does this read memory?
	pub fn reads_mem(&self) -> bool {
		((*self as u8) & R_BIT) != 0
	}

	/// Does this write memory?
	pub fn writes_mem(&self) -> bool {
		((*self as u8) & W_BIT) != 0
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
	Reg     { reg: u8 },
	/// The address is `reg + disp`.
	RegDisp { reg: u8, disp: i64 },
}

// ------------------------------------------------------------------------------------------------
// Operand
// ------------------------------------------------------------------------------------------------

/// Instruction operands.
#[derive(Debug, Display, PartialEq, Eq, Copy, Clone)]
#[display("{:?}")]
pub enum Operand {
	/// A register. The interpretation of the number is up to the architecture.
	Reg(u8),
	/// An unsigned immediate. If the radix is `None`, its value will be displayed in either
	/// decimal (for small values) or hex (for larger ones).
	UImm(u64, Option<Radix>),
	/// A signed immediate. The same radix rules apply.
	SImm(i64, Option<Radix>),
	/// A memory address, along with what kind of access it is.
	Mem(VA, MemAccess),
	/// An indirect memory access, where the address is not part of the instruction.
	Indir(MemIndir, MemAccess),
}

impl Default for Operand {
	fn default() -> Self {
		Operand::Reg(0)
	}
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
			Operand::Mem(va, _) => *va,
			_ => panic!("not a memory operand"),
		}
	}

	/// If this is an unsigned immediate value, get it as an unsigned number; panics otherwise.
	pub fn uimm(&self) -> u64 {
		match self {
			Operand::UImm(i, ..) => *i,
			_ => panic!("not an immediate operand"),
		}
	}

	/// If this is a signed immediate value, get it as a signed number; panics otherwise.
	pub fn simm(&self) -> i64 {
		match self {
			Operand::SImm(i, ..) => *i,
			_ => panic!("not an immediate operand"),
		}
	}

	/// If this is register, get it; panics otherwise.
	pub fn reg(&self) -> u8 {
		match self {
			Operand::Reg(r) => *r,
			Operand::Indir(MemIndir::Reg { reg: r }, ..) => *r,
			_ => panic!("not a register operand"),
		}
	}
}

// ------------------------------------------------------------------------------------------------
// OpInfo
// ------------------------------------------------------------------------------------------------

/// Additional information about operands. This information is either extracted automatically
/// by analysis passes or applied manually by the user. You can think of this as the higher-level
/// "interpretation" of each operand.
#[derive(Debug, Display, PartialEq, Eq, Copy, Clone)]
#[display("{:?}")]
pub enum OpInfo {
	/// No special info associated with it
	None,
	/// A memory reference. The actual address it points to is `target + delta`. `target` is the
	/// base address of the thing being pointed to. This way, a memory reference can point e.g.
	/// into the middle of an array, but be displayed as `arrayname + offset`. The `kind` says what
	/// size and kind of reference it is.
	Ref { target: EA, delta: isize, kind: RefKind },

	// TODO: more options here for enum values, struct fields, strings...
}

impl Default for OpInfo {
	fn default() -> Self {
		OpInfo::None
	}
}

/// The kind of reference.
#[derive(Debug, Display, PartialEq, Eq, Copy, Clone)]
#[display("{:?}")]
pub enum RefKind {
	/// Absolute address. `size` is how many bits the underlying operand value is.
	Abs { size: RefSize },
	/// Relative address. `size` is how many bits the underlying operand value is. `base` is the
	/// base address to which the relative address (the actual underlying operand value) is added
	/// to compute `OpInfo::target`.
	Rel { size: RefSize, base: EA },
}

/// The size of a reference. That is, how many bits were used to store the reference itself -
/// it's common for a reference to be a small number of bits, but it refers indirectly to a
/// larger address space.
#[derive(Debug, Display, PartialEq, Eq, PartialOrd, Ord, Copy, Clone, Hash)]
#[display("{:?}")]
pub enum RefSize {
	_8  = 8,
	_16 = 16,
	_32 = 32,
	_64 = 64,
}

// ------------------------------------------------------------------------------------------------
// InstructionKind
// ------------------------------------------------------------------------------------------------

#[derive(Debug, Display, PartialEq, Eq, Copy, Clone)]
/// What rough class of instruction one is.
pub enum InstructionKind {
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
	/// Control flow - indirect call.
	IndirCall,
	/// Control flow - halt/stop.
	Halt,
}

impl InstructionKind {
	pub fn is_control(&self) -> bool {
		use InstructionKind::*;
		match self {
			Other => false,
			_     => true,
		}
	}

	pub fn has_control_target(&self) -> bool {
		use InstructionKind::*;
		match self {
			Call | Cond | Uncond | Indir | IndirCall => true,
			Ret | Halt | Other                       => false,
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
	va:        VA,
	ea:        EA,
	kind:      InstructionKind,
	target:    Option<VA>,
	ops:       [Operand; MAX_OPS],
	opinfo:    [OpInfo;  MAX_OPS],
	// you might think, "why not have this be a slice into the image?" but
	// on a 64-bit platform a slice would be *twice the size*, and you'd
	// get all the annoyances of slice lifetimes... so nah
	bytes:     [u8; MAX_BYTES],
	num_ops:   u8,
	num_bytes: u8,
}

impl Instruction {
	#[allow(clippy::too_many_arguments)]
	pub(crate) fn new(va: VA, ea: EA, kind: InstructionKind,
	target: Option<VA>, ops: &[Operand], bytes: &[u8]) -> Self {
		let mut ret = Self {
			va,
			ea,
			kind,
			target,
			ops:       Default::default(),
			opinfo:    Default::default(),
			bytes:     Default::default(),
			num_ops:   ops.len() as u8,
			num_bytes: bytes.len() as u8,
		};

		ret.ops[..ops.len()].copy_from_slice(ops);
		ret.bytes[..bytes.len()].copy_from_slice(bytes);

		ret
	}

	/// Get EA.
	pub fn ea(&self) -> EA { self.ea }
	/// Get virtual address.
	pub fn va(&self) -> VA { self.va }
	/// Get the EA of the instruction after this one.
	pub fn next_ea(&self) -> EA { self.ea() + self.size() }
	/// Get the virtual address of the instruction after this one.
	pub fn next_va(&self) -> VA { self.va() + self.size() }
	/// Get size, in bytes.
	pub fn size(&self) -> usize { self.num_bytes as usize }
	/// The original bytes that this instruction was decoded from.
	pub fn bytes(&self) -> &[u8] { &self.bytes[..self.num_bytes as usize] }
	/// How many operands it has.
	pub fn num_ops(&self) -> usize { self.num_ops as usize }
	/// Accessor for operands.
	pub fn get_op(&self, i: usize) -> &Operand {
		assert!(i < self.num_ops as usize);
		&self.ops[i]
	}
	/// The array of operands.
	pub fn ops(&self) -> &[Operand] { &self.ops[..self.num_ops as usize] }
	/// If this is a control instruction, the target address of that control, if it has one.
	pub fn control_target(&self) -> Option<VA> { self.target }
	/// What kind of instruction is this?
	pub fn kind(&self) -> InstructionKind { self.kind }
	/// Is this a control flow instruction?
	pub fn is_control(&self) -> bool { self.kind().is_control() }
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
	/// Is this an indirect call (i.e. through a register)?
	pub fn is_indir_call(&self) -> bool { matches!(self.kind(), InstructionKind::IndirCall) }
	/// Is this some kind of halt instruction from which there is no recovery?
	pub fn is_halt(&self) -> bool { matches!(self.kind(), InstructionKind::Halt) }

	/// Accessor for operand info.
	pub fn get_opinfo(&self, i: usize) -> &OpInfo {
		assert!(i < self.num_ops as usize);
		&self.opinfo[i]
	}
	/// Mutable accessor for operand info.
	pub fn get_opinfo_mut(&mut self, i: usize) -> &mut OpInfo {
		assert!(i < self.num_ops as usize);
		&mut self.opinfo[i]
	}
	/// The array of operand infos.
	pub fn opinfo(&self) -> &[OpInfo] { &self.opinfo[..self.num_ops as usize] }
	/// Mutable array of operand infos.
	pub fn opinfo_mut(&mut self) -> &mut [OpInfo] { &mut self.opinfo[..self.num_ops as usize] }
}

