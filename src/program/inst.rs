
use parse_display::Display;

use crate::memory::{ EA, VA, MemAccess };
use crate::program::{ Radix };

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

	/// An unresolved memory reference. Refers to a virtual address, but the EA it refers
	/// to has not yet been determined. Converting this to a `Ref` is done using MMU state info
	/// determined by the state change pass.
	VARef { target: VA, info: RefInfo },

	/// A memory reference. The actual address it points to is `target + delta`.
	/// `target` is the base address of the thing being pointed to. This way, a memory reference
	/// can point e.g. into the middle of an array, but be displayed as `arrayname + offset`, where
	/// `offset` comes from the `delta` field.
	Ref { target: EA, delta: isize, info: RefInfo },

	// TODO: more options here for enum values, struct fields, strings...
}

impl Default for OpInfo {
	fn default() -> Self {
		OpInfo::None
	}
}

/// Details associated with a reference.
#[derive(Debug, Display, PartialEq, Eq, Copy, Clone)]
#[display("{:?}")]
pub struct RefInfo {
	/// what kind of reference it is (absolute or relative - relative addresses have an additional
	/// `base` which is factored into the address calculation).
	pub kind: RefKind,

	/// the number of bits in the underlying instruction operand or data item. This may be less than
	/// the number of bits in an address for this architecture.
	pub size: usize,

	/// which part of the address the underlying instruction operand or data item represents. It
	/// might be a full value (like, the full absolute address or the full offset of a relative
	/// address), or it might be the high or low half of the address.
	pub part: RefPart,
}

/// The kind of reference.
#[derive(Debug, Display, PartialEq, Eq, Copy, Clone)]
#[display("{:?}")]
pub enum RefKind {
	/// Absolute address.
	Abs,
	/// Relative address. `base` is the base address to which the relative address (the actual
	/// underlying operand value) is added to compute `OpInfo::Ref.target`.
	Rel { base: EA },
}

/// Which part of the address the operand encodes - either the `Full` address, or just the `Hi` or
/// `Lo` half of the address.
#[derive(Debug, Display, PartialEq, Eq, Copy, Clone)]
#[display("{:?}")]
pub enum RefPart {
	Full,
	Hi,
	Lo,
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
	// TODO: these mutable accessors might be problematic if there are OpInfo kinds which
	// are meant to be "internal-only"...
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

