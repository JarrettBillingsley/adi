
use std::fmt::{ Debug, Formatter, Result as FmtResult };
use crate::memory::{ EA };

// ------------------------------------------------------------------------------------------------
// Sub-modules
// ------------------------------------------------------------------------------------------------

pub mod builder;
pub mod interp;

pub use builder::*;
pub use interp::*;

// ------------------------------------------------------------------------------------------------
// ValSize
// ------------------------------------------------------------------------------------------------

/// Possible sizes of values used in the IR, measured in bits.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum ValSize {
	_8  = 8,
	_16 = 16,
	_32 = 32,
	_64 = 64,
}

impl ValSize {
	/// How many *bytes* a `ValSize` takes up.
	pub fn bytes(&self) -> usize {
		match self {
			ValSize::_8  => 1,
			ValSize::_16 => 2,
			ValSize::_32 => 4,
			ValSize::_64 => 8,
		}
	}

	fn is_twice(&self, other: ValSize) -> bool {
		match other {
			ValSize::_8  => matches!(self, ValSize::_16),
			ValSize::_16 => matches!(self, ValSize::_32),
			ValSize::_32 => matches!(self, ValSize::_64),
			ValSize::_64 => panic!("can't represent paired 64-bit values"),
		}
	}

	fn name(&self) -> &'static str {
		match self {
			ValSize::_8  => "b",
			ValSize::_16 => "s",
			ValSize::_32 => "i",
			ValSize::_64 => "l",
		}
	}
}

// ------------------------------------------------------------------------------------------------
// IrRegKind
// ------------------------------------------------------------------------------------------------

/// What kinds of [`Reg`]s there are.
#[derive(PartialEq, Eq, Clone, Copy)]
pub enum IrRegKind {
	/// An un-renumbered register. The field is its offset within the register "segment."
	Bare(u16),
	/// An SSA renumbered register. The second field is its "generation."
	Sub(u16, u32),
}

impl Debug for IrRegKind {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		match self {
			IrRegKind::Bare(n)   => write!(f, "r{}", n),
			IrRegKind::Sub(n, i) => write!(f, "r{}_{}", n, i),
		}
	}
}

// ------------------------------------------------------------------------------------------------
// IrReg
// ------------------------------------------------------------------------------------------------

/// A location where a value can be stored. Can appear as the destination/LHS of IR instructions.
#[derive(PartialEq, Eq, Clone, Copy)]
pub struct IrReg {
	size: ValSize,
	kind: IrRegKind,
}

impl Debug for IrReg {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		write!(f, "{:?}.{}", self.kind, self.size.name())
	}
}

impl IrReg {
	/// Constructs an 8-bit register place.
	pub const fn reg8(offs: u16) -> Self {
		Self { size: ValSize::_8, kind: IrRegKind::Bare(offs) }
	}

	/// Constructs a 16-bit register place.
	pub const fn reg16(offs: u16) -> Self {
		Self { size: ValSize::_16, kind: IrRegKind::Bare(offs) }
	}

	/// Constructs a 32-bit register place.
	pub const fn reg32(offs: u16) -> Self {
		Self { size: ValSize::_32, kind: IrRegKind::Bare(offs) }
	}

	/// Constructs a 64-bit register place.
	pub const fn reg64(offs: u16) -> Self {
		Self { size: ValSize::_64, kind: IrRegKind::Bare(offs) }
	}

	/// The size of this place.
	#[inline]
	pub fn size(&self) -> ValSize {
		self.size
	}

	/// What kind of place this is.
	#[inline]
	pub fn kind(&self) -> IrRegKind {
		self.kind
	}

	/// If this is a `IrRegKind::Bare`, returns a new `IrReg` subscripted with the given index.
	/// Panics if this is anything other than `IrRegKind::Bare`.
	fn sub(&self, i: u32) -> Self {
		match self.kind {
			IrRegKind::Bare(r) => Self { kind: IrRegKind::Sub(r, i), ..*self },
			_ => panic!(".sub() called on '{:?}'", self),
		}
	}
}

// ------------------------------------------------------------------------------------------------
// Const
// ------------------------------------------------------------------------------------------------

/// A constant value.
#[derive(PartialEq, Eq, Clone, Copy)]
pub struct Const {
	size: ValSize,
	val:  u64,
}

impl Debug for Const {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		match self.size {
			ValSize::_8  => write!(f, "const 0x{:02X}", self.val),
			ValSize::_16 => write!(f, "const 0x{:04X}", self.val),
			ValSize::_32 => write!(f, "const 0x{:08X}", self.val),
			ValSize::_64 => write!(f, "const 0x{:016X}", self.val),
		}
	}
}

impl Const {
	/// 8-bit constant 0.
	pub const ZERO_8:  Const = Self::_8(0);
	/// 16-bit constant 0.
	pub const ZERO_16: Const = Self::_16(0);
	/// 32-bit constant 0.
	pub const ZERO_32: Const = Self::_32(0);
	/// 64-bit constant 0.
	pub const ZERO_64: Const = Self::_64(0);
	/// 8-bit constant 1.
	pub const ONE_8:   Const = Self::_8(1);
	/// 16-bit constant 1.
	pub const ONE_16:  Const = Self::_16(1);
	/// 32-bit constant 1.
	pub const ONE_32:  Const = Self::_32(1);
	/// 64-bit constant 1.
	pub const ONE_64:  Const = Self::_64(1);

	/// Constructs an 8-bit constant.
	pub const fn _8(val: u8) -> Self {
		Self { size: ValSize::_8, val: val as u64 }
	}

	/// Constructs a 16-bit constant.
	pub const fn _16(val: u16) -> Self {
		Self { size: ValSize::_16, val: val as u64 }
	}

	/// Constructs a 32-bit constant.
	pub const fn _32(val: u32) -> Self {
		Self { size: ValSize::_32, val: val as u64 }
	}

	/// Constructs a 64-bit constant.
	pub const fn _64(val: u64) -> Self {
		Self { size: ValSize::_64, val }
	}
}

// ------------------------------------------------------------------------------------------------
// Src
// ------------------------------------------------------------------------------------------------

/// The source of a value. Can be either a [`IrReg`] or a [`Const`].
#[derive(PartialEq, Eq, Clone, Copy)]
pub enum Src {
	Reg(IrReg),
	Const(Const),
}

impl Debug for Src {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		match self {
			Src::Reg(p) => write!(f, "{:?}", p),
			Src::Const(c) => write!(f, "{:?}", c),
		}
	}
}

impl Src {
	/// The size of this value.
	#[inline]
	pub fn size(&self) -> ValSize {
		match self {
			Src::Reg(IrReg { size, .. }) |
			Src::Const(Const { size, .. }) => *size,
		}
	}
}

impl From<IrReg> for Src {
	fn from(p: IrReg) -> Self {
		Src::Reg(p)
	}
}

impl From<Const> for Src {
	fn from(c: Const) -> Self {
		Src::Const(c)
	}
}

// ------------------------------------------------------------------------------------------------
// IrUnOp, IrBinOp
// ------------------------------------------------------------------------------------------------

/// Unary operations.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum IrUnOp {
	IntZxt,  // dst = zxt(src)
	IntSxt,  // dst = sxt(src)
	IntNeg,  // dst = -src
	IntNot,  // dst = ~src
	BoolNot, // dst = not src
}

/// Binary operations.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum IrBinOp {
	IntEq,      // dst = s1 == s2
	IntNe,      // dst = s1 != s2
	IntSlt,     // dst = s1 < s2  (signed)
	IntSle,     // dst = s1 <= s2 (signed)
	IntUlt,     // dst = s1 < s2  (unsigned)
	IntUle,     // dst = s1 <= s2 (unsigned)

	IntUAdd,    // dst = s1 + s2  (unsigned)
	IntUSub,    // dst = s1 - s2  (unsigned)

	IntCarry,   // dst = true if (s1 unsigned+ s2) has carry-out
	IntSCarry,  // dst = true if (s1 signed+ s2) has carry-out
	IntSBorrow, // dst = true if (s1 signed- s2) has borrow-out
	IntMul,     // dst = s1 * s2
	IntUDiv,    // dst = s1 / s2  (unsigned)
	IntSDiv,    // dst = s1 / s2  (signed)
	IntUMod,    // dst = s1 % s2  (unsigned)
	IntSMod,    // dst = s1 % s2  (signed)

	IntXor,     // dst = s1 ^ s2
	IntAnd,     // dst = s1 & s2
	IntOr,      // dst = s1 | s2
	IntShl,     // dst = s1 << s2
	IntUShr,    // dst = s1 >> s2 (unsigned/logical)
	IntSShr,    // dst = s1 >> s2 (signed/arithmetic)

	IntPair,    // dst = (s1 in upper bits, s2 in lower bits)

	BoolXor,    // dst = s1 != s2
	BoolAnd,    // dst = s1 and s2
	BoolOr,     // dst = s1 or s2
}

/// Ternary operations.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum IrTernOp {
	IntUAddC,    // dst = s1 + s2 + s3  (unsigned, s3 = bool)
	IntUSubB,    // dst = s1 - s2 - s3  (unsigned, s3 = bool)

	IntCarryC,   // dst = true if unsigned sum(s1, s2, s3) has carry-out
	IntSCarryC,  // dst = true if signed sum(s1, s2, s3) has carry-out
	IntSBorrowC, // dst = true if signed (s1 - s2 - s3) has borrow-out
}

// ------------------------------------------------------------------------------------------------
// IrInstKind
// ------------------------------------------------------------------------------------------------

/// Represents IR instructions.
#[derive(PartialEq, Eq, Clone, Copy)]
pub enum IrInstKind {
	Nop,

	Assign  { dst: IrReg, src: Src },  // dst = src
	Load    { dst: IrReg, addr: Src }, // dst = *addr
	Store   { addr: Src,  src: Src },  // *addr = src

	Branch  { target: EA },            // pc = target
	CBranch { cond: Src, target: EA }, // if(cond) pc = target
	IBranch { target: Src },           // pc = src

	Call    { target: EA },  // pc = target (but it's a call)
	ICall   { target: Src }, // pc = src (but it's a call)
	Ret     { target: Src }, // pc = src (but it's a return)

	Unary   { dst: IrReg, op: IrUnOp, src: Src },                          // dst = op src
	Binary  { dst: IrReg, src1: Src, op: IrBinOp, src2: Src },             // dst = src1 op src2
	Ternary { dst: IrReg, src1: Src, op: IrTernOp, src2: Src, src3: Src }, // dst = ...yeah
}

impl Debug for IrInstKind {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		use IrInstKind::*;
		use IrUnOp::*;
		use IrBinOp::*;
		use IrTernOp::*;

		match self {
			Nop =>                      write!(f, "nop"),
			Assign { dst, src } =>      write!(f, "{:?} = {:?}", dst, src),
			Load { dst, addr } =>       write!(f, "{:?} = *{:?}", dst, addr),
			Store { addr,  src } =>     write!(f, "*{:?} = {:?}", addr, src),
			Branch { target } =>        write!(f, "goto {}", target),
			CBranch { cond, target } => write!(f, "if {:?} goto {}", cond, target),
			IBranch { target } =>       write!(f, "goto [{:?}]", target),
			Call { target } =>          write!(f, "call {}", target),
			ICall { target } =>         write!(f, "call [{:?}]", target),
			Ret { target } =>           write!(f, "ret [{:?}]", target),
			Unary { dst, op, src } => match op {
				IntZxt  => write!(f, "{:?} = zxt({:?})", dst, src),
				IntSxt  => write!(f, "{:?} = sxt({:?})", dst, src),
				IntNeg  => write!(f, "{:?} = -{:?}", dst, src),
				IntNot  => write!(f, "{:?} = ~{:?}", dst, src),
				BoolNot => write!(f, "{:?} = not {:?}", dst, src),
			},
			Binary { dst, src1, op, src2 } => match op {
				IntEq      => write!(f, "{:?} = {:?} == {:?}", dst, src1, src2),
				IntNe      => write!(f, "{:?} = {:?} != {:?}", dst, src1, src2),
				IntSlt     => write!(f, "{:?} = {:?} s< {:?}", dst, src1, src2),
				IntSle     => write!(f, "{:?} = {:?} s<= {:?}", dst, src1, src2),
				IntUlt     => write!(f, "{:?} = {:?} u< {:?}", dst, src1, src2),
				IntUle     => write!(f, "{:?} = {:?} u<= {:?}", dst, src1, src2),
				IntUAdd    => write!(f, "{:?} = {:?} u+ {:?}", dst, src1, src2),
				IntUSub    => write!(f, "{:?} = {:?} u- {:?}", dst, src1, src2),
				IntCarry   => write!(f, "{:?} = ucarry({:?}, {:?})", dst, src1, src2),
				IntSCarry  => write!(f, "{:?} = scarry({:?}, {:?})", dst, src1, src2),
				IntSBorrow => write!(f, "{:?} = sborrow({:?}, {:?})", dst, src1, src2),
				IntMul     => write!(f, "{:?} = {:?} * {:?}", dst, src1, src2),
				IntUDiv    => write!(f, "{:?} = {:?} u/ {:?}", dst, src1, src2),
				IntSDiv    => write!(f, "{:?} = {:?} s/ {:?}", dst, src1, src2),
				IntUMod    => write!(f, "{:?} = {:?} u% {:?}", dst, src1, src2),
				IntSMod    => write!(f, "{:?} = {:?} s% {:?}", dst, src1, src2),
				IntXor     => write!(f, "{:?} = {:?} ^ {:?}", dst, src1, src2),
				IntAnd     => write!(f, "{:?} = {:?} & {:?}", dst, src1, src2),
				IntOr      => write!(f, "{:?} = {:?} | {:?}", dst, src1, src2),
				IntShl     => write!(f, "{:?} = {:?} << {:?}", dst, src1, src2),
				IntUShr    => write!(f, "{:?} = {:?} u>> {:?}", dst, src1, src2),
				IntSShr    => write!(f, "{:?} = {:?} s>> {:?}", dst, src1, src2),
				IntPair    => write!(f, "{:?} = pair(hi = {:?}, lo = {:?})", dst, src1, src2),
				BoolXor    => write!(f, "{:?} = {:?} boolxor {:?}", dst, src1, src2),
				BoolAnd    => write!(f, "{:?} = {:?} booland {:?}", dst, src1, src2),
				BoolOr     => write!(f, "{:?} = {:?} boolor {:?}", dst, src1, src2),
			},
			Ternary { dst, src1, op, src2, src3 } => match op {
				IntUAddC    => write!(f, "{:?} = {:?} u+ {:?} u+ {:?}", dst, src1, src2, src3),
				IntUSubB    => write!(f, "{:?} = {:?} u- {:?} u- {:?}", dst, src1, src2, src3),
				IntCarryC   => write!(f, "{:?} = ucarryc({:?}, {:?}, {:?})", dst, src1, src2, src3),
				IntSCarryC  => write!(f, "{:?} = scarryc({:?}, {:?}, {:?})", dst, src1, src2, src3),
				IntSBorrowC => write!(f, "{:?} = sborrowc({:?}, {:?}, {:?})", dst, src1, src2, src3),
			},
		}
	}
}

impl IrInstKind {
	/// Gets ths size of the source value(s).
	/// Panics if called on an instruction that has no source.
	pub fn src_size(&self) -> ValSize {
		match self {
			IrInstKind::Nop
			// | IrInstKind::IrBranch { .. }
			| IrInstKind::Branch { .. }
			| IrInstKind::IBranch { .. }
			| IrInstKind::Call { .. }
			| IrInstKind::ICall { .. }
			| IrInstKind::Ret { .. } => panic!("no source"),

			IrInstKind::Assign    { src, .. }  => src.size(),
			IrInstKind::Load      { dst, .. }  => dst.size(), // yes, it's weird
			IrInstKind::Store     { src, .. }  => src.size(),
			// IrInstKind::IrCBranch { cond, .. } => cond.size(),
			IrInstKind::CBranch   { cond, .. } => cond.size(),
			IrInstKind::Unary     { src, .. }  => src.size(),
			IrInstKind::Binary    { src1, .. } => src1.size(),
			IrInstKind::Ternary   { src1, .. } => src1.size(),
		}
	}

	/// Gets ths size of the destination place.
	/// Panics if called on an instruction that has no destination.
	pub fn dst_size(&self) -> ValSize {
		match self {
			IrInstKind::Nop
			// | IrInstKind::IrBranch { .. }
			| IrInstKind::Branch { .. }
			| IrInstKind::IBranch { .. }
			| IrInstKind::Call { .. }
			| IrInstKind::ICall { .. }
			| IrInstKind::Ret { .. }
			// | IrInstKind::IrCBranch { .. }
			| IrInstKind::CBranch { .. } => panic!("no destination"),

			IrInstKind::Assign  { dst, .. } => dst.size(),
			IrInstKind::Load    { dst, .. } => dst.size(),
			IrInstKind::Store   { src, .. } => src.size(), // yes, it's weird
			IrInstKind::Unary   { dst, .. } => dst.size(),
			IrInstKind::Binary  { dst, .. } => dst.size(),
			IrInstKind::Ternary { dst, .. } => dst.size(),
		}
	}
}

// ------------------------------------------------------------------------------------------------
// IrInst
// ------------------------------------------------------------------------------------------------

#[derive(PartialEq, Eq, Clone, Copy)]
pub struct IrInst {
	ea:   EA,
	kind: IrInstKind,
}

impl Debug for IrInst {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		write!(f, "{:?} {:?}", self.ea, self.kind)
	}
}

impl IrInst {
	///
	pub fn nop(ea: EA) -> Self {
		Self { ea, kind: IrInstKind::Nop }
	}

	///
	pub fn assign(ea: EA, dst: IrReg, src: Src) -> Self {
		assert!(dst.size() == src.size());
		Self { ea, kind: IrInstKind::Assign { dst, src } }
	}

	///
	pub fn izxt(ea: EA, dst: IrReg, src: Src) -> Self {
		assert!(dst.size() > src.size());
		Self { ea, kind: IrInstKind::Unary { dst, op: IrUnOp::IntZxt, src } }
	}

	///
	pub fn isxt(ea: EA, dst: IrReg, src: Src) -> Self {
		assert!(dst.size() > src.size());
		Self { ea, kind: IrInstKind::Unary { dst, op: IrUnOp::IntSxt, src } }
	}

	///
	pub fn ineg(ea: EA, dst: IrReg, src: Src) -> Self {
		assert!(dst.size() == src.size());
		Self { ea, kind: IrInstKind::Unary { dst, op: IrUnOp::IntNeg, src } }
	}

	///
	pub fn inot(ea: EA, dst: IrReg, src: Src) -> Self {
		assert!(dst.size() == src.size());
		Self { ea, kind: IrInstKind::Unary { dst, op: IrUnOp::IntNot, src } }
	}

	///
	pub fn bnot(ea: EA, dst: IrReg, src: Src) -> Self {
		assert!(dst.size() == src.size());
		Self { ea, kind: IrInstKind::Unary { dst, op: IrUnOp::BoolNot, src } }
	}

	///
	pub fn ieq(ea: EA, dst: IrReg, src1: Src, src2: Src) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::IntEq, src2 } }
	}

	///
	pub fn ine(ea: EA, dst: IrReg, src1: Src, src2: Src) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::IntNe, src2 } }
	}

	///
	pub fn islt(ea: EA, dst: IrReg, src1: Src, src2: Src) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::IntSlt, src2 } }
	}

	///
	pub fn isle(ea: EA, dst: IrReg, src1: Src, src2: Src) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::IntSle, src2 } }
	}

	///
	pub fn iult(ea: EA, dst: IrReg, src1: Src, src2: Src) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::IntUlt, src2 } }
	}

	///
	pub fn iule(ea: EA, dst: IrReg, src1: Src, src2: Src) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::IntUle, src2 } }
	}

	///
	pub fn iuadd(ea: EA, dst: IrReg, src1: Src, src2: Src) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::IntUAdd, src2 } }
	}

	///
	pub fn iuaddc(ea: EA, dst: IrReg, src1: Src, src2: Src, src3: Src) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Ternary { dst, src1, op: IrTernOp::IntUAddC, src2, src3 } }
	}

	///
	pub fn iusub(ea: EA, dst: IrReg, src1: Src, src2: Src) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::IntUSub, src2 } }
	}

	///
	pub fn iusubb(ea: EA, dst: IrReg, src1: Src, src2: Src, src3: Src) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Ternary { dst, src1, op: IrTernOp::IntUSubB, src2, src3 } }
	}

	///
	pub fn icarry(ea: EA, dst: IrReg, src1: Src, src2: Src) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::IntCarry, src2 } }
	}

	///
	pub fn icarryc(ea: EA, dst: IrReg, src1: Src, src2: Src, src3: Src) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Ternary { dst, src1, op: IrTernOp::IntCarryC, src2, src3 } }
	}

	///
	pub fn iscarry(ea: EA, dst: IrReg, src1: Src, src2: Src) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::IntSCarry, src2 } }
	}

	///
	pub fn iscarryc(ea: EA, dst: IrReg, src1: Src, src2: Src, src3: Src) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Ternary { dst, src1, op: IrTernOp::IntSCarryC, src2, src3 } }
	}

	///
	pub fn isborrow(ea: EA, dst: IrReg, src1: Src, src2: Src) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::IntSBorrow, src2 } }
	}

	///
	pub fn isborrowc(ea: EA, dst: IrReg, src1: Src, src2: Src, src3: Src) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Ternary { dst, src1, op: IrTernOp::IntSBorrowC, src2, src3 } }
	}

	///
	pub fn imul(ea: EA, dst: IrReg, src1: Src, src2: Src) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::IntMul, src2 } }
	}

	///
	pub fn iudiv(ea: EA, dst: IrReg, src1: Src, src2: Src) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::IntUDiv, src2 } }
	}

	///
	pub fn isdiv(ea: EA, dst: IrReg, src1: Src, src2: Src) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::IntSDiv, src2 } }
	}

	///
	pub fn iumod(ea: EA, dst: IrReg, src1: Src, src2: Src) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::IntUMod, src2 } }
	}

	///
	pub fn ismod(ea: EA, dst: IrReg, src1: Src, src2: Src) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::IntSMod, src2 } }
	}

	///
	pub fn ixor(ea: EA, dst: IrReg, src1: Src, src2: Src) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::IntXor, src2 } }
	}

	///
	pub fn iand(ea: EA, dst: IrReg, src1: Src, src2: Src) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::IntAnd, src2 } }
	}

	///
	pub fn ior(ea: EA, dst: IrReg, src1: Src, src2: Src) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::IntOr, src2 } }
	}

	///
	pub fn ishl(ea: EA, dst: IrReg, src1: Src, src2: Src) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::IntShl, src2 } }
	}

	///
	pub fn iushr(ea: EA, dst: IrReg, src1: Src, src2: Src) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::IntUShr, src2 } }
	}

	///
	pub fn isshr(ea: EA, dst: IrReg, src1: Src, src2: Src) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::IntSShr, src2 } }
	}

	///
	pub fn ipair(ea: EA, dst: IrReg, src1: Src, src2: Src) -> Self {
		assert!(src1.size() == src2.size());
		assert!(dst.size().is_twice(src1.size()));
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::IntPair, src2 } }
	}

	///
	pub fn bxor(ea: EA, dst: IrReg, src1: Src, src2: Src) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::BoolXor, src2 } }
	}

	///
	pub fn band(ea: EA, dst: IrReg, src1: Src, src2: Src) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::BoolAnd, src2 } }
	}

	///
	pub fn bor(ea: EA, dst: IrReg, src1: Src, src2: Src) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::BoolOr, src2 } }
	}

	///
	pub fn load(ea: EA, dst: IrReg, addr: Src) -> Self {
		Self { ea, kind: IrInstKind::Load { dst, addr } }
	}

	///
	pub fn store(ea: EA, addr: Src, src: Src) -> Self {
		Self { ea, kind: IrInstKind::Store { addr, src } }
	}

	// ///
	// pub fn irbranch(ea: EA, offs: i32) -> Self {
	// 	Self { ea, kind: IrInstKind::IrBranch { offs } }
	// }

	// ///
	// pub fn ircbranch(ea: EA, cond: Src, offs: i32) -> Self {
	// 	Self { ea, kind: IrInstKind::IrCBranch { cond, offs } }
	// }

	///
	pub fn branch(ea: EA, target: EA) -> Self {
		Self { ea, kind: IrInstKind::Branch { target } }
	}

	///
	pub fn cbranch(ea: EA, cond: Src, target: EA) -> Self {
		Self { ea, kind: IrInstKind::CBranch { cond, target } }
	}

	///
	pub fn ibranch(ea: EA, target: Src) -> Self {
		Self { ea, kind: IrInstKind::IBranch { target } }
	}

	///
	pub fn call(ea: EA, target: EA) -> Self {
		Self { ea, kind: IrInstKind::Call { target } }
	}

	///
	pub fn icall(ea: EA, target: Src) -> Self {
		Self { ea, kind: IrInstKind::ICall { target } }
	}

	///
	pub fn ret(ea: EA, target: Src) -> Self {
		Self { ea, kind: IrInstKind::Ret { target } }
	}

	// --------------------------------------------------------------------------------------------

	/// The EA of the real instruction to which this belongs.
	pub fn ea(&self) -> EA {
		self.ea
	}

	/// What kind of instruction this is.
	pub fn kind(&self) -> IrInstKind {
		self.kind
	}
}

// ------------------------------------------------------------------------------------------------
// IrPhi
// ------------------------------------------------------------------------------------------------

// #[derive(PartialEq, Eq, Clone, Copy)]
// pub struct IrPhi {
// 	dst:
// }