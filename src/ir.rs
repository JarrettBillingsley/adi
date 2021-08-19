
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
}

// ------------------------------------------------------------------------------------------------
// PlaceKind
// ------------------------------------------------------------------------------------------------

/// What kinds of [`Place`]s there are.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum PlaceKind {
	/// A register. The field is its offset within the register "segment."
	Reg(u16),
	/// A memory address.
	Mem(EA),
}

// ------------------------------------------------------------------------------------------------
// Place
// ------------------------------------------------------------------------------------------------

/// A location where a value can be stored. Can appear as the destination/LHS of IR instructions.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Place {
	size: ValSize,
	kind: PlaceKind,
}

impl Place {
	/// Constructs an 8-bit register place.
	pub const fn reg8(offs: u16) -> Self {
		Self { size: ValSize::_8, kind: PlaceKind::Reg(offs) }
	}

	/// Constructs a 16-bit register place.
	pub const fn reg16(offs: u16) -> Self {
		Self { size: ValSize::_16, kind: PlaceKind::Reg(offs) }
	}

	/// Constructs a 32-bit register place.
	pub const fn reg32(offs: u16) -> Self {
		Self { size: ValSize::_32, kind: PlaceKind::Reg(offs) }
	}

	/// Constructs a 64-bit register place.
	pub const fn reg64(offs: u16) -> Self {
		Self { size: ValSize::_64, kind: PlaceKind::Reg(offs) }
	}

	/// Constructs an 8-bit memory place.
	pub const fn mem8(ea: EA) -> Self {
		Self { size: ValSize::_8, kind: PlaceKind::Mem(ea) }
	}

	/// Constructs a 16-bit memory place.
	pub const fn mem16(ea: EA) -> Self {
		Self { size: ValSize::_16, kind: PlaceKind::Mem(ea) }
	}

	/// Constructs a 32-bit memory place.
	pub const fn mem32(ea: EA) -> Self {
		Self { size: ValSize::_32, kind: PlaceKind::Mem(ea) }
	}

	/// Constructs a 64-bit memory place.
	pub const fn mem64(ea: EA) -> Self {
		Self { size: ValSize::_64, kind: PlaceKind::Mem(ea) }
	}

	/// The size of this place.
	#[inline]
	pub fn size(&self) -> ValSize {
		self.size
	}

	/// What kind of place this is.
	#[inline]
	pub fn kind(&self) -> PlaceKind {
		self.kind
	}
}

// ------------------------------------------------------------------------------------------------
// Const
// ------------------------------------------------------------------------------------------------

/// A constant value.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Const {
	size: ValSize,
	val:  u64,
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

/// The source of a value. Can be either a [`Place`] or a [`Const`].
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Src {
	Place(Place),
	Const(Const),
}

impl Src {
	/// The size of this value.
	#[inline]
	pub fn size(&self) -> ValSize {
		match self {
			Src::Place(Place { size, .. }) |
			Src::Const(Const { size, .. }) => *size,
		}
	}
}

impl From<Place> for Src {
	fn from(p: Place) -> Self {
		Src::Place(p)
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

	BoolXor,    // dst = s1 != s2
	BoolAnd,    // dst = s1 and s2
	BoolOr,     // dst = s1 or s2
}

// ------------------------------------------------------------------------------------------------
// IrInstKind
// ------------------------------------------------------------------------------------------------

/// Represents IR instructions.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum IrInstKind {
	Nop,

	Assign    { dst: Place, src: Src },                          // dst = src
	Load      { dst: Place, addr: Src },                         // dst = *addr
	Store     { addr: Src,  src: Src },                          // *addr = src

	IrBranch  { offs: i32 },                                     // irpc += offs
	IrCBranch { cond: Src, offs: i32 },                          // if(cond) irpc += offs

	Branch    { target: EA },                                    // pc = target
	CBranch   { cond: Src, target: EA },                         // if(cond) pc = target
	IBranch   { target: Src },                                   // pc = src

	Call      { target: EA },                                    // pc = target (but it's a call)
	ICall     { target: Src },                                   // pc = src (but it's a call)
	Ret       { target: Src },                                   // pc = src (but it's a return)

	Unary     { dst: Place, op: IrUnOp, src: Src },              // dst = op src
	Binary    { dst: Place, src1: Src, op: IrBinOp, src2: Src }, // dst = src1 op src2
	// Custom { dst: Place, src1: Src, op: u8, src2: Src },      // dst = op(src1, src2)
}

impl IrInstKind {
	/// Gets ths size of the source value(s).
	/// Panics if called on an instruction that has no source.
	pub fn src_size(&self) -> ValSize {
		match self {
			IrInstKind::Nop
			| IrInstKind::IrBranch { .. }
			| IrInstKind::Branch { .. }
			| IrInstKind::IBranch { .. }
			| IrInstKind::Call { .. }
			| IrInstKind::ICall { .. }
			| IrInstKind::Ret { .. } => panic!("no source"),

			IrInstKind::Assign    { src, .. }  => src.size(),
			IrInstKind::Load      { dst, .. }  => dst.size(), // yes, it's weird
			IrInstKind::Store     { src, .. }  => src.size(),
			IrInstKind::IrCBranch { cond, .. } => cond.size(),
			IrInstKind::CBranch   { cond, .. } => cond.size(),
			IrInstKind::Unary     { src, .. }  => src.size(),
			IrInstKind::Binary    { src1, .. } => src1.size(),
		}
	}

	/// Gets ths size of the destination place.
	/// Panics if called on an instruction that has no destination.
	pub fn dst_size(&self) -> ValSize {
		match self {
			IrInstKind::Nop
			| IrInstKind::IrBranch { .. }
			| IrInstKind::Branch { .. }
			| IrInstKind::IBranch { .. }
			| IrInstKind::Call { .. }
			| IrInstKind::ICall { .. }
			| IrInstKind::Ret { .. }
			| IrInstKind::IrCBranch { .. }
			| IrInstKind::CBranch { .. } => panic!("no destination"),

			IrInstKind::Assign { dst, .. } => dst.size(),
			IrInstKind::Load   { dst, .. } => dst.size(),
			IrInstKind::Store  { src, .. } => src.size(), // yes, it's weird
			IrInstKind::Unary  { dst, .. } => dst.size(),
			IrInstKind::Binary { dst, .. } => dst.size(),
		}
	}
}

// ------------------------------------------------------------------------------------------------
// IrInst
// ------------------------------------------------------------------------------------------------

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct IrInst {
	ea:   EA,
	kind: IrInstKind,
}

impl IrInst {
	///
	pub fn nop(ea: EA) -> Self {
		Self { ea, kind: IrInstKind::Nop }
	}

	///
	pub fn assign(ea: EA, dst: Place, src: Src) -> Self {
		assert!(dst.size() == src.size());
		Self { ea, kind: IrInstKind::Assign { dst, src } }
	}

	///
	pub fn izxt(ea: EA, dst: Place, src: Src) -> Self {
		assert!(dst.size() > src.size());
		Self { ea, kind: IrInstKind::Unary { dst, op: IrUnOp::IntZxt, src } }
	}

	///
	pub fn isxt(ea: EA, dst: Place, src: Src) -> Self {
		assert!(dst.size() > src.size());
		Self { ea, kind: IrInstKind::Unary { dst, op: IrUnOp::IntSxt, src } }
	}

	///
	pub fn ineg(ea: EA, dst: Place, src: Src) -> Self {
		assert!(dst.size() == src.size());
		Self { ea, kind: IrInstKind::Unary { dst, op: IrUnOp::IntNeg, src } }
	}

	///
	pub fn inot(ea: EA, dst: Place, src: Src) -> Self {
		assert!(dst.size() == src.size());
		Self { ea, kind: IrInstKind::Unary { dst, op: IrUnOp::IntNot, src } }
	}

	///
	pub fn bnot(ea: EA, dst: Place, src: Src) -> Self {
		assert!(dst.size() == src.size());
		Self { ea, kind: IrInstKind::Unary { dst, op: IrUnOp::BoolNot, src } }
	}

	///
	pub fn ieq(ea: EA, dst: Place, src1: Src, src2: Src) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::IntEq, src2 } }
	}

	///
	pub fn ine(ea: EA, dst: Place, src1: Src, src2: Src) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::IntNe, src2 } }
	}

	///
	pub fn islt(ea: EA, dst: Place, src1: Src, src2: Src) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::IntSlt, src2 } }
	}

	///
	pub fn isle(ea: EA, dst: Place, src1: Src, src2: Src) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::IntSle, src2 } }
	}

	///
	pub fn iult(ea: EA, dst: Place, src1: Src, src2: Src) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::IntUlt, src2 } }
	}

	///
	pub fn iule(ea: EA, dst: Place, src1: Src, src2: Src) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::IntUle, src2 } }
	}

	///
	pub fn iuadd(ea: EA, dst: Place, src1: Src, src2: Src) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::IntUAdd, src2 } }
	}

	///
	pub fn iusub(ea: EA, dst: Place, src1: Src, src2: Src) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::IntUSub, src2 } }
	}

	///
	pub fn icarry(ea: EA, dst: Place, src1: Src, src2: Src) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::IntCarry, src2 } }
	}

	///
	pub fn iscarry(ea: EA, dst: Place, src1: Src, src2: Src) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::IntSCarry, src2 } }
	}

	///
	pub fn isborrow(ea: EA, dst: Place, src1: Src, src2: Src) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::IntSBorrow, src2 } }
	}

	///
	pub fn imul(ea: EA, dst: Place, src1: Src, src2: Src) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::IntMul, src2 } }
	}

	///
	pub fn iudiv(ea: EA, dst: Place, src1: Src, src2: Src) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::IntUDiv, src2 } }
	}

	///
	pub fn isdiv(ea: EA, dst: Place, src1: Src, src2: Src) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::IntSDiv, src2 } }
	}

	///
	pub fn iumod(ea: EA, dst: Place, src1: Src, src2: Src) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::IntUMod, src2 } }
	}

	///
	pub fn ismod(ea: EA, dst: Place, src1: Src, src2: Src) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::IntSMod, src2 } }
	}

	///
	pub fn ixor(ea: EA, dst: Place, src1: Src, src2: Src) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::IntXor, src2 } }
	}

	///
	pub fn iand(ea: EA, dst: Place, src1: Src, src2: Src) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::IntAnd, src2 } }
	}

	///
	pub fn ior(ea: EA, dst: Place, src1: Src, src2: Src) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::IntOr, src2 } }
	}

	///
	pub fn ishl(ea: EA, dst: Place, src1: Src, src2: Src) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::IntShl, src2 } }
	}

	///
	pub fn iushr(ea: EA, dst: Place, src1: Src, src2: Src) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::IntUShr, src2 } }
	}

	///
	pub fn isshr(ea: EA, dst: Place, src1: Src, src2: Src) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::IntSShr, src2 } }
	}

	///
	pub fn bxor(ea: EA, dst: Place, src1: Src, src2: Src) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::BoolXor, src2 } }
	}

	///
	pub fn band(ea: EA, dst: Place, src1: Src, src2: Src) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::BoolAnd, src2 } }
	}

	///
	pub fn bor(ea: EA, dst: Place, src1: Src, src2: Src) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::BoolOr, src2 } }
	}

	///
	pub fn load(ea: EA, dst: Place, addr: Src) -> Self {
		Self { ea, kind: IrInstKind::Load { dst, addr } }
	}

	///
	pub fn store(ea: EA, addr: Src, src: Src) -> Self {
		Self { ea, kind: IrInstKind::Store { addr, src } }
	}

	///
	pub fn irbranch(ea: EA, offs: i32) -> Self {
		Self { ea, kind: IrInstKind::IrBranch { offs } }
	}

	///
	pub fn ircbranch(ea: EA, cond: Src, offs: i32) -> Self {
		Self { ea, kind: IrInstKind::IrCBranch { cond, offs } }
	}

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