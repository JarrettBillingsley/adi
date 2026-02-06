
use crate::memory::{ MemAccess };

use super::*;

// ------------------------------------------------------------------------------------------------
// IrUnOp, IrBinOp
// ------------------------------------------------------------------------------------------------

/// Unary operations.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub(crate) enum IrUnOp {
	IntZxt,  // dst = zxt(src)
	IntSxt,  // dst = sxt(src)
	IntNeg,  // dst = -src
	IntNot,  // dst = ~src
	BoolNot, // dst = not src
}

/// Binary operations.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub(crate) enum IrBinOp {
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
pub(crate) enum IrTernOp {
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
pub(crate) enum IrInstKind {
	Nop,

	Use     { reg: IrReg },            // dummy use of reg
	Assign  { dst: IrReg, src: IrSrc,  // dst = src
		dstn: i8, srcn: i8, },
	Load    { dst: IrReg, addr: IrSrc, // dst = *addr
		dstn: i8, addrn: i8, },
	Store   { addr: IrSrc, src: IrSrc, // *addr = src
		addrn: i8, srcn: i8, },

	Branch  { target: EA,              // pc = target (also subsumes jumps)
		targetn: i8, },
	CBranch { cond: IrSrc, target: EA, // if(cond) pc = target
		condn: i8, targetn: i8, },
	IBranch { target: IrSrc,           // pc = target (but it's indirect)
		targetn: i8, },

	Call    { target: EA,              // pc = target (but it's a call)
		targetn: i8, },
	ICall   { target: IrSrc,           // pc = target (but it's an indirect call)
		targetn: i8, },
	Ret     { target: IrSrc,           // pc = target (but it's a return)
		targetn: i8, },

	Unary   { dst: IrReg, op: IrUnOp, src: IrSrc, // dst = op src
		dstn: i8, srcn: i8, },
	Binary  { dst: IrReg, src1: IrSrc, op: IrBinOp, src2: IrSrc, // dst = src1 op src2
		dstn: i8, src1n: i8, src2n: i8, },
	Ternary { dst: IrReg, src1: IrSrc, op: IrTernOp, src2: IrSrc, src3: IrSrc, // dst = ...yeah
		dstn: i8, src1n: i8, src2n: i8, src3n: i8, },
}

// helper type for printing out operand numbers more easily
#[derive(PartialEq, Eq, Clone, Copy)]
struct Opn<'a>(&'a i8);

impl<'a> Debug for Opn<'a> {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		match *self.0 {
			n if n >= 0 => write!(f, "{{{}}}", n),
			_           => Ok(())
		}
	}
}

impl Debug for IrInstKind {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		use IrInstKind::*;
		use IrUnOp::*;
		use IrBinOp::*;
		use IrTernOp::*;

		match self {
			Nop =>
				write!(f, "nop"),
			Use { reg } =>
				write!(f, "use       {:?}", reg),
			Assign { dst, src, dstn, srcn } =>
				write!(f, "mov       {:?}{:?}, {:?}{:?}", dst, Opn(dstn), src, Opn(srcn)),
			Load { dst, addr, dstn, addrn } =>
				write!(f, "load      {:?}{:?}, [{:?}{:?}]", dst, Opn(dstn), addr, Opn(addrn)),
			Store { addr, src, addrn, srcn } =>
				write!(f, "store     [{:?}{:?}], {:?}{:?}", addr, Opn(addrn), src, Opn(srcn)),
			Branch { target, targetn } =>
				write!(f, "branch    {}{:?}", target, Opn(targetn)),
			CBranch { cond, target, condn, targetn } =>
				write!(f, "cbranch   {:?}{:?}, {}{:?}", cond, Opn(condn), target, Opn(targetn)),
			IBranch { target, targetn } =>
				write!(f, "ibranch   [{:?}{:?}]", target, Opn(targetn)),
			Call { target, targetn } =>
				write!(f, "call      {}{:?}", target, Opn(targetn)),
			ICall { target, targetn } =>
				write!(f, "icall     [{:?}{:?}]", target, Opn(targetn)),
			Ret { target, targetn } =>
				write!(f, "ret       [{:?}{:?}]", target, Opn(targetn)),

			Unary { dst, op, src, dstn, srcn } => match op {
				IntZxt  => write!(f, "izxt      {:?}{:?}, {:?}{:?}", dst, Opn(dstn), src, Opn(srcn)),
				IntSxt  => write!(f, "isxt      {:?}{:?}, {:?}{:?}", dst, Opn(dstn), src, Opn(srcn)),
				IntNeg  => write!(f, "ineg      {:?}{:?}, {:?}{:?}", dst, Opn(dstn), src, Opn(srcn)),
				IntNot  => write!(f, "inot      {:?}{:?}, {:?}{:?}", dst, Opn(dstn), src, Opn(srcn)),
				BoolNot => write!(f, "bnot      {:?}{:?}, {:?}{:?}", dst, Opn(dstn), src, Opn(srcn)),
			},

			Binary { dst, src1, op, src2, dstn, src1n, src2n } => match op {
				IntEq      => write!(f, "ieq       {:?}{:?}, {:?}{:?}, {:?}{:?}",
					dst, Opn(dstn), src1, Opn(src1n), src2, Opn(src2n)),
				IntNe      => write!(f, "ine       {:?}{:?}, {:?}{:?}, {:?}{:?}",
					dst, Opn(dstn), src1, Opn(src1n), src2, Opn(src2n)),
				IntSlt     => write!(f, "islt      {:?}{:?}, {:?}{:?}, {:?}{:?}",
					dst, Opn(dstn), src1, Opn(src1n), src2, Opn(src2n)),
				IntSle     => write!(f, "isle      {:?}{:?}, {:?}{:?}, {:?}{:?}",
					dst, Opn(dstn), src1, Opn(src1n), src2, Opn(src2n)),
				IntUlt     => write!(f, "iult      {:?}{:?}, {:?}{:?}, {:?}{:?}",
					dst, Opn(dstn), src1, Opn(src1n), src2, Opn(src2n)),
				IntUle     => write!(f, "iule      {:?}{:?}, {:?}{:?}, {:?}{:?}",
					dst, Opn(dstn), src1, Opn(src1n), src2, Opn(src2n)),
				IntUAdd    => write!(f, "iuadd     {:?}{:?}, {:?}{:?}, {:?}{:?}",
					dst, Opn(dstn), src1, Opn(src1n), src2, Opn(src2n)),
				IntUSub    => write!(f, "iusub     {:?}{:?}, {:?}{:?}, {:?}{:?}",
					dst, Opn(dstn), src1, Opn(src1n), src2, Opn(src2n)),
				IntCarry   => write!(f, "icarry    {:?}{:?}, {:?}{:?}, {:?}{:?}",
					dst, Opn(dstn), src1, Opn(src1n), src2, Opn(src2n)),
				IntSCarry  => write!(f, "iscarry   {:?}{:?}, {:?}{:?}, {:?}{:?}",
					dst, Opn(dstn), src1, Opn(src1n), src2, Opn(src2n)),
				IntSBorrow => write!(f, "isborrow  {:?}{:?}, {:?}{:?}, {:?}{:?}",
					dst, Opn(dstn), src1, Opn(src1n), src2, Opn(src2n)),
				IntMul     => write!(f, "imul      {:?}{:?}, {:?}{:?}, {:?}{:?}",
					dst, Opn(dstn), src1, Opn(src1n), src2, Opn(src2n)),
				IntUDiv    => write!(f, "iudiv     {:?}{:?}, {:?}{:?}, {:?}{:?}",
					dst, Opn(dstn), src1, Opn(src1n), src2, Opn(src2n)),
				IntSDiv    => write!(f, "isdiv     {:?}{:?}, {:?}{:?}, {:?}{:?}",
					dst, Opn(dstn), src1, Opn(src1n), src2, Opn(src2n)),
				IntUMod    => write!(f, "iumod     {:?}{:?}, {:?}{:?}, {:?}{:?}",
					dst, Opn(dstn), src1, Opn(src1n), src2, Opn(src2n)),
				IntSMod    => write!(f, "ismod     {:?}{:?}, {:?}{:?}, {:?}{:?}",
					dst, Opn(dstn), src1, Opn(src1n), src2, Opn(src2n)),
				IntXor     => write!(f, "ixor      {:?}{:?}, {:?}{:?}, {:?}{:?}",
					dst, Opn(dstn), src1, Opn(src1n), src2, Opn(src2n)),
				IntAnd     => write!(f, "iand      {:?}{:?}, {:?}{:?}, {:?}{:?}",
					dst, Opn(dstn), src1, Opn(src1n), src2, Opn(src2n)),
				IntOr      => write!(f, "ior       {:?}{:?}, {:?}{:?}, {:?}{:?}",
					dst, Opn(dstn), src1, Opn(src1n), src2, Opn(src2n)),
				IntShl     => write!(f, "ishl      {:?}{:?}, {:?}{:?}, {:?}{:?}",
					dst, Opn(dstn), src1, Opn(src1n), src2, Opn(src2n)),
				IntUShr    => write!(f, "iushr     {:?}{:?}, {:?}{:?}, {:?}{:?}",
					dst, Opn(dstn), src1, Opn(src1n), src2, Opn(src2n)),
				IntSShr    => write!(f, "isshr     {:?}{:?}, {:?}{:?}, {:?}{:?}",
					dst, Opn(dstn), src1, Opn(src1n), src2, Opn(src2n)),
				IntPair    => write!(f, "ipair     {:?}{:?}, hi = {:?}{:?}, lo = {:?}{:?}",
					dst, Opn(dstn), src1, Opn(src1n), src2, Opn(src2n)),
				BoolXor    => write!(f, "bxor      {:?}{:?}, {:?}{:?}, {:?}{:?}",
					dst, Opn(dstn), src1, Opn(src1n), src2, Opn(src2n)),
				BoolAnd    => write!(f, "band      {:?}{:?}, {:?}{:?}, {:?}{:?}",
					dst, Opn(dstn), src1, Opn(src1n), src2, Opn(src2n)),
				BoolOr     => write!(f, "bor       {:?}{:?}, {:?}{:?}, {:?}{:?}",
					dst, Opn(dstn), src1, Opn(src1n), src2, Opn(src2n)),
			},

			Ternary { dst, src1, op, src2, src3, dstn, src1n, src2n, src3n } => match op {
				IntUAddC    => write!(f, "iuaddc    {:?}{:?}, {:?}{:?}, {:?}{:?}, {:?}{:?}",
					dst, Opn(dstn), src1, Opn(src1n), src2, Opn(src2n), src3, Opn(src3n)),
				IntUSubB    => write!(f, "iusubb    {:?}{:?}, {:?}{:?}, {:?}{:?}, {:?}{:?}",
					dst, Opn(dstn), src1, Opn(src1n), src2, Opn(src2n), src3, Opn(src3n)),
				IntCarryC   => write!(f, "icarryc   {:?}{:?}, {:?}{:?}, {:?}{:?}, {:?}{:?}",
					dst, Opn(dstn), src1, Opn(src1n), src2, Opn(src2n), src3, Opn(src3n)),
				IntSCarryC  => write!(f, "iscarryc  {:?}{:?}, {:?}{:?}, {:?}{:?}, {:?}{:?}",
					dst, Opn(dstn), src1, Opn(src1n), src2, Opn(src2n), src3, Opn(src3n)),
				IntSBorrowC => write!(f, "isborrowc {:?}{:?}, {:?}{:?}, {:?}{:?}, {:?}{:?}",
					dst, Opn(dstn), src1, Opn(src1n), src2, Opn(src2n), src3, Opn(src3n)),
			},
		}
	}
}

// ------------------------------------------------------------------------------------------------
// IrInst
// ------------------------------------------------------------------------------------------------

#[derive(PartialEq, Eq, Clone, Copy)]
pub(crate) struct IrInst {
	ea:   EA,
	kind: IrInstKind,
}

impl Debug for IrInst {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		write!(f, "{:?} {:?}", self.ea, self.kind)
	}
}

#[allow(clippy::too_many_arguments)]
impl IrInst {
	/// TODO: docme
	pub(crate) fn nop(ea: EA) -> Self {
		Self { ea, kind: IrInstKind::Nop }
	}

	/// TODO: docme
	pub(crate) fn use_(ea: EA, reg: IrReg) -> Self {
		Self { ea, kind: IrInstKind::Use { reg } }
	}

	/// TODO: docme
	pub(crate) fn assign(ea: EA, dst: IrReg, src: IrSrc,
		dstn: i8, srcn: i8) -> Self {
		assert!(dst.size() == src.size());
		Self { ea, kind: IrInstKind::Assign { dst, src, dstn, srcn } }
	}

	/// TODO: docme
	pub(crate) fn izxt(ea: EA, dst: IrReg, src: IrSrc,
		dstn: i8, srcn: i8) -> Self {
		assert!(dst.size() > src.size());
		Self { ea, kind: IrInstKind::Unary { dst, op: IrUnOp::IntZxt, src, dstn, srcn } }
	}

	/// TODO: docme
	pub(crate) fn isxt(ea: EA, dst: IrReg, src: IrSrc,
		dstn: i8, srcn: i8) -> Self {
		assert!(dst.size() > src.size());
		Self { ea, kind: IrInstKind::Unary { dst, op: IrUnOp::IntSxt, src, dstn, srcn } }
	}

	/// TODO: docme
	pub(crate) fn ineg(ea: EA, dst: IrReg, src: IrSrc,
		dstn: i8, srcn: i8) -> Self {
		assert!(dst.size() == src.size());
		Self { ea, kind: IrInstKind::Unary { dst, op: IrUnOp::IntNeg, src, dstn, srcn } }
	}

	/// TODO: docme
	pub(crate) fn inot(ea: EA, dst: IrReg, src: IrSrc,
		dstn: i8, srcn: i8) -> Self {
		assert!(dst.size() == src.size());
		Self { ea, kind: IrInstKind::Unary { dst, op: IrUnOp::IntNot, src, dstn, srcn } }
	}

	/// TODO: docme
	pub(crate) fn bnot(ea: EA, dst: IrReg, src: IrSrc,
		dstn: i8, srcn: i8) -> Self {
		assert!(dst.size() == src.size());
		Self { ea, kind: IrInstKind::Unary { dst, op: IrUnOp::BoolNot, src, dstn, srcn } }
	}

	/// TODO: docme
	pub(crate) fn ieq(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc,
		dstn: i8, src1n: i8, src2n: i8) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary {
			dst, src1, op: IrBinOp::IntEq, src2, dstn, src1n, src2n } }
	}

	/// TODO: docme
	pub(crate) fn ine(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc,
		dstn: i8, src1n: i8, src2n: i8) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary {
			dst, src1, op: IrBinOp::IntNe, src2, dstn, src1n, src2n } }
	}

	/// TODO: docme
	pub(crate) fn islt(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc,
		dstn: i8, src1n: i8, src2n: i8) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary {
			dst, src1, op: IrBinOp::IntSlt, src2, dstn, src1n, src2n } }
	}

	/// TODO: docme
	pub(crate) fn isle(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc,
		dstn: i8, src1n: i8, src2n: i8) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary {
			dst, src1, op: IrBinOp::IntSle, src2, dstn, src1n, src2n } }
	}

	/// TODO: docme
	pub(crate) fn iult(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc,
		dstn: i8, src1n: i8, src2n: i8) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary {
			dst, src1, op: IrBinOp::IntUlt, src2, dstn, src1n, src2n } }
	}

	/// TODO: docme
	pub(crate) fn iule(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc,
		dstn: i8, src1n: i8, src2n: i8) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary {
			dst, src1, op: IrBinOp::IntUle, src2, dstn, src1n, src2n } }
	}

	/// TODO: docme
	pub(crate) fn iuadd(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc,
		dstn: i8, src1n: i8, src2n: i8) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary {
			dst, src1, op: IrBinOp::IntUAdd, src2, dstn, src1n, src2n } }
	}

	/// TODO: docme
	pub(crate) fn iuaddc(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc, src3: IrSrc,
		dstn: i8, src1n: i8, src2n: i8, src3n: i8) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Ternary {
			dst, src1, op: IrTernOp::IntUAddC, src2, src3, dstn, src1n, src2n, src3n } }
	}

	/// TODO: docme
	pub(crate) fn iusub(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc,
		dstn: i8, src1n: i8, src2n: i8) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary {
			dst, src1, op: IrBinOp::IntUSub, src2, dstn, src1n, src2n } }
	}

	/// TODO: docme
	pub(crate) fn iusubb(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc, src3: IrSrc,
		dstn: i8, src1n: i8, src2n: i8, src3n: i8) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Ternary {
			dst, src1, op: IrTernOp::IntUSubB, src2, src3, dstn, src1n, src2n, src3n } }
	}

	/// TODO: docme
	pub(crate) fn icarry(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc,
		dstn: i8, src1n: i8, src2n: i8) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary {
			dst, src1, op: IrBinOp::IntCarry, src2, dstn, src1n, src2n } }
	}

	/// TODO: docme
	pub(crate) fn icarryc(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc, src3: IrSrc,
		dstn: i8, src1n: i8, src2n: i8, src3n: i8) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Ternary {
			dst, src1, op: IrTernOp::IntCarryC, src2, src3, dstn, src1n, src2n, src3n } }
	}

	/// TODO: docme
	pub(crate) fn iscarry(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc,
		dstn: i8, src1n: i8, src2n: i8) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary {
			dst, src1, op: IrBinOp::IntSCarry, src2, dstn, src1n, src2n } }
	}

	/// TODO: docme
	pub(crate) fn iscarryc(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc, src3: IrSrc,
		dstn: i8, src1n: i8, src2n: i8, src3n: i8) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Ternary {
			dst, src1, op: IrTernOp::IntSCarryC, src2, src3, dstn, src1n, src2n, src3n } }
	}

	/// TODO: docme
	pub(crate) fn isborrow(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc,
		dstn: i8, src1n: i8, src2n: i8) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary {
			dst, src1, op: IrBinOp::IntSBorrow, src2, dstn, src1n, src2n } }
	}

	/// TODO: docme
	pub(crate) fn isborrowc(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc, src3: IrSrc,
		dstn: i8, src1n: i8, src2n: i8, src3n: i8) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Ternary {
			dst, src1, op: IrTernOp::IntSBorrowC, src2, src3, dstn, src1n, src2n, src3n } }
	}

	/// TODO: docme
	pub(crate) fn imul(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc,
		dstn: i8, src1n: i8, src2n: i8) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary {
			dst, src1, op: IrBinOp::IntMul, src2, dstn, src1n, src2n } }
	}

	/// TODO: docme
	pub(crate) fn iudiv(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc,
		dstn: i8, src1n: i8, src2n: i8) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary {
			dst, src1, op: IrBinOp::IntUDiv, src2, dstn, src1n, src2n } }
	}

	/// TODO: docme
	pub(crate) fn isdiv(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc,
		dstn: i8, src1n: i8, src2n: i8) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary {
			dst, src1, op: IrBinOp::IntSDiv, src2, dstn, src1n, src2n } }
	}

	/// TODO: docme
	pub(crate) fn iumod(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc,
		dstn: i8, src1n: i8, src2n: i8) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary {
			dst, src1, op: IrBinOp::IntUMod, src2, dstn, src1n, src2n } }
	}

	/// TODO: docme
	pub(crate) fn ismod(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc,
		dstn: i8, src1n: i8, src2n: i8) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary {
			dst, src1, op: IrBinOp::IntSMod, src2, dstn, src1n, src2n } }
	}

	/// TODO: docme
	pub(crate) fn ixor(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc,
		dstn: i8, src1n: i8, src2n: i8) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary {
			dst, src1, op: IrBinOp::IntXor, src2, dstn, src1n, src2n } }
	}

	/// TODO: docme
	pub(crate) fn iand(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc,
		dstn: i8, src1n: i8, src2n: i8) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary {
			dst, src1, op: IrBinOp::IntAnd, src2, dstn, src1n, src2n } }
	}

	/// TODO: docme
	pub(crate) fn ior(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc,
		dstn: i8, src1n: i8, src2n: i8) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary {
			dst, src1, op: IrBinOp::IntOr, src2, dstn, src1n, src2n } }
	}

	/// TODO: docme
	pub(crate) fn ishl(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc,
		dstn: i8, src1n: i8, src2n: i8) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary {
			dst, src1, op: IrBinOp::IntShl, src2, dstn, src1n, src2n } }
	}

	/// TODO: docme
	pub(crate) fn iushr(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc,
		dstn: i8, src1n: i8, src2n: i8) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary {
			dst, src1, op: IrBinOp::IntUShr, src2, dstn, src1n, src2n } }
	}

	/// TODO: docme
	pub(crate) fn isshr(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc,
		dstn: i8, src1n: i8, src2n: i8) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary {
			dst, src1, op: IrBinOp::IntSShr, src2, dstn, src1n, src2n } }
	}

	/// TODO: docme
	pub(crate) fn ipair(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc,
		dstn: i8, src1n: i8, src2n: i8) -> Self {
		assert!(src1.size() == src2.size());
		assert!(dst.size().is_twice(src1.size()));
		Self { ea, kind: IrInstKind::Binary {
			dst, src1, op: IrBinOp::IntPair, src2, dstn, src1n, src2n } }
	}

	/// TODO: docme
	pub(crate) fn bxor(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc,
		dstn: i8, src1n: i8, src2n: i8) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary {
			dst, src1, op: IrBinOp::BoolXor, src2, dstn, src1n, src2n } }
	}

	/// TODO: docme
	pub(crate) fn band(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc,
		dstn: i8, src1n: i8, src2n: i8) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary {
			dst, src1, op: IrBinOp::BoolAnd, src2, dstn, src1n, src2n } }
	}

	/// TODO: docme
	pub(crate) fn bor(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc,
		dstn: i8, src1n: i8, src2n: i8) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary {
			dst, src1, op: IrBinOp::BoolOr, src2, dstn, src1n, src2n } }
	}

	/// TODO: docme
	pub(crate) fn load(ea: EA, dst: IrReg, addr: IrSrc,
		dstn: i8, addrn: i8) -> Self {
		Self { ea, kind: IrInstKind::Load { dst, addr, dstn, addrn } }
	}

	/// TODO: docme
	pub(crate) fn store(ea: EA, addr: IrSrc, src: IrSrc,
		addrn: i8, srcn: i8) -> Self {
		Self { ea, kind: IrInstKind::Store { addr, src, addrn, srcn } }
	}

	/// TODO: docme
	pub(crate) fn branch(ea: EA, target: EA,
		targetn: i8) -> Self {
		Self { ea, kind: IrInstKind::Branch { target, targetn } }
	}

	/// TODO: docme
	pub(crate) fn cbranch(ea: EA, cond: IrSrc, target: EA,
		condn: i8, targetn: i8) -> Self {
		Self { ea, kind: IrInstKind::CBranch { cond, target, condn, targetn } }
	}

	/// TODO: docme
	pub(crate) fn ibranch(ea: EA, target: IrSrc,
		targetn: i8) -> Self {
		Self { ea, kind: IrInstKind::IBranch { target, targetn } }
	}

	/// TODO: docme
	pub(crate) fn call(ea: EA, target: EA,
		targetn: i8) -> Self {
		Self { ea, kind: IrInstKind::Call { target, targetn } }
	}

	/// TODO: docme
	pub(crate) fn icall(ea: EA, target: IrSrc,
		targetn: i8) -> Self {
		Self { ea, kind: IrInstKind::ICall { target, targetn } }
	}

	/// TODO: docme
	pub(crate) fn ret(ea: EA, target: IrSrc,
		targetn: i8) -> Self {
		Self { ea, kind: IrInstKind::Ret { target, targetn } }
	}

	// --------------------------------------------------------------------------------------------

	/// The EA of the real instruction to which this belongs.
	pub(crate) fn ea(&self) -> EA {
		self.ea
	}

	/// What kind of instruction this is.
	pub(crate) fn kind(&self) -> IrInstKind {
		self.kind
	}

	/// Gets ths size of the source value(s).
	/// Panics if called on an instruction that has no source.
	pub(crate) fn src_size(&self) -> ValSize {
		use IrInstKind::*;

		match &self.kind {
			Nop
			| Branch { .. }
			| IBranch { .. }
			| Call { .. }
			| ICall { .. }
			| Ret { .. } => panic!("no source"),

			Use       { reg }      => reg.size(),
			Assign    { src, .. }  => src.size(),
			Load      { dst, .. }  => dst.size(), // yes, it's weird
			Store     { src, .. }  => src.size(),
			CBranch   { cond, .. } => cond.size(),
			Unary     { src, .. }  => src.size(),
			Binary    { src1, .. } => src1.size(),
			Ternary   { src1, .. } => src1.size(),
		}
	}

	/// Gets ths size of the destination place.
	/// Panics if called on an instruction that has no destination.
	pub(crate) fn dst_size(&self) -> ValSize {
		use IrInstKind::*;

		match &self.kind {
			Nop
			| Branch { .. }
			| IBranch { .. }
			| Call { .. }
			| ICall { .. }
			| Ret { .. }
			| CBranch { .. }
			| Use { .. } => panic!("no destination"),

			Assign  { dst, .. } => dst.size(),
			Load    { dst, .. } => dst.size(),
			Store   { src, .. } => src.size(), // yes, it's weird
			Unary   { dst, .. } => dst.size(),
			Binary  { dst, .. } => dst.size(),
			Ternary { dst, .. } => dst.size(),
		}
	}

	/// Callback iterator over all regs used by this instruction.
	pub(crate) fn regs(&self, mut f: impl FnMut(&IrReg)) {
		use IrInstKind::*;

		match &self.kind {
			Nop
			| Branch { .. }
			| IBranch { .. }
			| Call { .. } => {}

			Use { reg }              => { f(reg); }
			Assign { dst, src, .. }  => { f(dst); src.regs(&mut f); }
			Load { dst, addr, .. }   => { f(dst); addr.regs(&mut f); }
			Store { addr,  src, .. } => { addr.regs(&mut f); src.regs(&mut f); }
			CBranch { cond, .. }     => { cond.regs(&mut f); }
			ICall { target, .. }     => { target.regs(&mut f); }
			Ret { target, .. }       => { target.regs(&mut f); }
			Unary { dst, src, .. }   => { f(dst); src.regs(&mut f); }

			Binary { dst, src1, src2, .. } => {
				f(dst);
				src1.regs(&mut f);
				src2.regs(&mut f);
			}
			Ternary { dst, src1, src2, src3, .. } => {
				f(dst);
				src1.regs(&mut f);
				src2.regs(&mut f);
				src3.regs(&mut f);
			}
		}
	}

	/// Does this instruction assign to the given reg?
	pub(crate) fn assigns(&self, reg: IrReg) -> bool {
		use IrInstKind::*;

		match &self.kind {
			Nop | Use { .. } | Branch { .. } | CBranch { .. } | ICall { .. } | Ret { .. }
			| IBranch { .. } | Store { .. } | Call { .. } => false,

			Assign { dst, .. } | Load { dst, .. } | Unary { dst, .. } | Binary { dst, .. }
			| Ternary { dst, .. } => *dst == reg,
		}
	}

	/// Callback iterator over all uses in this instruction.
	pub(crate) fn visit_uses(&self, mut f: impl FnMut(IrReg)) {
		use IrInstKind::*;

		match &self.kind {
			Nop
			| Branch { .. }
			| Call { .. } => {}

			Use { reg }             => { f(*reg); }
			Assign { src, .. }      => { src.visit_use(&mut f); }
			Load { addr, .. }       => { addr.visit_use(&mut f); }
			Store { addr, src, .. } => { addr.visit_use(&mut f); src.visit_use(&mut f); }
			CBranch { cond, .. }    => { cond.visit_use(&mut f); }
			IBranch { target, .. }  => { target.visit_use(&mut f); }
			ICall { target, .. }    => { target.visit_use(&mut f); }
			Ret { target, .. }      => { target.visit_use(&mut f); }
			Unary { src, .. }       => { src.visit_use(&mut f); }
			Binary { src1, src2, .. } => {
				src1.visit_use(&mut f);
				src2.visit_use(&mut f);
			}
			Ternary { src1, src2, src3, .. } => {
				src1.visit_use(&mut f);
				src2.visit_use(&mut f);
				src3.visit_use(&mut f);
			}
		}
	}

	/// Same as above but mutable.
	pub(crate) fn visit_uses_mut(&mut self, mut f: impl FnMut(&mut IrReg)) {
		use IrInstKind::*;

		match &mut self.kind {
			Nop
			| Branch { .. }
			| Call { .. } => {}

			Use { reg }             => { f(reg); }
			Assign { src, .. }      => { src.visit_use_mut(&mut f); }
			Load { addr, .. }       => { addr.visit_use_mut(&mut f); }
			Store { addr, src, .. } => { addr.visit_use_mut(&mut f); src.visit_use_mut(&mut f); }
			CBranch { cond, .. }    => { cond.visit_use_mut(&mut f); }
			IBranch { target, .. }  => { target.visit_use_mut(&mut f); }
			ICall { target, .. }    => { target.visit_use_mut(&mut f); }
			Ret { target, .. }      => { target.visit_use_mut(&mut f); }
			Unary { src, .. }       => { src.visit_use_mut(&mut f); }
			Binary { src1, src2, .. } => {
				src1.visit_use_mut(&mut f);
				src2.visit_use_mut(&mut f);
			}
			Ternary { src1, src2, src3, .. } => {
				src1.visit_use_mut(&mut f);
				src2.visit_use_mut(&mut f);
				src3.visit_use_mut(&mut f);
			}
		}
	}

	/// The destination register of this instruction, if it has one.
	pub(crate) fn dst_reg(&self) -> Option<IrReg> {
		use IrInstKind::*;

		match &self.kind {
			Nop
			| Use { .. }
			| Branch { .. }
			| IBranch { .. }
			| Call { .. }
			| ICall { .. }
			| Ret { .. }
			| Store { .. }
			| CBranch { .. } => None,

			Assign  { dst, .. }
			| Load    { dst, .. }
			| Unary   { dst, .. }
			| Binary  { dst, .. }
			| Ternary { dst, .. } => Some(*dst),
		}
	}

	/// Same as above but mutable.
	pub(crate) fn dst_reg_mut(&mut self) -> Option<&mut IrReg> {
		use IrInstKind::*;

		match &mut self.kind {
			Nop
			| Use { .. }
			| Branch { .. }
			| IBranch { .. }
			| Call { .. }
			| ICall { .. }
			| Ret { .. }
			| Store { .. }
			| CBranch { .. } => None,

			Assign  { dst, .. }
			| Load    { dst, .. }
			| Unary   { dst, .. }
			| Binary  { dst, .. }
			| Ternary { dst, .. } => Some(dst),
		}
	}

	/// What kind of memory access this instruction does, if any.
	pub(crate) fn mem_access(&self) -> Option<MemAccess> {
		use IrInstKind::*;

		match self.kind {
			Load { .. } => Some(MemAccess::R),
			Store { .. } => Some(MemAccess::W),
			Branch { .. }
			| CBranch { .. }
			| IBranch { .. }
			| Call { .. }
			| ICall { .. } => Some(MemAccess::Target),
			_ => None,
		}
	}
}