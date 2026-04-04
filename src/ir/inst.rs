
use crate::memory::{ MemAccess };

use super::*;

// ------------------------------------------------------------------------------------------------
// IrUnOp, IrBinOp, IrTernOp
// ------------------------------------------------------------------------------------------------

/// Unary operations.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[non_exhaustive]
pub(crate) enum IrUnOp {
	Zxt,  // dst = zxt(src)
	Sxt,  // dst = sxt(src)
	Lo,   // dst = lo(src) (half the number of bits)
	Hi,   // dst = hi(src) (half the number of bits)
	Neg,  // dst = -src
	INot, // dst = ~src
	BNot, // dst = not src
}

/// Binary operations.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[non_exhaustive]
pub(crate) enum IrBinOp {
	Eq,      // dst = s1 == s2
	Ne,      // dst = s1 != s2
	Slt,     // dst = s1 < s2  (signed)
	Sle,     // dst = s1 <= s2 (signed)
	Ult,     // dst = s1 < s2  (unsigned)
	Ule,     // dst = s1 <= s2 (unsigned)

	Add,     // dst = s1 + s2  (any signedness)
	Sub,     // dst = s1 - s2  (any signedness)

	UCarry,  // dst = true if (s1 unsigned+ s2) has carry-out
	SCarry,  // dst = true if (s1 signed+ s2) has carry-out
	SBorrow, // dst = true if (s1 signed- s2) has borrow-out
	Carries, // dst = carry-outs for each column of bits in (s1 unsigned+ s2)
	Borrows, // dst = borrow-outs for each column of bits in (s1 unsigned- s2)
	Mul,     // dst = s1 * s2
	UDiv,    // dst = s1 / s2  (unsigned)
	SDiv,    // dst = s1 / s2  (signed)
	UMod,    // dst = s1 % s2  (unsigned)
	SMod,    // dst = s1 % s2  (signed)

	IXor,    // dst = s1 ^ s2
	IAnd,    // dst = s1 & s2
	IOr,     // dst = s1 | s2
	Shl,     // dst = s1 << s2
	UShr,    // dst = s1 >> s2 (unsigned/logical)
	SShr,    // dst = s1 >> s2 (signed/arithmetic)
	Rol,     // dst = s1.rotate_left(s2)
	Ror,     // dst = s1.rotate_right(s2)

	Pair,    // dst = (s1 in upper bits, s2 in lower bits)
	Bit,     // dst = (s1 & (1 << s2)) ? 1 : 0;

	BXor,    // dst = s1 != s2
	BAnd,    // dst = s1 and s2
	BOr,     // dst = s1 or s2
}

/// Ternary operations.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[non_exhaustive]
pub(crate) enum IrTernOp {
	AddC,     // dst = s1 + s2 + s3  (any signedness, s3 = bool)
	SubB,     // dst = s1 - s2 - s3  (any signedness, s3 = bool)

	UCarryC,  // dst = true if unsigned (s1 + s2 + s3) has carry-out
	SCarryC,  // dst = true if signed (s1 + s2 + s3) has carry-out
	SBorrowB, // dst = true if signed (s1 - s2 - s3) has borrow-out
	CarriesC, // dst = carry-outs for each column of bits in unsigned (s1 + s2 + s3)
	BorrowsB, // dst = carry-outs for each column of bits in unsigned (s1 - s2 - s3)

	BSet,   // dst = (s1 & ~(1 << s2)) | (s3 << s2) (s3 must be 0 or 1)
}

impl IrUnOp {
	pub(crate) fn name(&self) -> &'static str {
		use IrUnOp::*;
		match self {
			Zxt  => "zxt",
			Sxt  => "sxt",
			Lo   => "lo",
			Hi   => "hi",
			Neg  => "neg",
			INot => "inot",
			BNot => "bnot",
		}
	}
}

impl IrBinOp {
	pub(crate) fn name(&self) -> &'static str {
		use IrBinOp::*;
		match self {
			Eq      => "eq",
			Ne      => "ne",
			Slt     => "slt",
			Sle     => "sle",
			Ult     => "ult",
			Ule     => "ule",
			Add     => "add",
			Sub     => "sub",
			UCarry  => "ucarry",
			SCarry  => "scarry",
			SBorrow => "sborrow",
			Carries => "carries",
			Borrows => "borrows",
			Mul     => "mul",
			UDiv    => "udiv",
			SDiv    => "sdiv",
			UMod    => "umod",
			SMod    => "smod",
			IXor    => "ixor",
			IAnd    => "iand",
			IOr     => "ior",
			Shl     => "shl",
			UShr    => "ushr",
			SShr    => "sshr",
			Rol     => "rol",
			Ror     => "ror",
			Pair    => "pair",
			Bit     => "bit",
			BXor    => "bxor",
			BAnd    => "band",
			BOr     => "bor",
		}
	}
}

impl IrTernOp {
	pub(crate) fn name(&self) -> &'static str {
		use IrTernOp::*;
		match self {
			AddC     => "addc",
			SubB     => "subb",
			UCarryC  => "ucarryc",
			SCarryC  => "scarryc",
			SBorrowB => "sborrowb",
			CarriesC => "carriesc",
			BorrowsB => "borrowsb",
			BSet     => "bset",
		}
	}
}

// ------------------------------------------------------------------------------------------------
// IrInstKind
// ------------------------------------------------------------------------------------------------

/// Represents IR instructions.
#[derive(PartialEq, Eq, Clone, Copy)]
#[non_exhaustive]
pub(crate) enum IrInstKind {
	// no operation
	Nop,

	// dummy use of reg
	Use     { reg: IrReg },
	// dst = src
	Mov     { dst: IrReg, src: IrSrc,                             dstn: i8, srcn: i8, },
	// dst = *addr
	Load    { dst: IrReg, addr: IrSrc,                            dstn: i8, addrn: i8, },
	// *addr = src
	Store   { addr: IrSrc, src: IrSrc,                            addrn: i8, srcn: i8, },

	// pc = dst (also subsumes jumps and fallthroughs)
	Branch  { dst: IrTarget,                                      dstn: i8, },
	// if(cond) pc = dst else pc = cont
	CBranch { cond: IrSrc, dst: IrTarget, cont: IrTarget,         condn: i8, dstn: i8 },
	// pc = dst (but it's indirect)
	IBranch { dst: IrSrc,                                         dstn: i8, },
	// pc = dst (but it's a call)
	Call    { dst: IrTarget, cont: IrTarget,                      dstn: i8, },
	// pc = dst (but it's an indirect call)
	ICall   { dst: IrSrc, cont: IrTarget,                         dstn: i8, },
	// pc = dst (but it's a return)
	Ret     { dst: IrSrc,                                         dstn: i8, },
	// either a halt or a dead end terminator
	Halt,

	// TODO: once indirect jumps/branches are implemented, `IrInstKind::IBranch/ICall` could have
	// some `Vec` of targets (which would make `IrInstKind` and `IrInst` no longer `Copy` but it's
	// not that disruptive), but that `Vec` may not be exhaustive... hmmm

	// dst = op src
	Unary   { dst: IrReg, op: IrUnOp, src: IrSrc,                 dstn: i8, srcn: i8, },
	// dst = src1 op src2
	Binary  { dst: IrReg, src1: IrSrc, op: IrBinOp, src2: IrSrc,  dstn: i8, src1n: i8, src2n: i8, },
	// dst = src1 op src2 op src3
	Ternary { dst: IrReg, src1: IrSrc, op: IrTernOp, src2: IrSrc, src3: IrSrc,
		dstn: i8, src1n: i8, src2n: i8, src3n: i8, },
}

// helper type for printing out operand numbers more easily
#[derive(PartialEq, Eq, Clone, Copy)]
struct Opn(i8);

impl Debug for Opn {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		if self.0 >= 0 {
			write!(f, "{{{}}}", self.0)?
		}
		Ok(())
	}
}

// helper type for printing out registers more easily
#[derive(Clone, Copy)]
pub(crate) struct RegDbg<'c>(pub(crate) IrReg, pub(crate) Option<&'c IrCompiler>);

impl<'c> Debug for RegDbg<'c> {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		self.0.debug_fmt(f, self.1)
	}
}

// helper type for printing out IrSrcs more easily
#[derive(Clone, Copy)]
pub(crate) struct SrcDbg<'c>(pub(crate) IrSrc, pub(crate) Option<&'c IrCompiler>);

impl<'c> Debug for SrcDbg<'c> {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		self.0.debug_fmt(f, self.1)
	}
}

impl Debug for IrInstKind {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		self.debug_fmt(f, None)
	}
}

impl IrInstKind {
	pub(crate) fn name(&self) -> &'static str {
		use IrInstKind::*;

		match self {
			Nop                => "nop",
			Use     { .. }     => "use",
			Mov     { .. }     => "mov",
			Load    { .. }     => "load",
			Store   { .. }     => "store",
			Branch  { .. }     => "branch",
			CBranch { .. }     => "cbranch",
			IBranch { .. }     => "ibranch",
			Call    { .. }     => "call",
			ICall   { .. }     => "icall",
			Ret     { .. }     => "ret",
			Halt               => "halt",
			Unary   { op, .. } => op.name(),
			Binary  { op, .. } => op.name(),
			Ternary { op, .. } => op.name(),
		}
	}

	pub(crate) fn target(&self) -> Option<IrTarget> {
		use IrInstKind::*;
		match self {
			Branch  { dst, .. } |
			CBranch { dst, .. } |
			Call    { dst, .. } => Some(*dst),
			_ => None,
		}
	}

	fn debug_fmt(&self, f: &mut Formatter, compiler: Option<&IrCompiler>) -> FmtResult {
		use IrInstKind::*;

		let r = |dst: IrReg| -> RegDbg { RegDbg(dst, compiler) };
		let s = |src: IrSrc| -> SrcDbg { SrcDbg(src, compiler) };

		write!(f, "{:<8} ", self.name())?;

		match *self {
			Nop =>
				Ok(()),
			Use { reg } =>
				write!(f, "{:?}", r(reg)),
			Mov { dst, src, dstn, srcn } =>
				write!(f, "{:?}{:?}, {:?}{:?}", r(dst), Opn(dstn), s(src), Opn(srcn)),
			Load { dst, addr, dstn, addrn } =>
				write!(f, "{:?}{:?}, [{:?}{:?}]", r(dst), Opn(dstn), s(addr), Opn(addrn)),
			Store { addr, src, addrn, srcn } =>
				write!(f, "[{:?}{:?}], {:?}{:?}", s(addr), Opn(addrn), s(src), Opn(srcn)),

			Branch { dst, dstn } =>
				write!(f, "{:?}{:?}", dst, Opn(dstn)),
			CBranch { cond, dst, cont, condn, dstn } =>
				write!(f, "{:?}{:?} ? {:?}{:?} : {:?}", s(cond), Opn(condn), dst, Opn(dstn), cont),
			IBranch { dst, dstn } =>
				write!(f, "[{:?}{:?}]", s(dst), Opn(dstn)),
			Call { dst, dstn, cont } =>
				write!(f, "{:?}{:?} (return to {:?})", dst, Opn(dstn), cont),
			ICall { dst, dstn, cont } =>
				write!(f, "[{:?}{:?}] (return to {:?})", s(dst), Opn(dstn), cont),
			Ret { dst, dstn } =>
				write!(f, "[{:?}{:?}]", s(dst), Opn(dstn)),
			Halt =>
				Ok(()),

			Unary { dst, op: _, src, dstn, srcn } =>
				write!(f, "{:?}{:?}, {:?}{:?}", r(dst), Opn(dstn), s(src), Opn(srcn)),

			Binary { dst, src1, op, src2, dstn, src1n, src2n } => match op {
				IrBinOp::Pair =>
					write!(f, "{:?}{:?}, hi = {:?}{:?}, lo = {:?}{:?}", r(dst), Opn(dstn),
						s(src1), Opn(src1n), s(src2), Opn(src2n)),
				IrBinOp::Bit =>
					write!(f, "{:?}{:?}, {:?}{:?}, bit# = {:?}{:?}", r(dst), Opn(dstn),
						s(src1), Opn(src1n), s(src2), Opn(src2n)),
				_ =>
					write!(f, "{:?}{:?}, {:?}{:?}, {:?}{:?}", r(dst), Opn(dstn),
						s(src1), Opn(src1n), s(src2), Opn(src2n)),
			}

			Ternary { dst, src1, op, src2, src3, dstn, src1n, src2n, src3n } => match op {
				IrTernOp::BSet =>
					write!(f, "{:?}{:?}, {:?}{:?}, bit# = {:?}{:?}, {:?}{:?}", r(dst), Opn(dstn),
						s(src1), Opn(src1n), s(src2), Opn(src2n), s(src3), Opn(src3n)),
				_ =>
					write!(f, "{:?}{:?}, {:?}{:?}, {:?}{:?}, {:?}{:?}", r(dst), Opn(dstn),
						s(src1), Opn(src1n), s(src2), Opn(src2n), s(src3), Opn(src3n)),
			},
		}
	}
}

// ------------------------------------------------------------------------------------------------
// IrInst
// ------------------------------------------------------------------------------------------------

/// An IR instruction.
///
/// Every instruction has an `EA` which is the `EA` of the `Instruction` from which it was
/// generated, which is the first argument to all the constructors.
#[derive(PartialEq, Eq, Clone, Copy)]
pub(crate) struct IrInst {
	ea:   EA,
	kind: IrInstKind,
}

impl Debug for IrInst {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		self.debug_fmt(f, None)
	}
}

#[allow(clippy::too_many_arguments)]
impl IrInst {
	pub(crate) fn target(&self) -> Option<IrTarget> {
		self.kind.target()
	}

	pub(crate) fn debug_fmt(&self, f: &mut Formatter, compiler: Option<&IrCompiler>) -> FmtResult {
		write!(f, "{:?} ", self.ea)?;
		self.kind.debug_fmt(f, compiler)
	}

	pub(crate) fn nop(ea: EA) -> Self {
		Self { ea, kind: IrInstKind::Nop }
	}

	pub(crate) fn use_(ea: EA, reg: IrReg) -> Self {
		Self { ea, kind: IrInstKind::Use { reg } }
	}

	pub(crate) fn mov(ea: EA, dst: IrReg, src: IrSrc,
		dstn: i8, srcn: i8) -> Self {
		assert!(dst.size() == src.size());
		Self { ea, kind: IrInstKind::Mov { dst, src, dstn, srcn } }
	}

	pub(crate) fn zxt(ea: EA, dst: IrReg, src: IrSrc,
		dstn: i8, srcn: i8) -> Self {
		assert!(dst.size() > src.size());
		Self { ea, kind: IrInstKind::Unary { dst, op: IrUnOp::Zxt, src, dstn, srcn } }
	}

	pub(crate) fn sxt(ea: EA, dst: IrReg, src: IrSrc,
		dstn: i8, srcn: i8) -> Self {
		assert!(dst.size() > src.size());
		Self { ea, kind: IrInstKind::Unary { dst, op: IrUnOp::Sxt, src, dstn, srcn } }
	}

	pub(crate) fn lo(ea: EA, dst: IrReg, src: IrSrc,
		dstn: i8, srcn: i8) -> Self {
		assert!(src.size().is_twice(dst.size()));
		Self { ea, kind: IrInstKind::Unary { dst, op: IrUnOp::Lo, src, dstn, srcn } }
	}

	pub(crate) fn hi(ea: EA, dst: IrReg, src: IrSrc,
		dstn: i8, srcn: i8) -> Self {
		assert!(src.size().is_twice(dst.size()));
		Self { ea, kind: IrInstKind::Unary { dst, op: IrUnOp::Hi, src, dstn, srcn } }
	}

	pub(crate) fn neg(ea: EA, dst: IrReg, src: IrSrc,
		dstn: i8, srcn: i8) -> Self {
		assert!(dst.size() == src.size());
		Self { ea, kind: IrInstKind::Unary { dst, op: IrUnOp::Neg, src, dstn, srcn } }
	}

	pub(crate) fn inot(ea: EA, dst: IrReg, src: IrSrc,
		dstn: i8, srcn: i8) -> Self {
		assert!(dst.size() == src.size());
		Self { ea, kind: IrInstKind::Unary { dst, op: IrUnOp::INot, src, dstn, srcn } }
	}

	pub(crate) fn bnot(ea: EA, dst: IrReg, src: IrSrc,
		dstn: i8, srcn: i8) -> Self {
		assert!(dst.size() == src.size());
		Self { ea, kind: IrInstKind::Unary { dst, op: IrUnOp::BNot, src, dstn, srcn } }
	}

	pub(crate) fn eq(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc,
		dstn: i8, src1n: i8, src2n: i8) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary {
			dst, src1, op: IrBinOp::Eq, src2, dstn, src1n, src2n } }
	}

	pub(crate) fn ne(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc,
		dstn: i8, src1n: i8, src2n: i8) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary {
			dst, src1, op: IrBinOp::Ne, src2, dstn, src1n, src2n } }
	}

	pub(crate) fn slt(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc,
		dstn: i8, src1n: i8, src2n: i8) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary {
			dst, src1, op: IrBinOp::Slt, src2, dstn, src1n, src2n } }
	}

	pub(crate) fn sle(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc,
		dstn: i8, src1n: i8, src2n: i8) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary {
			dst, src1, op: IrBinOp::Sle, src2, dstn, src1n, src2n } }
	}

	pub(crate) fn ult(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc,
		dstn: i8, src1n: i8, src2n: i8) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary {
			dst, src1, op: IrBinOp::Ult, src2, dstn, src1n, src2n } }
	}

	pub(crate) fn ule(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc,
		dstn: i8, src1n: i8, src2n: i8) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary {
			dst, src1, op: IrBinOp::Ule, src2, dstn, src1n, src2n } }
	}

	pub(crate) fn add(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc,
		dstn: i8, src1n: i8, src2n: i8) -> Self {
		assert!(dst.size() == src1.size());
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary {
			dst, src1, op: IrBinOp::Add, src2, dstn, src1n, src2n } }
	}

	pub(crate) fn addc(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc, src3: IrSrc,
		dstn: i8, src1n: i8, src2n: i8, src3n: i8) -> Self {
		assert!(dst.size() == src1.size());
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Ternary {
			dst, src1, op: IrTernOp::AddC, src2, src3, dstn, src1n, src2n, src3n } }
	}

	pub(crate) fn sub(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc,
		dstn: i8, src1n: i8, src2n: i8) -> Self {
		assert!(dst.size() == src1.size());
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary {
			dst, src1, op: IrBinOp::Sub, src2, dstn, src1n, src2n } }
	}

	pub(crate) fn subb(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc, src3: IrSrc,
		dstn: i8, src1n: i8, src2n: i8, src3n: i8) -> Self {
		assert!(dst.size() == src1.size());
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Ternary {
			dst, src1, op: IrTernOp::SubB, src2, src3, dstn, src1n, src2n, src3n } }
	}

	pub(crate) fn ucarry(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc,
		dstn: i8, src1n: i8, src2n: i8) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary {
			dst, src1, op: IrBinOp::UCarry, src2, dstn, src1n, src2n } }
	}

	pub(crate) fn ucarryc(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc, src3: IrSrc,
		dstn: i8, src1n: i8, src2n: i8, src3n: i8) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Ternary {
			dst, src1, op: IrTernOp::UCarryC, src2, src3, dstn, src1n, src2n, src3n } }
	}

	pub(crate) fn scarry(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc,
		dstn: i8, src1n: i8, src2n: i8) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary {
			dst, src1, op: IrBinOp::SCarry, src2, dstn, src1n, src2n } }
	}

	pub(crate) fn scarryc(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc, src3: IrSrc,
		dstn: i8, src1n: i8, src2n: i8, src3n: i8) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Ternary {
			dst, src1, op: IrTernOp::SCarryC, src2, src3, dstn, src1n, src2n, src3n } }
	}

	pub(crate) fn sborrow(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc,
		dstn: i8, src1n: i8, src2n: i8) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary {
			dst, src1, op: IrBinOp::SBorrow, src2, dstn, src1n, src2n } }
	}

	pub(crate) fn sborrowb(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc, src3: IrSrc,
		dstn: i8, src1n: i8, src2n: i8, src3n: i8) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Ternary {
			dst, src1, op: IrTernOp::SBorrowB, src2, src3, dstn, src1n, src2n, src3n } }
	}

	pub(crate) fn carries(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc,
		dstn: i8, src1n: i8, src2n: i8) -> Self {
		assert!(dst.size() == src1.size());
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary {
			dst, src1, op: IrBinOp::Carries, src2, dstn, src1n, src2n } }
	}

	pub(crate) fn carriesc(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc, src3: IrSrc,
		dstn: i8, src1n: i8, src2n: i8, src3n: i8) -> Self {
		assert!(dst.size() == src1.size());
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Ternary {
			dst, src1, op: IrTernOp::CarriesC, src2, src3, dstn, src1n, src2n, src3n } }
	}

	pub(crate) fn borrows(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc,
		dstn: i8, src1n: i8, src2n: i8) -> Self {
		assert!(dst.size() == src1.size());
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary {
			dst, src1, op: IrBinOp::Borrows, src2, dstn, src1n, src2n } }
	}

	pub(crate) fn borrowsb(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc, src3: IrSrc,
		dstn: i8, src1n: i8, src2n: i8, src3n: i8) -> Self {
		assert!(dst.size() == src1.size());
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Ternary {
			dst, src1, op: IrTernOp::BorrowsB, src2, src3, dstn, src1n, src2n, src3n } }
	}

	pub(crate) fn mul(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc,
		dstn: i8, src1n: i8, src2n: i8) -> Self {
		assert!(dst.size() == src1.size());
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary {
			dst, src1, op: IrBinOp::Mul, src2, dstn, src1n, src2n } }
	}

	pub(crate) fn udiv(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc,
		dstn: i8, src1n: i8, src2n: i8) -> Self {
		assert!(dst.size() == src1.size());
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary {
			dst, src1, op: IrBinOp::UDiv, src2, dstn, src1n, src2n } }
	}

	pub(crate) fn sdiv(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc,
		dstn: i8, src1n: i8, src2n: i8) -> Self {
		assert!(dst.size() == src1.size());
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary {
			dst, src1, op: IrBinOp::SDiv, src2, dstn, src1n, src2n } }
	}

	pub(crate) fn umod(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc,
		dstn: i8, src1n: i8, src2n: i8) -> Self {
		assert!(dst.size() == src1.size());
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary {
			dst, src1, op: IrBinOp::UMod, src2, dstn, src1n, src2n } }
	}

	pub(crate) fn smod(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc,
		dstn: i8, src1n: i8, src2n: i8) -> Self {
		assert!(dst.size() == src1.size());
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary {
			dst, src1, op: IrBinOp::SMod, src2, dstn, src1n, src2n } }
	}

	pub(crate) fn ixor(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc,
		dstn: i8, src1n: i8, src2n: i8) -> Self {
		assert!(dst.size() == src1.size());
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary {
			dst, src1, op: IrBinOp::IXor, src2, dstn, src1n, src2n } }
	}

	pub(crate) fn iand(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc,
		dstn: i8, src1n: i8, src2n: i8) -> Self {
		assert!(dst.size() == src1.size());
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary {
			dst, src1, op: IrBinOp::IAnd, src2, dstn, src1n, src2n } }
	}

	pub(crate) fn ior(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc,
		dstn: i8, src1n: i8, src2n: i8) -> Self {
		assert!(dst.size() == src1.size());
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary {
			dst, src1, op: IrBinOp::IOr, src2, dstn, src1n, src2n } }
	}

	pub(crate) fn shl(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc,
		dstn: i8, src1n: i8, src2n: i8) -> Self {
		assert!(dst.size() == src1.size());
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary {
			dst, src1, op: IrBinOp::Shl, src2, dstn, src1n, src2n } }
	}

	pub(crate) fn ushr(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc,
		dstn: i8, src1n: i8, src2n: i8) -> Self {
		assert!(dst.size() == src1.size());
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary {
			dst, src1, op: IrBinOp::UShr, src2, dstn, src1n, src2n } }
	}

	pub(crate) fn sshr(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc,
		dstn: i8, src1n: i8, src2n: i8) -> Self {
		assert!(dst.size() == src1.size());
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary {
			dst, src1, op: IrBinOp::SShr, src2, dstn, src1n, src2n } }
	}

	pub(crate) fn rol(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc,
		dstn: i8, src1n: i8, src2n: i8) -> Self {
		assert!(dst.size() == src1.size());
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary {
			dst, src1, op: IrBinOp::Rol, src2, dstn, src1n, src2n } }
	}

	pub(crate) fn ror(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc,
		dstn: i8, src1n: i8, src2n: i8) -> Self {
		assert!(dst.size() == src1.size());
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary {
			dst, src1, op: IrBinOp::Ror, src2, dstn, src1n, src2n } }
	}

	pub(crate) fn pair(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc,
		dstn: i8, src1n: i8, src2n: i8) -> Self {
		assert!(src1.size() == src2.size());
		assert!(dst.size().is_twice(src1.size()));
		Self { ea, kind: IrInstKind::Binary {
			dst, src1, op: IrBinOp::Pair, src2, dstn, src1n, src2n } }
	}

	pub(crate) fn bit(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc,
		dstn: i8, src1n: i8, src2n: i8) -> Self {

		if let IrSrc::Const(IrConst { val, .. }) = src2 {
			let num_bits = src1.size().bytes() as u64 * 8;
			assert!(val < num_bits, "bit position {} exceeds number of bits {}", val, num_bits);
		}

		Self { ea, kind: IrInstKind::Binary {
			dst, src1, op: IrBinOp::Bit, src2, dstn, src1n, src2n } }
	}

	pub(crate) fn bset(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc, src3: IrSrc,
		dstn: i8, src1n: i8, src2n: i8, src3n: i8) -> Self {

		if let IrSrc::Const(IrConst { val, .. }) = src2 {
			let num_bits = src1.size().bytes() as u64 * 8;
			assert!(val < num_bits, "bit position {} exceeds number of bits {}", val, num_bits);
		}

		Self { ea, kind: IrInstKind::Ternary {
			dst, src1, op: IrTernOp::BSet, src2, src3, dstn, src1n, src2n, src3n } }
	}

	pub(crate) fn bxor(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc,
		dstn: i8, src1n: i8, src2n: i8) -> Self {
		assert!(dst.size() == src1.size());
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary {
			dst, src1, op: IrBinOp::BXor, src2, dstn, src1n, src2n } }
	}

	pub(crate) fn band(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc,
		dstn: i8, src1n: i8, src2n: i8) -> Self {
		assert!(dst.size() == src1.size());
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary {
			dst, src1, op: IrBinOp::BAnd, src2, dstn, src1n, src2n } }
	}

	pub(crate) fn bor(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc,
		dstn: i8, src1n: i8, src2n: i8) -> Self {
		assert!(dst.size() == src1.size());
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary {
			dst, src1, op: IrBinOp::BOr, src2, dstn, src1n, src2n } }
	}

	pub(crate) fn load(ea: EA, dst: IrReg, addr: IrSrc,
		dstn: i8, addrn: i8) -> Self {
		Self { ea, kind: IrInstKind::Load { dst, addr, dstn, addrn } }
	}

	pub(crate) fn store(ea: EA, addr: IrSrc, src: IrSrc,
		addrn: i8, srcn: i8) -> Self {
		Self { ea, kind: IrInstKind::Store { addr, src, addrn, srcn } }
	}

	pub(crate) fn branch(ea: EA, dst: impl Into<IrTarget>,
		dstn: i8) -> Self {
		Self { ea, kind: IrInstKind::Branch { dst: dst.into(), dstn } }
	}

	pub(crate) fn cbranch(ea: EA, cond: IrSrc, dst: impl Into<IrTarget>, cont: impl Into<IrTarget>,
		condn: i8, dstn: i8) -> Self {
		Self { ea, kind: IrInstKind::CBranch { cond, dst: dst.into(), cont: cont.into(),
			condn, dstn } }
	}

	pub(crate) fn ibranch(ea: EA, dst: IrSrc,
		dstn: i8) -> Self {
		Self { ea, kind: IrInstKind::IBranch { dst, dstn } }
	}

	pub(crate) fn call(ea: EA, dst: impl Into<IrTarget>, cont: impl Into<IrTarget>,
		dstn: i8) -> Self {
		Self { ea, kind: IrInstKind::Call { dst: dst.into(), cont: cont.into(), dstn } }
	}

	pub(crate) fn icall(ea: EA, dst: IrSrc, cont: impl Into<IrTarget>,
		dstn: i8) -> Self {
		Self { ea, kind: IrInstKind::ICall { dst, dstn, cont: cont.into() } }
	}

	pub(crate) fn ret(ea: EA, dst: IrSrc,
		dstn: i8) -> Self {
		Self { ea, kind: IrInstKind::Ret { dst, dstn } }
	}

	pub(crate) fn halt(ea: EA) -> Self {
		Self { ea, kind: IrInstKind::Halt }
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

	/// What kind of instruction this is, mutable.
	pub(crate) fn kind_mut(&mut self) -> &mut IrInstKind {
		&mut self.kind
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
			| Ret { .. }
			| Halt => panic!("no source"),

			Use       { reg }      => reg.size(),
			Mov       { src, .. }  => src.size(),
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
			| Use { .. }
			| Halt => panic!("no destination"),

			Mov     { dst, .. } => dst.size(),
			Load    { dst, .. } => dst.size(),
			Store   { src, .. } => src.size(), // yes, it's weird
			Unary   { dst, .. } => dst.size(),
			Binary  { dst, .. } => dst.size(),
			Ternary { dst, .. } => dst.size(),
		}
	}

	/// Callback iterator over all regs used by this instruction.
	pub(crate) fn regs(&self, mut f: impl FnMut(IrReg)) {
		use IrInstKind::*;

		match &self.kind {
			Nop
			| Branch { .. }
			| IBranch { .. }
			| Call { .. }
			| Halt => {}

			Use { reg }              => { f(*reg); }
			Mov { dst, src, .. }     => { f(*dst); src.regs(&mut f); }
			Load { dst, addr, .. }   => { f(*dst); addr.regs(&mut f); }
			Store { addr,  src, .. } => { addr.regs(&mut f); src.regs(&mut f); }
			CBranch { cond, .. }     => { cond.regs(&mut f); }
			ICall { dst, .. }        => { dst.regs(&mut f); }
			Ret { dst, .. }          => { dst.regs(&mut f); }
			Unary { dst, src, .. }   => { f(*dst); src.regs(&mut f); }

			Binary { dst, src1, src2, .. } => {
				f(*dst);
				src1.regs(&mut f);
				src2.regs(&mut f);
			}
			Ternary { dst, src1, src2, src3, .. } => {
				f(*dst);
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
			| IBranch { .. } | Store { .. } | Call { .. } | Halt => false,

			Mov { dst, .. } | Load { dst, .. } | Unary { dst, .. } | Binary { dst, .. }
			| Ternary { dst, .. } => *dst == reg,
		}
	}

	/// Callback iterator over all uses in this instruction.
	pub(crate) fn visit_uses(&self, mut f: impl FnMut(IrReg)) {
		use IrInstKind::*;

		match &self.kind {
			Nop
			| Branch { .. }
			| Call { .. }
			| Halt => {}

			Use { reg }             => { f(*reg); }
			Mov { src, .. }         => { src.visit_use(&mut f); }
			Load { addr, .. }       => { addr.visit_use(&mut f); }
			Store { addr, src, .. } => { addr.visit_use(&mut f); src.visit_use(&mut f); }
			CBranch { cond, .. }    => { cond.visit_use(&mut f); }
			IBranch { dst, .. }  => { dst.visit_use(&mut f); }
			ICall { dst, .. }    => { dst.visit_use(&mut f); }
			Ret { dst, .. }      => { dst.visit_use(&mut f); }
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
			| Call { .. }
			| Halt => {}

			Use { reg }             => { f(reg); }
			Mov { src, .. }         => { src.visit_use_mut(&mut f); }
			Load { addr, .. }       => { addr.visit_use_mut(&mut f); }
			Store { addr, src, .. } => { addr.visit_use_mut(&mut f); src.visit_use_mut(&mut f); }
			CBranch { cond, .. }    => { cond.visit_use_mut(&mut f); }
			IBranch { dst, .. }  => { dst.visit_use_mut(&mut f); }
			ICall { dst, .. }    => { dst.visit_use_mut(&mut f); }
			Ret { dst, .. }      => { dst.visit_use_mut(&mut f); }
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
			| CBranch { .. }
			| Halt => None,

			Mov  { dst, .. }
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
			| CBranch { .. }
			| Halt => None,

			Mov  { dst, .. }
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