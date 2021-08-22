
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

	Assign  { dst: IrReg, src: IrSrc },  // dst = src
	Load    { dst: IrReg, addr: IrSrc }, // dst = *addr
	Store   { addr: IrSrc,  src: IrSrc },  // *addr = src

	Branch  { target: EA },            // pc = target
	CBranch { cond: IrSrc, target: EA }, // if(cond) pc = target
	IBranch { target: IrSrc },           // pc = src

	Call    { target: EA },  // pc = target (but it's a call)
	ICall   { target: IrSrc }, // pc = src (but it's a call)
	Ret     { target: IrSrc }, // pc = src (but it's a return)

	Unary   { dst: IrReg, op: IrUnOp, src: IrSrc },                          // dst = op src
	Binary  { dst: IrReg, src1: IrSrc, op: IrBinOp, src2: IrSrc },             // dst = src1 op src2
	Ternary { dst: IrReg, src1: IrSrc, op: IrTernOp, src2: IrSrc, src3: IrSrc }, // dst = ...yeah
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

impl IrInst {
	///
	pub(crate) fn nop(ea: EA) -> Self {
		Self { ea, kind: IrInstKind::Nop }
	}

	///
	pub(crate) fn assign(ea: EA, dst: IrReg, src: IrSrc) -> Self {
		assert!(dst.size() == src.size());
		Self { ea, kind: IrInstKind::Assign { dst, src } }
	}

	///
	pub(crate) fn izxt(ea: EA, dst: IrReg, src: IrSrc) -> Self {
		assert!(dst.size() > src.size());
		Self { ea, kind: IrInstKind::Unary { dst, op: IrUnOp::IntZxt, src } }
	}

	///
	pub(crate) fn isxt(ea: EA, dst: IrReg, src: IrSrc) -> Self {
		assert!(dst.size() > src.size());
		Self { ea, kind: IrInstKind::Unary { dst, op: IrUnOp::IntSxt, src } }
	}

	///
	pub(crate) fn ineg(ea: EA, dst: IrReg, src: IrSrc) -> Self {
		assert!(dst.size() == src.size());
		Self { ea, kind: IrInstKind::Unary { dst, op: IrUnOp::IntNeg, src } }
	}

	///
	pub(crate) fn inot(ea: EA, dst: IrReg, src: IrSrc) -> Self {
		assert!(dst.size() == src.size());
		Self { ea, kind: IrInstKind::Unary { dst, op: IrUnOp::IntNot, src } }
	}

	///
	pub(crate) fn bnot(ea: EA, dst: IrReg, src: IrSrc) -> Self {
		assert!(dst.size() == src.size());
		Self { ea, kind: IrInstKind::Unary { dst, op: IrUnOp::BoolNot, src } }
	}

	///
	pub(crate) fn ieq(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::IntEq, src2 } }
	}

	///
	pub(crate) fn ine(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::IntNe, src2 } }
	}

	///
	pub(crate) fn islt(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::IntSlt, src2 } }
	}

	///
	pub(crate) fn isle(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::IntSle, src2 } }
	}

	///
	pub(crate) fn iult(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::IntUlt, src2 } }
	}

	///
	pub(crate) fn iule(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::IntUle, src2 } }
	}

	///
	pub(crate) fn iuadd(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::IntUAdd, src2 } }
	}

	///
	pub(crate) fn iuaddc(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc, src3: IrSrc) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Ternary { dst, src1, op: IrTernOp::IntUAddC, src2, src3 } }
	}

	///
	pub(crate) fn iusub(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::IntUSub, src2 } }
	}

	///
	pub(crate) fn iusubb(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc, src3: IrSrc) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Ternary { dst, src1, op: IrTernOp::IntUSubB, src2, src3 } }
	}

	///
	pub(crate) fn icarry(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::IntCarry, src2 } }
	}

	///
	pub(crate) fn icarryc(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc, src3: IrSrc) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Ternary { dst, src1, op: IrTernOp::IntCarryC, src2, src3 } }
	}

	///
	pub(crate) fn iscarry(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::IntSCarry, src2 } }
	}

	///
	pub(crate) fn iscarryc(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc, src3: IrSrc) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Ternary { dst, src1, op: IrTernOp::IntSCarryC, src2, src3 } }
	}

	///
	pub(crate) fn isborrow(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::IntSBorrow, src2 } }
	}

	///
	pub(crate) fn isborrowc(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc, src3: IrSrc) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Ternary { dst, src1, op: IrTernOp::IntSBorrowC, src2, src3 } }
	}

	///
	pub(crate) fn imul(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::IntMul, src2 } }
	}

	///
	pub(crate) fn iudiv(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::IntUDiv, src2 } }
	}

	///
	pub(crate) fn isdiv(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::IntSDiv, src2 } }
	}

	///
	pub(crate) fn iumod(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::IntUMod, src2 } }
	}

	///
	pub(crate) fn ismod(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::IntSMod, src2 } }
	}

	///
	pub(crate) fn ixor(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::IntXor, src2 } }
	}

	///
	pub(crate) fn iand(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::IntAnd, src2 } }
	}

	///
	pub(crate) fn ior(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::IntOr, src2 } }
	}

	///
	pub(crate) fn ishl(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::IntShl, src2 } }
	}

	///
	pub(crate) fn iushr(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::IntUShr, src2 } }
	}

	///
	pub(crate) fn isshr(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::IntSShr, src2 } }
	}

	///
	pub(crate) fn ipair(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc) -> Self {
		assert!(src1.size() == src2.size());
		assert!(dst.size().is_twice(src1.size()));
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::IntPair, src2 } }
	}

	///
	pub(crate) fn bxor(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::BoolXor, src2 } }
	}

	///
	pub(crate) fn band(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::BoolAnd, src2 } }
	}

	///
	pub(crate) fn bor(ea: EA, dst: IrReg, src1: IrSrc, src2: IrSrc) -> Self {
		assert!(src1.size() == src2.size());
		Self { ea, kind: IrInstKind::Binary { dst, src1, op: IrBinOp::BoolOr, src2 } }
	}

	///
	pub(crate) fn load(ea: EA, dst: IrReg, addr: IrSrc) -> Self {
		Self { ea, kind: IrInstKind::Load { dst, addr } }
	}

	///
	pub(crate) fn store(ea: EA, addr: IrSrc, src: IrSrc) -> Self {
		Self { ea, kind: IrInstKind::Store { addr, src } }
	}

	// ///
	// pub(crate) fn irbranch(ea: EA, offs: i32) -> Self {
	// 	Self { ea, kind: IrInstKind::IrBranch { offs } }
	// }

	// ///
	// pub(crate) fn ircbranch(ea: EA, cond: IrSrc, offs: i32) -> Self {
	// 	Self { ea, kind: IrInstKind::IrCBranch { cond, offs } }
	// }

	///
	pub(crate) fn branch(ea: EA, target: EA) -> Self {
		Self { ea, kind: IrInstKind::Branch { target } }
	}

	///
	pub(crate) fn cbranch(ea: EA, cond: IrSrc, target: EA) -> Self {
		Self { ea, kind: IrInstKind::CBranch { cond, target } }
	}

	///
	pub(crate) fn ibranch(ea: EA, target: IrSrc) -> Self {
		Self { ea, kind: IrInstKind::IBranch { target } }
	}

	///
	pub(crate) fn call(ea: EA, target: EA) -> Self {
		Self { ea, kind: IrInstKind::Call { target } }
	}

	///
	pub(crate) fn icall(ea: EA, target: IrSrc) -> Self {
		Self { ea, kind: IrInstKind::ICall { target } }
	}

	///
	pub(crate) fn ret(ea: EA, target: IrSrc) -> Self {
		Self { ea, kind: IrInstKind::Ret { target } }
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
			// | IrBranch { .. }
			| Branch { .. }
			| IBranch { .. }
			| Call { .. }
			| ICall { .. }
			| Ret { .. } => panic!("no source"),

			Assign    { src, .. }  => src.size(),
			Load      { dst, .. }  => dst.size(), // yes, it's weird
			Store     { src, .. }  => src.size(),
			// IrCBranch { cond, .. } => cond.size(),
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
			// | IrBranch { .. }
			| Branch { .. }
			| IBranch { .. }
			| Call { .. }
			| ICall { .. }
			| Ret { .. }
			// | IrCBranch { .. }
			| CBranch { .. } => panic!("no destination"),

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

			Assign { dst, src }    => { f(dst); src.regs(&mut f); }
			Load { dst, addr }     => { f(dst); addr.regs(&mut f); }
			Store { addr,  src }   => { addr.regs(&mut f); src.regs(&mut f); }
			CBranch { cond, .. }   => { cond.regs(&mut f); }
			ICall { target }       => { target.regs(&mut f); }
			Ret { target }         => { target.regs(&mut f); }
			Unary { dst, src, .. } => { f(dst); src.regs(&mut f); }

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
			Nop | Branch { .. } | CBranch { .. } | ICall { .. } | Ret { .. } | IBranch { .. }
			| Store { .. } | Call { .. } => false,

			Assign { dst, .. } | Load { dst, .. } | Unary { dst, .. } | Binary { dst, .. }
			| Ternary { dst, .. } => *dst == reg,
		}
	}

	pub(crate) fn visit_uses(&self, mut f: impl FnMut(IrReg)) {
		use IrInstKind::*;

		match &self.kind {
			Nop
			| Branch { .. }
			| Call { .. } => {}

			Assign { src, .. }   => { src.visit_use(&mut f); }
			Load { addr, .. }    => { addr.visit_use(&mut f); }
			Store { addr, src }  => { addr.visit_use(&mut f); src.visit_use(&mut f); }
			CBranch { cond, .. } => { cond.visit_use(&mut f); }
			IBranch { target }   => { target.visit_use(&mut f); }
			ICall { target }     => { target.visit_use(&mut f); }
			Ret { target }       => { target.visit_use(&mut f); }
			Unary { src, .. }    => { src.visit_use(&mut f); }
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

	pub(crate) fn visit_uses_mut(&mut self, mut f: impl FnMut(&mut IrReg)) {
		use IrInstKind::*;

		match &mut self.kind {
			Nop
			| Branch { .. }
			| Call { .. } => {}

			Assign { src, .. }   => { src.visit_use_mut(&mut f); }
			Load { addr, .. }    => { addr.visit_use_mut(&mut f); }
			Store { addr, src }  => { addr.visit_use_mut(&mut f); src.visit_use_mut(&mut f); }
			CBranch { cond, .. } => { cond.visit_use_mut(&mut f); }
			IBranch { target }   => { target.visit_use_mut(&mut f); }
			ICall { target }     => { target.visit_use_mut(&mut f); }
			Ret { target }       => { target.visit_use_mut(&mut f); }
			Unary { src, .. }    => { src.visit_use_mut(&mut f); }
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

	pub(crate) fn dst_reg(&self) -> Option<IrReg> {
		use IrInstKind::*;

		match &self.kind {
			Nop
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

	pub(crate) fn dst_reg_mut(&mut self) -> Option<&mut IrReg> {
		use IrInstKind::*;

		match &mut self.kind {
			Nop
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
}