
use crate::ir::{ IrInst, IrSrc, IrReg, IrBBId };
use crate::memory::{ EA };
use crate::program::{ Instruction };

// ------------------------------------------------------------------------------------------------
// BuildReg, BuildSrc, BuildEA
// ------------------------------------------------------------------------------------------------

/// Helper type to allow passing either of these to `IrBuilder` methods which expect an `IrReg`:
///
/// - `(reg, opn)` - use register `reg` derived from `Instruction` operand `opn`
/// - `reg` - use register `reg` not derived from any `Instruction` operand
#[derive(Copy, Clone)]
pub(crate) struct BuildReg { r: IrReg, n: i8 }

impl<T: Into<IrReg>> From<(T, i8)> for BuildReg {
	fn from(other: (T, i8)) -> Self {
		Self { r: other.0.into(), n: other.1 }
	}
}

impl<T: Into<IrReg>> From<T> for BuildReg {
	fn from(other: T) -> Self {
		Self { r: other.into(), n: -1 }
	}
}

/// Helper type to allow passing either of these to `IrBuilder` methods which expect an `IrSrc`:
///
/// - `(src, opn)` - use source `src` derived from `Instruction` operand `opn`
/// - `src` - use source `src` not derived from any `Instruction` operand
///
/// You can also use a `BuildReg`, in which case the `IrReg` within is converted to an `IrSrc`.
#[derive(Copy, Clone)]
pub(crate) struct BuildSrc { s: IrSrc, n: i8 }

impl<T: Into<IrSrc>> From<(T, i8)> for BuildSrc {
	fn from(other: (T, i8)) -> Self {
		Self { s: other.0.into(), n: other.1 }
	}
}

impl<T: Into<IrSrc>> From<T> for BuildSrc {
	fn from(other: T) -> Self {
		Self { s: other.into(), n: -1 }
	}
}

impl From<BuildReg> for BuildSrc {
	fn from(other: BuildReg) -> Self {
		Self { s: other.r.into(), n: other.n }
	}
}

/// Helper type to allow passing either of these to `IrBuilder` methods which expect an `EA`:
///
/// - `(ea, opn)` - use address `ea` derived from `Instruction` operand `opn`
/// - `ea` - use address `ea` not derived from any `Instruction` operand
#[derive(Copy, Clone)]
pub(crate) struct BuildEA { ea: EA, n: i8 }

impl From<(EA, i8)> for BuildEA {
	fn from(other: (EA, i8)) -> Self {
		Self { ea: other.0, n: other.1 }
	}
}

impl From<EA> for BuildEA {
	fn from(other: EA) -> Self {
		Self { ea: other, n: -1 }
	}
}

// ------------------------------------------------------------------------------------------------
// IrBuilder
// ------------------------------------------------------------------------------------------------

/// Helper for building blocks of IR instructions.
pub(crate) struct IrBuilder<'i> {
	insts:       [Vec<IrInst>; 2],
	cur:         bool,
	ea:          EA,
	inst:        Option<&'i Instruction>,
	next_irbbid: IrBBId,
}

impl<'i> IrBuilder<'i> {
	/// Constructor. `next_irbbid` is the ID of the IR BB which will be used as the continuation
	/// target if `cbranch_and_split` is called. If you don't plan on calling that, you can just
	/// pass `0`.
	pub(crate) fn new(next_irbbid: IrBBId) -> Self {
		Self {
			// TODO: capacity chosen by vibes. need to run statistics to find out typical size (and
			// I'm sure that also depends on ISA...)
			insts:       [Vec::with_capacity(8), vec![]],
			cur:         false,
			ea:          EA::unresolved(0),
			inst:        None,
			next_irbbid,
		}
	}

	/// Finish building and get the finished vecs of instructions. (There can be more than one, if
	/// `cbranch_and_split` was called.)
	pub(crate) fn finish(self) -> (Vec<IrInst>, Option<Vec<IrInst>>) {
		let Self { insts: [mut ret1, mut ret2], cur, .. } = self;

		ret1.shrink_to_fit();
		if cur {
			ret2.shrink_to_fit();
			(ret1, Some(ret2))
		} else {
			(ret1, None)
		}
	}

	/// Finish building, assert there is only one vec of instructions, and get it.
	pub(crate) fn finish_one(self) -> Vec<IrInst> {
		let Self { insts: [mut ret, _], cur, .. } = self;
		assert!(!cur);
		ret.shrink_to_fit();
		ret
	}

	/// Get the current `Instruction` being translated.
	pub(crate) fn inst(&self) -> &'i Instruction {
		self.inst.unwrap()
	}

	pub(crate) fn set_inst(&mut self, inst: &'i Instruction) {
		self.ea = inst.ea();
		self.inst = Some(inst);
	}

	pub(crate) fn set_ea(&mut self, ea: EA) {
		assert!(self.inst.is_none());
		self.ea = ea;
	}

	fn push_inst(&mut self, inst: IrInst) -> &mut Self {
		self.insts[self.cur as usize].push(inst);
		self
	}
}

// ------------------------------------------------------------------------------------------------
// Misc instructions
// ------------------------------------------------------------------------------------------------

#[allow(clippy::too_many_arguments)]
impl IrBuilder<'_> {
	/// No operation. You can use this as a placeholder for unimplemented IR instructions, so that
	/// the resulting IR BB is not empty.
	pub(crate) fn nop(&mut self) -> &mut Self {
		self.push_inst(IrInst::nop(self.ea))
	}

	/// Copies a value from `src` to `dst`.
	///
	/// Panics if `src` and `dst` are different sizes.
	pub(crate) fn mov(&mut self, dst: impl Into<BuildReg>, src: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src = src.into();
		self.push_inst(IrInst::mov(self.ea, dst.r, src.s, dst.n, src.n))
	}
}

// ------------------------------------------------------------------------------------------------
// Computation
// ------------------------------------------------------------------------------------------------

#[allow(clippy::too_many_arguments)]
impl IrBuilder<'_> {
	/// Zero-extends `src`.
	///
	/// Panics if `dst` is not bigger than `src`.
	pub(crate) fn zxt(&mut self, dst: impl Into<BuildReg>, src: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src = src.into();
		self.push_inst(IrInst::zxt(self.ea, dst.r, src.s, dst.n, src.n))
	}

	/// Zero-extends `src`.
	///
	/// Panics if `dst` is not bigger than `src`.
	pub(crate) fn sxt(&mut self, dst: impl Into<BuildReg>, src: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src = src.into();
		self.push_inst(IrInst::sxt(self.ea, dst.r, src.s, dst.n, src.n))
	}

	/// Extracts the low half of `src`. One complementary operation to `pair`.
	///
	/// Panics if `dst` is not half the size of `src`.
	pub(crate) fn lo(&mut self, dst: impl Into<BuildReg>, src: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src = src.into();
		self.push_inst(IrInst::lo(self.ea, dst.r, src.s, dst.n, src.n))
	}

	/// Extracts the high half of `src`. One complementary operation to `pair`.
	///
	/// Panics if `dst` is not half the size of `src`.
	pub(crate) fn hi(&mut self, dst: impl Into<BuildReg>, src: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src = src.into();
		self.push_inst(IrInst::hi(self.ea, dst.r, src.s, dst.n, src.n))
	}

	/// Negates `src` (using 2's complement negation).
	///
	/// Panics if `src` and `dst` are different sizes.
	pub(crate) fn neg(&mut self, dst: impl Into<BuildReg>, src: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src = src.into();
		self.push_inst(IrInst::neg(self.ea, dst.r, src.s, dst.n, src.n))
	}

	/// Bitwise NOTs the integer `src` (aka "bitwise complement" or "1s' complement").
	///
	/// Panics if `src` and `dst` are different sizes.
	pub(crate) fn inot(&mut self, dst: impl Into<BuildReg>, src: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src = src.into();
		self.push_inst(IrInst::inot(self.ea, dst.r, src.s, dst.n, src.n))
	}

	/// Logically NOTs `src`. `src` is not required to be a boolean value; it can be any integer. If
	/// `src` is 0, `dst` is set to 1, else `dst` is set to 0.
	///
	/// Panics if `src` and `dst` are different sizes.
	pub(crate) fn bnot(&mut self, dst: impl Into<BuildReg>, src: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src = src.into();
		self.push_inst(IrInst::bnot(self.ea, dst.r, src.s, dst.n, src.n))
	}

	/// Tests if `src1 == src2`. The result is a boolean (0 = false, 1 = true).
	///
	/// Panics if `src1` and `src2` are different sizes.
	pub(crate) fn eq(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		self.push_inst(IrInst::eq(self.ea, dst.r, src1.s, src2.s, dst.n, src1.n, src2.n))
	}

	/// Tests if `src1 != src2`. The result is a boolean (0 = false, 1 = true).
	///
	/// Panics if `src1` and `src2` are different sizes.
	pub(crate) fn ne(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		self.push_inst(IrInst::ne(self.ea, dst.r, src1.s, src2.s, dst.n, src1.n, src2.n))
	}

	/// Tests if `src1 < src2` signed. The result is a boolean (0 = false, 1 = true).
	///
	/// Panics if `src1` and `src2` are different sizes.
	pub(crate) fn slt(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		self.push_inst(IrInst::slt(self.ea, dst.r, src1.s, src2.s, dst.n, src1.n, src2.n))
	}

	/// Tests if `src1 <= src2` signed. The result is a boolean (0 = false, 1 = true).
	///
	/// Panics if `src1` and `src2` are different sizes.
	pub(crate) fn sle(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		self.push_inst(IrInst::sle(self.ea, dst.r, src1.s, src2.s, dst.n, src1.n, src2.n))
	}

	/// Tests if `src1 > src2` signed. The result is a boolean (0 = false, 1 = true).
	///
	/// This is a convenience method for `slt` with the operands swapped.
	///
	/// Panics if `src1` and `src2` are different sizes.
	pub(crate) fn sgt(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		self.slt(dst, src2, src1)
	}

	/// Tests if `src1 >= src2` signed. The result is a boolean (0 = false, 1 = true).
	///
	/// This is a convenience method for `sle` with the operands swapped.
	///
	/// Panics if `src1` and `src2` are different sizes.
	pub(crate) fn sge(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		self.sle(dst, src2, src1)
	}

	/// Tests if `src1 < src2` unsigned. The result is a boolean (0 = false, 1 = true).
	///
	/// Panics if `src1` and `src2` are different sizes.
	pub(crate) fn ult(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		self.push_inst(IrInst::ult(self.ea, dst.r, src1.s, src2.s, dst.n, src1.n, src2.n))
	}

	/// Tests if `src1 <= src2` unsigned. The result is a boolean (0 = false, 1 = true).
	///
	/// Panics if `src1` and `src2` are different sizes.
	pub(crate) fn ule(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		self.push_inst(IrInst::ule(self.ea, dst.r, src1.s, src2.s, dst.n, src1.n, src2.n))
	}

	/// Tests if `src1 > src2` unsigned. The result is a boolean (0 = false, 1 = true).
	///
	/// This is a convenience method for `ult` with the operands swapped.
	///
	/// Panics if `src1` and `src2` are different sizes.
	pub(crate) fn ugt(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		self.ult(dst, src2, src1)
	}

	/// Tests if `src1 >= src2` unsigned. The result is a boolean (0 = false, 1 = true).
	///
	/// This is a convenience method for `ule` with the operands swapped.
	///
	/// Panics if `src1` and `src2` are different sizes.
	pub(crate) fn uge(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		self.ule(dst, src2, src1)
	}

	/// Computes `src1 + src2` unsigned and truncates the result to the same number of bits.
	///
	/// The signedness does not matter; only carries care about signedness.
	///
	/// Panics if `dst`, `src1`, and `src2` are not all the same size.
	pub(crate) fn add(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		self.push_inst(IrInst::add(self.ea, dst.r, src1.s, src2.s, dst.n, src1.n, src2.n))
	}

	/// Computes `src1 + src2 + src3` and truncates the result to the same number of bits. `src3` is
	/// meant to be the carry-out from a previous addition and should be 0 or 1.
	///
	/// The signedness does not matter; only carries care about signedness.
	///
	/// Panics if `dst`, `src1`, and `src2` are not all the same size. `src3` can be any size.
	pub(crate) fn addc(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>, src3: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		let src3 = src3.into();
		self.push_inst(IrInst::addc(self.ea, dst.r, src1.s, src2.s, src3.s,
			dst.n, src1.n, src2.n, src3.n))
	}

	/// Computes `src1 - src2` as `src1 + ~src2 + 1` and truncates the result to the same number of
	/// bits.
	///
	/// The signedness does not matter; only borrows care about signedness.
	///
	/// Panics if `dst`, `src1`, and `src2` are not all the same size.
	pub(crate) fn sub(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		self.push_inst(IrInst::sub(self.ea, dst.r, src1.s, src2.s, dst.n, src1.n, src2.n))
	}

	/// Computes `src1 - src2 - src3` as `src1 + ~src2 + ~src3 + 2` and truncates the result to the
	/// same number of bits. `src3` is meant to be the borrow-out from a previous subtraction and
	/// should be 0 or 1.
	///
	/// The signedness does not matter; only borrows care about signedness.
	///
	/// Panics if `dst`, `src1`, and `src2` are not all the same size. `src3` can be any size.
	pub(crate) fn subb(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>, src3: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		let src3 = src3.into();
		self.push_inst(IrInst::subb(self.ea, dst.r, src1.s, src2.s, src3.s,
			dst.n, src1.n, src2.n, src3.n))
	}

	/// Computes the unsigned carry-out of `src1 + src2`.
	///
	/// You should use this *before* the corresponding `add`, so that you are computing the carry
	/// from the original two sources. For example,
	///
	/// ```ignore
	///     b.ucarry(CARRY_FLAG, REG_A, REG_B);
	///     b.add   (REG_A,      REG_A, REG_B);
	/// ```
	///
	/// If you did it in the other order, `REG_A` would be changed by the `add` and the computed
	/// carry would be incorrect. Guess how I know!
	///
	/// Panics if `src1` and `src2` are different sizes.
	pub(crate) fn ucarry(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		self.push_inst(IrInst::ucarry(self.ea, dst.r, src1.s, src2.s, dst.n, src1.n, src2.n))
	}

	/// Computes the unsigned carry-out of `src1 + src2 + src3`.
	///
	/// You should use this *before* the corresponding `addc` for the same reason as `ucarry`.
	///
	/// Panics if `src1` and `src2` are different sizes.
	pub(crate) fn ucarryc(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>, src3: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		let src3 = src3.into();
		self.push_inst(IrInst::ucarryc(self.ea, dst.r, src1.s, src2.s, src3.s,
			dst.n, src1.n, src2.n, src3.n))
	}

	/// Computes the signed carry-out of `src1 + src2`.
	///
	/// You should use this *before* the corresponding `add` for the same reason as `ucarry`.
	///
	/// Panics if `src1` and `src2` are different sizes.
	pub(crate) fn scarry(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		self.push_inst(IrInst::scarry(self.ea, dst.r, src1.s, src2.s, dst.n, src1.n, src2.n))
	}

	/// Computes the signed carry-out of `src1 + src2 + src3`.
	///
	/// You should use this *before* the corresponding `addc` for the same reason as `ucarry`.
	///
	/// Panics if `src1` and `src2` are different sizes.
	pub(crate) fn scarryc(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>, src3: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		let src3 = src3.into();
		self.push_inst(IrInst::scarryc(self.ea, dst.r, src1.s, src2.s, src3.s,
			dst.n, src1.n, src2.n, src3.n))
	}

	/// Computes the signed borrow-out of `src1 - src2`. This uses the convention that the
	/// borrow-out is 1 if the subtraction overflows.
	///
	/// You should use this *before* the corresponding `sub` for the same reason as `ucarry`.
	///
	/// Panics if `src1` and `src2` are different sizes.
	pub(crate) fn sborrow(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		self.push_inst(IrInst::sborrow(self.ea, dst.r, src1.s, src2.s, dst.n, src1.n, src2.n))
	}

	/// Computes the signed borrow-out of `src1 - src2 - src3`. This uses the convention that the
	/// borrow-out is 1 if the subtraction overflows.
	///
	/// You should use this *before* the corresponding `sub` for the same reason as `ucarry`.
	///
	/// Panics if `src1` and `src2` are different sizes.
	pub(crate) fn sborrowb(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>, src3: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		let src3 = src3.into();
		self.push_inst(IrInst::sborrowb(self.ea, dst.r, src1.s, src2.s, src3.s,
			dst.n, src1.n, src2.n, src3.n))
	}

	/// Computes *all* carry-outs for all places in `src1 + src2`. Each bit of `dst` is the
	/// carry-out for that column; e.g. `dst` bit 0 is the carry-out of adding bits 0 of `src1` and
	/// `src2`. You can then use `bit` to extract the carries that you care about.
	///
	/// Note that if you only need the carry-out from the MSB (the typical case), `ucarry` or
	/// `scarry` is all you need.
	///
	/// You should use this *before* the corresponding `add` for the same reason as `ucarry`.
	///
	/// Panics if `dst`, `src1`, and `src2` are not all the same size.
	pub(crate) fn carries(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		self.push_inst(IrInst::carries(self.ea, dst.r, src1.s, src2.s, dst.n, src1.n, src2.n))
	}

	/// Computes *all* carry-outs for all places in `src1 + src2 + src3`. Works like `carries` but
	/// supports carry-in through `src3`.
	///
	/// Note that if you only need the carry-out from the MSB (the typical case), `ucarryc` or
	/// `scarryc` is all you need.
	///
	/// You should use this *before* the corresponding `addc` for the same reason as `ucarry`.
	///
	/// Panics if `dst`, `src1`, and `src2` are not all the same size. `src3` can be any size.
	pub(crate) fn carriesc(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>, src3: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		let src3 = src3.into();
		self.push_inst(IrInst::carriesc(self.ea, dst.r, src1.s, src2.s, src3.s,
			dst.n, src1.n, src2.n, src3.n))
	}

	/// Computes *all* borrow-outs for all places in `src1 - src2`. This uses the convention that
	/// the borrow-out is 1 if the subtraction overflows.
	///
	/// Note that if you only need the borrow-out from the MSB (the typical case), `sborrow` is all
	/// you need.
	///
	/// You should use this *before* the corresponding `sub` for the same reason as `ucarry`.
	///
	/// Panics if `dst`, `src1`, and `src2` are not all the same size.
	pub(crate) fn borrows(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		self.push_inst(IrInst::borrows(self.ea, dst.r, src1.s, src2.s, dst.n, src1.n, src2.n))
	}

	/// Computes *all* borrow-outs for all places in `src1 - src2 - src3`. This uses the convention
	/// that the borrow-out is 1 if the subtraction overflows.
	///
	/// Note that if you only need the borrow-out from the MSB (the typical case), `sborrowb` is all
	/// you need.
	///
	/// You should use this *before* the corresponding `sub` for the same reason as `ucarry`.
	///
	/// Panics if `dst`, `src1`, and `src2` are not all the same size. `src3` can be any size.
	pub(crate) fn borrowsb(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>, src3: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		let src3 = src3.into();
		self.push_inst(IrInst::borrowsb(self.ea, dst.r, src1.s, src2.s, src3.s,
			dst.n, src1.n, src2.n, src3.n))
	}

	/// Computes `src1 * src2` and truncates the result to the same number of bits.
	///
	/// There is currently no way to get a "double-wide" product. Sorry! I don't think most ISAs I'm
	/// interested in even have multiplication instructions.
	///
	/// Panics if `dst`, `src1`, and `src2` are not all the same size.
	pub(crate) fn mul(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		self.push_inst(IrInst::mul(self.ea, dst.r, src1.s, src2.s, dst.n, src1.n, src2.n))
	}

	/// Computes the quotient of `src1 / src2` unsigned.
	///
	/// Panics if `dst`, `src1`, and `src2` are not all the same size.
	pub(crate) fn udiv(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		self.push_inst(IrInst::udiv(self.ea, dst.r, src1.s, src2.s, dst.n, src1.n, src2.n))
	}

	/// Computes the quotient of `src1 / src2` signed.
	///
	/// Panics if `dst`, `src1`, and `src2` are not all the same size.
	pub(crate) fn sdiv(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		self.push_inst(IrInst::sdiv(self.ea, dst.r, src1.s, src2.s, dst.n, src1.n, src2.n))
	}

	/// Computes the remainder of `src1 / src2` (i.e. `src1 % src2`) unsigned.
	///
	/// Panics if `dst`, `src1`, and `src2` are not all the same size.
	pub(crate) fn umod(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		self.push_inst(IrInst::umod(self.ea, dst.r, src1.s, src2.s, dst.n, src1.n, src2.n))
	}

	/// Computes the remainder of `src1 / src2` (i.e. `src1 % src2`) signed.
	///
	/// NOTE: I haven't really defined the results of this, since modulo on signed numbers is
	/// poorly-defined. But I haven't encountered an ISA with a division instruction yet, so.
	///
	/// Panics if `dst`, `src1`, and `src2` are not all the same size.
	pub(crate) fn smod(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		self.push_inst(IrInst::smod(self.ea, dst.r, src1.s, src2.s, dst.n, src1.n, src2.n))
	}

	/// Computes bitwise XOR, `src1 ^ src2`.
	///
	/// Panics if `dst`, `src1`, and `src2` are not all the same size.
	pub(crate) fn ixor(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		self.push_inst(IrInst::ixor(self.ea, dst.r, src1.s, src2.s, dst.n, src1.n, src2.n))
	}

	/// Computes bitwise AND, `src1 & src2`.
	///
	/// Panics if `dst`, `src1`, and `src2` are not all the same size.
	pub(crate) fn iand(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		self.push_inst(IrInst::iand(self.ea, dst.r, src1.s, src2.s, dst.n, src1.n, src2.n))
	}

	/// Computes bitwise OR, `src1 | src2`.
	///
	/// Panics if `dst`, `src1`, and `src2` are not all the same size.
	pub(crate) fn ior(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		self.push_inst(IrInst::ior(self.ea, dst.r, src1.s, src2.s, dst.n, src1.n, src2.n))
	}

	/// Computes left-shift, `src1 << src2`.
	///
	/// This uses the convention that if `src2 >= number of bits in src1`, the result is 0.
	/// Different ISAs have different opinions about this; if you need the "wrapping" behavior
	/// (e.g. shifting an 8-bit number left by 8 gives the same result as shifting left by 0), you
	/// must mask the shift distance yourself before using this.
	///
	/// Panics if `dst`, `src1`, and `src2` are not all the same size.
	pub(crate) fn shl(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		self.push_inst(IrInst::shl(self.ea, dst.r, src1.s, src2.s, dst.n, src1.n, src2.n))
	}

	/// Computes unsigned or "logical" right-shift, `src1 >>> src2`.
	///
	/// See [`shl`] for shift distance considerations.
	///
	/// Panics if `dst`, `src1`, and `src2` are not all the same size.
	pub(crate) fn ushr(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		self.push_inst(IrInst::ushr(self.ea, dst.r, src1.s, src2.s, dst.n, src1.n, src2.n))
	}

	/// Computes signed or "arithmetic" right-shift, `src1 >> src2`.
	///
	/// See [`shl`] for shift distance considerations, with one addition: if all bits of `src1` are
	/// shifted off, and `src1` is negative, the result will be all 1 bits (-1); else the result
	/// will be all 0 bits.
	///
	/// It is currently undefined what will happen if `src2` is negative.
	///
	/// Panics if `dst`, `src1`, and `src2` are not all the same size.
	pub(crate) fn sshr(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		self.push_inst(IrInst::sshr(self.ea, dst.r, src1.s, src2.s, dst.n, src1.n, src2.n))
	}

	/// Computes `src1` rotated left by `src2` bits.
	///
	/// Rotation distances >= the size of `src1` are interpreted modulo its size. E.g. for an 8-bit
	/// value, rotating left by 0, 8, 16, 24 etc. all give the original value.
	///
	/// Panics if `dst`, `src1`, and `src2` are not all the same size.
	pub(crate) fn rol(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		self.push_inst(IrInst::rol(self.ea, dst.r, src1.s, src2.s, dst.n, src1.n, src2.n))
	}

	/// Computes `src1` rotated right by `src2` bits.
	///
	/// See [`rol`] for rotate distance considerations.
	///
	/// Panics if `dst`, `src1`, and `src2` are not all the same size.
	pub(crate) fn ror(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		self.push_inst(IrInst::ror(self.ea, dst.r, src1.s, src2.s, dst.n, src1.n, src2.n))
	}

	/// Pairs two smaller values into a single larger value twice the size. `src1` becomes the high
	/// bits, and `src2` the low bits.
	///
	/// This can be used to e.g. simulate a "paired" register built from two smaller registers, like
	/// the 16-bit `bc` register made from the 8-bit `b` and `c` registers on the Intel 8080/Zilog
	/// Z80 family. It is also more convenient than shift-and-or in some cases.
	///
	/// Panics if `src1` and `src2` are different sizes, or if `dst` is not twice the size of `src1`
	/// and `src2`.
	pub(crate) fn pair(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		self.push_inst(IrInst::pair(self.ea, dst.r, src1.s, src2.s, dst.n, src1.n, src2.n))
	}

	/// Extracts bit number `src2` from `src1`; the result is either 0 or 1.
	///
	/// Panics if `src2` is a constant >= the number of bits in `src1`.
	///
	/// If `src2` is not a constant, this can cause a panic during constant propagation analysis for
	/// the same reason.
	pub(crate) fn bit(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		self.push_inst(IrInst::bit(self.ea, dst.r, src1.s, src2.s, dst.n, src1.n, src2.n))
	}

	/// Computes `src1` with bit number `src2` set to the value of `src3`, which must be either 0 or
	/// 1.
	///
	/// Panics if `src2` is a constant >= the number of bits in `src1`.
	///
	/// If `src2` is not a constant, this can cause a panic during constant propagation analysis for
	/// the same reason. The same is true of `src3` - if it is not 0 or 1, constant propagation can
	/// panic.
	pub(crate) fn bset(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>, src3: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		let src3 = src3.into();
		self.push_inst(IrInst::bset(self.ea, dst.r, src1.s, src2.s, src3.s,
			dst.n, src1.n, src2.n, src3.n))
	}

	/// Computes the logical XOR of booleans `src1` and `src2`. Essentially `src1 != src2`, but
	/// assuming that they are booleans, not integers.
	///
	/// Panics if `dst`, `src1`, and `src2` are not all the same size.
	pub(crate) fn bxor(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		self.push_inst(IrInst::bxor(self.ea, dst.r, src1.s, src2.s, dst.n, src1.n, src2.n))
	}

	/// Computes the logical AND of booleans `src1` and `src2`.
	///
	/// Panics if `dst`, `src1`, and `src2` are not all the same size.
	pub(crate) fn band(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		self.push_inst(IrInst::band(self.ea, dst.r, src1.s, src2.s, dst.n, src1.n, src2.n))
	}

	/// Computes the logical AND of booleans `src1` and `src2`.
	///
	/// Panics if `dst`, `src1`, and `src2` are not all the same size.
	pub(crate) fn bor(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		self.push_inst(IrInst::bor(self.ea, dst.r, src1.s, src2.s, dst.n, src1.n, src2.n))
	}

	/// Loads from memory address `addr` into `dst`. The size of the loaded value is `dst.size()`.
	pub(crate) fn load(&mut self, dst: impl Into<BuildReg>, addr: impl Into<BuildSrc>)
		-> &mut Self {
		let dst = dst.into();
		let addr = addr.into();
		self.push_inst(IrInst::load(self.ea, dst.r, addr.s, dst.n, addr.n))
	}

	/// Stores into memory address `addr` the value from `src`. The size of the stored value is
	/// `src.size()`.
	pub(crate) fn store(&mut self, addr: impl Into<BuildSrc>, src: impl Into<BuildSrc>)
		-> &mut Self {
		let src = src.into();
		let addr = addr.into();
		self.push_inst(IrInst::store(self.ea, addr.s, src.s, addr.n, src.n))
	}
}

// ------------------------------------------------------------------------------------------------
// Control flow
// ------------------------------------------------------------------------------------------------

#[allow(clippy::too_many_arguments)]
impl IrBuilder<'_> {
	/// Unconditionally jump or branch to `dst`. The `EA` should come from the `BBTerm` that was
	/// passed to `build_ir_term`.
	pub(crate) fn branch(&mut self, dst: impl Into<BuildEA>) -> &mut Self {
		let dst = dst.into();
		self.push_inst(IrInst::branch(self.ea, dst.ea, dst.n))
	}

	/// If `cond` is true, jump or branch to `dst`; else continue at address `cont`.
	///
	/// The `EA`s should come from the `BBTerm` that was passed to `build_ir_term`.
	pub(crate) fn cbranch(&mut self, cond: impl Into<BuildSrc>, dst: impl Into<BuildEA>,
		cont: impl Into<BuildEA>) -> &mut Self {
		let cond = cond.into();
		let dst = dst.into();
		let cont = cont.into();
		self.push_inst(IrInst::cbranch(self.ea, cond.s, dst.ea, cont.ea, cond.n, dst.n))
	}

	/// This is used to implement conditional calls and returns. It splits this basic block into two
	/// parts: everything up to and including the `cbranch`, and everything after it.
	///
	/// # To build the IR for a conditional call instruction:
	///
	/// - you *must* call `IrBuilder::cbranch_and_split`, with the *negated* condition as
	///   `not_cond`, and `term.continuation_successor()` as `cont`
	///     - e.g. if the instruction is "call if zero", `cond` would be `!zero` to skip the call
	/// - then you *must* use `IrBuilder::call` as the last instruction, and that call *must* use
	///   `term.continuation_successor()` *again* as the second argument.
	///
	/// # To build the IR for a conditional return instruction:
	///
	/// - you *must* call `IrBuilder::cbranch_and_split`, with the *negated* condition as
	///   `not_cond`, and `term.continuation_successor()` as `cont`
	///     - e.g. if the instruction is "return if zero", `cond` would be `!zero` to skip the
	///       return and continue on to `cont`
	/// - then you *must* use `IrBuilder::ret` as the last instruction.
	///
	/// Panics if called twice in the same basic block.
	pub(crate) fn cbranch_and_split(&mut self, not_cond: impl Into<BuildSrc>, cont: impl Into<BuildEA>)
		-> &mut Self {
		assert!(!self.cur);
		let not_cond = not_cond.into();
		let cont = cont.into();
		self.push_inst(IrInst::cbranch(
			self.ea, not_cond.s, cont.ea, self.next_irbbid, not_cond.n, cont.n));
		self.cur = true;
		self
	}

	/// Unconditionally indirectly jump or branch to `dst`. (i.e. `dst` is the new value of the
	/// program counter register.)
	pub(crate) fn ibranch(&mut self, dst: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		self.push_inst(IrInst::ibranch(self.ea, dst.s, dst.n))
	}

	/// Call `dst`, and continue executing at `cont` after the call completes.
	///
	/// The `EA`s should come from the `BBTerm` that was passed to `build_ir_term`.
	pub(crate) fn call(&mut self, dst: impl Into<BuildEA>, cont: impl Into<BuildEA>) -> &mut Self {
		let dst = dst.into();
		let cont = cont.into();
		self.push_inst(IrInst::call(self.ea, dst.ea, cont.ea, dst.n))
	}

	/// Indirectly call `dst`, and continue executing at `cont` after the call completes. (i.e.
	/// `dst` is the new value of the program counter register.)
	///
	/// The `EA` should come from the `BBTerm` that was passed to `build_ir_term`.
	pub(crate) fn icall(&mut self, dst: impl Into<BuildSrc>, cont: impl Into<BuildEA>)
		-> &mut Self {
		let dst = dst.into();
		let cont = cont.into();
		self.push_inst(IrInst::icall(self.ea, dst.s, cont.ea, dst.n))
	}

	/// Return from this function using `dst` as the return address. For example, it might have been
	/// popped off the stack immediately before this instruction.
	pub(crate) fn ret(&mut self, dst: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		self.push_inst(IrInst::ret(self.ea, dst.s, dst.n))
	}

	/// Halt. The CPU stops executing instructions and never resumes.
	pub(crate) fn halt(&mut self) -> &mut Self {
		self.push_inst(IrInst::halt(self.ea))
	}
}