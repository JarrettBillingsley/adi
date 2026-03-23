
use crate::ir::{ IrInst, IrSrc, IrReg, IrBBId };
use crate::memory::{ EA };

// ------------------------------------------------------------------------------------------------
// BuildReg, BuildSrc, BuildEA
// ------------------------------------------------------------------------------------------------

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
pub(crate) struct IrBuilder {
	insts:       [Vec<IrInst>; 2],
	cur:         bool,
	ea:          EA,
	next_irbbid: IrBBId,
}

impl IrBuilder {
	/// Constructor. `next_irbbid` is the ID of the IR BB which will be used as the continuation
	/// target if `cbranch_and_split` is called.
	pub(crate) fn new(next_irbbid: IrBBId) -> Self {
		Self {
			insts:       [Vec::with_capacity(8), vec![]],
			cur:         false,
			ea:          EA::unresolved(0),
			next_irbbid,
		}
	}

	/// Finish building and get the finished vecs of instructions. (There can be more than one, if
	/// `cbranch_and_split` was called.)
	pub(crate) fn finish(self) -> (Vec<IrInst>, Option<Vec<IrInst>>) {
		let Self { insts: [mut ret1, mut ret2], cur, ea: _, next_irbbid: _ } = self;

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
		let Self { insts: [mut ret, _], cur, ea: _, next_irbbid: _ } = self;
		assert!(!cur);
		ret.shrink_to_fit();
		ret
	}

	/// Set the current EA that will be used for subsequent IR instructions.
	pub(crate) fn set_ea(&mut self, ea: EA) {
		self.ea = ea;
	}

	fn inst(&mut self, inst: IrInst) -> &mut Self {
		self.insts[self.cur as usize].push(inst);
		self
	}
}

// ------------------------------------------------------------------------------------------------
// Non-IR-branch instructions
// ------------------------------------------------------------------------------------------------

#[allow(clippy::too_many_arguments)]
impl IrBuilder {
	/// TODO: docme
	pub(crate) fn nop(&mut self) -> &mut Self {
		self.inst(IrInst::nop(self.ea))
	}

	/// TODO: docme
	pub(crate) fn use_(&mut self, reg: IrReg) -> &mut Self {
		self.inst(IrInst::use_(self.ea, reg))
	}

	/// TODO: docme
	pub(crate) fn mov(&mut self, dst: impl Into<BuildReg>, src: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src = src.into();
		self.inst(IrInst::mov(self.ea, dst.r, src.s, dst.n, src.n))
	}

	/// TODO: docme
	pub(crate) fn izxt(&mut self, dst: impl Into<BuildReg>, src: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src = src.into();
		self.inst(IrInst::izxt(self.ea, dst.r, src.s, dst.n, src.n))
	}

	/// TODO: docme
	pub(crate) fn isxt(&mut self, dst: impl Into<BuildReg>, src: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src = src.into();
		self.inst(IrInst::isxt(self.ea, dst.r, src.s, dst.n, src.n))
	}

	/// TODO: docme
	pub(crate) fn ilo(&mut self, dst: impl Into<BuildReg>, src: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src = src.into();
		self.inst(IrInst::ilo(self.ea, dst.r, src.s, dst.n, src.n))
	}

	/// TODO: docme
	pub(crate) fn ihi(&mut self, dst: impl Into<BuildReg>, src: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src = src.into();
		self.inst(IrInst::ihi(self.ea, dst.r, src.s, dst.n, src.n))
	}

	/// TODO: docme
	pub(crate) fn ineg(&mut self, dst: impl Into<BuildReg>, src: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src = src.into();
		self.inst(IrInst::ineg(self.ea, dst.r, src.s, dst.n, src.n))
	}

	/// TODO: docme
	pub(crate) fn inot(&mut self, dst: impl Into<BuildReg>, src: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src = src.into();
		self.inst(IrInst::inot(self.ea, dst.r, src.s, dst.n, src.n))
	}

	/// TODO: docme
	pub(crate) fn bnot(&mut self, dst: impl Into<BuildReg>, src: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src = src.into();
		self.inst(IrInst::bnot(self.ea, dst.r, src.s, dst.n, src.n))
	}

	/// TODO: docme
	pub(crate) fn ieq(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		self.inst(IrInst::ieq(self.ea, dst.r, src1.s, src2.s, dst.n, src1.n, src2.n))
	}

	/// TODO: docme
	pub(crate) fn ine(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		self.inst(IrInst::ine(self.ea, dst.r, src1.s, src2.s, dst.n, src1.n, src2.n))
	}

	/// TODO: docme
	pub(crate) fn islt(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		self.inst(IrInst::islt(self.ea, dst.r, src1.s, src2.s, dst.n, src1.n, src2.n))
	}

	/// TODO: docme
	pub(crate) fn isle(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		self.inst(IrInst::isle(self.ea, dst.r, src1.s, src2.s, dst.n, src1.n, src2.n))
	}

	/// TODO: docme
	pub(crate) fn isgt(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		self.inst(IrInst::islt(self.ea, dst.r, src2.s, src1.s, dst.n, src2.n, src1.n))
	}

	/// TODO: docme
	pub(crate) fn isge(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		self.inst(IrInst::isle(self.ea, dst.r, src2.s, src1.s, dst.n, src2.n, src1.n))
	}

	/// TODO: docme
	pub(crate) fn iult(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		self.inst(IrInst::iult(self.ea, dst.r, src1.s, src2.s, dst.n, src1.n, src2.n))
	}

	/// TODO: docme
	pub(crate) fn iule(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		self.inst(IrInst::iule(self.ea, dst.r, src1.s, src2.s, dst.n, src1.n, src2.n))
	}

	/// TODO: docme
	pub(crate) fn iugt(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		self.inst(IrInst::iult(self.ea, dst.r, src2.s, src1.s, dst.n, src2.n, src1.n))
	}

	/// TODO: docme
	pub(crate) fn iuge(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		self.inst(IrInst::iule(self.ea, dst.r, src2.s, src1.s, dst.n, src2.n, src1.n))
	}

	/// TODO: docme
	pub(crate) fn iuadd(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		self.inst(IrInst::iuadd(self.ea, dst.r, src1.s, src2.s, dst.n, src1.n, src2.n))
	}

	/// TODO: docme
	pub(crate) fn iuaddc(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>, src3: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		let src3 = src3.into();
		self.inst(IrInst::iuaddc(self.ea, dst.r, src1.s, src2.s, src3.s,
			dst.n, src1.n, src2.n, src3.n))
	}

	/// TODO: docme
	pub(crate) fn iusub(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		self.inst(IrInst::iusub(self.ea, dst.r, src1.s, src2.s, dst.n, src1.n, src2.n))
	}

	/// TODO: docme
	pub(crate) fn iusubb(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>, src3: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		let src3 = src3.into();
		self.inst(IrInst::iusubb(self.ea, dst.r, src1.s, src2.s, src3.s,
			dst.n, src1.n, src2.n, src3.n))
	}

	/// TODO: docme
	pub(crate) fn iucarry(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		self.inst(IrInst::iucarry(self.ea, dst.r, src1.s, src2.s, dst.n, src1.n, src2.n))
	}

	/// TODO: docme
	pub(crate) fn iucarryc(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>, src3: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		let src3 = src3.into();
		self.inst(IrInst::iucarryc(self.ea, dst.r, src1.s, src2.s, src3.s,
			dst.n, src1.n, src2.n, src3.n))
	}

	/// TODO: docme
	pub(crate) fn iscarry(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		self.inst(IrInst::iscarry(self.ea, dst.r, src1.s, src2.s, dst.n, src1.n, src2.n))
	}

	/// TODO: docme
	pub(crate) fn iscarryc(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>, src3: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		let src3 = src3.into();
		self.inst(IrInst::iscarryc(self.ea, dst.r, src1.s, src2.s, src3.s,
			dst.n, src1.n, src2.n, src3.n))
	}

	/// TODO: docme
	pub(crate) fn isborrow(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		self.inst(IrInst::isborrow(self.ea, dst.r, src1.s, src2.s, dst.n, src1.n, src2.n))
	}

	/// TODO: docme
	pub(crate) fn isborrowb(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>, src3: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		let src3 = src3.into();
		self.inst(IrInst::isborrowb(self.ea, dst.r, src1.s, src2.s, src3.s,
			dst.n, src1.n, src2.n, src3.n))
	}

	/// TODO: docme
	pub(crate) fn icarries(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		self.inst(IrInst::icarries(self.ea, dst.r, src1.s, src2.s, dst.n, src1.n, src2.n))
	}

	/// TODO: docme
	pub(crate) fn icarriesc(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>, src3: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		let src3 = src3.into();
		self.inst(IrInst::icarriesc(self.ea, dst.r, src1.s, src2.s, src3.s,
			dst.n, src1.n, src2.n, src3.n))
	}

	/// TODO: docme
	pub(crate) fn iborrows(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		self.inst(IrInst::iborrows(self.ea, dst.r, src1.s, src2.s, dst.n, src1.n, src2.n))
	}

	/// TODO: docme
	pub(crate) fn iborrowsb(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>, src3: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		let src3 = src3.into();
		self.inst(IrInst::iborrowsb(self.ea, dst.r, src1.s, src2.s, src3.s,
			dst.n, src1.n, src2.n, src3.n))
	}

	/// TODO: docme
	pub(crate) fn imul(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		self.inst(IrInst::imul(self.ea, dst.r, src1.s, src2.s, dst.n, src1.n, src2.n))
	}

	/// TODO: docme
	pub(crate) fn iudiv(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		self.inst(IrInst::iudiv(self.ea, dst.r, src1.s, src2.s, dst.n, src1.n, src2.n))
	}

	/// TODO: docme
	pub(crate) fn isdiv(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		self.inst(IrInst::isdiv(self.ea, dst.r, src1.s, src2.s, dst.n, src1.n, src2.n))
	}

	/// TODO: docme
	pub(crate) fn iumod(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		self.inst(IrInst::iumod(self.ea, dst.r, src1.s, src2.s, dst.n, src1.n, src2.n))
	}

	/// TODO: docme
	pub(crate) fn ismod(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		self.inst(IrInst::ismod(self.ea, dst.r, src1.s, src2.s, dst.n, src1.n, src2.n))
	}

	/// TODO: docme
	pub(crate) fn ixor(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		self.inst(IrInst::ixor(self.ea, dst.r, src1.s, src2.s, dst.n, src1.n, src2.n))
	}

	/// TODO: docme
	pub(crate) fn iand(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		self.inst(IrInst::iand(self.ea, dst.r, src1.s, src2.s, dst.n, src1.n, src2.n))
	}

	/// TODO: docme
	pub(crate) fn ior(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		self.inst(IrInst::ior(self.ea, dst.r, src1.s, src2.s, dst.n, src1.n, src2.n))
	}

	/// TODO: docme
	pub(crate) fn ishl(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		self.inst(IrInst::ishl(self.ea, dst.r, src1.s, src2.s, dst.n, src1.n, src2.n))
	}

	/// TODO: docme
	pub(crate) fn iushr(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		self.inst(IrInst::iushr(self.ea, dst.r, src1.s, src2.s, dst.n, src1.n, src2.n))
	}

	/// TODO: docme
	pub(crate) fn isshr(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		self.inst(IrInst::isshr(self.ea, dst.r, src1.s, src2.s, dst.n, src1.n, src2.n))
	}

	/// TODO: docme
	pub(crate) fn irol(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		self.inst(IrInst::irol(self.ea, dst.r, src1.s, src2.s, dst.n, src1.n, src2.n))
	}

	/// TODO: docme
	pub(crate) fn iror(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		self.inst(IrInst::iror(self.ea, dst.r, src1.s, src2.s, dst.n, src1.n, src2.n))
	}

	/// TODO: docme
	pub(crate) fn ipair(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		self.inst(IrInst::ipair(self.ea, dst.r, src1.s, src2.s, dst.n, src1.n, src2.n))
	}

	/// TODO: docme
	pub(crate) fn ibit(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		self.inst(IrInst::ibit(self.ea, dst.r, src1.s, src2.s, dst.n, src1.n, src2.n))
	}

	/// TODO: docme
	pub(crate) fn ibset(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>, src3: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		let src3 = src3.into();
		self.inst(IrInst::ibset(self.ea, dst.r, src1.s, src2.s, src3.s,
			dst.n, src1.n, src2.n, src3.n))
	}

	/// TODO: docme
	pub(crate) fn bxor(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		self.inst(IrInst::bxor(self.ea, dst.r, src1.s, src2.s, dst.n, src1.n, src2.n))
	}

	/// TODO: docme
	pub(crate) fn band(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		self.inst(IrInst::band(self.ea, dst.r, src1.s, src2.s, dst.n, src1.n, src2.n))
	}

	/// TODO: docme
	pub(crate) fn bor(&mut self, dst: impl Into<BuildReg>, src1: impl Into<BuildSrc>,
		src2: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		let src1 = src1.into();
		let src2 = src2.into();
		self.inst(IrInst::bor(self.ea, dst.r, src1.s, src2.s, dst.n, src1.n, src2.n))
	}

	/// TODO: docme
	pub(crate) fn load(&mut self, dst: impl Into<BuildReg>, addr: impl Into<BuildSrc>)
		-> &mut Self {
		let dst = dst.into();
		let addr = addr.into();
		self.inst(IrInst::load(self.ea, dst.r, addr.s, dst.n, addr.n))
	}

	/// TODO: docme
	pub(crate) fn store(&mut self, addr: impl Into<BuildSrc>, src: impl Into<BuildSrc>)
		-> &mut Self {
		let src = src.into();
		let addr = addr.into();
		self.inst(IrInst::store(self.ea, addr.s, src.s, addr.n, src.n))
	}

	/// TODO: docme
	pub(crate) fn branch(&mut self, dst: impl Into<BuildEA>) -> &mut Self {
		let dst = dst.into();
		self.inst(IrInst::branch(self.ea, dst.ea, dst.n))
	}

	/// TODO: docme
	pub(crate) fn cbranch(&mut self, cond: impl Into<BuildSrc>, dst: impl Into<BuildEA>,
		cont: impl Into<BuildEA>) -> &mut Self {
		let cond = cond.into();
		let dst = dst.into();
		let cont = cont.into();
		self.inst(IrInst::cbranch(self.ea, cond.s, dst.ea, cont.ea, cond.n, dst.n))
	}

	/// TODO: docme
	pub(crate) fn cbranch_and_split(&mut self, cond: impl Into<BuildSrc>, dst: impl Into<BuildEA>)
		-> &mut Self {
		assert!(!self.cur);
		let cond = cond.into();
		let dst = dst.into();
		self.inst(IrInst::cbranch(self.ea, cond.s, dst.ea, self.next_irbbid, cond.n, dst.n));
		self.cur = true;
		self
	}

	/// TODO: docme
	pub(crate) fn ibranch(&mut self, dst: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		self.inst(IrInst::ibranch(self.ea, dst.s, dst.n))
	}

	/// TODO: docme
	pub(crate) fn call(&mut self, dst: impl Into<BuildEA>, cont: impl Into<BuildEA>) -> &mut Self {
		let dst = dst.into();
		let cont = cont.into();
		self.inst(IrInst::call(self.ea, dst.ea, cont.ea, dst.n))
	}

	/// TODO: docme
	pub(crate) fn icall(&mut self, dst: impl Into<BuildSrc>, cont: impl Into<BuildEA>)
		-> &mut Self {
		let dst = dst.into();
		let cont = cont.into();
		self.inst(IrInst::icall(self.ea, dst.s, cont.ea, dst.n))
	}

	/// TODO: docme
	pub(crate) fn ret(&mut self, dst: impl Into<BuildSrc>) -> &mut Self {
		let dst = dst.into();
		self.inst(IrInst::ret(self.ea, dst.s, dst.n))
	}

	/// TODO: docme
	pub(crate) fn halt(&mut self) -> &mut Self {
		self.inst(IrInst::halt(self.ea))
	}
}