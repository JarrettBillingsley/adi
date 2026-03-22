
use crate::ir::{ IrInst, IrSrc, IrReg, IrBBId };
use crate::memory::{ EA };

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
		assert_eq!(cur, false);
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
	pub(crate) fn mov(&mut self, dst: IrReg, src: impl Into<IrSrc>,
		dstn: i8, srcn: i8) -> &mut Self {
		self.inst(IrInst::mov(self.ea, dst, src.into(), dstn, srcn))
	}

	/// TODO: docme
	pub(crate) fn izxt(&mut self, dst: IrReg, src: impl Into<IrSrc>,
		dstn: i8, srcn: i8) -> &mut Self {
		self.inst(IrInst::izxt(self.ea, dst, src.into(), dstn, srcn))
	}

	/// TODO: docme
	pub(crate) fn isxt(&mut self, dst: IrReg, src: impl Into<IrSrc>,
		dstn: i8, srcn: i8) -> &mut Self {
		self.inst(IrInst::isxt(self.ea, dst, src.into(), dstn, srcn))
	}

	/// TODO: docme
	pub(crate) fn ilo(&mut self, dst: IrReg, src: impl Into<IrSrc>,
		dstn: i8, srcn: i8) -> &mut Self {
		self.inst(IrInst::ilo(self.ea, dst, src.into(), dstn, srcn))
	}

	/// TODO: docme
	pub(crate) fn ihi(&mut self, dst: IrReg, src: impl Into<IrSrc>,
		dstn: i8, srcn: i8) -> &mut Self {
		self.inst(IrInst::ihi(self.ea, dst, src.into(), dstn, srcn))
	}

	/// TODO: docme
	pub(crate) fn ineg(&mut self, dst: IrReg, src: impl Into<IrSrc>,
		dstn: i8, srcn: i8) -> &mut Self {
		self.inst(IrInst::ineg(self.ea, dst, src.into(), dstn, srcn))
	}

	/// TODO: docme
	pub(crate) fn inot(&mut self, dst: IrReg, src: impl Into<IrSrc>,
		dstn: i8, srcn: i8) -> &mut Self {
		self.inst(IrInst::inot(self.ea, dst, src.into(), dstn, srcn))
	}

	/// TODO: docme
	pub(crate) fn bnot(&mut self, dst: IrReg, src: impl Into<IrSrc>,
		dstn: i8, srcn: i8) -> &mut Self {
		self.inst(IrInst::bnot(self.ea, dst, src.into(), dstn, srcn))
	}

	/// TODO: docme
	pub(crate) fn ieq(&mut self, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, dstn: i8, src1n: i8, src2n: i8) -> &mut Self {
		self.inst(IrInst::ieq(self.ea, dst, src1.into(), src2.into(), dstn, src1n, src2n))
	}

	/// TODO: docme
	pub(crate) fn ine(&mut self, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, dstn: i8, src1n: i8, src2n: i8) -> &mut Self {
		self.inst(IrInst::ine(self.ea, dst, src1.into(), src2.into(), dstn, src1n, src2n))
	}

	/// TODO: docme
	pub(crate) fn islt(&mut self, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, dstn: i8, src1n: i8, src2n: i8) -> &mut Self {
		self.inst(IrInst::islt(self.ea, dst, src1.into(), src2.into(), dstn, src1n, src2n))
	}

	/// TODO: docme
	pub(crate) fn isle(&mut self, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, dstn: i8, src1n: i8, src2n: i8) -> &mut Self {
		self.inst(IrInst::isle(self.ea, dst, src1.into(), src2.into(), dstn, src1n, src2n))
	}

	/// TODO: docme
	pub(crate) fn isgt(&mut self, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, dstn: i8, src1n: i8, src2n: i8) -> &mut Self {
		self.inst(IrInst::islt(self.ea, dst, src2.into(), src1.into(), dstn, src2n, src1n))
	}

	/// TODO: docme
	pub(crate) fn isge(&mut self, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, dstn: i8, src1n: i8, src2n: i8) -> &mut Self {
		self.inst(IrInst::isle(self.ea, dst, src2.into(), src1.into(), dstn, src2n, src1n))
	}

	/// TODO: docme
	pub(crate) fn iult(&mut self, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, dstn: i8, src1n: i8, src2n: i8) -> &mut Self {
		self.inst(IrInst::iult(self.ea, dst, src1.into(), src2.into(), dstn, src1n, src2n))
	}

	/// TODO: docme
	pub(crate) fn iule(&mut self, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, dstn: i8, src1n: i8, src2n: i8) -> &mut Self {
		self.inst(IrInst::iule(self.ea, dst, src1.into(), src2.into(), dstn, src1n, src2n))
	}

	/// TODO: docme
	pub(crate) fn iugt(&mut self, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, dstn: i8, src1n: i8, src2n: i8) -> &mut Self {
		self.inst(IrInst::iult(self.ea, dst, src2.into(), src1.into(), dstn, src2n, src1n))
	}

	/// TODO: docme
	pub(crate) fn iuge(&mut self, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, dstn: i8, src1n: i8, src2n: i8) -> &mut Self {
		self.inst(IrInst::iule(self.ea, dst, src2.into(), src1.into(), dstn, src2n, src1n))
	}

	/// TODO: docme
	pub(crate) fn iuadd(&mut self, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, dstn: i8, src1n: i8, src2n: i8) -> &mut Self {
		self.inst(IrInst::iuadd(self.ea, dst, src1.into(), src2.into(), dstn, src1n, src2n))
	}

	/// TODO: docme
	pub(crate) fn iuaddc(&mut self, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, src3: impl Into<IrSrc>,
		dstn: i8, src1n: i8, src2n: i8, src3n: i8) -> &mut Self {
		self.inst(IrInst::iuaddc(self.ea, dst, src1.into(), src2.into(), src3.into(),
			dstn, src1n, src2n, src3n))
	}

	/// TODO: docme
	pub(crate) fn iusub(&mut self, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, dstn: i8, src1n: i8, src2n: i8) -> &mut Self {
		self.inst(IrInst::iusub(self.ea, dst, src1.into(), src2.into(), dstn, src1n, src2n))
	}

	/// TODO: docme
	pub(crate) fn iusubb(&mut self, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, src3: impl Into<IrSrc>,
		dstn: i8, src1n: i8, src2n: i8, src3n: i8) -> &mut Self {
		self.inst(IrInst::iusubb(self.ea, dst, src1.into(), src2.into(), src3.into(),
			dstn, src1n, src2n, src3n))
	}

	/// TODO: docme
	pub(crate) fn iucarry(&mut self, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, dstn: i8, src1n: i8, src2n: i8) -> &mut Self {
		self.inst(IrInst::iucarry(self.ea, dst, src1.into(), src2.into(), dstn, src1n, src2n))
	}

	/// TODO: docme
	pub(crate) fn iucarryc(&mut self, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, src3: impl Into<IrSrc>,
		dstn: i8, src1n: i8, src2n: i8, src3n: i8) -> &mut Self {
		self.inst(IrInst::iucarryc(self.ea, dst, src1.into(), src2.into(), src3.into(),
			dstn, src1n, src2n, src3n))
	}

	/// TODO: docme
	pub(crate) fn iscarry(&mut self, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, dstn: i8, src1n: i8, src2n: i8) -> &mut Self {
		self.inst(IrInst::iscarry(self.ea, dst, src1.into(), src2.into(), dstn, src1n, src2n))
	}

	/// TODO: docme
	pub(crate) fn iscarryc(&mut self, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, src3: impl Into<IrSrc>,
		dstn: i8, src1n: i8, src2n: i8, src3n: i8) -> &mut Self {
		self.inst(IrInst::iscarryc(self.ea, dst, src1.into(), src2.into(), src3.into(),
			dstn, src1n, src2n, src3n))
	}

	/// TODO: docme
	pub(crate) fn isborrow(&mut self, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, dstn: i8, src1n: i8, src2n: i8) -> &mut Self {
		self.inst(IrInst::isborrow(self.ea, dst, src1.into(), src2.into(), dstn, src1n, src2n))
	}

	/// TODO: docme
	pub(crate) fn isborrowb(&mut self, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, src3: impl Into<IrSrc>,
		dstn: i8, src1n: i8, src2n: i8, src3n: i8) -> &mut Self {
		self.inst(IrInst::isborrowb(self.ea, dst, src1.into(), src2.into(), src3.into(),
			dstn, src1n, src2n, src3n))
	}

	/// TODO: docme
	pub(crate) fn icarries(&mut self, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, dstn: i8, src1n: i8, src2n: i8) -> &mut Self {
		self.inst(IrInst::icarries(self.ea, dst, src1.into(), src2.into(), dstn, src1n, src2n))
	}

	/// TODO: docme
	pub(crate) fn icarriesc(&mut self, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, src3: impl Into<IrSrc>,
		dstn: i8, src1n: i8, src2n: i8, src3n: i8) -> &mut Self {
		self.inst(IrInst::icarriesc(self.ea, dst, src1.into(), src2.into(), src3.into(),
			dstn, src1n, src2n, src3n))
	}

	/// TODO: docme
	pub(crate) fn iborrows(&mut self, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, dstn: i8, src1n: i8, src2n: i8) -> &mut Self {
		self.inst(IrInst::iborrows(self.ea, dst, src1.into(), src2.into(), dstn, src1n, src2n))
	}

	/// TODO: docme
	pub(crate) fn iborrowsb(&mut self, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, src3: impl Into<IrSrc>,
		dstn: i8, src1n: i8, src2n: i8, src3n: i8) -> &mut Self {
		self.inst(IrInst::iborrowsb(self.ea, dst, src1.into(), src2.into(), src3.into(),
			dstn, src1n, src2n, src3n))
	}

	/// TODO: docme
	pub(crate) fn imul(&mut self, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, dstn: i8, src1n: i8, src2n: i8) -> &mut Self {
		self.inst(IrInst::imul(self.ea, dst, src1.into(), src2.into(), dstn, src1n, src2n))
	}

	/// TODO: docme
	pub(crate) fn iudiv(&mut self, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, dstn: i8, src1n: i8, src2n: i8) -> &mut Self {
		self.inst(IrInst::iudiv(self.ea, dst, src1.into(), src2.into(), dstn, src1n, src2n))
	}

	/// TODO: docme
	pub(crate) fn isdiv(&mut self, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, dstn: i8, src1n: i8, src2n: i8) -> &mut Self {
		self.inst(IrInst::isdiv(self.ea, dst, src1.into(), src2.into(), dstn, src1n, src2n))
	}

	/// TODO: docme
	pub(crate) fn iumod(&mut self, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, dstn: i8, src1n: i8, src2n: i8) -> &mut Self {
		self.inst(IrInst::iumod(self.ea, dst, src1.into(), src2.into(), dstn, src1n, src2n))
	}

	/// TODO: docme
	pub(crate) fn ismod(&mut self, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, dstn: i8, src1n: i8, src2n: i8) -> &mut Self {
		self.inst(IrInst::ismod(self.ea, dst, src1.into(), src2.into(), dstn, src1n, src2n))
	}

	/// TODO: docme
	pub(crate) fn ixor(&mut self, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, dstn: i8, src1n: i8, src2n: i8) -> &mut Self {
		self.inst(IrInst::ixor(self.ea, dst, src1.into(), src2.into(), dstn, src1n, src2n))
	}

	/// TODO: docme
	pub(crate) fn iand(&mut self, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, dstn: i8, src1n: i8, src2n: i8) -> &mut Self {
		self.inst(IrInst::iand(self.ea, dst, src1.into(), src2.into(), dstn, src1n, src2n))
	}

	/// TODO: docme
	pub(crate) fn ior(&mut self, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, dstn: i8, src1n: i8, src2n: i8) -> &mut Self {
		self.inst(IrInst::ior(self.ea, dst, src1.into(), src2.into(), dstn, src1n, src2n))
	}

	/// TODO: docme
	pub(crate) fn ishl(&mut self, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, dstn: i8, src1n: i8, src2n: i8) -> &mut Self {
		self.inst(IrInst::ishl(self.ea, dst, src1.into(), src2.into(), dstn, src1n, src2n))
	}

	/// TODO: docme
	pub(crate) fn iushr(&mut self, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, dstn: i8, src1n: i8, src2n: i8) -> &mut Self {
		self.inst(IrInst::iushr(self.ea, dst, src1.into(), src2.into(), dstn, src1n, src2n))
	}

	/// TODO: docme
	pub(crate) fn isshr(&mut self, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, dstn: i8, src1n: i8, src2n: i8) -> &mut Self {
		self.inst(IrInst::isshr(self.ea, dst, src1.into(), src2.into(), dstn, src1n, src2n))
	}

	/// TODO: docme
	pub(crate) fn irol(&mut self, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, dstn: i8, src1n: i8, src2n: i8) -> &mut Self {
		self.inst(IrInst::irol(self.ea, dst, src1.into(), src2.into(), dstn, src1n, src2n))
	}

	/// TODO: docme
	pub(crate) fn iror(&mut self, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, dstn: i8, src1n: i8, src2n: i8) -> &mut Self {
		self.inst(IrInst::iror(self.ea, dst, src1.into(), src2.into(), dstn, src1n, src2n))
	}

	/// TODO: docme
	pub(crate) fn ipair(&mut self, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, dstn: i8, src1n: i8, src2n: i8) -> &mut Self {
		self.inst(IrInst::ipair(self.ea, dst, src1.into(), src2.into(), dstn, src1n, src2n))
	}

	/// TODO: docme
	pub(crate) fn ibit(&mut self, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, dstn: i8, src1n: i8, src2n: i8) -> &mut Self {
		self.inst(IrInst::ibit(self.ea, dst, src1.into(), src2.into(), dstn, src1n, src2n))
	}

	/// TODO: docme
	pub(crate) fn ibset(&mut self, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, src3: impl Into<IrSrc>,
		dstn: i8, src1n: i8, src2n: i8, src3n: i8) -> &mut Self {
		self.inst(IrInst::ibset(self.ea, dst, src1.into(), src2.into(), src3.into(),
			dstn, src1n, src2n, src3n))
	}

	/// TODO: docme
	pub(crate) fn bxor(&mut self, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, dstn: i8, src1n: i8, src2n: i8) -> &mut Self {
		self.inst(IrInst::bxor(self.ea, dst, src1.into(), src2.into(), dstn, src1n, src2n))
	}

	/// TODO: docme
	pub(crate) fn band(&mut self, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, dstn: i8, src1n: i8, src2n: i8) -> &mut Self {
		self.inst(IrInst::band(self.ea, dst, src1.into(), src2.into(), dstn, src1n, src2n))
	}

	/// TODO: docme
	pub(crate) fn bor(&mut self, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, dstn: i8, src1n: i8, src2n: i8) -> &mut Self {
		self.inst(IrInst::bor(self.ea, dst, src1.into(), src2.into(), dstn, src1n, src2n))
	}

	/// TODO: docme
	pub(crate) fn load(&mut self, dst: IrReg, addr: impl Into<IrSrc>,
		dstn: i8, addrn: i8) -> &mut Self {
		self.inst(IrInst::load(self.ea, dst, addr.into(), dstn, addrn))
	}

	/// TODO: docme
	pub(crate) fn store(&mut self, addr: impl Into<IrSrc>, src: impl Into<IrSrc>,
		addrn: i8, srcn: i8) -> &mut Self {
		self.inst(IrInst::store(self.ea, addr.into(), src.into(), addrn, srcn))
	}

	/// TODO: docme
	pub(crate) fn branch(&mut self, dst: EA, dstn: i8) -> &mut Self {
		self.inst(IrInst::branch(self.ea, dst, dstn))
	}

	/// TODO: docme
	pub(crate) fn cbranch(&mut self, cond: impl Into<IrSrc>, dst: EA, cont: EA,
		condn: i8, dstn: i8) -> &mut Self {
		self.inst(IrInst::cbranch(self.ea, cond.into(), dst, cont, condn, dstn))
	}

	/// TODO: docme
	pub(crate) fn cbranch_and_split(&mut self, cond: impl Into<IrSrc>, dst: EA,
		condn: i8, dstn: i8) -> &mut Self {
		assert_eq!(self.cur, false);
		self.inst(IrInst::cbranch(self.ea, cond.into(), dst, self.next_irbbid, condn, dstn));
		self.cur = true;
		self
	}

	/// TODO: docme
	pub(crate) fn ibranch(&mut self, dst: impl Into<IrSrc>, dstn: i8) -> &mut Self {
		self.inst(IrInst::ibranch(self.ea, dst.into(), dstn))
	}

	/// TODO: docme
	pub(crate) fn call(&mut self, dst: EA, cont: EA, dstn: i8) -> &mut Self {
		self.inst(IrInst::call(self.ea, dst, cont, dstn))
	}

	/// TODO: docme
	pub(crate) fn icall(&mut self, dst: impl Into<IrSrc>, cont: EA,
		dstn: i8) -> &mut Self {
		self.inst(IrInst::icall(self.ea, dst.into(), cont, dstn))
	}

	/// TODO: docme
	pub(crate) fn ret(&mut self, dst: impl Into<IrSrc>, dstn: i8) -> &mut Self {
		self.inst(IrInst::ret(self.ea, dst.into(), dstn))
	}

	/// TODO: docme
	pub(crate) fn halt(&mut self) -> &mut Self {
		self.inst(IrInst::halt(self.ea))
	}
}