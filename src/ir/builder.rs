
use crate::ir::{ IrInst, IrSrc, IrReg };
use crate::memory::{ EA };

// ------------------------------------------------------------------------------------------------
// IrBuilder
// ------------------------------------------------------------------------------------------------

/// Helper for building blocks of IR instructions.
pub(crate) struct IrBuilder {
	insts: Vec<IrInst>,
}

impl IrBuilder {
	/// Constructor.
	pub(crate) fn new() -> Self {
		Self {
			insts: Vec::with_capacity(8),
		}
	}

	/// Finish building and get the finished vec of instructions.
	pub(crate) fn finish(mut self) -> Vec<IrInst> {
		self.insts.shrink_to_fit();
		self.insts
	}
}

// ------------------------------------------------------------------------------------------------
// Non-IR-branch instructions
// ------------------------------------------------------------------------------------------------

#[allow(clippy::too_many_arguments)]
impl IrBuilder {
	/// TODO: docme
	pub(crate) fn nop(&mut self, ea: EA) -> usize {
		self.inst(IrInst::nop(ea))
	}

	/// TODO: docme
	pub(crate) fn use_(&mut self, ea: EA, reg: IrReg) -> usize {
		self.inst(IrInst::use_(ea, reg))
	}

	/// TODO: docme
	pub(crate) fn assign(&mut self, ea: EA, dst: IrReg, src: impl Into<IrSrc>,
		dstn: i8, srcn: i8) -> usize {
		self.inst(IrInst::assign(ea, dst, src.into(), dstn, srcn))
	}

	/// TODO: docme
	pub(crate) fn izxt(&mut self, ea: EA, dst: IrReg, src: impl Into<IrSrc>,
		dstn: i8, srcn: i8) -> usize {
		self.inst(IrInst::izxt(ea, dst, src.into(), dstn, srcn))
	}

	/// TODO: docme
	pub(crate) fn isxt(&mut self, ea: EA, dst: IrReg, src: impl Into<IrSrc>,
		dstn: i8, srcn: i8) -> usize {
		self.inst(IrInst::isxt(ea, dst, src.into(), dstn, srcn))
	}

	/// TODO: docme
	pub(crate) fn ineg(&mut self, ea: EA, dst: IrReg, src: impl Into<IrSrc>,
		dstn: i8, srcn: i8) -> usize {
		self.inst(IrInst::ineg(ea, dst, src.into(), dstn, srcn))
	}

	/// TODO: docme
	pub(crate) fn inot(&mut self, ea: EA, dst: IrReg, src: impl Into<IrSrc>,
		dstn: i8, srcn: i8) -> usize {
		self.inst(IrInst::inot(ea, dst, src.into(), dstn, srcn))
	}

	/// TODO: docme
	pub(crate) fn bnot(&mut self, ea: EA, dst: IrReg, src: impl Into<IrSrc>,
		dstn: i8, srcn: i8) -> usize {
		self.inst(IrInst::bnot(ea, dst, src.into(), dstn, srcn))
	}

	/// TODO: docme
	pub(crate) fn ieq(&mut self, ea: EA, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, dstn: i8, src1n: i8, src2n: i8) -> usize {
		self.inst(IrInst::ieq(ea, dst, src1.into(), src2.into(), dstn, src1n, src2n))
	}

	/// TODO: docme
	pub(crate) fn ine(&mut self, ea: EA, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, dstn: i8, src1n: i8, src2n: i8) -> usize {
		self.inst(IrInst::ine(ea, dst, src1.into(), src2.into(), dstn, src1n, src2n))
	}

	/// TODO: docme
	pub(crate) fn islt(&mut self, ea: EA, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, dstn: i8, src1n: i8, src2n: i8) -> usize {
		self.inst(IrInst::islt(ea, dst, src1.into(), src2.into(), dstn, src1n, src2n))
	}

	/// TODO: docme
	pub(crate) fn isle(&mut self, ea: EA, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, dstn: i8, src1n: i8, src2n: i8) -> usize {
		self.inst(IrInst::isle(ea, dst, src1.into(), src2.into(), dstn, src1n, src2n))
	}

	/// TODO: docme
	pub(crate) fn iult(&mut self, ea: EA, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, dstn: i8, src1n: i8, src2n: i8) -> usize {
		self.inst(IrInst::iult(ea, dst, src1.into(), src2.into(), dstn, src1n, src2n))
	}

	/// TODO: docme
	pub(crate) fn iule(&mut self, ea: EA, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, dstn: i8, src1n: i8, src2n: i8) -> usize {
		self.inst(IrInst::iule(ea, dst, src1.into(), src2.into(), dstn, src1n, src2n))
	}

	/// TODO: docme
	pub(crate) fn iuadd(&mut self, ea: EA, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, dstn: i8, src1n: i8, src2n: i8) -> usize {
		self.inst(IrInst::iuadd(ea, dst, src1.into(), src2.into(), dstn, src1n, src2n))
	}

	/// TODO: docme
	pub(crate) fn iuaddc(&mut self, ea: EA, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, src3: impl Into<IrSrc>,
		dstn: i8, src1n: i8, src2n: i8, src3n: i8) -> usize {
		self.inst(IrInst::iuaddc(ea, dst, src1.into(), src2.into(), src3.into(),
			dstn, src1n, src2n, src3n))
	}

	/// TODO: docme
	pub(crate) fn iusub(&mut self, ea: EA, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, dstn: i8, src1n: i8, src2n: i8) -> usize {
		self.inst(IrInst::iusub(ea, dst, src1.into(), src2.into(), dstn, src1n, src2n))
	}

	/// TODO: docme
	pub(crate) fn iusubb(&mut self, ea: EA, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, src3: impl Into<IrSrc>,
		dstn: i8, src1n: i8, src2n: i8, src3n: i8) -> usize {
		self.inst(IrInst::iusubb(ea, dst, src1.into(), src2.into(), src3.into(),
			dstn, src1n, src2n, src3n))
	}

	/// TODO: docme
	pub(crate) fn icarry(&mut self, ea: EA, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, dstn: i8, src1n: i8, src2n: i8) -> usize {
		self.inst(IrInst::icarry(ea, dst, src1.into(), src2.into(), dstn, src1n, src2n))
	}

	/// TODO: docme
	pub(crate) fn icarryc(&mut self, ea: EA, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, src3: impl Into<IrSrc>,
		dstn: i8, src1n: i8, src2n: i8, src3n: i8) -> usize {
		self.inst(IrInst::icarryc(ea, dst, src1.into(), src2.into(), src3.into(),
			dstn, src1n, src2n, src3n))
	}

	/// TODO: docme
	pub(crate) fn iscarry(&mut self, ea: EA, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, dstn: i8, src1n: i8, src2n: i8) -> usize {
		self.inst(IrInst::iscarry(ea, dst, src1.into(), src2.into(), dstn, src1n, src2n))
	}

	/// TODO: docme
	pub(crate) fn iscarryc(&mut self, ea: EA, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, src3: impl Into<IrSrc>,
		dstn: i8, src1n: i8, src2n: i8, src3n: i8) -> usize {
		self.inst(IrInst::iscarryc(ea, dst, src1.into(), src2.into(), src3.into(),
			dstn, src1n, src2n, src3n))
	}

	/// TODO: docme
	pub(crate) fn isborrow(&mut self, ea: EA, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, dstn: i8, src1n: i8, src2n: i8) -> usize {
		self.inst(IrInst::isborrow(ea, dst, src1.into(), src2.into(), dstn, src1n, src2n))
	}

	/// TODO: docme
	pub(crate) fn isborrowc(&mut self, ea: EA, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, src3: impl Into<IrSrc>,
		dstn: i8, src1n: i8, src2n: i8, src3n: i8) -> usize {
		self.inst(IrInst::isborrowc(ea, dst, src1.into(), src2.into(), src3.into(),
			dstn, src1n, src2n, src3n))
	}

	/// TODO: docme
	pub(crate) fn imul(&mut self, ea: EA, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, dstn: i8, src1n: i8, src2n: i8) -> usize {
		self.inst(IrInst::imul(ea, dst, src1.into(), src2.into(), dstn, src1n, src2n))
	}

	/// TODO: docme
	pub(crate) fn iudiv(&mut self, ea: EA, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, dstn: i8, src1n: i8, src2n: i8) -> usize {
		self.inst(IrInst::iudiv(ea, dst, src1.into(), src2.into(), dstn, src1n, src2n))
	}

	/// TODO: docme
	pub(crate) fn isdiv(&mut self, ea: EA, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, dstn: i8, src1n: i8, src2n: i8) -> usize {
		self.inst(IrInst::isdiv(ea, dst, src1.into(), src2.into(), dstn, src1n, src2n))
	}

	/// TODO: docme
	pub(crate) fn iumod(&mut self, ea: EA, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, dstn: i8, src1n: i8, src2n: i8) -> usize {
		self.inst(IrInst::iumod(ea, dst, src1.into(), src2.into(), dstn, src1n, src2n))
	}

	/// TODO: docme
	pub(crate) fn ismod(&mut self, ea: EA, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, dstn: i8, src1n: i8, src2n: i8) -> usize {
		self.inst(IrInst::ismod(ea, dst, src1.into(), src2.into(), dstn, src1n, src2n))
	}

	/// TODO: docme
	pub(crate) fn ixor(&mut self, ea: EA, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, dstn: i8, src1n: i8, src2n: i8) -> usize {
		self.inst(IrInst::ixor(ea, dst, src1.into(), src2.into(), dstn, src1n, src2n))
	}

	/// TODO: docme
	pub(crate) fn iand(&mut self, ea: EA, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, dstn: i8, src1n: i8, src2n: i8) -> usize {
		self.inst(IrInst::iand(ea, dst, src1.into(), src2.into(), dstn, src1n, src2n))
	}

	/// TODO: docme
	pub(crate) fn ior(&mut self, ea: EA, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, dstn: i8, src1n: i8, src2n: i8) -> usize {
		self.inst(IrInst::ior(ea, dst, src1.into(), src2.into(), dstn, src1n, src2n))
	}

	/// TODO: docme
	pub(crate) fn ishl(&mut self, ea: EA, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, dstn: i8, src1n: i8, src2n: i8) -> usize {
		self.inst(IrInst::ishl(ea, dst, src1.into(), src2.into(), dstn, src1n, src2n))
	}

	/// TODO: docme
	pub(crate) fn iushr(&mut self, ea: EA, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, dstn: i8, src1n: i8, src2n: i8) -> usize {
		self.inst(IrInst::iushr(ea, dst, src1.into(), src2.into(), dstn, src1n, src2n))
	}

	/// TODO: docme
	pub(crate) fn isshr(&mut self, ea: EA, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, dstn: i8, src1n: i8, src2n: i8) -> usize {
		self.inst(IrInst::isshr(ea, dst, src1.into(), src2.into(), dstn, src1n, src2n))
	}

	/// TODO: docme
	pub(crate) fn ipair(&mut self, ea: EA, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, dstn: i8, src1n: i8, src2n: i8) -> usize {
		self.inst(IrInst::ipair(ea, dst, src1.into(), src2.into(), dstn, src1n, src2n))
	}

	/// TODO: docme
	pub(crate) fn bxor(&mut self, ea: EA, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, dstn: i8, src1n: i8, src2n: i8) -> usize {
		self.inst(IrInst::bxor(ea, dst, src1.into(), src2.into(), dstn, src1n, src2n))
	}

	/// TODO: docme
	pub(crate) fn band(&mut self, ea: EA, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, dstn: i8, src1n: i8, src2n: i8) -> usize {
		self.inst(IrInst::band(ea, dst, src1.into(), src2.into(), dstn, src1n, src2n))
	}

	/// TODO: docme
	pub(crate) fn bor(&mut self, ea: EA, dst: IrReg, src1: impl Into<IrSrc>,
		src2: impl Into<IrSrc>, dstn: i8, src1n: i8, src2n: i8) -> usize {
		self.inst(IrInst::bor(ea, dst, src1.into(), src2.into(), dstn, src1n, src2n))
	}

	/// TODO: docme
	pub(crate) fn load(&mut self, ea: EA, dst: IrReg, addr: impl Into<IrSrc>,
		dstn: i8, addrn: i8) -> usize {
		self.inst(IrInst::load(ea, dst, addr.into(), dstn, addrn))
	}

	/// TODO: docme
	pub(crate) fn store(&mut self, ea: EA, addr: impl Into<IrSrc>, src: impl Into<IrSrc>,
		addrn: i8, srcn: i8) -> usize {
		self.inst(IrInst::store(ea, addr.into(), src.into(), addrn, srcn))
	}

	/// TODO: docme
	pub(crate) fn branch(&mut self, ea: EA, target: EA, targetn: i8) -> usize {
		self.inst(IrInst::branch(ea, target, targetn))
	}

	/// TODO: docme
	pub(crate) fn cbranch(&mut self, ea: EA, cond: impl Into<IrSrc>, target: EA,
		condn: i8, targetn: i8) -> usize {
		self.inst(IrInst::cbranch(ea, cond.into(), target, condn, targetn))
	}

	/// TODO: docme
	pub(crate) fn ibranch(&mut self, ea: EA, target: impl Into<IrSrc>, targetn: i8) -> usize {
		self.inst(IrInst::ibranch(ea, target.into(), targetn))
	}

	/// TODO: docme
	pub(crate) fn call(&mut self, ea: EA, target: EA, targetn: i8) -> usize {
		self.inst(IrInst::call(ea, target, targetn))
	}

	/// TODO: docme
	pub(crate) fn icall(&mut self, ea: EA, target: impl Into<IrSrc>, targetn: i8) -> usize {
		self.inst(IrInst::icall(ea, target.into(), targetn))
	}

	/// TODO: docme
	pub(crate) fn ret(&mut self, ea: EA, target: impl Into<IrSrc>, targetn: i8) -> usize {
		self.inst(IrInst::ret(ea, target.into(), targetn))
	}

	fn inst(&mut self, inst: IrInst) -> usize {
		let ret = self.insts.len();
		self.insts.push(inst);
		ret
	}
}