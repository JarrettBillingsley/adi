
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

impl IrBuilder {
	///
	pub(crate) fn nop(&mut self, ea: EA) -> usize {
		self.inst(IrInst::nop(ea))
	}

	///
	pub(crate) fn assign(&mut self, ea: EA, dst: IrReg, src: impl Into<IrSrc>) -> usize {
		self.inst(IrInst::assign(ea, dst, src.into()))
	}

	///
	pub(crate) fn izxt(&mut self, ea: EA, dst: IrReg, src: impl Into<IrSrc>) -> usize {
		self.inst(IrInst::izxt(ea, dst, src.into()))
	}

	///
	pub(crate) fn isxt(&mut self, ea: EA, dst: IrReg, src: impl Into<IrSrc>) -> usize {
		self.inst(IrInst::isxt(ea, dst, src.into()))
	}

	///
	pub(crate) fn ineg(&mut self, ea: EA, dst: IrReg, src: impl Into<IrSrc>) -> usize {
		self.inst(IrInst::ineg(ea, dst, src.into()))
	}

	///
	pub(crate) fn inot(&mut self, ea: EA, dst: IrReg, src: impl Into<IrSrc>) -> usize {
		self.inst(IrInst::inot(ea, dst, src.into()))
	}

	///
	pub(crate) fn bnot(&mut self, ea: EA, dst: IrReg, src: impl Into<IrSrc>) -> usize {
		self.inst(IrInst::bnot(ea, dst, src.into()))
	}

	///
	pub(crate) fn ieq(&mut self, ea: EA, dst: IrReg, src1: impl Into<IrSrc>, src2: impl Into<IrSrc>) -> usize {
		self.inst(IrInst::ieq(ea, dst, src1.into(), src2.into()))
	}

	///
	pub(crate) fn ine(&mut self, ea: EA, dst: IrReg, src1: impl Into<IrSrc>, src2: impl Into<IrSrc>) -> usize {
		self.inst(IrInst::ine(ea, dst, src1.into(), src2.into()))
	}

	///
	pub(crate) fn islt(&mut self, ea: EA, dst: IrReg, src1: impl Into<IrSrc>, src2: impl Into<IrSrc>) -> usize {
		self.inst(IrInst::islt(ea, dst, src1.into(), src2.into()))
	}

	///
	pub(crate) fn isle(&mut self, ea: EA, dst: IrReg, src1: impl Into<IrSrc>, src2: impl Into<IrSrc>) -> usize {
		self.inst(IrInst::isle(ea, dst, src1.into(), src2.into()))
	}

	///
	pub(crate) fn iult(&mut self, ea: EA, dst: IrReg, src1: impl Into<IrSrc>, src2: impl Into<IrSrc>) -> usize {
		self.inst(IrInst::iult(ea, dst, src1.into(), src2.into()))
	}

	///
	pub(crate) fn iule(&mut self, ea: EA, dst: IrReg, src1: impl Into<IrSrc>, src2: impl Into<IrSrc>) -> usize {
		self.inst(IrInst::iule(ea, dst, src1.into(), src2.into()))
	}

	///
	pub(crate) fn iuadd(&mut self, ea: EA, dst: IrReg, src1: impl Into<IrSrc>, src2: impl Into<IrSrc>) -> usize {
		self.inst(IrInst::iuadd(ea, dst, src1.into(), src2.into()))
	}

	///
	pub(crate) fn iuaddc(&mut self, ea: EA, dst: IrReg, src1: impl Into<IrSrc>, src2: impl Into<IrSrc>,
	src3: impl Into<IrSrc>) -> usize {
		self.inst(IrInst::iuaddc(ea, dst, src1.into(), src2.into(), src3.into()))
	}

	///
	pub(crate) fn iusub(&mut self, ea: EA, dst: IrReg, src1: impl Into<IrSrc>, src2: impl Into<IrSrc>) -> usize {
		self.inst(IrInst::iusub(ea, dst, src1.into(), src2.into()))
	}

	///
	pub(crate) fn iusubb(&mut self, ea: EA, dst: IrReg, src1: impl Into<IrSrc>, src2: impl Into<IrSrc>,
	src3: impl Into<IrSrc>) -> usize {
		self.inst(IrInst::iusubb(ea, dst, src1.into(), src2.into(), src3.into()))
	}

	///
	pub(crate) fn icarry(&mut self, ea: EA, dst: IrReg, src1: impl Into<IrSrc>, src2: impl Into<IrSrc>) -> usize {
		self.inst(IrInst::icarry(ea, dst, src1.into(), src2.into()))
	}

	///
	pub(crate) fn icarryc(&mut self, ea: EA, dst: IrReg, src1: impl Into<IrSrc>, src2: impl Into<IrSrc>,
	src3: impl Into<IrSrc>) -> usize {
		self.inst(IrInst::icarryc(ea, dst, src1.into(), src2.into(), src3.into()))
	}

	///
	pub(crate) fn iscarry(&mut self, ea: EA, dst: IrReg, src1: impl Into<IrSrc>, src2: impl Into<IrSrc>) -> usize {
		self.inst(IrInst::iscarry(ea, dst, src1.into(), src2.into()))
	}

	///
	pub(crate) fn iscarryc(&mut self, ea: EA, dst: IrReg, src1: impl Into<IrSrc>, src2: impl Into<IrSrc>,
	src3: impl Into<IrSrc>) -> usize {
		self.inst(IrInst::iscarryc(ea, dst, src1.into(), src2.into(), src3.into()))
	}

	///
	pub(crate) fn isborrow(&mut self, ea: EA, dst: IrReg, src1: impl Into<IrSrc>, src2: impl Into<IrSrc>) -> usize {
		self.inst(IrInst::isborrow(ea, dst, src1.into(), src2.into()))
	}

	///
	pub(crate) fn isborrowc(&mut self, ea: EA, dst: IrReg, src1: impl Into<IrSrc>, src2: impl Into<IrSrc>,
	src3: impl Into<IrSrc>) -> usize {
		self.inst(IrInst::isborrowc(ea, dst, src1.into(), src2.into(), src3.into()))
	}

	///
	pub(crate) fn imul(&mut self, ea: EA, dst: IrReg, src1: impl Into<IrSrc>, src2: impl Into<IrSrc>) -> usize {
		self.inst(IrInst::imul(ea, dst, src1.into(), src2.into()))
	}

	///
	pub(crate) fn iudiv(&mut self, ea: EA, dst: IrReg, src1: impl Into<IrSrc>, src2: impl Into<IrSrc>) -> usize {
		self.inst(IrInst::iudiv(ea, dst, src1.into(), src2.into()))
	}

	///
	pub(crate) fn isdiv(&mut self, ea: EA, dst: IrReg, src1: impl Into<IrSrc>, src2: impl Into<IrSrc>) -> usize {
		self.inst(IrInst::isdiv(ea, dst, src1.into(), src2.into()))
	}

	///
	pub(crate) fn iumod(&mut self, ea: EA, dst: IrReg, src1: impl Into<IrSrc>, src2: impl Into<IrSrc>) -> usize {
		self.inst(IrInst::iumod(ea, dst, src1.into(), src2.into()))
	}

	///
	pub(crate) fn ismod(&mut self, ea: EA, dst: IrReg, src1: impl Into<IrSrc>, src2: impl Into<IrSrc>) -> usize {
		self.inst(IrInst::ismod(ea, dst, src1.into(), src2.into()))
	}

	///
	pub(crate) fn ixor(&mut self, ea: EA, dst: IrReg, src1: impl Into<IrSrc>, src2: impl Into<IrSrc>) -> usize {
		self.inst(IrInst::ixor(ea, dst, src1.into(), src2.into()))
	}

	///
	pub(crate) fn iand(&mut self, ea: EA, dst: IrReg, src1: impl Into<IrSrc>, src2: impl Into<IrSrc>) -> usize {
		self.inst(IrInst::iand(ea, dst, src1.into(), src2.into()))
	}

	///
	pub(crate) fn ior(&mut self, ea: EA, dst: IrReg, src1: impl Into<IrSrc>, src2: impl Into<IrSrc>) -> usize {
		self.inst(IrInst::ior(ea, dst, src1.into(), src2.into()))
	}

	///
	pub(crate) fn ishl(&mut self, ea: EA, dst: IrReg, src1: impl Into<IrSrc>, src2: impl Into<IrSrc>) -> usize {
		self.inst(IrInst::ishl(ea, dst, src1.into(), src2.into()))
	}

	///
	pub(crate) fn iushr(&mut self, ea: EA, dst: IrReg, src1: impl Into<IrSrc>, src2: impl Into<IrSrc>) -> usize {
		self.inst(IrInst::iushr(ea, dst, src1.into(), src2.into()))
	}

	///
	pub(crate) fn isshr(&mut self, ea: EA, dst: IrReg, src1: impl Into<IrSrc>, src2: impl Into<IrSrc>) -> usize {
		self.inst(IrInst::isshr(ea, dst, src1.into(), src2.into()))
	}

	///
	pub(crate) fn ipair(&mut self, ea: EA, dst: IrReg, src1: impl Into<IrSrc>, src2: impl Into<IrSrc>) -> usize {
		self.inst(IrInst::ipair(ea, dst, src1.into(), src2.into()))
	}

	///
	pub(crate) fn bxor(&mut self, ea: EA, dst: IrReg, src1: impl Into<IrSrc>, src2: impl Into<IrSrc>) -> usize {
		self.inst(IrInst::bxor(ea, dst, src1.into(), src2.into()))
	}

	///
	pub(crate) fn band(&mut self, ea: EA, dst: IrReg, src1: impl Into<IrSrc>, src2: impl Into<IrSrc>) -> usize {
		self.inst(IrInst::band(ea, dst, src1.into(), src2.into()))
	}

	///
	pub(crate) fn bor(&mut self, ea: EA, dst: IrReg, src1: impl Into<IrSrc>, src2: impl Into<IrSrc>) -> usize {
		self.inst(IrInst::bor(ea, dst, src1.into(), src2.into()))
	}

	///
	pub(crate) fn load(&mut self, ea: EA, dst: IrReg, addr: impl Into<IrSrc>) -> usize {
		self.inst(IrInst::load(ea, dst, addr.into()))
	}

	///
	pub(crate) fn store(&mut self, ea: EA, addr: impl Into<IrSrc>, src: impl Into<IrSrc>) -> usize {
		self.inst(IrInst::store(ea, addr.into(), src.into()))
	}

	///
	pub(crate) fn branch(&mut self, ea: EA, target: EA) -> usize {
		self.inst(IrInst::branch(ea, target))
	}

	///
	pub(crate) fn cbranch(&mut self, ea: EA, cond: impl Into<IrSrc>, target: EA) -> usize {
		self.inst(IrInst::cbranch(ea, cond.into(), target))
	}

	///
	pub(crate) fn ibranch(&mut self, ea: EA, target: impl Into<IrSrc>) -> usize {
		self.inst(IrInst::ibranch(ea, target.into()))
	}

	///
	pub(crate) fn call(&mut self, ea: EA, target: EA) -> usize {
		self.inst(IrInst::call(ea, target))
	}

	///
	pub(crate) fn icall(&mut self, ea: EA, target: impl Into<IrSrc>) -> usize {
		self.inst(IrInst::icall(ea, target.into()))
	}

	///
	pub(crate) fn ret(&mut self, ea: EA, target: impl Into<IrSrc>) -> usize {
		self.inst(IrInst::ret(ea, target.into()))
	}

	fn inst(&mut self, inst: IrInst) -> usize {
		let ret = self.insts.len();
		self.insts.push(inst);
		ret
	}
}