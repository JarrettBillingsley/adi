
use crate::ir::{ IrInst, /*IrInstKind,*/ Src, Place };
use crate::memory::{ EA };

// ------------------------------------------------------------------------------------------------
// IrBuilder
// ------------------------------------------------------------------------------------------------

/// Helper for building blocks of IR instructions.
pub struct IrBuilder {
	ea:    EA,
	insts: Vec<IrInst>,
}

impl IrBuilder {
	/// Constructor. `ea` is the address of the beginning of this block of IR,
	/// mostly used for debugging/error messages.
	pub fn new(ea: EA) -> Self {
		Self {
			ea,
			insts: Vec::with_capacity(8),
		}
	}

	/// Finish building and get the finished vec of instructions.
	pub fn finish(mut self) -> Vec<IrInst> {
		self.insts.shrink_to_fit();
		self.insts
	}
}

// ------------------------------------------------------------------------------------------------
// Non-IR-branch instructions
// ------------------------------------------------------------------------------------------------

impl IrBuilder {
	///
	pub fn nop(&mut self, ea: EA) -> usize {
		self.inst(IrInst::nop(ea))
	}

	///
	pub fn assign(&mut self, ea: EA, dst: Place, src: impl Into<Src>) -> usize {
		self.inst(IrInst::assign(ea, dst, src.into()))
	}

	///
	pub fn izxt(&mut self, ea: EA, dst: Place, src: impl Into<Src>) -> usize {
		self.inst(IrInst::izxt(ea, dst, src.into()))
	}

	///
	pub fn isxt(&mut self, ea: EA, dst: Place, src: impl Into<Src>) -> usize {
		self.inst(IrInst::isxt(ea, dst, src.into()))
	}

	///
	pub fn ineg(&mut self, ea: EA, dst: Place, src: impl Into<Src>) -> usize {
		self.inst(IrInst::ineg(ea, dst, src.into()))
	}

	///
	pub fn inot(&mut self, ea: EA, dst: Place, src: impl Into<Src>) -> usize {
		self.inst(IrInst::inot(ea, dst, src.into()))
	}

	///
	pub fn bnot(&mut self, ea: EA, dst: Place, src: impl Into<Src>) -> usize {
		self.inst(IrInst::bnot(ea, dst, src.into()))
	}

	///
	pub fn ieq(&mut self, ea: EA, dst: Place, src1: impl Into<Src>, src2: impl Into<Src>) -> usize {
		self.inst(IrInst::ieq(ea, dst, src1.into(), src2.into()))
	}

	///
	pub fn ine(&mut self, ea: EA, dst: Place, src1: impl Into<Src>, src2: impl Into<Src>) -> usize {
		self.inst(IrInst::ine(ea, dst, src1.into(), src2.into()))
	}

	///
	pub fn islt(&mut self, ea: EA, dst: Place, src1: impl Into<Src>, src2: impl Into<Src>) -> usize {
		self.inst(IrInst::islt(ea, dst, src1.into(), src2.into()))
	}

	///
	pub fn isle(&mut self, ea: EA, dst: Place, src1: impl Into<Src>, src2: impl Into<Src>) -> usize {
		self.inst(IrInst::isle(ea, dst, src1.into(), src2.into()))
	}

	///
	pub fn iult(&mut self, ea: EA, dst: Place, src1: impl Into<Src>, src2: impl Into<Src>) -> usize {
		self.inst(IrInst::iult(ea, dst, src1.into(), src2.into()))
	}

	///
	pub fn iule(&mut self, ea: EA, dst: Place, src1: impl Into<Src>, src2: impl Into<Src>) -> usize {
		self.inst(IrInst::iule(ea, dst, src1.into(), src2.into()))
	}

	///
	pub fn iuadd(&mut self, ea: EA, dst: Place, src1: impl Into<Src>, src2: impl Into<Src>) -> usize {
		self.inst(IrInst::iuadd(ea, dst, src1.into(), src2.into()))
	}

	///
	pub fn iusub(&mut self, ea: EA, dst: Place, src1: impl Into<Src>, src2: impl Into<Src>) -> usize {
		self.inst(IrInst::iusub(ea, dst, src1.into(), src2.into()))
	}

	///
	pub fn icarry(&mut self, ea: EA, dst: Place, src1: impl Into<Src>, src2: impl Into<Src>) -> usize {
		self.inst(IrInst::icarry(ea, dst, src1.into(), src2.into()))
	}

	///
	pub fn iscarry(&mut self, ea: EA, dst: Place, src1: impl Into<Src>, src2: impl Into<Src>) -> usize {
		self.inst(IrInst::iscarry(ea, dst, src1.into(), src2.into()))
	}

	///
	pub fn isborrow(&mut self, ea: EA, dst: Place, src1: impl Into<Src>, src2: impl Into<Src>) -> usize {
		self.inst(IrInst::isborrow(ea, dst, src1.into(), src2.into()))
	}

	///
	pub fn imul(&mut self, ea: EA, dst: Place, src1: impl Into<Src>, src2: impl Into<Src>) -> usize {
		self.inst(IrInst::imul(ea, dst, src1.into(), src2.into()))
	}

	///
	pub fn iudiv(&mut self, ea: EA, dst: Place, src1: impl Into<Src>, src2: impl Into<Src>) -> usize {
		self.inst(IrInst::iudiv(ea, dst, src1.into(), src2.into()))
	}

	///
	pub fn isdiv(&mut self, ea: EA, dst: Place, src1: impl Into<Src>, src2: impl Into<Src>) -> usize {
		self.inst(IrInst::isdiv(ea, dst, src1.into(), src2.into()))
	}

	///
	pub fn iumod(&mut self, ea: EA, dst: Place, src1: impl Into<Src>, src2: impl Into<Src>) -> usize {
		self.inst(IrInst::iumod(ea, dst, src1.into(), src2.into()))
	}

	///
	pub fn ismod(&mut self, ea: EA, dst: Place, src1: impl Into<Src>, src2: impl Into<Src>) -> usize {
		self.inst(IrInst::ismod(ea, dst, src1.into(), src2.into()))
	}

	///
	pub fn ixor(&mut self, ea: EA, dst: Place, src1: impl Into<Src>, src2: impl Into<Src>) -> usize {
		self.inst(IrInst::ixor(ea, dst, src1.into(), src2.into()))
	}

	///
	pub fn iand(&mut self, ea: EA, dst: Place, src1: impl Into<Src>, src2: impl Into<Src>) -> usize {
		self.inst(IrInst::iand(ea, dst, src1.into(), src2.into()))
	}

	///
	pub fn ior(&mut self, ea: EA, dst: Place, src1: impl Into<Src>, src2: impl Into<Src>) -> usize {
		self.inst(IrInst::ior(ea, dst, src1.into(), src2.into()))
	}

	///
	pub fn ishl(&mut self, ea: EA, dst: Place, src1: impl Into<Src>, src2: impl Into<Src>) -> usize {
		self.inst(IrInst::ishl(ea, dst, src1.into(), src2.into()))
	}

	///
	pub fn iushr(&mut self, ea: EA, dst: Place, src1: impl Into<Src>, src2: impl Into<Src>) -> usize {
		self.inst(IrInst::iushr(ea, dst, src1.into(), src2.into()))
	}

	///
	pub fn isshr(&mut self, ea: EA, dst: Place, src1: impl Into<Src>, src2: impl Into<Src>) -> usize {
		self.inst(IrInst::isshr(ea, dst, src1.into(), src2.into()))
	}

	///
	pub fn bxor(&mut self, ea: EA, dst: Place, src1: impl Into<Src>, src2: impl Into<Src>) -> usize {
		self.inst(IrInst::bxor(ea, dst, src1.into(), src2.into()))
	}

	///
	pub fn band(&mut self, ea: EA, dst: Place, src1: impl Into<Src>, src2: impl Into<Src>) -> usize {
		self.inst(IrInst::band(ea, dst, src1.into(), src2.into()))
	}

	///
	pub fn bor(&mut self, ea: EA, dst: Place, src1: impl Into<Src>, src2: impl Into<Src>) -> usize {
		self.inst(IrInst::bor(ea, dst, src1.into(), src2.into()))
	}

	///
	pub fn load(&mut self, ea: EA, dst: Place, addr: impl Into<Src>) -> usize {
		self.inst(IrInst::load(ea, dst, addr.into()))
	}

	///
	pub fn store(&mut self, ea: EA, addr: impl Into<Src>, src: impl Into<Src>) -> usize {
		self.inst(IrInst::store(ea, addr.into(), src.into()))
	}

	///
	pub fn branch(&mut self, ea: EA, target: EA) -> usize {
		self.inst(IrInst::branch(ea, target))
	}

	///
	pub fn cbranch(&mut self, ea: EA, cond: impl Into<Src>, target: EA) -> usize {
		self.inst(IrInst::cbranch(ea, cond.into(), target))
	}

	///
	pub fn ibranch(&mut self, ea: EA, target: impl Into<Src>) -> usize {
		self.inst(IrInst::ibranch(ea, target.into()))
	}

	///
	pub fn call(&mut self, ea: EA, target: EA) -> usize {
		self.inst(IrInst::call(ea, target))
	}

	///
	pub fn icall(&mut self, ea: EA, target: impl Into<Src>) -> usize {
		self.inst(IrInst::icall(ea, target.into()))
	}

	///
	pub fn ret(&mut self, ea: EA, target: impl Into<Src>) -> usize {
		self.inst(IrInst::ret(ea, target.into()))
	}

	fn inst(&mut self, inst: IrInst) -> usize {
		let ret = self.insts.len();
		self.insts.push(inst);
		ret
	}
}

/*
// ------------------------------------------------------------------------------------------------
// IR Branching
// ------------------------------------------------------------------------------------------------

/// Represents an unfinished IR branch instruction. See [`IrBuilder::irbranch`],
/// [`IrBuilder::ircbranch`], and [`IrBuilder::branch_here`].
pub struct IrBuilderBranch {
	ea:      EA,
	offset:  usize,
}

impl IrBuilderBranch {
	/// If you need the offset of this branch (e.g. to use it as the target of *another* branch),
	/// you can get it with this.
	pub fn offset(&self) -> usize {
		self.offset
	}
}

impl Drop for IrBuilderBranch {
	fn drop(&mut self) {
		panic!("You didn't give the branch in the block at {:?} at offset {} a target!",
			self.ea, self.offset);
	}
}

impl IrBuilder {
	/// Add an IR branch to the **absolute** target address within this IR block.
	pub fn irbranch_to(&mut self, ea: EA, target: usize) -> usize {
		let branch_offs = self.calc_branch(self.insts.len(), target);
		self.inst(IrInst::irbranch(ea, branch_offs))
	}

	/// Same as above, but conditional.
	pub fn ircbranch_to(&mut self, ea: EA, cond: impl Into<Src>, target: usize) -> usize {
		let branch_offs = self.calc_branch(self.insts.len(), target);
		self.inst(IrInst::ircbranch(ea, cond.into(), branch_offs))
	}

	/// Add an IR branch without knowing the target yet. The return value must be
	/// passed to [`branch_here`] later, or your code will panic.
	#[must_use]
	pub fn irbranch(&mut self, ea: EA) -> IrBuilderBranch {
		let offset = self.inst(IrInst::irbranch(ea, 0));
		IrBuilderBranch { ea: self.ea, offset }
	}

	/// Same as above, but conditional.
	#[must_use]
	pub fn ircbranch(&mut self, ea: EA, cond: impl Into<Src>) -> IrBuilderBranch {
		let offset = self.inst(IrInst::ircbranch(ea, cond.into(), 0));
		IrBuilderBranch { ea: self.ea, offset }
	}

	/// Consumes an [`IrBuilderBranch`] and patches the branch that it refers to to the current
	/// location. This must be called to prevent a panic.
	pub fn branch_here(&mut self, branch: IrBuilderBranch) {
		// simple sanity check
		assert!(branch.ea == self.ea);

		let branch_offs = self.calc_branch(branch.offset, self.insts.len());

		let replacement = match self.insts[branch.offset] {
			IrInst { ea, kind: IrInstKind::IrBranch { .. } } =>
				IrInst::irbranch(ea, branch_offs),

			IrInst { ea, kind: IrInstKind::IrCBranch { cond, .. } } =>
				IrInst::ircbranch(ea, cond, branch_offs),

			_ => unreachable!(),
		};

		self.insts[branch.offset] = replacement;

		// then defuse the bomb
		std::mem::forget(branch);
	}

	fn calc_branch(&self, origin: usize, target: usize) -> i32 {
		use std::convert::TryInto;
		let diff = (target as isize) - (origin as isize);
		diff.try_into().expect("branch offset outside i32 range?!?")
	}
}

#[test]
fn build_ir_branch_ok() {
	use crate::memory::{ SegId };

	let ea = EA::new(SegId(0), 0x1234);
	let mut b = IrBuilder::new(ea);
	let my_branch = b.irbranch(ea);
	b.branch_here(my_branch);
}

#[test]
#[should_panic]
fn build_ir_branch_err() {
	use crate::memory::{ SegId };

	let ea = EA::new(SegId(0), 0x1234);
	let mut b = IrBuilder::new(ea);
	let _ = b.irbranch(ea);
}
*/