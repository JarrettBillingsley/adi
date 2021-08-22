
use std::fmt::{ Debug, Formatter, Result as FmtResult };
use crate::memory::{ EA };
use crate::program::{ BBId, FuncId };

// ------------------------------------------------------------------------------------------------
// Sub-modules
// ------------------------------------------------------------------------------------------------

pub mod builder;
pub mod inst;
pub mod ssa;

pub(crate) use builder::*;
pub(crate) use inst::*;
// pub use ssa::*;

// ------------------------------------------------------------------------------------------------
// ValSize
// ------------------------------------------------------------------------------------------------

/// Possible sizes of values used in the IR, measured in bits.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub(crate) enum ValSize {
	_8  = 8,
	_16 = 16,
	_32 = 32,
	_64 = 64,
}

impl ValSize {
	/// How many *bytes* a `ValSize` takes up.
	fn bytes(&self) -> usize {
		match self {
			ValSize::_8  => 1,
			ValSize::_16 => 2,
			ValSize::_32 => 4,
			ValSize::_64 => 8,
		}
	}

	fn is_twice(&self, other: ValSize) -> bool {
		match other {
			ValSize::_8  => matches!(self, ValSize::_16),
			ValSize::_16 => matches!(self, ValSize::_32),
			ValSize::_32 => matches!(self, ValSize::_64),
			ValSize::_64 => panic!("can't represent paired 64-bit values"),
		}
	}

	fn name(&self) -> &'static str {
		match self {
			ValSize::_8  => "b",
			ValSize::_16 => "s",
			ValSize::_32 => "i",
			ValSize::_64 => "l",
		}
	}
}

// ------------------------------------------------------------------------------------------------
// IrReg
// ------------------------------------------------------------------------------------------------

/// Represents a register in the IR. Can appear as the destination of instructions.
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub(crate) struct IrReg {
	size:   ValSize,
	offset: u16,
	gen:    Option<u32>
}

impl Debug for IrReg {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		if let Some(gen) = self.gen {
			write!(f, "r{}_{}.{}", self.offset, gen, self.size.name())
		} else {
			write!(f, "r{}.{}", self.offset, self.size.name())
		}
	}
}

impl IrReg {
	/// Constructs an 8-bit register.
	pub(crate) const fn reg8(offset: u16) -> Self {
		Self { size: ValSize::_8, offset, gen: None }
	}

	/// Constructs a 16-bit register.
	pub(crate) const fn reg16(offset: u16) -> Self {
		Self { size: ValSize::_16, offset, gen: None }
	}

	/// Constructs a 32-bit register.
	pub(crate) const fn reg32(offset: u16) -> Self {
		Self { size: ValSize::_32, offset, gen: None }
	}

	/// Constructs a 64-bit register.
	pub(crate) const fn reg64(offset: u16) -> Self {
		Self { size: ValSize::_64, offset, gen: None }
	}

	/// The size of this register.
	#[inline]
	pub(crate) fn size(&self) -> ValSize {
		self.size
	}

	/// Its offset into the registers "segment."
	#[inline]
	pub(crate) fn offset(&self) -> u16 {
		self.offset
	}

	/// True if this register has been given an SSA generation.
	fn is_ssa(&self) -> bool {
		self.gen.is_some()
	}

	/// If this is not an SSA register, returns a new `IrReg` subscripted with the given index.
	/// Panics if this is already an SSA register.
	fn sub(&self, i: u32) -> Self {
		assert!(self.gen.is_none(), ".sub() called on '{:?}'", self);
		Self {
			gen: Some(i),
			..*self
		}
	}
}

// ------------------------------------------------------------------------------------------------
// IrConst
// ------------------------------------------------------------------------------------------------

/// A constant value.
#[derive(PartialEq, Eq, Clone, Copy)]
pub(crate) struct IrConst {
	size: ValSize,
	val:  u64,
}

impl Debug for IrConst {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		match self.size {
			ValSize::_8  => write!(f, "const 0x{:02X}", self.val),
			ValSize::_16 => write!(f, "const 0x{:04X}", self.val),
			ValSize::_32 => write!(f, "const 0x{:08X}", self.val),
			ValSize::_64 => write!(f, "const 0x{:016X}", self.val),
		}
	}
}

impl IrConst {
	/// 8-bit constant 0.
	pub(crate) const ZERO_8:  IrConst = Self::_8(0);
	/// 16-bit constant 0.
	pub(crate) const ZERO_16: IrConst = Self::_16(0);
	/// 32-bit constant 0.
	pub(crate) const ZERO_32: IrConst = Self::_32(0);
	/// 64-bit constant 0.
	pub(crate) const ZERO_64: IrConst = Self::_64(0);
	/// 8-bit constant 1.
	pub(crate) const ONE_8:   IrConst = Self::_8(1);
	/// 16-bit constant 1.
	pub(crate) const ONE_16:  IrConst = Self::_16(1);
	/// 32-bit constant 1.
	pub(crate) const ONE_32:  IrConst = Self::_32(1);
	/// 64-bit constant 1.
	pub(crate) const ONE_64:  IrConst = Self::_64(1);

	/// Constructs an 8-bit constant.
	pub(crate) const fn _8(val: u8) -> Self {
		Self { size: ValSize::_8, val: val as u64 }
	}

	/// Constructs a 16-bit constant.
	pub(crate) const fn _16(val: u16) -> Self {
		Self { size: ValSize::_16, val: val as u64 }
	}

	/// Constructs a 32-bit constant.
	pub(crate) const fn _32(val: u32) -> Self {
		Self { size: ValSize::_32, val: val as u64 }
	}

	/// Constructs a 64-bit constant.
	pub(crate) const fn _64(val: u64) -> Self {
		Self { size: ValSize::_64, val }
	}
}

// ------------------------------------------------------------------------------------------------
// IrSrc
// ------------------------------------------------------------------------------------------------

/// The source of a value. Can be either an [`IrReg`] or an [`IrConst`].
#[derive(PartialEq, Eq, Clone, Copy)]
pub(crate) enum IrSrc {
	Reg(IrReg),
	Const(IrConst),
}

impl Debug for IrSrc {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		match self {
			IrSrc::Reg(p) => write!(f, "{:?}", p),
			IrSrc::Const(c) => write!(f, "{:?}", c),
		}
	}
}

impl IrSrc {
	/// The size of this value.
	#[inline]
	pub(crate) fn size(&self) -> ValSize {
		match self {
			IrSrc::Reg(IrReg { size, .. }) |
			IrSrc::Const(IrConst { size, .. }) => *size,
		}
	}

	/// Callback iterator over regs (well, reg) represented by this source.
	pub(crate) fn regs(&self, f: &mut impl FnMut(&IrReg)) {
		if let IrSrc::Reg(r) = self {
			f(r);
		}
	}

	pub(crate) fn visit_use(&self, mut f: impl FnMut(IrReg)) {
		if let IrSrc::Reg(r) = self {
			f(*r);
		}
	}

	pub(crate) fn visit_use_mut(&mut self, mut f: impl FnMut(&mut IrReg)) {
		if let IrSrc::Reg(r) = self {
			f(r);
		}
	}
}

impl From<IrReg> for IrSrc {
	fn from(p: IrReg) -> Self {
		IrSrc::Reg(p)
	}
}

impl From<IrConst> for IrSrc {
	fn from(c: IrConst) -> Self {
		IrSrc::Const(c)
	}
}

// ------------------------------------------------------------------------------------------------
// IrPhi
// ------------------------------------------------------------------------------------------------

#[derive(PartialEq, Eq, Clone)]
struct IrPhi {
	dst:  IrReg,
	args: Vec<IrReg>,
}

impl IrPhi {
	fn new(reg: IrReg, num_args: usize) -> Self {
		assert!(!reg.is_ssa());

		Self {
			dst:  reg,
			args: vec![reg; num_args],
		}
	}

	fn assigns(&self, reg: IrReg) -> bool {
		self.dst == reg
	}

	fn dst_reg(&self) -> &IrReg {
		&self.dst
	}

	fn dst_reg_mut(&mut self) -> &mut IrReg {
		&mut self.dst
	}

	fn args(&self) -> &[IrReg] {
		&self.args
	}

	fn args_mut(&mut self) -> &mut [IrReg] {
		&mut self.args
	}
}

impl Debug for IrPhi {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		write!(f, "{:?} = φ", self.dst)?;
		write!(f, "(")?;

		let mut args = self.args.iter();

		if let Some(arg) = args.next() {
			write!(f, "{:?}", arg)?;

			for arg in args {
				write!(f, ", {:?}", arg)?;
			}
		}

		write!(f, ")")
	}
}

// ------------------------------------------------------------------------------------------------
// IrBasicBlock
// ------------------------------------------------------------------------------------------------

pub(crate) type IrBBId = usize;

pub(crate) struct IrBasicBlock {
	id:        IrBBId,
	real_bbid: BBId,
	phis:      Vec<IrPhi>,
	insts:     Vec<IrInst>,
}

impl IrBasicBlock {
	pub(crate) fn new(id: IrBBId, real_bbid: BBId, insts: Vec<IrInst>) -> Self {
		Self {
			id,
			real_bbid,
			phis: vec![],
			insts,
		}
	}

	fn has_assignment_to(&self, reg: IrReg) -> bool {
		self.phis.iter().any(|p| p.assigns(reg)) ||
		self.insts.iter().any(|i| i.assigns(reg))
	}

	fn phis(&self) -> impl Iterator<Item = &IrPhi> {
		self.phis.iter()
	}

	fn phis_mut(&mut self) -> impl Iterator<Item = &mut IrPhi> {
		self.phis.iter_mut()
	}

	fn add_phi(&mut self, reg: IrReg, num_preds: usize) {
		self.phis.push(IrPhi::new(reg, num_preds));
	}

	fn phi_for_reg(&self, reg: &IrReg) -> Option<&IrPhi> {
		// TODO: this is linear time. is that a problem? (how many phi funcs are there likely
		// to be at the start of a BB?)
		// since phis execute conceptually in parallel, and since we need to look them up by
		// what reg they define, might make sense to use a map { reg => phi }.
		for phi in self.phis() {
			if phi.dst_reg() == reg {
				return Some(phi);
			}
		}

		None
	}

	fn retain_phis(&mut self, p: impl Fn(&IrReg) -> bool) {
		self.phis.retain(|phi| p(phi.dst_reg()))
	}

	fn insts(&self) -> impl Iterator<Item = &IrInst> {
		self.insts.iter()
	}

	fn insts_mut(&mut self) -> impl Iterator<Item = &mut IrInst> {
		self.insts.iter_mut()
	}
}

impl Debug for IrBasicBlock {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		writeln!(f, "bb{}: (real BB: {:?})", self.id, self.real_bbid)?;

		for p in self.phis.iter() {
			writeln!(f, "    {:?}", p)?;
		}

		if !self.phis.is_empty() {
			writeln!(f, "    ---")?;
		}

		for i in self.insts.iter() {
			writeln!(f, "    {:?}", i)?;
		}

		Ok(())
	}
}

// ------------------------------------------------------------------------------------------------
// IrFunction
// ------------------------------------------------------------------------------------------------

use petgraph::{
	// Direction,
	graphmap::{ DiGraphMap },
	// algo::dominators::{ self },
	dot::{ Dot, Config as DotConfig },
};

pub(crate) type IrCfg = DiGraphMap<IrBBId, ()>;

pub(crate) struct IrFunction {
	real_fid: FuncId,
	bbs:      Vec<IrBasicBlock>,
	cfg:      IrCfg,
}

impl IrFunction {
	pub(crate) fn new(real_fid: FuncId, mut bbs: Vec<IrBasicBlock>, cfg: IrCfg) -> Self {
		ssa::to_ssa(&mut bbs, &cfg);
		Self { real_fid, bbs, cfg, }
	}
}

impl Debug for IrFunction {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		writeln!(f, "-------------------------------------------------------")?;
		writeln!(f, "IR for {:?}", self.real_fid)?;
		writeln!(f, "\nCFG:\n")?;
		writeln!(f, "{:?}", Dot::with_config(&self.cfg, &[DotConfig::EdgeNoLabel]))?;

		for bb in &self.bbs {
			Debug::fmt(&bb, f)?;
		}

		Ok(())
	}
}