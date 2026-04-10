
use std::fmt::{ Debug, Formatter, Result as FmtResult };

use lazycell::{ LazyCell };

use petgraph::{
	graphmap::{ DiGraphMap },
	dot::{ Dot, Config as DotConfig },
	visit::{ DfsPostOrder },
};

use crate::arch::{ Architecture };
use crate::dataflow::{ DataflowCfg };
use crate::memory::{ EA };
use crate::program::{ BBId, FuncId };

// ------------------------------------------------------------------------------------------------
// Sub-modules
// ------------------------------------------------------------------------------------------------

pub mod builder;
pub mod constaddrs;
pub mod inst;
pub mod ssa;
pub mod constprop;
pub mod defuse;
pub mod dom;
pub mod dse;

pub(crate) use builder::*;
pub(crate) use constaddrs::*;
pub(crate) use inst::*;
pub(crate) use ssa::*;
pub(crate) use constprop::*;
pub(crate) use defuse::*;
pub(crate) use dom::*;
use dse::*;

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
	offset: u8,
	gen_:   Option<u32>
}

impl Debug for IrReg {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		self.debug_fmt(f, None)
	}
}

/// Integer type which can be used as a bitset to represent registers.
pub(crate) type IrRegSetType = u64;

impl IrReg {
	/// Number of valid registers.
	pub(crate) const MAX_NUM: usize = IrRegSetType::BITS as usize;
	/// Maximum valid IR register offset.
	pub(crate) const MAX: u8 = Self::MAX_NUM as u8 - 1;

	fn debug_fmt(&self, f: &mut Formatter, arch: Option<&Architecture>) -> FmtResult {
		// have to do it like this for borrowing reasons
		match arch {
			Some(arch) => self.debug_fmt_name(f, arch.reg_name(self.offset)),
			None       => self.debug_fmt_name(f, &format!("r{}", self.offset)),
		}
	}

	fn debug_fmt_name(&self, f: &mut Formatter, name: &str) -> FmtResult {
		if let Some(gen_) = self.gen_ {
			write!(f, "{}_{}.{}", name, gen_, self.size.name())
		} else {
			write!(f, "{}.{}", name, self.size.name())
		}
	}

	/// Constructs a register. Panics if `offset` > `IrReg::MAX`.
	pub(crate) const fn new(size: ValSize, offset: u8) -> Self {
		assert!(offset <= Self::MAX, "cannot have an IR register with an offset > 63");
		Self { size, offset, gen_: None }
	}

	/// Constructs an 8-bit register. Panics if `offset` > `IrReg::MAX`.
	pub(crate) const fn reg8(offset: u8) -> Self {
		Self::new(ValSize::_8, offset)
	}

	/// Constructs a 16-bit register. Panics if `offset` > `IrReg::MAX`.
	pub(crate) const fn reg16(offset: u8) -> Self {
		Self::new(ValSize::_16, offset)
	}

	/// Constructs a 32-bit register. Panics if `offset` > `IrReg::MAX`.
	pub(crate) const fn reg32(offset: u8) -> Self {
		Self::new(ValSize::_32, offset)
	}

	/// Constructs a 64-bit register. Panics if `offset` > `IrReg::MAX`.
	pub(crate) const fn reg64(offset: u8) -> Self {
		Self::new(ValSize::_64, offset)
	}

	/// The size of this register.
	#[inline]
	pub(crate) fn size(&self) -> ValSize {
		self.size
	}

	/// Its offset into the registers "segment."
	#[inline]
	pub(crate) fn offset(&self) -> u8 {
		self.offset
	}

	/// True if this register has been given an SSA generation.
	fn is_ssa(&self) -> bool {
		self.gen_.is_some()
	}

	/// True if this register is an SSA register with generation 0.
	pub(crate) fn is_gen0(&self) -> bool {
		assert!(self.gen_.is_some(), ".is_gen0() called on non-SSA reg '{:?}'", self);
		self.gen_ == Some(0)
	}

	/// If this is not an SSA register, returns a new `IrReg` subscripted with the given index.
	/// Panics if this is already an SSA register.
	pub(crate) fn sub(&self, i: u32) -> Self {
		assert!(self.gen_.is_none(), ".sub() called on '{:?}'", self);
		Self {
			gen_: Some(i),
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
			ValSize::_8  => write!(f, "#0x{:02X}", self.val),
			ValSize::_16 => write!(f, "#0x{:04X}", self.val),
			ValSize::_32 => write!(f, "#0x{:08X}", self.val),
			ValSize::_64 => write!(f, "#0x{:016X}", self.val),
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

	/// Constructs a constant with a given size.
	pub(crate) const fn with_size(size: ValSize, val: u64) -> Self {
		Self { size, val }
	}

	/// Get the value.
	pub(crate) fn val(&self) -> u64 {
		self.val
	}

	/// Get the size.
	pub(crate) fn size(&self) -> ValSize {
		self.size
	}
}

// ------------------------------------------------------------------------------------------------
// IrSrc
// ------------------------------------------------------------------------------------------------

/// The source of a value. Can be an [`IrReg`], an [`IrConst`], or a special value indicating a
/// return value from a function call.
#[derive(PartialEq, Eq, Clone, Copy)]
pub(crate) enum IrSrc {
	Reg(IrReg),
	Const(IrConst),
	Return(ValSize),
}

impl Debug for IrSrc {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		self.debug_fmt(f, None)
	}
}

impl IrSrc {
	fn debug_fmt(&self, f: &mut Formatter, arch: Option<&Architecture>) -> FmtResult {
		match self {
			IrSrc::Reg(r)    => r.debug_fmt(f, arch),
			IrSrc::Const(c)  => write!(f, "{:?}", c),
			IrSrc::Return(s) => write!(f, "<return.{}>", s.name()),
		}
	}

	pub(crate) fn ret(reg: IrReg) -> Self {
		IrSrc::Return(reg.size())
	}

	/// The size of this value.
	#[inline]
	pub(crate) fn size(&self) -> ValSize {
		match self {
			IrSrc::Reg(IrReg { size, .. }) |
			IrSrc::Const(IrConst { size, .. }) |
			IrSrc::Return(size) => *size,
		}
	}

	/// Callback iterator over regs (well, reg) represented by this source.
	pub(crate) fn regs(&self, f: &mut impl FnMut(IrReg)) {
		if let IrSrc::Reg(r) = self {
			f(*r);
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
	fn new(reg: IrReg, num_args: usize, is_entrypoint: bool) -> Self {
		assert!(!reg.is_ssa());

		let args = if is_entrypoint {
			assert!(num_args > 0);
			let mut args = vec![reg; num_args + 1];
			args[num_args] = reg.sub(0);
			args
		} else {
			assert!(num_args > 1);
			vec![reg; num_args]
		};

		Self {
			dst:  reg,
			args,
		}
	}

	fn assigns(&self, reg: IrReg) -> bool {
		self.dst == reg
	}

	fn dst_reg(&self) -> IrReg {
		self.dst
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

	fn debug_fmt(&self, f: &mut Formatter, arch: Option<&Architecture>) -> FmtResult {
		self.dst.debug_fmt(f, arch)?;
		write!(f, " = φ(")?;

		let mut args = self.args.iter();

		if let Some(arg) = args.next() {
			arg.debug_fmt(f, arch)?;

			for arg in args {
				write!(f, ", ")?;
				arg.debug_fmt(f, arch)?;
			}
		}

		write!(f, ")")
	}
}

impl Debug for IrPhi {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		self.debug_fmt(f, None)
	}
}

// ------------------------------------------------------------------------------------------------
// IrTarget
// ------------------------------------------------------------------------------------------------

/// Possible targets for an IR control flow instruction
#[derive(PartialEq, Eq, Clone, Copy)]
pub(crate) enum IrTarget {
	/// Inside the function
	Internal(IrBBId),
	/// Outside the function
	External(EA),
}

impl From<IrBBId> for IrTarget {
	fn from(other: IrBBId) -> Self {
		Self::Internal(other)
	}
}

impl From<EA> for IrTarget {
	fn from(other: EA) -> Self {
		Self::External(other)
	}
}

impl Debug for IrTarget {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		match self {
			IrTarget::Internal(bbid) => write!(f, "bb{}", bbid),
			IrTarget::External(ea)   => write!(f, "{}", ea),
		}
	}
}

// ------------------------------------------------------------------------------------------------
// IrBasicBlock
// ------------------------------------------------------------------------------------------------

pub(crate) type IrBBId = usize;

pub(crate) struct IrBasicBlock {
	pub(crate) id:        IrBBId,
	pub(crate) real_bbid: BBId,
	pub(crate) ea:        EA,
	pub(crate) insts:     Vec<IrInst>,

	phis:                 Vec<IrPhi>,
}

impl IrBasicBlock {
	pub(crate) fn new(id: IrBBId, real_bbid: BBId, ea: EA, insts: Vec<IrInst>) -> Self {
		assert!(!insts.is_empty());
		Self {
			id,
			real_bbid,
			ea,
			insts,
			phis: vec![],
		}
	}

	fn has_assignment_to(&self, reg: IrReg) -> bool {
		self.phis.iter().any(|p| p.assigns(reg)) ||
		self.insts.iter().any(|i| i.assigns(reg))
	}

	fn get_phi(&self, i: usize) -> &IrPhi {
		&self.phis[i]
	}

	fn phis(&self) -> impl Iterator<Item = &IrPhi> {
		self.phis.iter()
	}

	fn phis_mut(&mut self) -> impl Iterator<Item = &mut IrPhi> {
		self.phis.iter_mut()
	}

	fn add_phi(&mut self, reg: IrReg, num_preds: usize, is_entrypoint: bool) {
		self.phis.push(IrPhi::new(reg, num_preds, is_entrypoint));
	}

	fn phi_for_reg(&self, reg: IrReg) -> Option<&IrPhi> {
		// TODO: this is linear time. is that a problem? (how many phi funcs are there likely
		// to be at the start of a BB?)
		// since phis execute conceptually in parallel, and since we need to look them up by
		// what reg they define, might make sense to use a map { reg => phi }.
		self.phis().find(|&phi| phi.dst_reg() == reg)
	}

	fn retain_phis(&mut self, p: impl Fn(IrReg) -> bool) {
		self.phis.retain(|phi| p(phi.dst_reg()))
	}

	fn insts(&self) -> impl Iterator<Item = &IrInst> {
		self.insts.iter()
	}

	fn insts_mut(&mut self) -> impl Iterator<Item = &mut IrInst> {
		self.insts.iter_mut()
	}

	/// The terminating instruction.
	pub(crate) fn term_inst(&self) -> &IrInst {
		self.insts.last().unwrap()
	}

	/// Same as above but mutable.
	pub(crate) fn term_inst_mut(&mut self) -> &mut IrInst {
		self.insts.last_mut().unwrap()
	}

	/// Iterator over all registers used by uses (`IrInstKind::Use`) in this BB.
	pub(crate) fn use_regs(&self) -> impl Iterator<Item = IrReg> {
		self.insts.iter().filter_map(IrInst::use_reg)
	}

	/// Iterator over all registers used by clobbers (`IrInstKind::Clobber`) in this BB.
	pub(crate) fn clobber_regs(&self) -> impl Iterator<Item = IrReg> {
		self.insts.iter().filter_map(IrInst::clobber_reg)
	}

	/// Iterator over all registers def'd by `mov _, <return>` in this BB.
	pub(crate) fn return_use_regs(&self) -> impl Iterator<Item = IrReg> {
		self.insts.iter().filter_map(IrInst::return_use_reg)
	}

	fn debug_fmt(&self, f: &mut Formatter, arch: Option<&Architecture>) -> FmtResult {
		writeln!(f, "bb{}: (real BB: {:?})", self.id, self.real_bbid)?;

		for p in self.phis.iter() {
			write!(f, "    ")?;
			p.debug_fmt(f, arch)?;
			writeln!(f)?;
		}

		if !self.phis.is_empty() {
			writeln!(f, "    ---")?;
		}

		for i in self.insts.iter() {
			write!(f, "    ")?;
			i.debug_fmt(f, arch)?;
			writeln!(f)?;
		}

		Ok(())
	}
}

impl Debug for IrBasicBlock {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		self.debug_fmt(f, None)
	}
}

// ------------------------------------------------------------------------------------------------
// IrCfg
// ------------------------------------------------------------------------------------------------

pub(crate) type IrCfg = DiGraphMap<IrBBId, ()>;

impl DataflowCfg<IrBBId> for IrCfg {
	fn num_nodes(&self) -> usize {
		self.node_count()
	}

	fn initial_order(&self) -> impl Iterator<Item = IrBBId> {
		let mut rpo = Vec::<IrBBId>::with_capacity(self.num_nodes());
		let mut postorder = DfsPostOrder::new(self, 0);
		while let Some(id) = postorder.next(self) {
			rpo.push(id);
		}

		rpo.into_iter().rev()
	}

	fn successors(&self, id: IrBBId) -> impl Iterator<Item = IrBBId> {
		self.edges(id).map(|(_, succ, _)| succ)
	}
}

// ------------------------------------------------------------------------------------------------
// IrFunction
// ------------------------------------------------------------------------------------------------

pub(crate) struct IrFunction {
	real_fid:    FuncId,
	bbs:         Vec<IrBasicBlock>,
	entrypoints: Vec<IrBBId>,
	exitpoints:  Vec<IrBBId>,
	cfg:         IrCfg,

	// TODO: any time the bbs/cfg are modified, this needs to be invalidated...
	consts:      LazyCell<ConstPropResults>,
}

impl IrFunction {
	pub(crate) fn new(
		real_fid: FuncId,
		mut bbs: Vec<IrBasicBlock>,
		cfg: IrCfg,
		entrypoints: Vec<IrBBId>,
		exitpoints: Vec<IrBBId>,
	) -> Self {
		ssa::to_ssa(&mut bbs, &cfg, &entrypoints);
		Self {
			real_fid,
			bbs,
			entrypoints,
			exitpoints,
			cfg,
			consts: LazyCell::new(),
		}
	}

	/// Lazily performs constant propagation, and returns a map from SSA registers to their
	/// determined constant values. If a register is not in the map, no constant value was able
	/// to be determined for it.
	pub(crate) fn constants(&self) -> &ConstPropResults {
		if !self.consts.filled() {
			let consts = propagate_constants(&self.bbs, &self.cfg);
			self.consts.fill(consts).unwrap();
		}

		self.consts.borrow().unwrap()
	}

	/// Perform def-use analysis on this function.
	pub(crate) fn find_defs_and_uses(&self) -> DefMap {
		find_defs_and_uses(&self.bbs)
	}

	/// A list of exitpoints (BBs where control flow leaves the function). Each exitpoint may have
	/// `clobber` instructions before the terminator which encode the registers in use at that
	/// exit, and possibly `use` instructions which encode the arguments to the function to which
	/// control is being transferred (if this is a tailcall/tailbranch/fallthrough).
	pub(crate) fn exitpoints(&self) -> &[IrBBId] {
		&self.exitpoints
	}

	/// Get the number of basic blocks.
	pub(crate) fn num_bbs(&self) -> usize {
		self.bbs.len()
	}

	/// Get the basic block with the given id.
	pub(crate) fn get_bb(&self, id: IrBBId) -> &IrBasicBlock {
		&self.bbs[id]
	}

	/// Eliminate any dead stores from the IR.
	pub(crate) fn elim_dead_stores(&mut self) {
		// TODO: invalidate self.consts
		elim_dead_stores(&mut self.bbs);
	}

	fn debug_fmt(&self, f: &mut Formatter, arch: Option<&Architecture>) -> FmtResult {
		writeln!(f, "-------------------------------------------------------")?;
		writeln!(f, "IR for {:?}", self.real_fid)?;
		writeln!(f, "Entrypoint BBs: {:?}", self.entrypoints)?;
		writeln!(f, "Exitpoint BBs: {:?}", self.exitpoints)?;
		writeln!(f, "{:?}", DebugWorkaroundThing(&self.cfg, &self.bbs, arch))
	}
}

impl Debug for IrFunction {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		self.debug_fmt(f, None)
	}
}

pub(crate) struct IrFunctionWithNames<'f, 'a>(pub &'f IrFunction, pub &'a Architecture);

impl<'f, 'a> Debug for IrFunctionWithNames<'f, 'a> {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		self.0.debug_fmt(f, Some(self.1))
	}
}

struct DebugWorkaroundThing<'a, 'b>(&'a IrCfg, &'a [IrBasicBlock], Option<&'b Architecture>);

impl<'a, 'b> Debug for DebugWorkaroundThing<'a, 'b> {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		let DebugWorkaroundThing(cfg, bbs, arch) = *self;
		writeln!(f)?;
		writeln!(f, "CFG (NOTE!!!! numbers in \"a -> b\" are NOT NECESSARILY BB NUMBERS,")?;
		writeln!(f, "only trust the actual dot graph output or look at the successors")?;
		writeln!(f, "at the end of the BBs below):")?;
		writeln!(f)?;
		writeln!(f, "{:?}", Dot::with_config(cfg, &[DotConfig::EdgeNoLabel]))?;

		for bb in bbs {
			bb.debug_fmt(f, arch)?;

			for dst in cfg.edges(bb.id).map(|(_, n, _)|n) {
				writeln!(f, "    -> bb{}", dst)?;
			}
		}

		Ok(())
	}
}

pub(crate) fn debug_dump_ir_cfg_and_bbs(cfg: &IrCfg, bbs: &[IrBasicBlock]) {
	log::debug!("{:?}", DebugWorkaroundThing(cfg, bbs, None));
}