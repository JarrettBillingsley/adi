
use std::fmt::{ Debug, Formatter, Result as FmtResult };

use lazycell::LazyCell;

use crate::arch::{ IIrCompiler };
use crate::memory::{ EA, MemAccess };
use crate::program::{ BBId, FuncId };

// ------------------------------------------------------------------------------------------------
// Sub-modules
// ------------------------------------------------------------------------------------------------

pub mod builder;
pub mod inst;
pub mod ssa;
pub mod constprop;
pub mod defuse;
pub mod dom;
pub mod dse;

pub(crate) use builder::*;
pub(crate) use inst::*;
pub(crate) use ssa::*;
pub(crate) use constprop::*;
pub(crate) use defuse::*;
pub(crate) use dom::*;
pub(crate) use dse::*;

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
		match self {
			IrSrc::Reg(p)    => write!(f, "{:?}", p),
			IrSrc::Const(c)  => write!(f, "{:?}", c),
			IrSrc::Return(s) => write!(f, "<return.{}>", s.name()),
		}
	}
}

impl IrSrc {
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

	// TODO: any time the bbs/cfg are modified, this needs to be invalidated...
	consts:   LazyCell<ConstPropResults>,
}

impl IrFunction {
	pub(crate) fn new(compiler: &impl IIrCompiler, real_fid: FuncId, mut bbs: Vec<IrBasicBlock>,
	mut cfg: IrCfg) -> Self {
		rewrite_calls_and_rets(compiler, &mut bbs, &mut cfg);
		ssa::to_ssa(&mut bbs, &cfg);
		Self {
			real_fid,
			bbs,
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

	/// Eliminate any dead stores from the IR.
	pub(crate) fn elim_dead_stores(&mut self) {
		elim_dead_stores(&mut self.bbs);
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

// ------------------------------------------------------------------------------------------------
// Finding loads/stores with constant addresses
// ------------------------------------------------------------------------------------------------

impl IrFunction {
	/// Returns an iterator over all
	pub(crate) fn const_addrs(&self) -> ConstAddrsIter<'_> {
		ConstAddrsIter {
			bbidx:   0,
			instidx: 0,
			consts:  self.constants(),
			func:    self,
		}
	}
}

pub(crate) struct ConstAddrsIter<'func> {
	bbidx:   usize,
	instidx: usize,
	consts:  &'func ConstPropResults,
	func:    &'func IrFunction,
}

pub(crate) struct ConstAddr {
	pub bbid:    BBId,
	pub ea:      EA,
	pub opn:     usize,
	pub addr:    EA, // may or may not be resolved!
	pub val:     Option<u64>,
	pub access:  MemAccess,
	pub srcs:    [Option<IrSrc>; 3],
}

impl ConstAddr {
	pub(crate) fn dump(&self) {
		let ConstAddr { bbid, ea, opn, addr, val, access, srcs } = self;
		println!("{:?} in {:?} operand {} is a {} {} <from {:?}>",
			ea,
			bbid,
			opn,
			match access {
				MemAccess::R => "load from".into(),
				MemAccess::W => {
					if let Some(val) = val {
						format!("store of constant value 0x{:08X} to", val)
					} else {
						"store to".into()
					}
				},
				MemAccess::Offset => "reference to".into(),
				MemAccess::Target => "control flow target to".into(),
				_ => unreachable!("const_addrs returned something bad"),
			},
			addr,
			srcs);
	}
}

impl<'func> std::iter::Iterator for ConstAddrsIter<'func> {
	type Item = ConstAddr;

	fn next(&mut self) -> Option<Self::Item> {
		while let Some(inst) = self.next_instruction() {
			// if this is a store, is it *also* storing a constant value?
			let val = match inst.kind() {
				IrInstKind::Store { src, .. } => {
					match src {
						IrSrc::Const(IrConst { val, .. }) => Some(val),
						IrSrc::Reg(r) => self.consts.get(&r).map(|(val, _)| *val),
						_ => None,
					}
				},
				_ => None,
			};

			// it's not necessarily Offset, but the match below will only let through
			// instructions it has deemed "memory-accessing".
			let access = inst.mem_access().unwrap_or(MemAccess::Offset);

			match inst.kind() {
				IrInstKind::Branch  { target, targetn: opn } |
				IrInstKind::CBranch { target, targetn: opn, .. } |
				IrInstKind::Call    { target, targetn: opn } if opn >= 0 => {
					return Some(ConstAddr {
						bbid: self.func.bbs[self.bbidx].real_bbid,
						ea: inst.ea(),
						opn: opn as usize,
						addr: target,
						val,
						access,
						srcs: [None, None, None],
					});
				}

				IrInstKind::Load    { addr, addrn: opn, .. } |
				IrInstKind::Store   { addr, addrn: opn, .. } |
				IrInstKind::IBranch { target: addr, targetn: opn, .. } |
				IrInstKind::ICall   { target: addr, targetn: opn, .. }  if opn >= 0 => {
					let addr = match addr {
						IrSrc::Const(IrConst { val, .. }) => Some((val, [None, None, None])),
						IrSrc::Reg(r)                     => self.consts.get(&r).copied(),
						_                                 => None,
					};


					if let Some((addr, srcs)) = addr {
						let addr = EA::unresolved(addr as usize);

						return Some(ConstAddr {
							bbid: self.func.bbs[self.bbidx].real_bbid,
							ea: inst.ea(),
							opn: opn as usize,
							addr,
							val,
							access,
							srcs,
						});
					}
				}

				_ => {}
			}
		}

		None
	}
}

impl<'func> ConstAddrsIter<'func> {
	fn next_instruction(&mut self) -> Option<&'func IrInst> {
		while self.bbidx < self.func.bbs.len() {
			let insts = &self.func.bbs[self.bbidx].insts;

			if self.instidx < insts.len() {
				let ret = Some(&insts[self.instidx]);
				self.instidx += 1;
				return ret;
			}

			self.bbidx += 1;
			self.instidx = 0;
		}

		None
	}
}

// ------------------------------------------------------------------------------------------------
// Call/return rewriting
// ------------------------------------------------------------------------------------------------

/// Rewrites each call instruction in two ways:
/// 1. before, insert 'use' instructions to mark all argument registers as being used
///    as arguments to the call.
/// 2. after, insert a new dummy BB that assigns a special "return" value to each
///    return register.
/// Also inserts 'use' instructions before each return to mark all return registers as used.
fn rewrite_calls_and_rets(compiler: &impl IIrCompiler, bbs: &mut Vec<IrBasicBlock>,
cfg: &mut IrCfg) {
	// TODO: what about tailcalls/tailbranches? aaa...
	let arg_regs = compiler.arg_regs();
	let ret_regs = compiler.return_regs();

	let mut new_bbs = vec![];
	let mut new_bbid = bbs.len();

	for bb in bbs.iter_mut() {
		if bb.rewrite_call_or_ret(arg_regs, ret_regs) {
			// it was a call; we have to make a new dummy bb!

			// first update the cfg.
			let mut edges_iter = cfg.edges(bb.id);
			let old_dest = edges_iter.next().unwrap().1;
			assert!(edges_iter.next().is_none(), "more than 1 edge coming out of call bb??");

			assert!(cfg.remove_edge(bb.id, old_dest).is_some());
			cfg.add_edge(bb.id, new_bbid, ());
			cfg.add_edge(new_bbid, old_dest, ());

			// now make the dummy BB.
			// this unwrap is safe, because the rewrite_call_or_ret ensures it has at least 1 inst.
			let ea = bb.insts.last().unwrap().ea();

			let mut b = IrBuilder::new();

			for &reg in ret_regs.iter() {
				b.assign(ea, reg, IrSrc::Return(reg.size()), -1, -1);
			}

			let new_bb = IrBasicBlock::new(new_bbid, bb.real_bbid, b.finish());
			new_bbid += 1;
			new_bbs.push(new_bb);
		}
	}

	bbs.append(&mut new_bbs);
}

impl IrBasicBlock {
	/// Inserts dummy uses of the arg/return regs before call and return instructions.
	/// Returns true if a call was rewritten, so the caller can then insert the dummy BB.
	fn rewrite_call_or_ret(&mut self, arg_regs: &[IrReg], ret_regs: &[IrReg]) -> bool {
		if self.insts.is_empty() {
			return false;
		}

		// unwraps below are safe because of the above check.
		let (is_call, regs) = match self.insts.last().unwrap().kind() {
			IrInstKind::Call { .. } |
			IrInstKind::ICall { .. } => (true, Some(arg_regs)),
			IrInstKind::Ret { .. }   => (false, Some(ret_regs)),
			_                        => (false, None),
		};

		if let Some(regs) = regs {
			if regs.len() > 0 {
				let old_inst = self.insts.pop().unwrap();
				let ea = old_inst.ea();

				for &reg in regs.iter() {
					self.insts.push(IrInst::use_(ea, reg));
				}

				self.insts.push(old_inst);
			}
		}

		is_call
	}
}