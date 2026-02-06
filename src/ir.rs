
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
		self.phis().find(|&phi| phi.dst_reg() == reg)
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
// IrRewrite
// ------------------------------------------------------------------------------------------------

#[derive(Debug, Copy, Clone)]
pub(crate) enum IrRewrite {
	Uses { before_last: bool },
	Returns,
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
	pub(crate) fn new(
		compiler: &impl IIrCompiler,
		rewrites: Vec<(IrBBId, IrRewrite)>,
		real_fid: FuncId,
		mut bbs: Vec<IrBasicBlock>,
		mut cfg: IrCfg
	) -> Self {
		perform_rewrites(compiler, rewrites, &mut bbs, &mut cfg);
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
		writeln!(f, "{:?}", DebugWorkaroundThing(&self.cfg, &self.bbs))
	}
}

// WOw!!!! look at all the dumb bullshit I have to do just to reuse one piece of code as both
// a Debug::fmt impl and a standalone function!!!!!!! AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
struct DebugWorkaroundThing<'aaaa>(&'aaaa IrCfg, &'aaaa [IrBasicBlock]);

impl<'aaaa> Debug for DebugWorkaroundThing<'aaaa> {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		let DebugWorkaroundThing(cfg, bbs) = *self;
		writeln!(f, "")?;
		writeln!(f, "CFG (NOTE!!!! numbers in \"a ->b \" are NOT NECESSARILY BB NUMBERS,")?;
		writeln!(f, "only trust the actual dot graph output or look at the successors")?;
		writeln!(f, "at the end of the BBs below):")?;
		writeln!(f, "")?;
		writeln!(f, "{:?}", Dot::with_config(cfg, &[DotConfig::EdgeNoLabel]))?;

		for bb in bbs {
			Debug::fmt(&bb, f)?;

			for target in cfg.edges(bb.id).map(|(_, n, _)|n) {
				writeln!(f, "    -> bb{}", target)?;
			}
		}

		Ok(())
	}
}

fn debug_dump_ir_cfg_and_bbs(cfg: &IrCfg, bbs: &[IrBasicBlock]) {
	log::debug!("{:?}", DebugWorkaroundThing(cfg, bbs));
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

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub(crate) enum ConstAddrKind {
	/// Load (read)
	Load,
	/// Store (write). The optional value is the constant value being stored, if known.
	Store(Option<u64>),
	/// Control flow target
	Target,
	/// Some other reference
	Offset,
}

impl ConstAddrKind {
	pub(crate) fn access(&self) -> MemAccess {
		use ConstAddrKind::*;
		match self {
			Load      => MemAccess::R,
			Store(..) => MemAccess::W,
			Target    => MemAccess::Target,
			Offset    => MemAccess::Offset,
		}
	}
}

pub(crate) struct ConstAddr {
	pub bbid:    BBId,
	pub ea:      EA,
	pub opn:     usize,
	pub addr:    EA, // may or may not be resolved!
	pub kind:    ConstAddrKind,
	pub srcs:    [Option<IrSrc>; 3],
}

impl ConstAddr {
	pub(crate) fn dump(&self) {
		let ConstAddr { bbid, ea, opn, addr, kind, srcs } = self;
		use ConstAddrKind::*;
		println!("{:?} in {:?} operand {} is a {} <from {:?}>",
			ea,
			bbid,
			opn,
			match kind {
				Load             => format!("load from {}", addr),
				Store(Some(val)) => format!("store of const value 0x{:08X} to {}", val, addr),
				Store(None)      => format!("store to {}", addr),
				Offset           => format!("reference to {}", addr),
				Target           => format!("control flow target to {}", addr),
			},
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

			match inst.kind() {
				IrInstKind::Branch  { target, targetn: opn } |
				IrInstKind::CBranch { target, targetn: opn, .. } |
				IrInstKind::Call    { target, targetn: opn } if opn >= 0 => {
					return Some(ConstAddr {
						bbid: self.func.bbs[self.bbidx].real_bbid,
						ea: inst.ea(),
						opn: opn as usize,
						addr: target,
						kind: ConstAddrKind::Target,
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

						let kind = match inst.kind() {
							IrInstKind::Load{ .. }  => ConstAddrKind::Load,
							IrInstKind::Store{ .. } => ConstAddrKind::Store(val),
							_                       => ConstAddrKind::Target,
						};

						return Some(ConstAddr {
							bbid: self.func.bbs[self.bbidx].real_bbid,
							ea: inst.ea(),
							opn: opn as usize,
							addr,
							kind,
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
// Rewrites
// ------------------------------------------------------------------------------------------------

fn perform_rewrites(
	compiler: &impl IIrCompiler,
	rewrites: Vec<(IrBBId, IrRewrite)>,
	bbs: &mut Vec<IrBasicBlock>,
	cfg: &mut IrCfg
) {
	let arg_regs = compiler.arg_regs();
	let ret_regs = compiler.return_regs();

	// log::debug!("-------------BEFORE REWRITE----------------");
	// debug_dump_ir_cfg_and_bbs(cfg, bbs);

	// first pass: insert uses
	for (irbbid, rewrite) in rewrites.iter() {
		if let IrRewrite::Uses { before_last } = rewrite {
			bbs[*irbbid].insert_dummy_uses(arg_regs, ret_regs, *before_last);
		}
	}

	let mut new_bbs = vec![];
	let mut new_bbid = bbs.len();

	// second pass: insert dummy BBs for return-uses after calls
	for (irbbid, rewrite) in rewrites.into_iter() {
		if matches!(rewrite, IrRewrite::Returns) {
			let bb = &mut bbs[irbbid];

			// first update the cfg.
			// println!("{}: {:?}", bb.id, cfg.edges(bb.id).map(|(_, n, _)|n).collect::<Vec<_>>());
			let targets = cfg.edges(bb.id).map(|(_, n, _)|n).collect::<Vec<_>>();

			let old_dest = match targets[..] {
				[] => panic!("IrWrite::Returns put on a BB with no in-function successor"),
				[target] => target, // ok cool beans
				[target1, target2] => {
					// this case can happen if a function is recursive, which is okay. but any
					// NON-recursive call would be an error.
					if target1 == 0 {
						target2
					} else if target2 == 0 {
						target1
					} else {
						panic!("IrWrite::Returns put on BB @ {} where one of the call targets \
							({}, {}) is self-call but NOT a recursive call. Why hasn't this \
							function been split?",
							bb.insts[0].ea(),
							target1, target2);
					}
				}
				_ => panic!("UHHHHHHHHHHHHHHHH TOO MANY EDGES"),
			};

			log::debug!("  changing bb{}'s dest from bb{} to bb{}", bb.id, old_dest, new_bbid);

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

	// log::debug!("-------------AFTER REWRITE----------------");
	// debug_dump_ir_cfg_and_bbs(cfg, bbs);
}

impl IrBasicBlock {
	fn insert_dummy_uses(&mut self, arg_regs: &[IrReg], ret_regs: &[IrReg], before_last: bool) {
		// safe because `func_to_ir` checks that every IR BB has at least 1 instruction.
		let (terminating_inst, _) = self.insts.split_last().unwrap();
		let terminating_inst = *terminating_inst;

		// the match is also valid because irbb_terminator_sanity_check ensured that any BB that
		// ends in IrInstKind::Ret really did come from a BB with BBTerm::Ret.
		let regs = match terminating_inst.kind() {
			IrInstKind::Ret { .. } => ret_regs,
			_                      => arg_regs,
		};

		if !regs.is_empty() {
			let ea = terminating_inst.ea();

			if before_last {
				self.insts.pop();
			}

			for &reg in regs.iter() {
				self.insts.push(IrInst::use_(ea, reg));
			}

			if before_last {
				self.insts.push(terminating_inst);
			}
		}
	}
}