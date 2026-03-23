
use crate::{ EA, MemAccess, BBId, IrTarget, IrInst, IrFunction, IrSrc, IrInstKind, IrConst,
	ConstPropResults };

// ------------------------------------------------------------------------------------------------
// Finding loads/stores with constant addresses
// ------------------------------------------------------------------------------------------------

impl IrFunction {
	/// Returns an iterator over all constant addresses used in a function.
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
	/// Control flow dst
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
				Target           => format!("control flow dst to {}", addr),
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
				IrInstKind::Branch  { dst, dstn: opn } |
				IrInstKind::CBranch { dst, dstn: opn, .. } |
				IrInstKind::Call    { dst, dstn: opn, .. } if opn >= 0 => {
					return Some(ConstAddr {
						bbid: self.func.bbs[self.bbidx].real_bbid,
						ea: inst.ea(),
						opn: opn as usize,
						addr: match dst {
							IrTarget::Internal(irbbid) => self.func.bbs[irbbid].ea,
							IrTarget::External(ea) => ea,
						},
						kind: ConstAddrKind::Target,
						srcs: [None, None, None],
					});
				}

				IrInstKind::Load    { addr, addrn: opn, .. } |
				IrInstKind::Store   { addr, addrn: opn, .. } |
				IrInstKind::IBranch { dst: addr, dstn: opn, .. } |
				IrInstKind::ICall   { dst: addr, dstn: opn, .. }  if opn >= 0 => {
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
							ea:   inst.ea(),
							opn:  opn as usize,
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