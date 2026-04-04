
use std::fmt::{ Debug, Formatter, Result as FmtResult };
use std::collections::{ BTreeMap };

use crate::fxhash::{ FxHashMap as HashMap, FxHashMapEx };

use crate::dataflow::{ JoinSemiLattice, DataflowAlgorithm };

use super::*;

// ------------------------------------------------------------------------------------------------
// Submodules
// ------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests;

mod node;
mod transfer;
pub(crate) use node::*;
use transfer::*;

// ------------------------------------------------------------------------------------------------
// Public interface
// ------------------------------------------------------------------------------------------------

/// Results of constant propagation.
#[derive(Debug)]
pub(crate) struct ConstPropResults {
	regs:  BTreeMap<IrReg, ConstPropResult>,
	nodes: Nodes,
}

#[derive(Clone, Copy)]
pub(crate) struct ConstPropResult {
	/// The computed value for this register. If `is_multi`, this is only one possible value.
	pub(crate) val:      u64,
	/// The root of the node DAG which describes the computation which produced `val`.
	pub(crate) node:     NodeId,
	/// If false, this is the only possible constant value for this register; otherwise, `val` is
	/// one possibility, but not the only. Commonly this happens in loops where a register's value
	/// is known for the first iteration but not subsequent ones.
	pub(crate) is_multi: bool,
}

impl Debug for ConstPropResult {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		write!(f, "{:08X}{} <from {:?}>",
			self.val,
			if self.is_multi { " (and others)" } else { "" },
			self.node)
	}
}

impl ConstPropResults {
	pub(crate) fn get(&self, r: IrReg) -> Option<&ConstPropResult> {
		self.regs.get(&r)
	}

	pub(crate) fn regs(&self) -> impl Iterator<Item = (IrReg, &ConstPropResult)> {
		self.regs.iter().map(|(reg, res)| (*reg, res))
	}

	pub(crate) fn dump_node(&self, node: NodeId) {
		self.nodes.dump(node);
	}
}

/// Runs constant propagation on the given IR code and CFG.
pub(crate) fn propagate_constants(bbs: &[IrBasicBlock], cfg: &IrCfg) -> ConstPropResults {
	// since each variable is only assigned once, there's no need to track changing state -
	// the state of a variable is determined at its def.
	let mut prop = ConstProp::new(bbs);
	prop.run(cfg);
	prop.finish()
}

// ------------------------------------------------------------------------------------------------
// Info
// ------------------------------------------------------------------------------------------------

#[derive(Debug, Copy, Clone)]
enum Info {
	/// ??? dunno
	Unk,
	/// some known constant. `from` is the root of the AST tree that computed this value. if
	/// `is_multi`, this is only one of multiple possible constants, but all known values are
	/// constant.
	Some { val: u64, from: NodeId, is_multi: bool },
	/// could be anything (non-constant)
	Any,
}

// this manual impl is necessary because otherwise, it will take `Some { from }` into account
// which ruins termination.
impl std::cmp::PartialEq for Info {
	fn eq(&self, other: &Self) -> bool {
		use Info::*;
		match (self, other) {
			(Unk, Unk) => true,
			(Some { val: val1, is_multi: is_multi1, from: _ },
			Some { val: val2, is_multi: is_multi2, from: _ }) =>
				val1 == val2 && is_multi1 == is_multi2,
			(Any, Any) => true,
			_ => false,
		}
	}
}

impl std::cmp::Eq for Info {}

impl JoinSemiLattice for Info {
	fn join(&mut self, other: &Self) -> bool {
		use Info::*;

		//   ↪Unk ------------------+
		//     ↓                    ↓
		//  ↪Some(multi=false) --→ Any↩
		//     ↓                    ↑
		//  ↪Some(multi=true) ------+

		// log::trace!("    joining {:?} with {:?}", self, other);

		let new = match (&self, &other) {
			// Unk -> Unk
			(Unk, Unk) => Unk,

			// Unk -> Some
			// Unk -> Any
			(Unk, x @ Some { .. }) |
			(Unk, x @ Any)         => **x,
			(x @ Some { .. }, Unk) |
			(x @ Any, Unk)         => **x,

			// Some -> Any
			(Any, Some { .. }) |
			(Some { .. }, Any) => Any,

			// Some(false) -> Some(false)
			// Some(false) -> Some(true)
			(Some { val: a, from: from1, is_multi: false },
			Some { val: b, from: _from2, is_multi: false }) => {
				if a == b {
					// TODO: how DO we handle this? just pick from1 or from2 or merge them somehow?
					// have a phi node in the AST?
					Some { val: *a, from: *from1, is_multi: false }
				} else {
					// just pick the first one.
					Some { val: *a, from: *from1, is_multi: true }
				}
			}

			// Some(true) -> Some(true)
			(Some { val: a, from: from1, is_multi: false }, Some { is_multi: true, .. }) |
			(Some { val: a, from: from1, is_multi: true  }, Some { .. }) =>
				Some { val: *a, from: *from1, is_multi: true },

			// Any -> Any
			(Any, Any) => Any,
		};


		if *self != new {
			// log::trace!("      => {:?}", new);
			*self = new;
			true
		} else {
			false
		}
	}
}

// ------------------------------------------------------------------------------------------------
// ConstProp
// ------------------------------------------------------------------------------------------------

struct ConstPropState {
	regs:  BTreeMap<IrReg, Info>,
	nodes: Nodes,
}

struct ConstProp<'bb> {
	bbs:   &'bb [IrBasicBlock],
	state: ConstPropState,
}

impl<'bb> ConstProp<'bb> {
	fn new(bbs: &'bb [IrBasicBlock]) -> Self {
		Self {
			state: ConstPropState {
				regs: find_all_regs(bbs)
					.into_iter()
					.map(|r| (r, Info::Unk))
					.collect(),
				nodes: Nodes::new(),
			},
			bbs,
		}
	}

	fn simplify(&mut self) {
		let mut replacements: HashMap<NodeId, NodeId> = HashMap::new();

		use NodeKind::*;
		use IrUnOp::*;
		use IrBinOp::*;

		let nodes = &mut self.state.nodes.nodes;

		for n in nodes.iter() {
			// look for pair(hi(x), lo(x)) and replace with x
			if let Binary { op: Pair, src1, src2, .. } = n.kind {
				match (nodes[src1.0].kind, nodes[src2.0].kind) {
					(Unary { op: Hi, src: x, .. }, Unary { op: Lo, src: y, .. }) if x == y => {
						replacements.insert(n.id, x);
					}
					_ => {}
				}
			}
		}

		let repl = |src: &mut NodeId, srcn: Option<&mut i8>| {
			if let Some(replacement) = replacements.get(src) {
				*src = *replacement;

				if let Some(srcn) = srcn {
					*srcn = -1;
				}
			}
		};

		for n in nodes.iter_mut() {
			match &mut n.kind {
				Const { .. } => {},
				Unary { src, srcn, .. } => {
					repl(src, Some(srcn));
				}
				Binary { src1, src2, src1n, src2n, .. } => {
					repl(src1, Some(src1n));
					repl(src2, Some(src2n));
				}
				Ternary { src1, src2, src3, src1n, src2n, src3n, .. } => {
					repl(src1, Some(src1n));
					repl(src2, Some(src2n));
					repl(src3, Some(src3n));
				}
			}
		}

		for (_, mut info) in self.state.regs.iter_mut() {
			if let Info::Some { from, .. } = &mut info {
				repl(from, None);
			}
		}
	}

	fn finish(mut self) -> ConstPropResults {
		self.simplify();

		let regs = self.state.regs
			.into_iter()
			.filter_map(|(reg, info)|
				match info {
					Info::Unk | Info::Any => None,
					Info::Some { val, from, is_multi } => {
						// print!("{:>8} = 0x{:04X} @ {:?} from ",
						// 	format!("{:?}", reg), val, self.state.nodes.ea_of(from));
						// self.state.nodes.dump(from);
						Some((reg, ConstPropResult { val, node: from, is_multi }))
					}
				})
			.collect();

		ConstPropResults {
			regs,
			nodes: self.state.nodes,
		}
	}
}

impl<'bb> DataflowAlgorithm for ConstProp<'bb> {
	type ID = IrBBId;

	fn visit(&mut self, bbid: IrBBId) -> bool {
		let mut changed = false;

		for phi in self.bbs[bbid].phis() {
			changed |= phi_join(phi, &mut self.state);
		}

		for inst in self.bbs[bbid].insts() {
			changed |= transfer(inst, &mut self.state);
		}

		changed
	}
}

// ------------------------------------------------------------------------------------------------
// Phi join function
// ------------------------------------------------------------------------------------------------

fn phi_join(phi: &IrPhi, state: &mut ConstPropState) -> bool {
	let mut reg_state = state.regs[&phi.dst_reg()];
	let mut changed = false;

	// log::trace!("  const phi join {:?}", phi.dst_reg());

	for arg in phi.args() {
		changed |= reg_state.join(&state.regs[arg]);
	}

	state.regs.insert(phi.dst_reg(), reg_state);
	changed
}