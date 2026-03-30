
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

/// Results of constant propagation. It maps from IR Registers to a tuple of:
///
/// - The determined constant value for that register
/// - A list of up to 3 sources from which that constant was computed
///
/// The sources can be used to propagate information backwards, such as in cases
/// where a constant address is computed by combining two smaller pieces, and those
/// smaller pieces need to be marked as references to that address.
#[derive(Debug)]
pub(crate) struct ConstPropResults {
	regs:  BTreeMap<IrReg, (u64, NodeId)>,
	nodes: Nodes,
}

impl ConstPropResults {
	pub(crate) fn get(&self, r: &IrReg) -> Option<&(u64, NodeId)> {
		self.regs.get(r)
	}

	pub(crate) fn regs(&self) -> impl Iterator<Item = (&IrReg, &(u64, NodeId))> {
		self.regs.iter()
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
	/// some known constant; `from` is the root of the AST tree that computed this value
	Some { val: u64, from: NodeId },
	/// could be anything
	Any,
}

// this manual impl is necessary because otherwise, it will take `Some { from }` into account
// which ruins termination.
impl std::cmp::PartialEq for Info {
	fn eq(&self, other: &Self) -> bool {
		use Info::*;
		match (self, other) {
			(Unk, Unk) => true,
			(Some { val: val1, ..}, Some { val: val2, .. }) => val1 == val2,
			(Any, Any) => true,
			_ => false,
		}
	}
}

impl std::cmp::Eq for Info {}

impl JoinSemiLattice for Info {
	fn join(&mut self, other: &Self) -> bool {
		use Info::*;

		let new = match (&self, &other) {
			(Unk, x)                     => **x,
			(x, Unk)                     => **x,
			(Any, _) | (_, Any)          => Any,
			(Some { val: a, from: from1 }, Some { val: b, from: from2 }) if a == b => {
				// TODO: how DO we handle this? just pick from1 or from2 or merge them somehow?
				// have a phi node in the AST?
				Some { val: *a, from: *from1 }
			}
			_                            => Any,
		};

		if *self != new {
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
					Info::Some { val, from } => {
						// print!("{:>8} = 0x{:04X} @ {:?} from ",
						// 	format!("{:?}", reg), val, self.state.nodes.ea_of(from));
						// self.state.nodes.dump(from);
						Some((reg, (val, from)))
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
	let mut reg_state = state.regs[phi.dst_reg()];
	let mut changed = false;

	for arg in phi.args() {
		changed |= reg_state.join(&state.regs[arg]);
	}

	state.regs.insert(*phi.dst_reg(), reg_state);
	changed
}