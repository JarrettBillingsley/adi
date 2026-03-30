
use std::collections::{ BTreeMap };

use crate::fxhash::{ FxHashMap as HashMap, FxHashMapEx };

use crate::dataflow::{ JoinSemiLattice, DataflowAlgorithm };

use super::*;

// ------------------------------------------------------------------------------------------------
// Submodules
// ------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests;

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
// AST types
// ------------------------------------------------------------------------------------------------

/// Unique node identifier within this IrFunction.
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub(crate) struct NodeId(usize);

/// Kinds of AST nodes.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub(crate) enum NodeKind {
	Const   { c:  IrConst,  cn: i8 },
	Unary   { op: IrUnOp,   src:  NodeId, srcn:  i8 },
	Binary  { op: IrBinOp,  src1: NodeId, src2: NodeId, src1n: i8, src2n: i8 },
	Ternary { op: IrTernOp, src1: NodeId, src2: NodeId, src3: NodeId,
		src1n: i8, src2n: i8, src3n: i8 },
	// TODO: phi nodes?
}

/// A single AST node, associated with an instruction, which contains both the computed constant
/// value and the kind of AST node it is (which can refer to other AST nodes).
///
/// A constant node identifies which instruction (`ea`) and which operand of that instruction
/// (`NodeKind::Const { cn }`) was the source of a constant. Everything else is just fluff I guess?
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub(crate) struct Node {
	pub(crate) id:   NodeId,
	pub(crate) ea:   EA,
	pub(crate) kind: NodeKind,
}

impl Node {
	fn new(id: NodeId, ea: EA, kind: NodeKind) -> Self {
		Self { id, ea, kind }
	}
}

/// A collection of AST nodes associated with a single IrFunction.
#[derive(Debug)]
struct Nodes {
	nodes: Vec<Node>,
}

impl Nodes {
	fn new() -> Self {
		Self { nodes: vec![] }
	}

	fn new_node(&mut self, ea: EA, kind: NodeKind) -> NodeId {
		let id = NodeId(self.nodes.len());
		self.nodes.push(Node::new(id, ea, kind));
		id
	}

	fn new_const(&mut self, ea: EA, c: IrConst, cn: i8) -> Info {
		Info::Some {
			val:  c.val(),
			from: self.new_node(ea, NodeKind::Const { c, cn })
		}
	}

	fn new_unary(&mut self, ea: EA, val: u64, op: IrUnOp,
		src: NodeId, srcn: i8) -> Info {
		Info::Some {
			val,
			from: self.new_node(ea, NodeKind::Unary { op, src, srcn })
		}
	}

	fn new_binary(&mut self, ea: EA, val: u64, op: IrBinOp,
		src1: NodeId, src2: NodeId, src1n: i8, src2n: i8) -> Info {
		Info::Some {
			val,
			from: self.new_node(ea, NodeKind::Binary { op, src1, src2, src1n, src2n })
		}
	}

	fn new_ternary(&mut self, ea: EA, val: u64, op: IrTernOp,
		src1: NodeId, src2: NodeId, src3: NodeId, src1n: i8, src2n: i8, src3n: i8) -> Info {
		Info::Some {
			val,
			from: self.new_node(ea, NodeKind::Ternary {
				op, src1, src2, src3, src1n, src2n, src3n })
		}
	}

	fn ea_of(&self, node: NodeId) -> EA {
		self.nodes[node.0].ea
	}

	pub(crate) fn dump(&self, root: NodeId) {
		// self.dump_rec(Indent(0), root);
		self.dump_arrows(root);
		println!();
	}

	fn dump_arrows(&self, node: NodeId) {
		use NodeKind::*;
		let n = &self.nodes[node.0];

		if let Const { c, cn } = n.kind {
			println!("{0} [ label=\"(id = {0})\\n{1}\\n{2} {3:?} {4:?}\"]",
				node.0, n.ea, self.opstr(n), Opn(cn), c);
		} else {
			println!("{0} [ label=\"(id = {0})\\n{1}\\n{2}\"]", node.0, n.ea, self.opstr(n));

			match n.kind {
				Const { .. } => {},
				Unary { src, .. } => {
					println!("{} -> {}", node.0, src.0);
					self.dump_arrows(src);
				}
				Binary { src1, src2, .. } => {
					println!("{} -> {}", node.0, src1.0);
					println!("{} -> {}", node.0, src2.0);
					self.dump_arrows(src1);
					self.dump_arrows(src2);
				}
				Ternary { src1, src2, src3, .. } => {
					println!("{} -> {}", node.0, src1.0);
					println!("{} -> {}", node.0, src2.0);
					println!("{} -> {}", node.0, src3.0);
					self.dump_arrows(src1);
					self.dump_arrows(src2);
					self.dump_arrows(src3);
				}
			}
		}
	}

	fn dump_rec(&self, indent: Indent, node: NodeId) {
		let node = &self.nodes[node.0];

		// 0000:00000000 name
		print!("{} (id = {:<3}) {}", node.ea, node.id.0, self.opstr(node));

		use NodeKind::*;
		match node.kind {
			Const   { c, cn } => print!(" {:?} {:?}", Opn(cn), c),
			Unary   { op: _, src, srcn } => {
				let indent = indent + 1;
				print!("\n{}{:?}", indent, Opn(srcn));
				self.dump_rec(indent, src);
			}
			Binary  { op: _, src1, src2, src1n, src2n } => {
				let indent = indent + 1;
				print!("\n{}{:?}", indent, Opn(src1n));
				self.dump_rec(indent, src1);
				print!("\n{}{:?}", indent, Opn(src2n));
				self.dump_rec(indent, src2);
			}
			Ternary { op: _, src1, src2, src3, src1n, src2n, src3n } => {
				let indent = indent + 1;
				print!("\n{}{:?}", indent, Opn(src1n));
				self.dump_rec(indent, src1);
				print!("\n{}{:?}", indent, Opn(src2n));
				self.dump_rec(indent, src2);
				print!("\n{}{:?}", indent, Opn(src3n));
				self.dump_rec(indent, src3);
			}
		}
	}

	fn opstr(&self, node: &Node) -> &'static str {
		match node.kind {
			NodeKind::Const   { .. }     => "const",
			NodeKind::Unary   { op, .. } => op.name(),
			NodeKind::Binary  { op, .. } => op.name(),
			NodeKind::Ternary { op, .. } => op.name(),
		}
	}
}

// helper type for printing out operand numbers more easily
#[derive(PartialEq, Eq, Clone, Copy)]
struct Opn(i8);

impl Debug for Opn {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		if self.0 >= 0 {
			write!(f, "{{{}}}", self.0)
		} else {
			write!(f, "   ")
		}
	}
}

#[derive(Copy, Clone)]
struct Indent(usize);

impl std::ops::Add<usize> for Indent {
	type Output = Self;
	fn add(self, other: usize) -> Self {
		Indent(self.0 + other)
	}
}

impl std::fmt::Display for Indent {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		for _ in 0 .. self.0 {
			write!(f, "  ")?;
		}
		Ok(())
	}
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

// ------------------------------------------------------------------------------------------------
// Transfer function
// ------------------------------------------------------------------------------------------------

fn transfer(inst: &IrInst, state: &mut ConstPropState) -> bool {
	use IrInstKind::*;

	let src_to_info = |src: IrSrc, srcn: i8, state: &mut ConstPropState| {
		match src {
			IrSrc::Reg(reg)   => state.regs[&reg],
			IrSrc::Const(c)   => state.nodes.new_const(inst.ea(), c, srcn),
			IrSrc::Return(..) => Info::Any,
		}
	};

	let thing = match inst.kind() {
		// no change!
		Nop | Use { .. } | Store { .. } | Branch { .. } | CBranch { .. } | IBranch { .. }
		| Call { .. } | ICall { .. } | Ret { .. } | Halt => None,

		Mov  { dst, src, srcn, .. } => Some((dst, src_to_info(src, srcn, state))),
		Load { dst, .. }            => Some((dst, Info::Any)),

		Unary { dst, op, src, srcn, .. } => {
			let src_info = src_to_info(src, srcn, state);
			let new_info = match src_info {
				Info::Some { val, from } => {
					let result = do_unop(op, val, src.size(), dst.size());
					state.nodes.new_unary(inst.ea(), result, op, from, srcn)
				},
				_ => Info::Any,
			};

			Some((dst, new_info))
		}

		Binary { dst, src1, op, src2, src1n, src2n, .. } => {
			let src1_info = src_to_info(src1, src1n, state);
			let src2_info = src_to_info(src2, src2n, state);

			let new_info = match (src1_info, src2_info) {
				(Info::Some { val: val1, from: from1 }, Info::Some { val: val2, from: from2 }) => {
					match do_binop(op, val1, val2, src1.size()) {
						Some(result) => state.nodes.new_binary(
							inst.ea(), result, op, from1, from2, src1n, src2n),
						None => Info::Any,
					}
				}
				_ => Info::Any,
			};

			Some((dst, new_info))
		}

		Ternary { dst, src1, op, src2, src3, src1n, src2n, src3n, .. } => {
			let src1_info = src_to_info(src1, src1n, state);
			let src2_info = src_to_info(src2, src2n, state);
			let src3_info = src_to_info(src3, src3n, state);

			let new_info = match (src1_info, src2_info, src3_info) {
				(	Info::Some{ val: val1, from: from1, .. },
					Info::Some{ val: val2, from: from2, .. },
					Info::Some{ val: val3, from: from3, .. }) => {

					let result = do_ternop(op, val1, val2, val3, src1.size());
					state.nodes.new_ternary(
						inst.ea(), result, op, from1, from2, from3, src1n, src2n, src3n)
				}
				_ => Info::Any,
			};

			Some((dst, new_info))
		}
	};

	match thing {
		Some((var, new_info)) => {
			let changed = state.regs[&var] != new_info;
			state.regs.insert(var, new_info);
			changed
		}
		_ => false
	}
}

fn do_unop(op: IrUnOp, val: u64, src_size: ValSize, dst_size: ValSize) -> u64 {
	use IrUnOp::*;

	match op {
		Zxt => val,
		// IrInst::sxt ensures that src_size < dst_size
		Sxt => match src_size {
			ValSize::_8 =>  match dst_size {
				ValSize::_16 => val as u8 as i8 as i16 as u16 as u64,
				ValSize::_32 => val as u8 as i8 as i32 as u32 as u64,
				ValSize::_64 => val as u8 as i8 as i64 as u64,
				_ => unreachable!(),
			}
			ValSize::_16 => match dst_size {
				ValSize::_32 => val as u16 as i16 as i32 as u32 as u64,
				ValSize::_64 => val as u16 as i16 as i64 as u64,
				_ => unreachable!(),
			}
			ValSize::_32 => match dst_size {
				ValSize::_64 => val as u32 as i32 as i64 as u64,
				_ => unreachable!(),
			}
			ValSize::_64 => unreachable!(),
		},
		Lo => match src_size {
			ValSize::_8  => unreachable!(),
			ValSize::_16 => val & 0xFF,
			ValSize::_32 => val & 0xFFFF,
			ValSize::_64 => val & 0xFFFFFFFF,
		},
		Hi => match src_size {
			ValSize::_8  => unreachable!(),
			ValSize::_16 => (val >>  8) & 0xFF,
			ValSize::_32 => (val >> 16) & 0xFFFF,
			ValSize::_64 => (val >> 32) & 0xFFFFFFFF,
		},
		Neg => match src_size {
			ValSize::_8 =>  (-(val as i8 )) as u8 as u64,
			ValSize::_16 => (-(val as i16)) as u16 as u64,
			ValSize::_32 => (-(val as i32)) as u32 as u64,
			ValSize::_64 => (-(val as i64)) as u64,
		},
		INot => match src_size {
			ValSize::_8 =>  (!(val as i8 )) as u8 as u64,
			ValSize::_16 => (!(val as i16)) as u16 as u64,
			ValSize::_32 => (!(val as i32)) as u32 as u64,
			ValSize::_64 => (!(val as i64)) as u64,
		},
		BNot => (val == 0) as u64,
	}
}

/// Wraps the given value to `NBITS` bits. (Just masks off any higher bits)
fn mask_to<const NBITS: usize>(v: u64) -> u64 {
	v & ((1 << NBITS) - 1)
}

/// Computes the carry-outs of all places in the unsigned addition `a + b + ci_0`. `ci_0` is meant
/// to be the carry in to bit 0, and should be 0 or 1. (This is untested for values of `ci_0` other
/// than 0 or 1.)
///
/// This is worst-case linear time in the number of bits, but has early-out if the carries stabilize
/// sooner so can complete in as little as 1 iteration.
fn carries<const NBITS: usize>(a: u64, b: u64, ci_0: u64) -> u64 {
	let mut ci = ci_0;
	let mut co = 0;
	let mut old_co = co;
	for _ in 0 .. NBITS {
		co = ((a ^ b) & ci) | (a & b);
		// early out if it stabilized
		if co == old_co {
			break;
		}
		old_co = co;
		ci = (co << 1) | ci_0;
	}

	// expression below was for testing, should be equal to a+b+ci_0 (and it was, for all
	// combinations of ci_0 in {0, 1}, a in {0, 65535}, and b in {0, 65535}
	co //, mask_to::<NBITS>(a ^ b ^ ci))
}

/// Computes the borrow-outs of all places in the unsigned subtraction `a - b - bi_0`. `bi_0` is
/// meant to be the borrow in to bit 0, and should be 0 or 1. (This is untested for values of
/// `bi_0` other than 0 or 1.)
///
/// This is worst-case linear time in the number of bits, but has early-out if the borrows stabilize
/// sooner so can complete in as little as 1 iteration.
fn borrows<const NBITS: usize>(a: u64, b: u64, bi_0: u64) -> u64 {
	let mut bi = bi_0;
	let mut bo = 0;
	let mut old_bo = bo;
	for _ in 0 .. NBITS {
		bo = (mask_to::<NBITS>(!(a ^ b)) & bi) | (mask_to::<NBITS>(!a) & b);
		// early out if it stabilized
		if bo == old_bo {
			break;
		}
		old_bo = bo;
		bi = (bo << 1) | bi_0;
	}
	// expression below was for testing, should be equal to a-b-bi_0 (and it was, for all
	// combinations of bi_0 in {0, 1}, a in {0, 65535}, and b in {0, 65535}
	bo //, mask_to::<NBITS>(a ^ b ^ bi))
}

fn do_binop(op: IrBinOp, val1: u64, val2: u64, size: ValSize) -> Option<u64> {
	use IrBinOp::*;

	let val = match op {
		Eq  => (val1 == val2) as u64,
		Ne  => (val1 != val2) as u64,

		Slt => match size {
			ValSize::_8  => ((val1 as i8)  < (val2 as i8)) as u64,
			ValSize::_16 => ((val1 as i16) < (val2 as i16)) as u64,
			ValSize::_32 => ((val1 as i32) < (val2 as i32)) as u64,
			ValSize::_64 => ((val1 as i64) < (val2 as i64)) as u64,
		},
		Sle => match size {
			ValSize::_8  => ((val1 as i8)  <= (val2 as i8)) as u64,
			ValSize::_16 => ((val1 as i16) <= (val2 as i16)) as u64,
			ValSize::_32 => ((val1 as i32) <= (val2 as i32)) as u64,
			ValSize::_64 => ((val1 as i64) <= (val2 as i64)) as u64,
		},

		Ult => (val1 < val2) as u64,
		Ule => (val1 <= val2) as u64,

		Add => match size {
			ValSize::_8  => (val1 as u8).wrapping_add(val2 as u8) as u64,
			ValSize::_16 => (val1 as u16).wrapping_add(val2 as u16) as u64,
			ValSize::_32 => (val1 as u32).wrapping_add(val2 as u32) as u64,
			ValSize::_64 => val1.wrapping_add(val2),
		}
		Sub  => match size {
			ValSize::_8  => (val1 as u8).wrapping_sub(val2 as u8) as u64,
			ValSize::_16 => (val1 as u16).wrapping_sub(val2 as u16) as u64,
			ValSize::_32 => (val1 as u32).wrapping_sub(val2 as u32) as u64,
			ValSize::_64 => val1.wrapping_sub(val2),
		}
		UCarry => match size {
			ValSize::_8  => (val1 as u8).overflowing_add(val2 as u8).1 as u64,
			ValSize::_16 => (val1 as u16).overflowing_add(val2 as u16).1 as u64,
			ValSize::_32 => (val1 as u32).overflowing_add(val2 as u32).1 as u64,
			ValSize::_64 => val1.overflowing_add(val2).1 as u64,
		}
		SCarry => match size {
			ValSize::_8  => (val1 as i8).overflowing_add(val2 as i8).1 as u64,
			ValSize::_16 => (val1 as i16).overflowing_add(val2 as i16).1 as u64,
			ValSize::_32 => (val1 as i32).overflowing_add(val2 as i32).1 as u64,
			ValSize::_64 => (val1 as i64).overflowing_add(val2 as i64).1 as u64,
		}
		SBorrow => match size {
			ValSize::_8  => (val1 as i8).overflowing_sub(val2 as i8).1 as u64,
			ValSize::_16 => (val1 as i16).overflowing_sub(val2 as i16).1 as u64,
			ValSize::_32 => (val1 as i32).overflowing_sub(val2 as i32).1 as u64,
			ValSize::_64 => (val1 as i64).overflowing_sub(val2 as i64).1 as u64,
		}
		Carries => match size {
			ValSize::_8  => carries::< 8>(val1, val2, 0),
			ValSize::_16 => carries::<16>(val1, val2, 0),
			ValSize::_32 => carries::<32>(val1, val2, 0),
			ValSize::_64 => carries::<64>(val1, val2, 0),
		}
		Borrows => match size {
			ValSize::_8  => borrows::< 8>(val1, val2, 0),
			ValSize::_16 => borrows::<16>(val1, val2, 0),
			ValSize::_32 => borrows::<32>(val1, val2, 0),
			ValSize::_64 => borrows::<64>(val1, val2, 0),
		}
		//
		// : this is poorly-defined. would it make more sense to have an n*n=>2n multiplication
		// operation?  well we'll punt for now cause I don't forsee implementing arches with
		// multiplication any time soon.
		Mul => match size {
			ValSize::_8  => (val1 as u8).wrapping_mul(val2 as u8) as u64,
			ValSize::_16 => (val1 as u16).wrapping_mul(val2 as u16) as u64,
			ValSize::_32 => (val1 as u32).wrapping_mul(val2 as u32) as u64,
			ValSize::_64 => val1.wrapping_mul(val2),
		}
		UDiv => {
			// not using checked_div et al. because the result has to be u64, and this is
			// less awkward imo
			if val2 == 0 {
				return None;
			} else {
				match size {
					ValSize::_8  => (val1 as u8 / val2 as u8) as u64,
					ValSize::_16 => (val1 as u16 / val2 as u16) as u64,
					ValSize::_32 => (val1 as u32 / val2 as u32) as u64,
					ValSize::_64 => val1 / val2,
				}
			}
		}
		SDiv => {
			if val2 == 0 {
				return None;
			} else {
				match size {
					ValSize::_8  => (val1 as i8 / val2 as i8) as u8 as u64,
					ValSize::_16 => (val1 as i16 / val2 as i16) as u16 as u64,
					ValSize::_32 => (val1 as i32 / val2 as i32) as u32 as u64,
					ValSize::_64 => (val1 as i64 / val2 as i64) as u64,
				}
			}
		}
		UMod => {
			if val2 == 0 {
				return None;
			} else {
				match size {
					ValSize::_8  => (val1 as u8 % val2 as u8) as u64,
					ValSize::_16 => (val1 as u16 % val2 as u16) as u64,
					ValSize::_32 => (val1 as u32 % val2 as u32) as u64,
					ValSize::_64 => val1 % val2,
				}
			}
		}
		// TODO: modulo on signed numbers is poorly-defined! aaaah!!!!!
		SMod => {
			if val2 == 0 {
				return None;
			} else {
				match size {
					ValSize::_8  => (val1 as i8 % val2 as i8) as u8 as u64,
					ValSize::_16 => (val1 as i16 % val2 as i16) as u16 as u64,
					ValSize::_32 => (val1 as i32 % val2 as i32) as u32 as u64,
					ValSize::_64 => (val1 as i64 % val2 as i64) as u64,
				}
			}
		}

		IXor => val1 ^ val2,
		IAnd => val1 & val2,
		IOr =>  val1 | val2,

		Shl => match size {
			ValSize::_8  => (val1 as u8).checked_shl(val2 as u32).unwrap_or(0) as u64,
			ValSize::_16 => (val1 as u16).checked_shl(val2 as u32).unwrap_or(0) as u64,
			ValSize::_32 => (val1 as u32).checked_shl(val2 as u32).unwrap_or(0) as u64,
			ValSize::_64 => val1.checked_shl(val2 as u32).unwrap_or(0),
		}
		UShr => match size {
			ValSize::_8  => (val1 as u8).checked_shr(val2 as u32).unwrap_or(0) as u64,
			ValSize::_16 => (val1 as u16).checked_shr(val2 as u32).unwrap_or(0) as u64,
			ValSize::_32 => (val1 as u32).checked_shr(val2 as u32).unwrap_or(0) as u64,
			ValSize::_64 => val1.checked_shr(val2 as u32).unwrap_or(0),
		}
		// TODO: what if val2 is negative?
		SShr => match size {
			ValSize::_8  => (val1 as i8).checked_shr(val2 as u32)
				.unwrap_or(if (val1 as i8) < 0 { -1 } else { 0 }) as u8 as u64,
			ValSize::_16 => (val1 as i16).checked_shr(val2 as u32)
				.unwrap_or(if (val1 as i16) < 0 { -1 } else { 0 }) as u16 as u64,
			ValSize::_32 => (val1 as i32).checked_shr(val2 as u32)
				.unwrap_or(if (val1 as i32) < 0 { -1 } else { 0 }) as u32 as u64,
			ValSize::_64 => (val1 as i64).checked_shr(val2 as u32)
				.unwrap_or(if (val1 as i64) < 0 { -1 } else { 0 }) as u64,
		}

		Rol => match size {
			ValSize::_8  => (val1 as u8).rotate_left(val2 as u32) as u64,
			ValSize::_16 => (val1 as u16).rotate_left(val2 as u32) as u64,
			ValSize::_32 => (val1 as u32).rotate_left(val2 as u32) as u64,
			ValSize::_64 => val1.rotate_left(val2 as u32),
		}
		Ror => match size {
			ValSize::_8  => (val1 as u8).rotate_right(val2 as u32) as u64,
			ValSize::_16 => (val1 as u16).rotate_right(val2 as u32) as u64,
			ValSize::_32 => (val1 as u32).rotate_right(val2 as u32) as u64,
			ValSize::_64 => val1.rotate_right(val2 as u32),
		}

		Pair => (val1 << size as u32) | val2,

		Bit => {
			let num_bits = size.bytes() as u64 * 8;
			assert!(val2 < num_bits, "bit position {} exceeds number of bits {}", val2, num_bits);
			if (val1 & (1 << val2)) != 0 { 1 } else { 0 }
		}

		BXor => (val1 != val2) as u64,
		BAnd => (val1 != 0 && val2 != 0) as u64,
		BOr =>  (val1 != 0 || val2 != 0) as u64,
	};

	Some(val)
}

fn do_ternop(op: IrTernOp, val1: u64, val2: u64, val3: u64, size: ValSize) -> u64 {
	use IrTernOp::*;

	match op {
		AddC => match size {
			ValSize::_8 => (val1 as u8).wrapping_add(val2 as u8).wrapping_add(val3 as u8) as u64,
			ValSize::_16 => (val1 as u16).wrapping_add(val2 as u16).wrapping_add(val3 as u16) as u64,
			ValSize::_32 => (val1 as u32).wrapping_add(val2 as u32).wrapping_add(val3 as u32) as u64,
			ValSize::_64 => val1.wrapping_add(val2).wrapping_add(val3),
		},
		SubB => match size {
			ValSize::_8 => (val1 as u8).wrapping_sub(val2 as u8).wrapping_sub(val3 as u8) as u64,
			ValSize::_16 => (val1 as u16).wrapping_sub(val2 as u16).wrapping_sub(val3 as u16) as u64,
			ValSize::_32 => (val1 as u32).wrapping_sub(val2 as u32).wrapping_sub(val3 as u32) as u64,
			ValSize::_64 => val1.wrapping_sub(val2).wrapping_sub(val3),
		},
		UCarryC => {
			let (sum, carry) = match size {
				ValSize::_8 => {
					let (sum, carry) = (val1 as u8).overflowing_add(val2 as u8);
					(sum as u64, carry)
				}
				ValSize::_16 => {
					let (sum, carry) = (val1 as u16).overflowing_add(val2 as u16);
					(sum as u64, carry)
				}
				ValSize::_32 => {
					let (sum, carry) = (val1 as u32).overflowing_add(val2 as u32);
					(sum as u64, carry)
				}
				ValSize::_64 => {
					let (sum, carry) = val1.overflowing_add(val2);
					(sum, carry)
				}
			};

			if carry {
				1
			} else {
				match size {
					ValSize::_8  => (sum as u8).overflowing_add(val3 as u8).1 as u64,
					ValSize::_16 => (sum as u16).overflowing_add(val3 as u16).1 as u64,
					ValSize::_32 => (sum as u32).overflowing_add(val3 as u32).1 as u64,
					ValSize::_64 => sum.overflowing_add(val3).1 as u64,
				}
			}
		}
		SCarryC => {
			let (sum, carry) = match size {
				ValSize::_8 => {
					let (sum, carry) = (val1 as i8).overflowing_add(val2 as i8);
					(sum as u64, carry)
				}
				ValSize::_16 => {
					let (sum, carry) = (val1 as i16).overflowing_add(val2 as i16);
					(sum as u64, carry)
				}
				ValSize::_32 => {
					let (sum, carry) = (val1 as i32).overflowing_add(val2 as i32);
					(sum as u64, carry)
				}
				ValSize::_64 => {
					let (sum, carry) = (val1 as i64).overflowing_add(val2 as i64);
					(sum as u64, carry)
				}
			};

			if carry {
				1
			} else {
				match size {
					ValSize::_8  => (sum as i8).overflowing_add(val3 as i8).1 as u64,
					ValSize::_16 => (sum as i16).overflowing_add(val3 as i16).1 as u64,
					ValSize::_32 => (sum as i32).overflowing_add(val3 as i32).1 as u64,
					ValSize::_64 => (sum as i64).overflowing_add(val3 as i64).1 as u64,
				}
			}
		}
		SBorrowB => {
			let (sum, borrow) = match size {
				ValSize::_8 => {
					let (sum, borrow) = (val1 as i8).overflowing_sub(val2 as i8);
					(sum as u64, borrow)
				}
				ValSize::_16 => {
					let (sum, borrow) = (val1 as i16).overflowing_sub(val2 as i16);
					(sum as u64, borrow)
				}
				ValSize::_32 => {
					let (sum, borrow) = (val1 as i32).overflowing_sub(val2 as i32);
					(sum as u64, borrow)
				}
				ValSize::_64 => {
					let (sum, borrow) = (val1 as i64).overflowing_sub(val2 as i64);
					(sum as u64, borrow)
				}
			};

			if borrow {
				1
			} else {
				match size {
					ValSize::_8  => (sum as i8).overflowing_sub(val3 as i8).1 as u64,
					ValSize::_16 => (sum as i16).overflowing_sub(val3 as i16).1 as u64,
					ValSize::_32 => (sum as i32).overflowing_sub(val3 as i32).1 as u64,
					ValSize::_64 => (sum as i64).overflowing_sub(val3 as i64).1 as u64,
				}
			}
		}
		CarriesC => match size {
			ValSize::_8  => carries::< 8>(val1, val2, val3),
			ValSize::_16 => carries::<16>(val1, val2, val3),
			ValSize::_32 => carries::<32>(val1, val2, val3),
			ValSize::_64 => carries::<64>(val1, val2, val3),
		}
		BorrowsB => match size {
			ValSize::_8  => borrows::< 8>(val1, val2, val3),
			ValSize::_16 => borrows::<16>(val1, val2, val3),
			ValSize::_32 => borrows::<32>(val1, val2, val3),
			ValSize::_64 => borrows::<64>(val1, val2, val3),
		}
		BSet => {
			let num_bits = size.bytes() as u64 * 8;
			assert!(val2 < num_bits, "bit position {} exceeds number of bits {}", val2, num_bits);
			assert!(val3 == 0 || val3 == 1, "src3 must be 0 or 1");
			(val1 & !(1 << val2)) | (val3 << val2)
		}
	}
}