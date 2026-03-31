
use super::*;

// ------------------------------------------------------------------------------------------------
// AST types
// ------------------------------------------------------------------------------------------------

/// Unique node identifier within this IrFunction.
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub(crate) struct NodeId(pub(super) usize);

/// Kinds of AST nodes.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub(super) enum NodeKind {
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
pub(super) struct Node {
	pub(super) id:   NodeId,
	pub(super) ea:   EA,
	pub(super) kind: NodeKind,
}

impl Node {
	fn new(id: NodeId, ea: EA, kind: NodeKind) -> Self {
		Self { id, ea, kind }
	}
}

/// A collection of AST nodes associated with a single IrFunction.
#[derive(Debug)]
pub(super) struct Nodes {
	pub(super) nodes: Vec<Node>,
}

impl Nodes {
	pub(super) fn new() -> Self {
		Self { nodes: vec![] }
	}

	pub(super) fn new_node(&mut self, ea: EA, kind: NodeKind) -> NodeId {
		let id = NodeId(self.nodes.len());
		self.nodes.push(Node::new(id, ea, kind));
		id
	}

	pub(super) fn new_const(&mut self, is_multi: bool, ea: EA, c: IrConst, cn: i8) -> Info {
		Info::Some {
			val:  c.val(),
			from: self.new_node(ea, NodeKind::Const { c, cn }),
			is_multi,
		}
	}

	pub(super) fn new_unary(&mut self, is_multi: bool, ea: EA, val: u64, op: IrUnOp,
		src: NodeId, srcn: i8) -> Info {
		Info::Some {
			val,
			from: self.new_node(ea, NodeKind::Unary { op, src, srcn }),
			is_multi,
		}
	}

	pub(super) fn new_binary(&mut self, is_multi: bool, ea: EA, val: u64, op: IrBinOp,
		src1: NodeId, src2: NodeId, src1n: i8, src2n: i8) -> Info {
		Info::Some {
			val,
			from: self.new_node(ea, NodeKind::Binary { op, src1, src2, src1n, src2n }),
			is_multi,
		}
	}

	pub(super) fn new_ternary(&mut self, is_multi: bool, ea: EA, val: u64, op: IrTernOp,
		src1: NodeId, src2: NodeId, src3: NodeId, src1n: i8, src2n: i8, src3n: i8) -> Info {
		Info::Some {
			val,
			from: self.new_node(ea, NodeKind::Ternary {
				op, src1, src2, src3, src1n, src2n, src3n }),
			is_multi,
		}
	}

	pub(super) fn ea_of(&self, node: NodeId) -> EA {
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
