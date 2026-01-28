
use std::collections::{ BTreeSet };

use petgraph::{
	Direction,
	graphmap::{ DiGraphMap },
	algo::dominators::{ self },
	dot::{ Dot, Config as DotConfig },
};

use super::*;

// ------------------------------------------------------------------------------------------------
// Dominator tree
// ------------------------------------------------------------------------------------------------

// TODO: implement this directly rather than in terms of a DiGraphMap?
pub(crate) struct DomTree {
	g: DiGraphMap<IrBBId, ()>,
}

impl DomTree {
	pub(crate) fn new(cfg: &IrCfg) -> Self {
		let doms = dominators::simple_fast(&cfg, 0);
		let mut g = DiGraphMap::<IrBBId, ()>::new();

		for n in cfg.nodes() {
			// doing this because Dominators uses a HashMap internally and the child iteration
			// order is nondeterministic.
			let mut children = doms.immediately_dominated_by(n).collect::<Vec<_>>();
			children.sort();

			for m in children.into_iter() {
				// only add edges for non-self domination.
				if n != m {
					g.add_edge(n, m, ());
				}
			}
		}

		// sanity check, this is a strict tree so in-degree should be 1;
		// should also not have any nodes disconnected from the root.
		for n in cfg.nodes() {
			if n != 0 {
				assert_eq!(g.edges_directed(n, Direction::Incoming).count(), 1, "node {}", n);
			}
		}

		Self { g }
	}

	pub(crate) fn show(&self) {
		println!("Dominators");
		println!("-------------");
		println!();
		println!("{:?}", Dot::with_config(&self.g, &[DotConfig::EdgeNoLabel]));
	}

	pub(crate) fn idom(&self, node: IrBBId) -> Option<IrBBId> {
		self.g.edges_directed(node, Direction::Incoming).map(|(d, _, _)| d).next()
	}

	pub(crate) fn immediately_dominated_by(&self, node: IrBBId)
		-> impl Iterator<Item = IrBBId> + '_ {
		self.g.edges_directed(node, Direction::Outgoing).map(|(_, c, _)| c)
	}

	/// `true` if `x` strictly dominates `b` (directly or indirectly).
	pub(crate) fn strictly_dominates(&self, x: IrBBId, mut b: IrBBId) -> bool {
		while let Some(dom) = self.idom(b) {
			if x == dom {
				return true;
			}

			b = dom;
		}

		false
	}
}

// ------------------------------------------------------------------------------------------------
// Dominance frontiers
// ------------------------------------------------------------------------------------------------

pub(crate) type DomFrontiers = Vec<BTreeSet<IrBBId>>;

pub(crate) fn compute_dominance_frontiers(cfg: &IrCfg, doms: &DomTree) -> DomFrontiers {
	let mut df = cfg.nodes().map(|_| BTreeSet::new()).collect::<DomFrontiers>();

	for (a, b, _) in cfg.all_edges() {
		let mut x = a;

		while !doms.strictly_dominates(x, b) {
			df[x].insert(b);

			match doms.idom(x) {
				Some(d) => x = d,
				None    => break,
			}
		}
	}

	df
}

fn show_df(df: &DomFrontiers) {
	println!("Dominance Frontiers");
	println!("-------------------");
	println!();

	for (i, df) in df.iter().enumerate() {
		println!("DF[{}] = {:?}", i, df);
	}
}
