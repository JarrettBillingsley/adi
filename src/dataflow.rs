//! Generic dataflow algorithm framework.

use std::hash::{ Hash };
use std::collections::{ VecDeque, HashSet };

// ------------------------------------------------------------------------------------------------
// JoinSemiLattice
// ------------------------------------------------------------------------------------------------

/// From rustc!
pub(crate) trait JoinSemiLattice: Eq {
	fn join(&mut self, other: &Self) -> bool;
}

// ------------------------------------------------------------------------------------------------
// WorkQueue
// ------------------------------------------------------------------------------------------------

/// Shamelessly ripped from `rustc_data_structures::WorkQueue`.
pub(crate) struct WorkQueue<T> {
	list: VecDeque<T>,
	// the set is here to avoid enqueueing items which are already in the queue.
	set:  HashSet<T>,
}

impl<T: Eq + Hash + Clone + Copy> WorkQueue<T> {
	pub(crate) fn new(cap: usize) -> Self {
		Self {
			list: VecDeque::with_capacity(cap),
			set: HashSet::with_capacity(cap),
		}
	}

	pub(crate) fn enqueue(&mut self, item: T) {
		if self.set.insert(item) {
			self.list.push_back(item);
		}
	}

	pub(crate) fn dequeue(&mut self) -> Option<T> {
		if let Some(item) = self.list.pop_front() {
			self.set.remove(&item);
			Some(item)
		} else {
			None
		}
	}
}

// ------------------------------------------------------------------------------------------------
// DataflowAlgorithm
// ------------------------------------------------------------------------------------------------

// TODO: support reverse dataflow algorithms too

/// Trait for a control flow graph used by `DataflowAlgorithm`. `ID` is meant to be a type used to
/// uniquely identify each node in the graph.
pub(crate) trait DataflowCfg<ID>
where
	ID: Eq + Hash + Clone + Copy
{
	/// Should return the number of nodes in the CFG.
	fn num_nodes(&self) -> usize;

	/// Should return an iterator which lists all nodes in the CFG in the initial order for them to
	/// be visited. For a forward analysis, reverse postorder (RPO) is preferred, but really any
	/// order will work, just not necessarily as fast as RPO. For a backward analysis, postorder is
	/// Probably Fine™. Hey, that's what the rustc people think.
	fn initial_order(&self) -> impl Iterator<Item = ID>;

	/// Should return an iterator over all successors of the given node.
	fn successors(&self, id: ID) -> impl Iterator<Item = ID>;
}

/// Trait for an abstract dataflow algorithm. This implements the "boring" parts of dataflow so that
/// all you have to do is implement the interesting bits.
pub(crate) trait DataflowAlgorithm {
	/// Associated type used to uniquely identify each node in a control flow graph. Used as the
	/// type parameter to `DataflowCfg`.
	type ID: Eq + Hash + Clone + Copy;

	/// The main method you need to implement. Visits a node of the CFG. Should perform state joins
	/// from predecessors and then perform the transfer function across the node. Should return
	/// true if any state changes occurred. If so, its successors will be enqueued to be visited in
	/// the future.
	fn visit(&mut self, id: Self::ID) -> bool;

	/// Runs the dataflow algorithm to fixpoint. `cfg` is the control flow graph to run the
	/// algorithm on. Returns nothing, as it's assumed that you will look at the internal state of
	/// the nodes in the CFG after the fact to determine the results.
	fn run(&mut self, cfg: &impl DataflowCfg<Self::ID>) {
		let mut work = WorkQueue::<Self::ID>::new(cfg.num_nodes());

		for id in cfg.initial_order() {
			work.enqueue(id);
		}

		while let Some(id) = work.dequeue() {
			let changed = self.visit(id);
			if changed {
				for s in cfg.successors(id) {
					work.enqueue(s);
				}
			}
		}
	}
}