
use std::iter::{ IntoIterator };

use log::*;

use crate::program::{ Program, BBId, FuncId, UnsplittableError, ReachableBBs };
use crate::memory::{ EA };

// ------------------------------------------------------------------------------------------------
// Splitting previously-analyzed functions
// ------------------------------------------------------------------------------------------------

impl Program {
	pub(super) fn split_func_pass(&mut self, ea: EA) {
		info!("------------------------------------------------------------------------");
		info!("- begin function splitting pass at {}", ea);

		let bbid = self.span_at_ea(ea).bb().expect("uh, there used to be a function here");
		let fid = self.bbidx.get(bbid).func();
		let func_ea = self.funcs.get(fid).ea();

		// early out: were we asked to split a function at its first address?
		if func_ea == ea {
			debug!("- oop nevermind, I was asked to split the function at the start");
			// since no change, return and DON'T enqueue for static analysis.
			// LOL THIS IS GONNA BITE ME AT SOME POINT IN THE FUTURE, ISN'T IT?
			return;
		}

		// first: split target BB if needed
		let Some(bbid) = self.split_target_bb(ea, bbid, fid) else {
			return;
		};

		// TODO: technically, it *is* possible to split multi-entry functions, as long as the split
		// point dominates/is dominated by all the other entry points. but I don't care to deal
		// with that right now.
		if self.get_func(fid).is_multi_entry() {
			debug!("  function at {} is multi-entry already", func_ea);

			if self.get_func_mut(fid).add_entrypoint(bbid) {
				// since we technically changed the CFG (a new entry point means MMU state may be
				// different!), enqueue this for re-analysis.
				self.queue.enqueue_func_analysis(fid);
			}

			return;
		}

		let mut cfg = self.func_analyze_cfg(self.get_func(fid));
		// self.func_dump_cfg(&cfg);

		use UnsplittableError::*;

		match cfg.split_plan_for(bbid) {
			Ok(plan) => {
				for split_point in plan.into_iter() {
					let r = cfg.reachable(split_point);
					self.split_off_new_func(r, func_ea, fid, split_point);
					cfg = self.func_analyze_cfg(self.get_func(fid));
					// self.func_dump_cfg(&cfg);
				}

				self.queue.enqueue_func_analysis(fid);
			}

			Err(e) => {
				match e {
					IsHead =>
						unreachable!(), // checked for that up top...
					InCycleWithHead =>
						debug!("  can't split: split node in cycle with head node"),
					OverlapsIrreducible(irred) =>
						debug!("  can't split: overlaps irreducible nodes {:?}", irred),
					DomRootOverlapsIrreducible(dom_root, irred) =>
						debug!("  can't split: dom root {:?} overlaps irreducible nodes {:?}",
							dom_root, irred),
					DomRootDominatesSplitPoint(dom_root) =>
						debug!("  can't split: dom root {:?} dominates split point (cyclic)",
							dom_root),
					NoSplittableDomRoots(dom_roots) =>
						panic!("  can't split: no splittable dom roots {:?}. this really should \
							not happen but it probably is happening", dom_roots),
				}
				// otherwise, give up and mark it a multi-entry function.
				debug!("  marking function at {} as multi-entry", func_ea);
				if self.get_func_mut(fid).add_entrypoint(bbid) {
					self.queue.enqueue_func_analysis(fid);
				}
			}
		}
	}

	// Split the target BB at `ea`. Returns the index of the newly-split BB, or `bbid` if `ea`
	// pointed to its first instruction already.
	fn split_target_bb(&mut self, ea: EA, bbid: BBId, fid: FuncId) -> Option<BBId> {
		match self.split_bb(bbid, ea, Some(fid)) {
			Ok(Some(new_bbid)) => {
				// add it to the function's vec of BBs,
				self.get_func_mut(fid).bbs.push(new_bbid);
				// and now we're working with the new BB.
				Some(new_bbid)
			}
			Ok(None) => Some(bbid), // didn't split, s'fine
			Err(_) => {
				// TODO: mark referrer as being invalid somehow.
				warn!("  attempted to split function starting at {} at EA {}, but it failed",
					self.get_func(fid).ea(), ea);
				None
			}
		}
	}

	fn split_off_new_func(&mut self, r: ReachableBBs, func_ea: EA, fid: FuncId, split_point: BBId) {
		// alright, we can split! conveniently, the reachable set is the set of BBs that the
		// new function will inherit, and split_point will become its head.

		assert!(r.splittable(), "UHHHHHH THE PLAN WAS WRONG");

		let reachable = r.all_reachable();

		// first, remove all the 'reachable' bbs from func
		self.get_func_mut(fid).bbs
			.retain(|&to_keep| to_keep != split_point && !reachable.contains(&to_keep));

		assert!(!self.get_func(fid).bbs.is_empty(),
			"function at {} was stripped of all BBs!", func_ea);

		// then, turn 'reachable' into a vec, with split_point as the first item.
		let new_func_bbs = Some(split_point).into_iter()
			.chain(reachable)
			.collect::<Vec<_>>();

		// last, make a new function out of the new_func_bbs, and change what function
		// they belong to.
		let new_func_ea = self.bbidx.get(split_point).ea();
		let new_fid = self.funcs.new_func(new_func_ea, new_func_bbs);

		for &bb in &self.funcs.get(new_fid).bbs {
			self.bbidx.get_mut(bb).change_func(new_fid);
		}

		debug!("  split off new function {:?} at {}.", new_fid, new_func_ea);
		self.queue.enqueue_func_analysis(new_fid);
	}
}
