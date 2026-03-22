
use std::iter::{ IntoIterator };

use log::*;

use crate::program::{ Program };
use crate::memory::{ EA };

// ------------------------------------------------------------------------------------------------
// Splitting previously-analyzed functions
// ------------------------------------------------------------------------------------------------

impl Program {
	pub(super) fn split_func_pass(&mut self, ea: EA) {
		info!("------------------------------------------------------------------------");
		info!("- begin function splitting pass at {}", ea);

		let mut bbid = self.span_at_ea(ea).bb().expect("uh, there used to be a function here");
		let fid = self.bbidx.get(bbid).func();

		// early out: were we asked to split a function at its first address?
		if self.funcs.get(fid).ea() == ea {
			debug!("- oop nevermind, I was asked to split the function at the start");
			// since no change, return and DON'T enqueue for static analysis.
			// LOL THIS IS GONNA BITE ME AT SOME POINT IN THE FUTURE, ISN'T IT?
			return;
		}

		// first: split target BB if needed
		match self.split_bb(bbid, ea, Some(fid)) {
			Ok(Some(new_bbid)) => {
				// add it to the function's vec of BBs,
				self.get_func_mut(fid).bbs.push(new_bbid);
				// and now we're working with the new BB.
				bbid = new_bbid;
			}
			Ok(None) => {} // didn't split, s'fine
			Err(_) => {
				// TODO: mark referrer as being invalid somehow.
				warn!("  attempted to split function starting at {} at EA {}, but it failed",
					self.get_func(fid).ea(), ea);

				// let's bail.
				return;
			}
		}

		// TODO: technically, it *is* possible to split multi-entry functions, as long as
		// the head node dominates all the other entry points. but I don't care to deal with
		// that right now.
		if self.get_func(fid).is_multi_entry() {
			debug!("  function at {} is multi-entry already", self.get_func(fid).ea());
			self.get_func_mut(fid).add_entrypoint(bbid);

			// since we technically changed the CFG (a new entry point means MMU state may be
			// different!), enqueue this for re-analysis.
			self.queue.enqueue_func_analysis(fid);
			return;
		}

		let cfg = self.func_analyze_cfg(self.get_func(fid));
		// self.func_dump_cfg(&cfg);

		let r = cfg.reachable(bbid);

		if r.splittable() {
			// alright, we can split! conveniently, the reachable set is the set of BBs that the
			// new function will inherit, and bbid will become its head.

			let reachable = r.all_reachable();

			// first, remove all the 'reachable' bbs from func
			self.get_func_mut(fid).bbs
				.retain(|&to_keep| to_keep != bbid && !reachable.contains(&to_keep));

			assert!(!self.get_func(fid).bbs.is_empty(),
				"function at {} was stripped of all BBs!", self.get_func(fid).ea());

			// then, turn 'reachable' into a vec, with bbid as the first item.
			let new_func_bbs = Some(bbid).into_iter()
				.chain(reachable)
				.collect::<Vec<_>>();

			// last, make a new function out of the new_func_bbs, and change what function
			// they belong to.
			let new_fid = self.funcs.new_func(ea, new_func_bbs);

			for &bb in &self.funcs.get(new_fid).bbs {
				self.bbidx.get_mut(bb).change_func(new_fid);
			}

			debug!("  split off new function {:?} at {}.", new_fid, ea);
			self.queue.enqueue_func_analysis(new_fid);
		} else {
			// otherwise, give up and mark it a multi-entry function.
			debug!("  can't split, marking function at {} as multi-entry", self.get_func(fid).ea());
			self.get_func_mut(fid).add_entrypoint(bbid);
		}

		// either way, the old function needs a static analysis
		self.queue.enqueue_func_analysis(fid);
	}
}
