
use std::collections::{ HashMap };
use std::iter::{ IntoIterator} ;

use log::*;

use crate::program::{ Program, BBId, BBTerm, Function, FuncId };
use crate::memory::{ MmuState, VA, StateChange };
use crate::ir::{ ConstAddr };

// ------------------------------------------------------------------------------------------------
// MMU state change analysis
// ------------------------------------------------------------------------------------------------

impl Program {
	pub(super) fn state_change_pass(&mut self, fid: FuncId) {
		let func = self.get_func(fid);

		trace!("------------------------------------------------------------------------");
		trace!("- begin func state change analysis at {}", func.ea());

		// part 1: construct IR, do constant propagation, and determine *where* state changes are

		let irfunc = self.func_to_ir(fid);
		println!("------------------------------------------------------------------");
		// println!("Constants:");
		// let consts = irfunc.constants();
		// for (reg, (val, from)) in consts {
		// 	println!("{:?} = {:08X} <from {:?}>", reg, val, from);
		// }
		// println!("{:?}", irfunc);

		for ConstAddr { bbidx, instidx, inst, opn, val, srcs } in irfunc.const_addrs() {
			// TODO: add OpInfo::VARef to each constant operand
			println!("{:?} ({},{}) operand {} is a constant 0x{:08X} <from {:?}>",
				inst.ea(),
				bbidx, instidx,
				opn,
				val,
				srcs);
			// TODO: recursively visit instructions based on `srcs` (after constprop makes AST)
			// TODO: detect instruction state changes *here*, not down there in the loop
			// TODO: split BBs at state change instructions
		}

		// --------------------------------------------------------
		// part 2: propagate state changes determined above

		// 1. make a list of "new states" for each bb.
		let mut new_states = BBStateChanger::new(func);

		// 2. gather all the BBs that change banks.
		let changers = func.all_bbs()
			.filter(|&b| matches!(self.bbidx.get(b).term(), BBTerm::BankChange(..)))
			.collect::<Vec<_>>();

		// 3. for each one, try a few techniques to determine the new state.
		for bbid in changers {
			let bb = self.bbidx.get(bbid);
			// it's possible this BB's state was changed on a previous iteration.

			// TODO: this is commented out just for now. eventually it will be used again?
			// it was once used in the StateChange::Dynamic case, where it'd interpret the
			// BB to find out the new state, because earlier iterations of this loop
			// might have come up with a new state for this BB that is out of sync with
			// the state currently stored in the BB (that's what the comment above means)
			// let state = new_states.new_state_for(bbid);

			// See if we get a new state...
			let new_state = match self.mem.inst_state_change(bb.mmu_state(), bb.term_inst()) {
				StateChange::None              => unreachable!(),
				StateChange::Maybe             => None, // TODO: log this!
				StateChange::Dynamic           => None, // TODO: uhhh
				StateChange::Static(new_state) => Some(new_state),
			};

			// 4. now, propagate that state change to the successors.
			if let Some(new_state) = new_state {
				log::trace!("  new state: {:?}", new_state);
				match new_states.propagate(self, bbid, new_state) {
					Ok(()) => {}
					Err(()) => todo!("oh noooooooo"),
				}
			} else {
				log::warn!("  could not determine new state");
				// TODO: mark this function as incompletely analyzed somehow.
			}
		}

		// 5. Finally, apply the changes.
		for (bbid, new_state) in new_states.into_iter() {
			self.bbidx.get_mut(bbid).set_mmu_state(new_state);
			// with new knowledge, we might be able to resolve unresolved EAs in the terminator.
			let changed = self.resolve_unresolved_terminator(new_state, bbid);

			if changed {
				trace!("changed terminator of {:?}", bbid);
			}
		}

		// 6. And set this function up for its refs pass.
		self.enqueue_func_refs(fid);
	}

	fn resolve_unresolved_terminator(&mut self, state: MmuState, bbid: BBId) -> bool {
		let mut term = self.bbidx.get(bbid).term().clone();
		let mut changed = false;

		for target in term.successors_mut() {
			let old_target = *target;

			if target.is_invalid() {
				*target = self.resolve_target(state, VA(old_target.offs()));
				// the above *could* still fail!
				if old_target != *target {
					changed = true;
					trace!("resolved terminator from {:?} to {:?}", old_target, *target);
				}
			}
		}

		if changed {
			*self.bbidx.get_mut(bbid).term_mut() = term;
		}

		changed
	}
}

struct BBStateChangeStatus {
	new_state: Option<MmuState>,
	visited:   bool,
}

struct BBStateChanger {
	map: HashMap<BBId, BBStateChangeStatus>,
}

impl BBStateChanger {
	fn new(func: &Function) -> Self {
		let mut map = HashMap::new();

		for bbid in func.all_bbs() {
			map.insert(bbid, BBStateChangeStatus { new_state: None, visited: false });
		}

		Self { map }
	}

	fn into_iter(self) -> impl Iterator<Item = (BBId, MmuState)> {
		self.map.into_iter().filter_map(|(bbid, cs)| cs.new_state.map(|ns| (bbid, ns)))
	}

	fn propagate(&mut self, prog: &Program, from: BBId, new_state: MmuState) -> Result<(), ()> {
		self.clear_visited();
		self.walk(prog, from, new_state)
	}

	fn clear_visited(&mut self) {
		self.map.iter_mut().for_each(|(_, cs)| cs.visited = false);
	}

	fn is_visited(&self, bb: BBId) -> bool {
		self.map[&bb].visited
	}

	fn mark_visited(&mut self, bb: BBId) {
		self.map.get_mut(&bb).unwrap().visited = true;
	}

	fn new_state_for(&self, bb: BBId) -> Option<MmuState> {
		self.map[&bb].new_state
	}

	fn set_new_state(&mut self, bb: BBId, new_state: MmuState) {
		self.map.get_mut(&bb).unwrap().new_state = Some(new_state);
	}

	fn visit(&mut self, prog: &Program, from: BBId, new_state: MmuState) -> Result<(), ()> {
		if self.is_visited(from) {
			Ok(())
		} else {
			self.mark_visited(from);

			if let Some(ns) = self.new_state_for(from) {
				// uh oh. conflict
				let ea = prog.get_bb(from).ea();
				log::trace!("conflicting states @ {} (changing to both {:?} and {:?})",
					ea, ns, new_state);
				Err(())
			} else {
				self.set_new_state(from, new_state);
				self.walk(prog, from, new_state)
			}
		}
	}

	fn walk(&mut self, prog: &Program, from: BBId, new_state: MmuState) -> Result<(), ()> {
		let from = prog.get_bb(from);
		let fid = from.func();

		for succ in from.successors() {
			// TODO: having a "magical" invalid EA value is annoying.
			if succ.is_invalid() { continue; }

			match prog.span_at_ea(*succ).bb() {
				Some(succ_id) => {
					let succ = prog.get_bb(succ_id);
					if succ.func() == fid {
						self.visit(prog, succ_id, new_state)?;
					} else {
						// successor BB belongs to another function...
						// TODO: is there anything we need to do here??
					}
				}
				_ => {
					// no BB at the successor location...
					// TODO: is there anything we need to do here??
				}
			}
		}

		Ok(())
	}
}
