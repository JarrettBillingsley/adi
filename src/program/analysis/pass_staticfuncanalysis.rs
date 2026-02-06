
use std::collections::{ HashMap, HashSet };
use std::iter::{ IntoIterator} ;

use log::*;

use crate::dataflow::{ JoinSemiLattice, DataflowAlgorithm };
use crate::program::{ Program, BBId, BBTerm, FuncId, OpInfo, RefInfo, Operand };
use crate::arch::{ IArchitecture };
use crate::platform::{ IPlatform };
use crate::program::{ CfgPredecessors };
use crate::memory::{ MmuState, VA, EA, StateChange };
use crate::ir::{ ConstAddr, ConstAddrKind };

// ------------------------------------------------------------------------------------------------
// Function static analysis
// ------------------------------------------------------------------------------------------------

fn dump_func(prog: &Program, fid: FuncId) {
	let func = prog.get_func(fid);
	let mut bbs: Vec<BBId> = func.all_bbs().collect();
	bbs.sort_by_key(|bbid| prog.bbidx.get(*bbid).ea());
	for bbid in bbs {
		let bb = prog.bbidx.get(bbid);
		let state = bb.mmu_state_after();
		debug!("    {:?}:", bbid);
		for inst in bb.insts().iter() {
			debug!("      {} {}", inst.ea(), prog.inst_to_string(inst, state));
		}
	}
}

impl Program {
	pub(super) fn static_func_analysis_pass(&mut self, fid: FuncId) {
		info!("------------------------------------------------------------------------");
		info!("- begin function static analysis pass at {}", self.get_func(fid).ea());

		// dump_func(self, fid);

		let changes = self.func_find_state_changes(fid);
		self.func_apply_state_changes(fid, changes);

		let new_states = self.func_run_state_change_dataflow(fid);
		debug!("- applying new states");

		for (bbid, new_state) in new_states {
			self.bb_apply_new_state(bbid, new_state);
		}

		self.queue.enqueue_func_refs(fid);
	}

	/// part 1: construct IR, do constant propagation, and determine *where* state changes are
	fn func_find_state_changes(&mut self, fid: FuncId) -> Vec<(EA, MmuState)> {
		debug!("- finding state changes");

		let irfunc = self.func_to_ir(fid);
		// debug!("------------------------------------------------------------------");
		// debug!("Constants:");
		// let consts = irfunc.constants();
		// for (reg, (val, from)) in consts {
		// 	debug!("{:?} = {:08X} <from {:?}>", reg, val, from);
		// }
		// debug!("{:?}", irfunc);

		let addr_bits = self.plat.arch().addr_bits();

		// for tracking locations of MMU state changes.
		//
		// EA is the EA of the instruction which causes the state change; the new state takes effect
		// on the instruction *after* it.
		//
		// MmuState is that new state.

		let mut changes: Vec<(EA, MmuState)> = vec![];

		for ConstAddr { bbid, ea, opn, addr, kind, srcs } in irfunc.const_addrs() {
			// 1. add OpInfo::Ref to each constant operand
			let bb = self.bbidx.get_mut(bbid);
			let inst = bb.inst_at_ea_mut(ea).unwrap(); // safe because of above

			// let's double-check to make sure the arch is implemented properly.
			assert!(matches!(inst.get_op(opn), Operand::Mem(_, _) | Operand::Indir(_, _)),
				"only Mem and Indir operands are allowed to have constant addresses");

			// have to see if there is already a ref here - if it's to the same address, we may be
			// just accessing it in a different way (e.g. for an increment instruction, the IR will
			// break it into a load and a store - so we will have a RefInfo with a MemAccess::R on
			// it already, and we will have to add MemAccess::W to it)
			let access = match inst.get_opinfo(opn) {
				OpInfo::Ref { target: old_target, info: old_info } if *old_target == addr =>
					old_info.access.union(kind.access()),
				_ => kind.access() // otherwise it's just a brand new reference
			};

			// hi/lo come into play once we visit the instructions which computed this address.
			// TODO: I'm not totally confident that this is always absolute...
			let info = RefInfo::abs(addr_bits, access);
			*inst.get_opinfo_mut(opn) = OpInfo::Ref { target: addr, info };

			// 2. detect instruction state changes
			match kind {
				ConstAddrKind::Load | ConstAddrKind::Store(..) => {
					// The only instructions marked MemAccess::R/W are loads and stores which, in
					// the IR, do not have resolved EAs. So, the EA here must be unresolved, in
					// which case it can be turned back into a VA.
					assert!(addr.is_unresolved());

					let old_state = bb.mmu_state();
					let addr = VA(addr.offs());
					let val = if let ConstAddrKind::Store(val) = kind { val } else { None };
					let load = kind == ConstAddrKind::Load;

					match self.mem.state_change(old_state, addr, val, load) {
						StateChange::None => {
							trace!("  no state change at {}", ea);
						}
						StateChange::Dynamic => {
							trace!("  found a dynamic state change at {}", ea);
							// TODO: log this as a point of interest for user to investigate
							// TODO: if this BB already ends in BBTerm::StateChange, it needs to
							// be updated??? once BBs and StateChanges can understand this...
						}
						StateChange::Static(new_state) => {
							trace!("  found a static state change at {} to {:?}", ea, new_state);
							changes.push((ea, new_state));
						}
					}
				}

				ConstAddrKind::Target | ConstAddrKind::Offset => {}
			}

			// TODO: once constprop makes AST, recursively visit instructions based on `srcs`
			// (or have const_addrs do that for us)
			let _ = srcs;
		}

		// TODO: if any *existing* BBTerm::StateChange terminators were *not* seen in const_addrs,
		// mark them as Dynamic state changes. (this can happen on re-analysis where old analysis
		// *thought* they were Static but a revisit determines they're not...)
		// (cropped up in battletoads bankswitch function at EA 0003:00007F84 (PRG0:FF84))

		changes
	}

	/// part 2: split BBs at state change instructions
	fn func_apply_state_changes(&mut self, fid: FuncId, changes: Vec<(EA, MmuState)>) {
		debug!("- applying state changes");

		for (ea, new_state) in changes.into_iter() {
			let bbid = self.span_at_ea(ea).bb().expect("I mean it's in here cause it was in a BB");

			let inst = self.bbidx.get(bbid).inst_at_ea(ea)
				.expect("something went wrong with the IR EA mapping");

			// if the state-changing instruction is *not* the terminating instruction, split the BB.
			if inst.ea() != self.bbidx.get(bbid).term_ea() {
				match self.split_bb(bbid, inst.next_ea(), Some(fid)) {
					Ok(Some(new_bbid)) => {
						self.get_func_mut(fid).bbs.push(new_bbid);
					}
					Ok(None) => {} // it ok
					Err(_) => {
						unreachable!("this should literally be impossible");
					}
				}
			}

			let target = match self.bbidx.get(bbid).term() {
				BBTerm::FallThru(target) => *target,
				BBTerm::StateChange(target, old_new_state) => {
					assert_eq!(new_state, *old_new_state,
						"BB already had a StateChange terminator, but its old \"new state\" and \
						its new \"new state\" don't match");
					*target
				}
				_ => {
					unreachable!("either split_bb should have made this a FallThru, \
					or the original unsplit BB should have been a FallThru.");
				}
			};

			*self.bbidx.get_mut(bbid).term_mut() = BBTerm::StateChange(target, new_state);
			let bb = self.bbidx.get(bbid);
			debug!("rewrote terminator of BB {:?} @ {:?} to {:?}", bbid, bb.ea(), bb.term());
		}
	}

	/// part 3: run state change dataflow algorithm and determine new states for all BBs
	fn func_run_state_change_dataflow(&self, fid: FuncId) -> impl Iterator<Item = (BBId, StateInfo)> {
		debug!("- running state change dataflow");
		let func       = self.get_func(fid);
		let ana        = self.func_begin_analysis(func);
		let all_bbs    = ana.all_bbs().collect::<Vec<_>>();
		let preds      = self.func_bb_predecessors(&ana);
		let head_state = self.bbidx.get(func.head_id()).mmu_state();
		// vector of BBs which end in a `BBTerm::StateChange`, and the new MMU state that its
		// terminating instruction produced, to be propagated to its successors (note: NOT the new
		// MMU state for the BEGINNING of that BB!!)
		let mut to_propagate: Vec<(BBId, MmuState)> = vec![];

		trace!("  BB EAs:");
		for bbid in all_bbs.iter() {
			trace!("    {:?} @ {:?}", bbid, self.bbidx.get(*bbid).ea());

			if let BBTerm::StateChange(_, new_state) = self.bbidx.get(*bbid).term() {
				to_propagate.push((*bbid, *new_state));
			}
		}

		let mut flow = StateFlow::new(preds, &all_bbs, ana.head_id(), head_state, &to_propagate);
		flow.run(ana.cfg());
		flow.dump_state("final", &all_bbs);
		flow.into_iter()
	}

	/// apply new state from the dataflow algorithm to a BB
	fn bb_apply_new_state(&mut self, bbid: BBId, new_state: StateInfo) {
		match new_state {
			StateInfo::Unk => {
				// I could see this happening if there is a disconnected BB in the CFG
				// but uhhhhhhh that shouldn't happen right now
				panic!("this shouldn't be possible? flow algo determined unknown state...");
			}
			StateInfo::Some(new_state) => {
				self.bbidx.get_mut(bbid).set_mmu_state(new_state);
				self.resolve_unresolved_terminator(new_state, bbid);
			}
			StateInfo::Multi(states) => {
				let new_state = *states.iter().next().unwrap();
				warn!("multiple possible states found for BB {:?} at {}: {:?}; picking \
					state {:?} for it arbitrarily",
					bbid, self.bbidx.get(bbid).ea(), states, new_state);
				// TODO: point of interest for user to investigate

				// TODO: this is just temporary code!!!!!! until BBs can have multi-states
				self.bbidx.get_mut(bbid).set_mmu_state(new_state);
				self.resolve_unresolved_terminator(new_state, bbid);
			}
		}
	}

	/// attempt to resolve an unresolved terminator, now that new state is known
	fn resolve_unresolved_terminator(&mut self, state: MmuState, bbid: BBId) {
		let mut term = self.bbidx.get(bbid).term().clone();
		let mut changed = false;

		for target in term.successors_mut() {
			let old_target = *target;

			if target.is_unresolved() {
				*target = self.resolve_target(state, VA(old_target.offs()));
				// the above *could* still fail!
				if old_target != *target {
					changed = true;
					debug!("resolved terminator from {:?} to {:?}", old_target, *target);
				}
			}
		}

		if changed {
			debug!("changed terminator of {:?} to {:?}", bbid, term);
			*self.bbidx.get_mut(bbid).term_mut() = term;
		}
	}
}

// ------------------------------------------------------------------------------------------------
// StateInfo
// ------------------------------------------------------------------------------------------------

#[derive(Debug, PartialEq, Eq, Clone)]
enum StateInfo {
	Unk,
	Some(MmuState),
	Multi(HashSet<MmuState>),
	// TODO: Dynamic (once those are actually representable in BasicBlock...)
}

impl JoinSemiLattice for StateInfo {
	fn join(&mut self, other: &Self) -> bool {
		use StateInfo::*;

		let new = match (&self, &other) {
			(Unk, x)                     => (*x).clone(),
			(x, Unk)                     => (*x).clone(),
			(Some(a), Some(b)) if a == b => Some(*a),
			(Some(a), Some(b))           => Multi(HashSet::from([*a, *b])),
			(Some(a), Multi(m))          => Multi({ let mut s = m.clone(); s.insert(*a); s}),
			(Multi(m), Some(a))          => Multi({ let mut s = m.clone(); s.insert(*a); s}),
			(Multi(m1), Multi(m2))       => Multi(m1.union(m2).copied().collect()),
		};

		if *self != new {
			*self = new;
			trace!("    instate changed to {:?}", self);
			true
		} else {
			false
		}
	}
}

// ------------------------------------------------------------------------------------------------
// StateFlow
// ------------------------------------------------------------------------------------------------

struct StateFlow<'f> {
	preds:    &'f CfgPredecessors,
	instate:  HashMap<BBId, StateInfo>,
	outstate: HashMap<BBId, StateInfo>,
}

impl<'f> StateFlow<'f> {
	fn new(
		preds: &'f CfgPredecessors,
		all_bbs: &[BBId],
		head_id: BBId,
		head_state: MmuState,
		to_propagate: &[(BBId, MmuState)],
	) -> Self {
		// seed instates and outstates of every BB in the function.
		let mut instate = HashMap::new();
		let mut outstate = HashMap::new();

		// first, set everything to unknown.
		for &bbid in all_bbs {
			instate.insert(bbid, StateInfo::Unk);
			outstate.insert(bbid, StateInfo::Unk);
		}

		// then, for the head block, set its instate to the known head state,
		// and its outstate to unknown (outstate may be overridden by following loop).
		instate.insert(head_id, StateInfo::Some(head_state));
		outstate.insert(head_id, StateInfo::Unk);

		// finally, for all the state changes determined by const prop, set those as
		// the outstates for those blocks.
		for (bbid, new_state) in to_propagate.iter() {
			outstate.insert(*bbid, StateInfo::Some(*new_state));
		}

		let ret = Self { preds, instate, outstate };
		ret.dump_state("initial", all_bbs);
		ret
	}

	fn dump_state(&self, kind: &str, all_bbs: &[BBId]) {
		trace!("");
		trace!("  {} state:", kind);
		for bbid in all_bbs {
			trace!("    {:?} in = {:?}, out = {:?}",
				bbid, self.instate.get(bbid).unwrap(), self.outstate.get(bbid).unwrap());
		}
		trace!("");
	}

	fn transfer(&mut self, id: BBId) -> bool {
		trace!("  transferring across {:?}", id);

		// both safe because ctor added every BB in this function
		let instate = self.instate.get(&id).unwrap();
		let outstate = self.outstate.get_mut(&id).unwrap();

		match outstate {
			StateInfo::Unk => {
				*outstate = instate.clone();
				trace!("    outstate changed to {:?}", outstate);
				true
			}
			_ => {
				false
			}
		}
	}

	fn into_iter(self) -> impl Iterator<Item = (BBId, StateInfo)> {
		self.instate.into_iter()
	}
}

impl<'f> DataflowAlgorithm for StateFlow<'f> {
	type ID = BBId;

	fn visit(&mut self, id: Self::ID) -> bool {
		trace!("  joining preds of {:?}", id);

		// safe because all BBs were added to this map in the ctor
		let instate = self.instate.get_mut(&id).unwrap();
		let mut changed = false;

		// safe because `func_bb_predecessors` puts all BBs in the map
		for p in self.preds.get(&id).unwrap() {
			// safe because blah blah you get it
			changed |= instate.join(self.outstate.get(p).unwrap());
		}

		changed |= self.transfer(id);
		changed
	}
}