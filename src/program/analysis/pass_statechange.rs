
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
// MMU state change analysis
// ------------------------------------------------------------------------------------------------

impl Program {
	pub(super) fn state_change_pass(&mut self, fid: FuncId) {
		trace!("------------------------------------------------------------------------");
		trace!("- begin func state change analysis at {}", self.get_func(fid).ea());

		// --------------------------------------------------------
		// part 1: construct IR, do constant propagation, and determine *where* state changes are

		let irfunc = self.func_to_ir(fid);
		// println!("------------------------------------------------------------------");
		// println!("Constants:");
		// let consts = irfunc.constants();
		// for (reg, (val, from)) in consts {
		// 	println!("{:?} = {:08X} <from {:?}>", reg, val, from);
		// }
		// println!("{:?}", irfunc);

		let addr_bits = self.plat.arch().addr_bits();

		// for tracking locations of MMU state changes.
		//
		// EA is the EA of the instruction which causes the state change; the new state takes effect
		// on the instruction *after* it.
		//
		// MmuState is that new state.
		let mut changes: Vec<(EA, MmuState)> = vec![];

		for /*aaa@*/ ConstAddr { bbid, ea, opn, addr, kind, srcs } in irfunc.const_addrs() {
			// aaa.dump();

			// 1. add OpInfo::VARef to each constant operand
			let bb = self.bbidx.get_mut(bbid);
			let inst = bb.inst_at_ea_mut(ea).unwrap(); // safe because of above

			// let's double-check to make sure the arch is implemented properly.
			let op = inst.get_op(opn);
			match op {
				Operand::Mem(_, _) | Operand::Indir(_, _) => {}
				Operand::Reg(_) =>
					panic!("this shouldn't be possible. should it be Operand::Indir?"),
				Operand::UImm(_, _) | Operand::SImm(_, _) =>
					panic!("this shouldn't be possible. should it be Operand::Mem?"),
			};

			// hi/lo come into play once we visit the instructions which computed this address.
			// TODO: I'm not totally confident that this is always absolute...
			let info = RefInfo::abs(addr_bits, kind.access());
			*inst.get_opinfo_mut(opn) = if addr.is_resolved() {
				OpInfo::Ref { target: addr, info }
			} else {
				OpInfo::VARef { target: VA(addr.offs()), info }
			};

			// 2. detect instruction state changes
			let val = if let ConstAddrKind::Store(val) = kind { val } else { None };

			match kind {
				ConstAddrKind::Load | ConstAddrKind::Store(..) => {
					let old_state = bb.mmu_state();
					let load = kind == ConstAddrKind::Load;

					// The only instructions marked MemAccess::R/W are loads and stores which, in
					// the IR, do not have resolved EAs. So, the EA here must be unresolved, in
					// which case it can be turned back into a VA.
					assert!(addr.is_unresolved());
					let addr = VA(addr.offs());

					match self.mem.state_change(old_state, addr, val, load) {
						StateChange::None => {
							trace!("  no state change at {}", ea);
						}
						StateChange::Dynamic => {
							trace!("  found a dynamic state change at {}", ea);
							// TODO: log this as a point of interest for user to investigate
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

		// --------------------------------------------------------
		// part 2: split BBs at state change instructions

		// we're about to change the function, so let's drop this so we don't accidentally use the
		// outdated IR
		drop(irfunc);

		// vector of BBs which now end in a `BBTerm::StateChange`, and the new MMU state that its
		// terminating instruction produced, to be propagated to its successors (note: NOT the new
		// MMU state for the BEGINNING of that BB!!)
		let mut to_propagate: Vec<(BBId, MmuState)> = vec![];

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
				_ => unreachable!("either split_bb should have made this a FallThru,\
					or the original unsplit BB should have been a FallThru.")
			};

			*self.bbidx.get_mut(bbid).term_mut() = BBTerm::StateChange(target, new_state);
			to_propagate.push((bbid, new_state));
		}

		// --------------------------------------------------------
		// part 3: propagate state changes determined above

		// 1. run the dataflow algorithm
		let func       = self.get_func(fid);
		let ana        = self.func_begin_analysis(func);
		let all_bbs    = ana.all_bbs().collect::<Vec<_>>();
		let preds      = self.func_bb_predecessors(&ana);
		let head_state = self.bbidx.get(func.head_id()).mmu_state();
		let mut flow = StateFlow::new(preds, &all_bbs, ana.head_id(), head_state, &to_propagate);
		flow.run(ana.cfg());
		flow.dump_state("final", &all_bbs);

		// 2. apply the changes (if possible)
		for (bbid, new_state) in flow.into_iter() {
			match new_state {
				StateInfo::Unk => {
					// I could see this happening if there is a disconnected BB in the CFG
					// but uhhhhhhh that shouldn't happen right now
					panic!("this shouldn't be possible? flow algo determined unknown state...");
				}
				StateInfo::Some(new_state) => {
					self.bbidx.get_mut(bbid).set_mmu_state(new_state);
					// we might be able to resolve unresolved EAs in the terminator.
					let changed = self.resolve_unresolved_terminator(new_state, bbid);

					if changed {
						trace!("changed terminator of {:?}", bbid);
					}
				}
				StateInfo::Multi(states) => {
					let new_state = *states.iter().next().unwrap();
					warn!("multiple possible states found for BB {:?} at {}: {:?}; picking \
						state {:?} for it arbitrarily",
						bbid, self.bbidx.get(bbid).ea(), states, new_state);
					// TODO: point of interest for user to investigate

					// TODO: this is just temporary code!!!!!!
					self.bbidx.get_mut(bbid).set_mmu_state(new_state);
					// we might be able to resolve unresolved EAs in the terminator.
					let changed = self.resolve_unresolved_terminator(new_state, bbid);

					if changed {
						trace!("changed terminator of {:?}", bbid);
					}
				}
			}
		}

		// and set this function up for its refs pass.
		self.queue.enqueue_func_refs(fid);
	}

	fn resolve_unresolved_terminator(&mut self, state: MmuState, bbid: BBId) -> bool {
		let mut term = self.bbidx.get(bbid).term().clone();
		let mut changed = false;

		for target in term.successors_mut() {
			let old_target = *target;

			if target.is_unresolved() {
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

// ------------------------------------------------------------------------------------------------
// NEW SHIT
// ------------------------------------------------------------------------------------------------

#[derive(Debug, PartialEq, Eq, Clone)]
enum StateInfo {
	Unk,
	Some(MmuState),
	Multi(HashSet<MmuState>),
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

		trace!("- begin state flow");
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