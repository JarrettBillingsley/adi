
use std::collections::{ HashMap };
use std::iter::{ IntoIterator} ;

use log::*;

use crate::program::{ Program, BBId, BBTerm, Function, FuncId, OpInfo, RefInfo, Operand };
use crate::arch::{ IArchitecture };
use crate::platform::{ IPlatform };
use crate::memory::{ MmuState, VA, EA, MemAccess, StateChange };
use crate::ir::{ ConstAddr };

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
		let mut changes: Vec<(EA, MmuState)> = vec![];

		for /*aaa@*/ ConstAddr { bbid, ea, opn, addr, val, access, srcs } in irfunc.const_addrs() {
			// aaa.dump();

			// 1. add OpInfo::VARef to each constant operand
			let bb = self.bbidx.get_mut(bbid);
			let next_ea = bb.inst_at_ea(ea)
				.expect("something went wrong with the IR EA mapping")
				.next_ea();
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

			*inst.get_opinfo_mut(opn) = OpInfo::VARef {
				target: addr,
				// hi/lo come into play once we visit the instructions which computed this address.
				// TODO: I'm not totally confident that this is always absolute...
				info: RefInfo::abs(addr_bits),
			};

			// 2. detect instruction state changes
			match access {
				MemAccess::R | MemAccess::W => {
					let old_state = bb.mmu_state();
					let load = access == MemAccess::R;
					match self.mem.state_change(old_state, addr, val, load) {
						StateChange::None              => {
							trace!("no state change at {}", ea);

						} // no change
						StateChange::Maybe             => {
							trace!("found a maybe state change at {}", ea);
							// TODO: log this as a point of interest for user to investigate
							// TOOD: also is this even possible in this new model? Maybe was for
							// the old model where the state change looked at the macro-instruction.
							// here, we know exactly what address is being used.
						}
						StateChange::Dynamic           => {
							trace!("found a dynamic state change at {}", ea);
							// TODO: log this as a point of interest for user to investigate
						}
						StateChange::Static(new_state) => {
							trace!("found a static state change at {} to {:?}", ea, new_state);
							changes.push((next_ea, new_state));
						}
					}
				}

				MemAccess::Target | MemAccess::Offset => {}
				_ => unreachable!("const_addrs gave a bad access"),
			}

			// TODO: once constprop makes AST, recursively visit instructions based on `srcs`
			let _ = srcs;
		}

		// --------------------------------------------------------
		// part 2: split BBs at state change instructions

		// we're about to change the function, so let's drop this so we don't accidentally use the
		// outdated IR (TODO: should IrFunction hold a ref to the owning function to prevent issues
		// like this?)
		drop(irfunc);

		// vector of BBs which now end in a `BBTerm::BankChange`, and the new MMU state that its
		// terminating instruction produced, to be propagated to its successors (note: NOT the new
		// MMU state for the BEGINNING of that BB!!)
		let mut to_propagate: Vec<(BBId, MmuState)> = vec![];

		for (ea, new_state) in changes.into_iter() {
			let bbid = self.span_at_ea(ea).bb().expect("I mean it's in here cause it was in a BB");

			match self.split_bb(bbid, ea, Some(fid)) {
				Ok(Some(new_bbid)) => {
					self.get_func_mut(fid).bbs.push(new_bbid);

					let ea = match self.bbidx.get(bbid).term() {
						BBTerm::FallThru(ea) => *ea,
						_ => unreachable!("split_bb should have made this a FallThru")
					};

					*self.bbidx.get_mut(bbid).term_mut() = BBTerm::BankChange(ea);
					to_propagate.push((bbid, new_state));
				}
				Ok(None) => {} // it ok
				Err(_) => {
					unreachable!("this should literally be impossible");
				}
			}
		}

		// --------------------------------------------------------
		// part 3: propagate state changes determined above

		// 1. make a list of "new states" for each bb.
		let func = self.get_func(fid);
		let mut new_states = BBStateChanger::new(func);

		// 2. for each one, propagate that state change to the successors.
		for (bbid, new_state_after) in to_propagate {
			trace!("  new state being propagated from {:?}: {:?}", bbid, new_state_after);
			match new_states.propagate(self, bbid, new_state_after) {
				Ok(()) => {}
				Err(()) => {} // TODO: there were conflicting states
			}
		}

		// 3. Finally, apply the changes.
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

// ------------------------------------------------------------------------------------------------
// Helper object for tracking and propagating state changes through a function's BB CFG
// ------------------------------------------------------------------------------------------------

#[derive(Debug)]
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
				trace!("    conflicting states @ {} (changing to both {:?} and {:?})",
					ea, ns, new_state);
				Err(())
			} else {
				trace!("    setting new state of {:?} to {:?}", from, new_state);
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
