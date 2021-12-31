
use std::collections::{ VecDeque, HashMap };
use std::iter::IntoIterator;

use log::*;

use crate::arch::{ IArchitecture };
use crate::platform::{ IPlatform };
use crate::program::{ Instruction, InstructionKind, Program, BBId, BBTerm, Function, FuncId };
use crate::memory::{ MmuState, EA, ImageSliceable, SpanKind, VA, StateChange, SegId };

// ------------------------------------------------------------------------------------------------
// Analyzer
// ------------------------------------------------------------------------------------------------

/// Things that can be put onto the analysis queue.
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub(crate) enum AnalysisItem {
	/// Explore an unexplored function.
	NewFunc(EA, MmuState),
	/// This function changes MMU state somehow; we have to figure out how.
	StateChange(FuncId),
	/// Any MMU state changes have been propagated; now to resolve references outside the function.
	FuncRefs(FuncId),
	/// A jump table. The EA points to the jump instruction.
	JumpTable(EA),
	/// Split an existing function (another function called/jumped to the given EA).
	SplitFunc(EA),
}

impl Program {
	/// Puts an EA on the queue that should be the start of a function.
	pub fn enqueue_function(&mut self, state: MmuState, ea: EA) {
		self.queue.push_back(AnalysisItem::NewFunc(ea, state))
	}

	/// Puts an EA on the queue that should be the jump instruction for a jump table.
	pub fn enqueue_jump_table(&mut self, ea: EA) {
		self.queue.push_back(AnalysisItem::JumpTable(ea))
	}

	/// Schedules a function to have its references analyzed.
	fn enqueue_func_refs(&mut self, fid: FuncId) {
		self.queue.push_back(AnalysisItem::FuncRefs(fid));
	}

	/// Schedules a function to have its MMU state changes analyzed.
	fn enqueue_state_change(&mut self, fid: FuncId) {
		self.queue.push_back(AnalysisItem::StateChange(fid));
	}

	/// Schedules a function to attempt to be split at the given EA.
	fn enqueue_split_func(&mut self, ea: EA) {
		self.queue.push_back(AnalysisItem::SplitFunc(ea));
	}

	/// Analyzes all items in the analysis queue. Analysis may generate more items to analyze,
	/// so this can do a lot of work in a single call.
	pub fn analyze_queue(&mut self) {
		while let Some(item) = self.queue.pop_front() {
			use AnalysisItem::*;
			match item {
				NewFunc(ea, state) => self.func_first_pass(ea, state),
				StateChange(fid)   => self.determine_state_change(fid),
				FuncRefs(fid)      => self.func_refs(fid),
				JumpTable(ea)      => self.analyze_jump_table(ea),
				SplitFunc(ea)      => self.split_func(ea),
			}
		}
	}
}

// ------------------------------------------------------------------------------------------------
// First-pass function analysis
// ------------------------------------------------------------------------------------------------

impl Program {
	fn func_first_pass(&mut self, ea: EA, state: MmuState) {
		trace!("------------------------------------------------------------------------");
		trace!("- begin function 1st pass at {}", ea);

		if !self.should_analyze_func(ea) {
			return;
		}

		// first pass is to build up the CFG. we're just looking for control flow
		// and finding the boundaries of this function.
		let mut bbs            = Vec::<BBId>::new();
		let mut potential_bbs  = VecDeque::<EA>::new();
		potential_bbs.push_back(ea);

		'outer: while let Some(start) = potential_bbs.pop_front() {
			trace!("evaluating potential bb at {}", start);

			// ok. should we analyze this?
			if !self.should_analyze_bb(&mut bbs, start) {
				continue 'outer;
			}

			// yes. mark the span as under analysis.
			self.span_begin_analysis(start);

			// now, we need the actual data.
			let (seg, span)  = self.seg_and_span_at_ea(start);
			let slice        = seg.image_slice(span).into_data();
			let va           = self.va_from_ea(state, start);

			// let's start disassembling instructions
			let dis          = self.plat().arch().new_disassembler();
			let mut end_ea   = start;
			let mut term     = None;
			let mut insts    = Vec::new();
			let mut iter     = dis.disas_all(slice, state, va, start);

			'instloop: for inst in &mut iter {
				// trace!("{:04X} {:?}", inst.va(), inst.bytes());
				end_ea = inst.next_ea();

				use InstructionKind::*;
				match inst.kind() {
					Other => {
						insts.push(inst);
						continue 'instloop;
					}
					Ret => {
						// TODO: what about e.g. Z80 where you can have conditional returns?
						// should that end a BB?
						// well I think it should be like a conditional - yes, end this
						// BB, but push another BB of the next instruction.
						term = Some(BBTerm::Return);
					}
					Halt => {
						term = Some(BBTerm::Halt);
					}
					Indir => {
						term = Some(BBTerm::JumpTbl(vec![]));
					}
					Call => {
						let target_va = inst.control_target().expect("should have control target");
						let target_ea = self.va_to_ea_in_same_seg(seg.id(), state, target_va);

						let next = inst.next_ea();
						potential_bbs.push_back(next);

						// debug!("{:04X} t: {} next: {}", inst.va(), target_ea, next);

						term = Some(BBTerm::Call { dst: target_ea, ret: next });
					}
					Uncond | Cond => {
						let target_va = inst.control_target().expect("should have control target");
						let target_ea = self.va_to_ea_in_same_seg(seg.id(), state, target_va);

						// if it's into the same segment, it might be part of this function.
						// if not, it's probably a tailcall to another function.
						if seg.contains_ea(target_ea) {
							potential_bbs.push_back(target_ea);
						}

						if inst.kind() == Uncond {
							term = Some(BBTerm::Jump(target_ea));
						} else {
							let next = inst.next_ea();
							potential_bbs.push_back(next);

							// debug!("{:04X} t: {} next: {}", inst.va(), target_ea, next);

							term = Some(BBTerm::Cond { t: target_ea, f: next });
						}
					}
				}

				// this is down here for BORROWING REASONS
				insts.push(inst);
				break 'instloop;
			}

			if start == end_ea {
				// oop. no good.
				trace!("{} is NO GOOD", start);
				self.span_cancel_analysis(start);
			} else {
				if let Some(..) = iter.err() {
					assert!(end_ea == iter.err_ea());
					term = Some(BBTerm::DeadEnd);
				} else if term.is_none() {
					// we got through the whole slice with no errors, meaning
					// this is a fallthrough to the next bb.
					// if we got to the end of the *segment,* though, end_ea may be invalid.
					if end_ea.offs() >= seg.len() {
						term = Some(BBTerm::DeadEnd)
					} else {
						term = Some(BBTerm::FallThru(end_ea));
					}
				}

				let bbid = self.new_bb(start, term.unwrap(), insts, state);
				bbs.push(bbid);
				self.span_end_analysis(start, end_ea, SpanKind::AnaCode(bbid));
			}
		}

		assert!(potential_bbs.is_empty());

		// empty func can happen if the very first BB was NO GOOD and was canceled.
		if bbs.len() == 0 {
			trace!("NOPE that's not a good function.");
		} else {
			// now, make the function!!
			let fid = self.new_func(bbs);

			// finally, turn the crank by putting more work on the queue
			self.enqueue_state_change(fid);
		}
	}

	fn should_analyze_func(&mut self, ea: EA) -> bool {
		if let Some(bbid) = self.span_at_ea(ea).bb() {
			let bb = self.bbidx.get(bbid);
			let orig_func = self.get_func(bb.func());

			if ea == orig_func.ea() {
				// the beginning of an already-analyzed function.
				trace!("oh, I've seen this one already. NEVERMIIIND");
			} else {
				// the middle of an already-analyzed function.
				trace!("middle of an existing function...");
				self.enqueue_split_func(ea);
			}
			false
		} else if self.segment_from_ea(ea).is_fake() {
			trace!("skipping function at invalid EA: {:?}", ea);
			false
		} else {
			true
		}
	}

	fn should_analyze_bb(&mut self, bbs: &mut Vec<BBId>, start: EA) -> bool {
		// let's look at this EA to see what's here.
		// we want a fresh, undefined region of memory.

		match self.span_at_ea(start).kind() {
			SpanKind::Unk         => true, // yeeeeee that's what we want
			SpanKind::Code(..)    => false,   // fell through into another function.
			SpanKind::AnaCode(id) => {
				// oh, we've already analyzed this. we might have to split it though.
				if let Some(bbid) = self.check_split_bb(id, start, None) {
					bbs.push(bbid);
				}

				false
			}

			SpanKind::Ana => panic!("span at {} is somehow being analyzed", start),
			SpanKind::Data(..) => todo!("uh oh. what do we do here? {}", start),
		}
	}

	fn new_bb(&mut self, ea: EA, term: BBTerm, insts: Vec<Instruction>, state: MmuState) -> BBId {
		log::trace!("new bb ea: {}, term: {:?}", ea, term);
		self.bbidx.new_bb(ea, term, insts, state)
	}

	fn va_to_ea_in_same_seg(&self, seg: SegId, state: MmuState, va: VA) -> EA {
		match self.ea_for_va(state, va) {
			Some(l) if l.seg() == seg => l,
			_                         => EA::invalid(va.0)
		}
	}
}

// ------------------------------------------------------------------------------------------------
// MMU state change analysis
// ------------------------------------------------------------------------------------------------

impl Program {
	fn determine_state_change(&mut self, fid: FuncId) {
		let func = self.get_func(fid);

		trace!("------------------------------------------------------------------------");
		trace!("- begin func state change analysis at {}", func.ea());

		// 1. make a list of "new states" for each bb.
		let mut new_states = BBStateChanger::new(func);

		// 2. gather all the BBs that change banks.
		let changers = func.all_bbs()
			.filter(|&b| {
				let b = self.bbidx.get(b);
				matches!(b.term(), BBTerm::BankChange(..))
			})
			.collect::<Vec<_>>();

		// 3. for each one, try a few techniques to determine the new state.
		for bbid in changers {
			let bb = self.bbidx.get(bbid);
			// it's possible this BB's state was changed on a previous iteration.
			let state = new_states.new_state_for(bbid);

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

// ------------------------------------------------------------------------------------------------
// Refs pass
// ------------------------------------------------------------------------------------------------

impl Program {
	fn func_refs(&mut self, fid: FuncId) {
		let func = self.get_func(fid);

		trace!("------------------------------------------------------------------------");
		trace!("- begin function refs pass at {}", func.ea());

		let mut jumptables = Vec::new();
		let mut funcs      = Vec::new();
		let mut refs       = Vec::new();

		for bb in func.all_bbs() {
			let bb = self.bbidx.get(bb);

			let state = bb.mmu_state();
			for inst in bb.insts() {
				let src = inst.ea();

				for i in 0 .. inst.num_ops() {
					let op = inst.get_op(i);

					if op.has_addr() {
						let dst = self.ea_from_va(state, op.addr());
						refs.push((src, dst));
					}
				}

				use InstructionKind::*;
				match inst.kind() {
					Indir => jumptables.push(inst.ea()),
					Call | Cond | Uncond => {
						let target_ea = self.resolve_target(state, inst.control_target().unwrap());

						if target_ea.is_valid() {
							use SpanKind::*;
							let should_push = match self.span_at_ea(target_ea).kind() {
								Unk => true,
								Code(other_bbid) => self.get_bb(other_bbid).func() != func.id(),
								_ => false,
							};

							if should_push {
								funcs.push((target_ea, state));
							}
						}
					}
					_ => {}
				}
			}

			for &succ in bb.explicit_successors() {
				refs.push((bb.term_ea(), succ));
			}
		}

		for (src, dst) in refs.into_iter() { self.add_ref(src, dst); }
		for t in jumptables.into_iter()    { self.enqueue_jump_table(t);  }
		for (f, s) in funcs.into_iter()    { self.enqueue_function(s, f); }
	}
}

// ------------------------------------------------------------------------------------------------
// Jump table analysis
// ------------------------------------------------------------------------------------------------

impl Program {
	fn analyze_jump_table(&mut self, ea: EA) {
		trace!("there's a jumptable at {}", ea);
		// todo!()
	}
}

// ------------------------------------------------------------------------------------------------
// Splitting previously-analyzed functions
// ------------------------------------------------------------------------------------------------

impl Program {
	fn split_func(&mut self, ea: EA) {
		trace!("------------------------------------------------------------------------");
		trace!("- begin function splitting at {}", ea);

		let mut bbid = self.span_at_ea(ea).bb().expect("uh, there used to be a function here");
		let fid = self.bbidx.get(bbid).func();

		// first: do we have to split the target BB?
		if let Some(new_bbid) = self.check_split_bb(bbid, ea, Some(fid)) {
			// add it to the function's vec of BBs,
			self.get_func_mut(fid).bbs.push(new_bbid);
			// and now we're working with the new BB.
			bbid = new_bbid;
		}

		// TODO: technically, it *is* possible to split multi-entry functions, as long as
		// the head node dominates all the other entry points. but I don't care to deal with
		// that right now.
		if self.get_func(fid).is_multi_entry() {
			trace!(" function at {} is multi-entry already", self.get_func(fid).ea());
			self.get_func_mut(fid).try_add_entrypoint(bbid);
			return;
		}

		// idea: if this bb is the dominator of all its successors including descendants,
		// then we can split the function at `ea`.
		let ana           = self.func_begin_analysis(self.get_func(fid));
		let doms          = self.func_bb_dominators(&ana);
		let mut reachable = self.func_reachable_bbs(&ana, bbid);

		// this makes some subsequent operations simpler.
		reachable.remove(&bbid);

		// self.func_dump_cfg(&ana);
		// println!("trying to add entry point in {:?}", bbid);
		// println!("{:#?}", doms);
		// println!("reachable: {:#?}", reachable);

		// if this BB doesn't dominate all BBs in reachable (except itself), then we can't split.
		let mut can_split = true;

		for &n in reachable.iter() {
			let mut doms_of_n = doms.strict_dominators(n).expect("unreachable from start");

			if !doms_of_n.any(|d| d == bbid) {
				can_split = false;
				break;
			}
		}

		if can_split {
			// alright, we can split! conveniently, the reachable set is the set of BBs that the
			// new function will inherit, and bbid will become its head.

			// first, remove all the 'reachable' bbs from func
			self.get_func_mut(fid).bbs
				.retain(|&to_keep| to_keep != bbid && !reachable.contains(&to_keep));

			// then, turn 'reachable' into a vec, with bbid as the first item.
			let new_func_bbs = Some(bbid).into_iter()
				.chain(reachable.into_iter())
				.collect::<Vec<_>>();

			// last, make a new function out of the new_func_bbs, and change what function
			// they belong to.
			let new_fid = self.funcs.new_func(ea, new_func_bbs);

			for &bb in &self.funcs.get(new_fid).bbs {
				self.bbidx.get_mut(bb).change_func(new_fid);
			}

			trace!(" split off new function {:?} at {}.", new_fid, ea);
		} else {
			// otherwise, give up and mark it a multi-entry function.
			trace!(" can't split, marking function at {} as multi-entry", self.get_func(fid).ea());
			self.get_func_mut(fid).try_add_entrypoint(bbid);
		}
	}
}

// ------------------------------------------------------------------------------------------------
// Misc helpers
// ------------------------------------------------------------------------------------------------

impl Program {
	fn check_split_bb(&mut self, old_id: BBId, start: EA, owner: Option<FuncId>) -> Option<BBId> {
		let old_bb = self.bbidx.get(old_id);

		if start != old_bb.ea {
			// ooh, now we have to split the existing bb.
			// first, let's make sure that `start` points to the beginning of an instruction,
			// because otherwise we'd be jumping to an invalid EA.
			let idx = match old_bb.last_instr_before(start) {
				Some(idx) => idx,
				None      => {
					log::warn!("splitting bb at {} failed", old_bb.ea);
					// todo!("have to flag referrer as being invalid somehow"),
					return None;
				}
			};

			// now we can split the existing BB...
			let new_bb = self.split_bb(old_id, idx, start);

			// ...and update the span map.
			let span_kind = if let Some(fid) = owner {
				// (oh and, fill in the owner)
				self.bbidx.get_mut(new_bb).mark_complete(fid);
				SpanKind::Code(new_bb)
			} else {
				SpanKind::AnaCode(new_bb)
			};

			self.segment_from_ea_mut(start).split_span(start, span_kind);

			Some(new_bb)
		} else {
			None
		}
	}

	// returns id of newly split-off BB
	fn split_bb(&mut self, old_id: BBId, inst_idx: usize, new_start: EA) -> BBId {
		let old = self.bbidx.get_mut(old_id);
		let term_ea = old.insts[inst_idx].ea();
		let state = old.mmu_state();
		let insts = old.insts.split_off(inst_idx + 1);

		assert!(old.ea < new_start);
		assert!(term_ea < new_start);

		let new_id = self.bbidx.new_bb(
			new_start,
			BBTerm::FallThru(new_start), // NOT WRONG, they get swapped below.
			insts,
			state
		);

		let (old, new) = self.bbidx.get2_mut(old_id, new_id);
		std::mem::swap(&mut old.term, &mut new.term);

		log::trace!("split bb new id: {:?} ea: {}, term: {:?}", new_id, new.ea, new.term);
		new_id
	}

	fn resolve_target(&self, state: MmuState, target: VA) -> EA {
		match self.ea_for_va(state, target) {
			Some(l) => l,
			None    => EA::invalid(target.0),
		}
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