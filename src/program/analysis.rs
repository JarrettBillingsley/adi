
use std::collections::{ VecDeque, HashMap };
use std::iter::IntoIterator;

use log::*;

use crate::arch::{ IInterpreter, IArchitecture, ValueKind };
use crate::platform::{ IPlatform };
use crate::program::{ Instruction, InstructionKind, Program, BBId, BasicBlock, BBTerm, Function,
	FuncId };
use crate::memory::{ MmuState, Location, ImageSliceable, SpanKind, VA, StateChange, SegId };

// ------------------------------------------------------------------------------------------------
// Analyzer
// ------------------------------------------------------------------------------------------------

/// Things that can be put onto the analysis queue.
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub(crate) enum AnalysisItem {
	/// Explore an unexplored function.
	NewFunc(Location, MmuState),
	/// This function changes MMU state somehow; we have to figure out how.
	DetermineStateChange(FuncId),
	/// Any MMU state changes have been propagated; now to resolve references outside the function.
	FuncRefs(FuncId),
	/// A jump table. The location points to the jump instruction.
	JumpTable(Location),
}

impl Program {
	/// Puts a location on the queue that should be the start of a function.
	pub fn enqueue_function(&mut self, state: MmuState, loc: Location) {
		self.queue.push_back(AnalysisItem::NewFunc(loc, state))
	}

	/// Puts a location on the queue that should be the jump instruction for a jump table.
	pub fn enqueue_jump_table(&mut self, loc: Location) {
		self.queue.push_back(AnalysisItem::JumpTable(loc))
	}

	/// Analyzes all items in the analysis queue. Analysis may generate more items to analyze,
	/// so this can do a lot of work in a single call.
	pub fn analyze_queue(&mut self) {
		while let Some(item) = self.queue.pop_front() {
			use AnalysisItem::*;
			match item {
				NewFunc(loc, state)       => self.func_first_pass(loc, state),
				FuncRefs(fid)             => self.func_refs(fid),
				JumpTable(loc)            => self.analyze_jump_table(loc),
				DetermineStateChange(fid) => self.determine_state_change(fid),
			}
		}
	}

	// ---------------------------------------------------------------------------------------------
	// First pass

	pub(crate) fn func_first_pass(&mut self, loc: Location, state: MmuState) {
		trace!("------------------------------------------------------------------------");
		trace!("- begin function 1st pass at {}", loc);

		if !self._should_analyze_func(loc) {
			return;
		}

		// first pass is to build up the CFG. we're just looking for control flow
		// and finding the boundaries of this function.
		let mut bbs            = Vec::<BBId>::new();
		let mut potential_bbs  = VecDeque::<Location>::new();
		let mut changes_state  = false;
		potential_bbs.push_back(loc);

		'outer: while let Some(start) = potential_bbs.pop_front() {
			trace!("evaluating potential bb at {}", start);

			// ok. should we analyze this?
			if !self._should_analyze_bb(&mut bbs, start) {
				continue 'outer;
			}

			// yes. mark the span as under analysis.
			self.span_begin_analysis(start);

			// now, we need the actual data.
			let (seg, span)  = self.seg_and_span_at_loc(start);
			let slice        = seg.image_slice(span).into_data();
			let va           = self.va_from_loc(state, start);

			// let's start disassembling instructions
			let dis          = self.plat().arch().new_disassembler();
			let mut end_loc  = start;
			let mut term     = None;
			let mut insts    = Vec::new();
			let mut iter     = dis.disas_all(slice, state, va, start);

			'instloop: for inst in &mut iter {
				// trace!("{:04X} {:?}", inst.va(), inst.bytes());
				end_loc = inst.next_loc();

				if self.mem.inst_state_change(state, &inst).is_some() {
					changes_state = true;
					term = Some(BBTerm::BankChange(end_loc));
					potential_bbs.push_back(inst.next_loc());
					insts.push(inst);
					break 'instloop;
				}

				use InstructionKind::*;
				match inst.kind() {
					Invalid => panic!("disas_all gave an invalid instruction"),
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
						let target_loc = self._va_to_loc_in_same_seg(seg.id(), state, target_va);

						let next = inst.next_loc();
						potential_bbs.push_back(next);

						// debug!("{:04X} t: {} next: {}", inst.va(), target_loc, next);

						term = Some(BBTerm::Call { dst: target_loc, ret: next });
					}
					Uncond | Cond => {
						let target_va = inst.control_target().expect("should have control target");
						let target_loc = self._va_to_loc_in_same_seg(seg.id(), state, target_va);

						// if it's into the same segment, it might be part of this function.
						// if not, it's probably a tailcall to another function.
						if seg.contains_loc(target_loc) {
							potential_bbs.push_back(target_loc);
						}

						if inst.kind() == Uncond {
							term = Some(BBTerm::Jump(target_loc));
						} else {
							let next = inst.next_loc();
							potential_bbs.push_back(next);

							// debug!("{:04X} t: {} next: {}", inst.va(), target_loc, next);

							term = Some(BBTerm::Cond { t: target_loc, f: next });
						}
					}
				}

				// this is down here for BORROWING REASONS
				insts.push(inst);
				break 'instloop;
			}

			if start == end_loc {
				// oop. no good.
				trace!("{} is NO GOOD", start);
				self.span_cancel_analysis(start);
			} else {
				if let Some(..) = iter.err() {
					assert!(end_loc == iter.err_loc());
					term = Some(BBTerm::DeadEnd);
				} else if term.is_none() {
					// we got through the whole slice with no errors, meaning
					// this is a fallthrough to the next bb.
					term = Some(BBTerm::FallThru(end_loc));
				}

				let bb_id = self._new_bb(&mut bbs, start, term.unwrap(), insts, state);
				self.span_end_analysis(start, end_loc, SpanKind::AnaCode(bb_id));
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
			if changes_state {
				self.queue.push_back(AnalysisItem::DetermineStateChange(fid));
			} else {
				self.queue.push_back(AnalysisItem::FuncRefs(fid));
			}
		}
	}

	// ---------------------------------------------------------------------------------------------
	// Refs pass

	pub(crate) fn func_refs(&mut self, fid: FuncId) {
		let func = self.get_func(fid);

		trace!("------------------------------------------------------------------------");
		trace!("- begin function 2nd pass at {}", func.loc());

		let mut jumptables = Vec::new();
		let mut funcs      = Vec::new();
		let mut refs       = Vec::new();

		for bb in func.all_bbs() {
			let bb = self.bbidx.get(bb);

			let state = bb.mmu_state();
			for inst in bb.insts() {
				let src = inst.loc();

				for i in 0 .. inst.num_ops() {
					let op = inst.get_op(i);

					if op.has_addr() {
						let dst = self.loc_from_va(state, op.addr());
						refs.push((src, dst));
					}
				}

				use InstructionKind::*;
				match inst.kind() {
					Indir => jumptables.push(inst.loc()),
					Call => {
						let target_loc = self._resolve_target(state, inst.control_target().unwrap());
						funcs.push((target_loc, state));
					}
					_ => {}
				}
			}

			for &succ in bb.explicit_successors() {
				refs.push((bb.term_loc(), succ));
			}
		}

		for (src, dst) in refs.into_iter() { self.add_ref(src, dst); }
		for t in jumptables.into_iter()    { self.enqueue_jump_table(t);  }
		for (f, s) in funcs.into_iter()    { self.enqueue_function(s, f); }
	}

	// ---------------------------------------------------------------------------------------------
	// MMU state change analysis

	pub(crate) fn determine_state_change(&mut self, fid: FuncId) {
		let func = self.get_func(fid);

		trace!("------------------------------------------------------------------------");
		trace!("- begin func state change analysis at {}", func.loc());

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
				StateChange::Dynamic           => self._dynamic_state_change(func, bb, state),
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
			}
		}

		// 5. Finally, apply the changes.
		for (bbid, new_state) in new_states.into_iter() {
			self.bbidx.get_mut(bbid).set_mmu_state(new_state);
		}

		// 6. And set this function up for its refs pass.
		self.queue.push_back(AnalysisItem::FuncRefs(fid));
	}

	// ---------------------------------------------------------------------------------------------
	// Jump table analysis

	pub(crate) fn analyze_jump_table(&mut self, loc: Location) {
		trace!("there's a jumptable at {}", loc);
		// todo!()
	}

	// ---------------------------------------------------------------------------------------------
	// Private

	fn _should_analyze_func(&self, loc: Location) -> bool {
		if let Some(bbid) = self.span_at_loc(loc).bb() {
			let bb = self.bbidx.get(bbid);
			let orig_func_head = self.get_func(bb.func()).head_id();

			if bbid == orig_func_head {
				// the beginning of an already-analyzed function.
				trace!("oh, I've seen this one already. NEVERMIIIND");
			} else {
				// the middle of an already-analyzed function. TODO: maybe we should
				// split the function, maybe not. Consider diamond-shaped CFG where we
				// call a BB in the middle of one path. who owns the BBs at/after
				// the rejoining point?
				trace!("middle of an existing function... let's move on");
			}
			false
		} else if self.segment_from_loc(loc).is_fake() {
			trace!("skipping function at invalid location: {:?}", loc);
			false
		} else {
			true
		}
	}

	fn _should_analyze_bb(&mut self, bbs: &mut Vec<BBId>, start: Location) -> bool {
		// let's look at this location to see what's here.
		// we want a fresh, undefined region of memory.

		match self.span_at_loc(start).kind() {
			SpanKind::Unk         => true, // yeeeeee that's what we want
			SpanKind::Code(..)    => false,   // fell through into another function.
			SpanKind::AnaCode(id) => {
				// oh, we've already analyzed this. but maybe we have some work to do...
				self._check_split_bb(bbs, id, start);
				false
			}

			SpanKind::Ana => panic!("span at {} is somehow being analyzed", start),
			SpanKind::Data(..) => todo!("uh oh. what do we do here? {}", start),
		}
	}

	fn _check_split_bb(&mut self, bbs: &mut Vec<BBId>, old_id: BBId, start: Location) {
		let old_bb = self.bbidx.get(old_id);

		if start != old_bb.loc {
			// ooh, now we have to split the existing bb.
			// first, let's make sure that `start` points to the beginning of an instruction,
			// because otherwise we'd be jumping to an invalid location.
			let idx = match old_bb.last_instr_before(start) {
				Some(idx) => idx,
				None      => {
					// todo!("have to flag referrer as being invalid somehow"),
					return;
				}
			};

			// now we can split the existing BB...
			let new_bb = self._split_bb(bbs, old_id, idx, start);

			// ...and update the span map.
			self.segment_from_loc_mut(start).split_span(start, SpanKind::AnaCode(new_bb));
		}
	}

	fn _new_bb(&mut self, bbs: &mut Vec<BBId>, loc: Location, term: BBTerm, insts: Vec<Instruction>,
	state: MmuState) -> BBId {
		log::trace!("new bb loc: {}, term: {:?}", loc, term);
		let bbid = self.bbidx.new_bb(loc, term, insts, state);
		bbs.push(bbid);
		bbid
	}

	// returns id of newly split-off BB
	fn _split_bb(&mut self, bbs: &mut Vec<BBId>, old_id: BBId, inst_idx: usize, new_start: Location)
	-> BBId {
		let old = self.bbidx.get_mut(old_id);
		let term_loc = old.insts[inst_idx].loc();
		let state = old.mmu_state();
		let insts = old.insts.split_off(inst_idx + 1);

		assert!(old.loc < new_start);
		assert!(term_loc < new_start);

		let new_id = self.bbidx.new_bb(
			new_start,
			BBTerm::FallThru(new_start), // NOT WRONG, they get swapped below.
			insts,
			state
		);

		let (old, new) = self.bbidx.get2_mut(old_id, new_id);
		std::mem::swap(&mut old.term, &mut new.term);

		log::trace!("split bb loc: {}, term: {:?}", new.loc, new.term);
		bbs.push(new_id);
		new_id
	}

	fn _va_to_loc_in_same_seg(&self, seg: SegId, state: MmuState, va: VA) -> Location {
		match self.loc_for_va(state, va) {
			Some(l) if l.seg == seg => l,
			_                       => Location::invalid(va.0)
		}
	}

	fn _resolve_target(&self, state: MmuState, target: VA) -> Location {
		match self.loc_for_va(state, target) {
			Some(l) => l,
			None    => Location::invalid(target.0),
		}
	}

	// _func if/when we need it in the future
	fn _dynamic_state_change(&self, _func: &Function, bb: &BasicBlock, state: Option<MmuState>)
	-> Option<MmuState> {
		match self._interp_bb(bb, state) {
			Some((new_state, kind)) => {
				if kind == ValueKind::Immediate {
					Some(new_state)
				} else {
					log::error!("interpreter found a value but can't yet resolve.");
					None
				}
			}

			// TODO: more, more!
			None => None
		}
	}

	fn _interp_bb(&self, bb: &BasicBlock, state: Option<MmuState>) -> Option<(MmuState, ValueKind)> {
		let mut interpreter = self.plat.arch().new_interpreter();
		interpreter.interpret_bb(&self.mem, bb, state);
		interpreter.last_mmu_state_change()
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

	fn new_state_for(&self, bb: BBId) -> Option<MmuState> {
		self.map[&bb].new_state
	}

	fn into_iter(self) -> impl Iterator<Item = (BBId, MmuState)> {
		self.map.into_iter().filter_map(|(bbid, cs)| cs.new_state.map(|ns| (bbid, ns)))
	}

	fn propagate(&mut self, prog: &Program, from: BBId, new_state: MmuState)
	-> Result<(), ()> {
		self.map.iter_mut().for_each(|(_, cs)| cs.visited = false);
		self._propagate_succ(prog, from, new_state)
	}

	fn _propagate_walk(&mut self, prog: &Program, from: BBId, new_state: MmuState)
	-> Result<(), ()> {
		if self.map[&from].visited {
			Ok(())
		} else {
			self.map.get_mut(&from).unwrap().visited = true;

			if let Some(ns) = self.map[&from].new_state {
				// uh oh. conflict
				let loc = prog.get_bb(from).loc();
				log::trace!("conflicting states @ {} (changing to both {:?} and {:?})",
					loc, ns, new_state);
				Err(())
			} else {
				self.map.get_mut(&from).unwrap().new_state = Some(new_state);
				self._propagate_succ(prog, from, new_state)
			}
		}
	}

	fn _propagate_succ(&mut self, prog: &Program, from: BBId, new_state: MmuState)
	-> Result<(), ()> {
		let from = prog.get_bb(from);
		let fid = from.func();

		for succ in from.successors() {
			// TODO: having a "magical" invalid location value is annoying.
			if succ.is_invalid() { continue; }

			match prog.span_at_loc(*succ).bb() {
				Some(succ_id) => {
					let succ = prog.get_bb(succ_id);
					if succ.func() == fid {
						if let Err(()) = self._propagate_walk(prog, succ_id, new_state) {
							return Err(());
						}
					} else {
						// TODO: is there anything we need to do here??
					}
				}
				_ => {
					// TODO: is there anything we need to do here??
				}
			}
		}

		Ok(())
	}
}