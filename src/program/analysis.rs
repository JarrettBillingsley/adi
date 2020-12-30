
use std::collections::VecDeque;
use std::iter::IntoIterator;

use log::*;

use crate::arch::{ IArchitecture, ValueKind };
use crate::platform::{ IPlatform };
use crate::program::{ InstructionKind, Program, BasicBlock, BBTerm, Function, FuncId };
use crate::memory::{ MmuState, Location, ImageSliceable, SpanKind, VA, StateChange };

// ------------------------------------------------------------------------------------------------
// Analyzer
// ------------------------------------------------------------------------------------------------

/// Things that can be put onto an `Analyzer`'s analysis queue.
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub(crate) enum AnalysisItem {
	/// An unexplored function.
	Func1stPass(Location, MmuState),
	/// A function that needs to be analyzed more deeply.
	Func2ndPass(FuncId),
	FuncBankChange(FuncId),
	/// A jump table. The location points to the jump instruction.
	JumpTable(Location),
}

impl Program {
	/// Puts a location on the queue that should be the start of a function.
	pub fn enqueue_function(&mut self, state: MmuState, loc: Location) {
		self.queue.push_back(AnalysisItem::Func1stPass(loc, state))
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
				Func1stPass(loc, state) => self.func_first_pass(loc, state),
				Func2ndPass(fid)        => self.func_second_pass(fid),
				FuncBankChange(fid)     => self.func_bank_change(fid),
				JumpTable(loc)          => self.analyze_jump_table(loc),
			}
		}
	}

	// ---------------------------------------------------------------------------------------------

	fn should_analyze_func(&self, loc: Location) -> bool {
		if let Some(bbid) = self.span_at_loc(loc).bb() {
			let orig_func_head = self.get_func(bbid.func()).head_id();

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
		} else {
			true
		}
	}

	fn should_analyze_bb(
		&mut self, func: &mut Function, start: Location) -> bool {
		// let's look at this location to see what's here.
		// we want a fresh, undefined region of memory.

		match self.span_at_loc(start).kind() {
			SpanKind::Unk         => true, // yeeeeee that's what we want
			SpanKind::Code(..)    => false,   // fell through into another function.
			SpanKind::AnaCode(id) => {
				// oh, we've already analyzed this. but maybe we have some work to do...
				self.check_split_bb(func, id, start);
				false
			}

			SpanKind::Ana => panic!("span at {} is somehow being analyzed", start),
			SpanKind::Data => todo!("uh oh. what do we do here? {}", start),
		}
	}

	fn check_split_bb(
		&mut self, func: &mut Function, old_idx: usize, start: Location) {
		let old_bb = func.get_bb_by_idx(old_idx);

		if start != old_bb.loc {
			// ooh, now we have to split the existing bb.
			// first, let's make sure that `start` points to the beginning of an instruction,
			// because otherwise we'd be jumping to an invalid location.
			let idx = match old_bb.last_instr_before(start) {
				Some(idx) => idx,
				None      => todo!("have to flag referrer as being invalid somehow"),
			};

			// now we can split the existing BB...
			let new_bb = func.split_bb(old_idx, idx, start);

			// ...and update the span map.
			self.segment_from_loc_mut(start).split_span(start, SpanKind::AnaCode(new_bb));
		}
	}

	fn resolve_target(&self, state: MmuState, target: VA) -> Location {
		match self.loc_for_va(state, target) {
			Some(l) => l,
			None    => Location::invalid(target.0),
		}
	}

	// ---------------------------------------------------------------------------------------------
	// First pass

	pub(crate) fn func_first_pass(&mut self, loc: Location, state: MmuState) {
		trace!("------------------------------------------------------------------------");
		trace!("- begin function 1st pass at {}", loc);

		if !self.should_analyze_func(loc) {
			return;
		}

		// first pass is to build up the CFG. we're just looking for control flow
		// and finding the boundaries of this function.
		let mut func           = Function::new_inprogress();
		let mut potential_bbs  = VecDeque::<Location>::new();
		potential_bbs.push_back(loc);

		'outer: while let Some(start) = potential_bbs.pop_front() {
			trace!("evaluating potential bb at {}", start);

			// ok. should we analyze this?
			if !self.should_analyze_bb(&mut func, start) {
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
				let target_loc = inst.control_target().map(|t| self.resolve_target(state, t));

				if self.mem.inst_state_change(state, &inst).is_some() {
					term = Some(BBTerm::BankChange(end_loc));
					let next = self.resolve_target(state, inst.next_va());
					potential_bbs.push_back(next);
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
						let target_loc = target_loc.expect("instruction should have control target");

						let next = self.resolve_target(state, inst.next_va());
						potential_bbs.push_back(next);

						// debug!("{:04X} t: {} next: {}", inst.va(), target_loc, next);

						term = Some(BBTerm::Call { dst: target_loc, ret: next });
					}
					Uncond | Cond => {
						// if it's into the same segment, it might be part of this function.
						// if not, it's probably a tailcall to another function.
						let target_loc = target_loc.expect("instruction should have control target");
						if seg.contains_loc(target_loc) {
							potential_bbs.push_back(target_loc);
						}

						if inst.kind() == Uncond {
							term = Some(BBTerm::Jump(target_loc));
						} else {
							let next = self.resolve_target(state, inst.next_va());
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

				let bb_id = func.new_bb(start, term.unwrap(), insts, state);
				self.span_end_analysis(start, end_loc, SpanKind::AnaCode(bb_id));
			}
		}

		assert!(potential_bbs.is_empty());

		// empty func can happen if the very first BB was NO GOOD and was canceled.
		if func.bbs.len() == 0 {
			trace!("NOPE that's not a good function.");
		} else {
			// now, turn the proto func into a real boy!!
			let fid = self.new_func(func);

			// finally, turn the crank by putting more work on the queue
			self.queue.push_back(AnalysisItem::Func2ndPass(fid));
		}
	}

	// ---------------------------------------------------------------------------------------------
	// Second pass

	pub(crate) fn func_second_pass(&mut self, fid: FuncId) {
		let func = self.get_func(fid);

		trace!("------------------------------------------------------------------------");
		trace!("- begin function 2nd pass at {}", func.start_loc());

		let mut jumptables = Vec::new();
		let mut funcs      = Vec::new();
		let mut refs       = Vec::new();
		let mut has_bank_change = false;

		for bb in func.all_bbs() {
			let state = bb.mmu_state();
			for inst in bb.insts() {
				let src = inst.loc();

				for i in 0 .. inst.num_ops() {
					let op = inst.get_op(i);

					if op.is_mem() {
						let dst = self.loc_from_va(state, op.addr());
						refs.push((src, dst));
					}
				}

				use InstructionKind::*;
				match inst.kind() {
					Indir => jumptables.push(inst.loc()),
					Call => {
						let target_loc = self.resolve_target(state, inst.control_target().unwrap());
						funcs.push((target_loc, state));
					}
					_ => {}
				}
			}

			for &succ in bb.explicit_successors() {
				refs.push((bb.term_loc(), succ));
			}

			has_bank_change |= matches!(bb.term(), BBTerm::BankChange(..));
		}

		for (src, dst) in refs.into_iter() { self.add_ref(src, dst); }
		for t in jumptables.into_iter()    { self.enqueue_jump_table(t);  }
		for (f, s) in funcs.into_iter()    { self.enqueue_function(s, f); }

		if has_bank_change { self.queue.push_back(AnalysisItem::FuncBankChange(fid)) }
	}

	// ---------------------------------------------------------------------------------------------
	// Function bank change analysis

	pub(crate) fn func_bank_change(&mut self, fid: FuncId) {
		let func = self.get_func(fid);

		trace!("------------------------------------------------------------------------");
		trace!("- begin function bank change analysis at {}", func.start_loc());

		// 1. make a list of "new states" for each bb.
		let mut new_states = BBStateChanger::new(func.num_bbs());

		// 2. gather all the BBs that change banks.
		let changers = func.all_bbs()
			.filter(|b| matches!(b.term(), BBTerm::BankChange(..)))
			.collect::<Vec<_>>();

		// 3. for each one, try a few techniques to determine the new state.
		for bb in changers {
			log::trace!("checking bb @ {}...", bb.loc());

			let state = if let Some(s) = new_states.new_state_for(bb) {
				log::trace!("  replacing {:?} with {:?}", bb.mmu_state(), s);
				s
			} else {
				bb.mmu_state()
			};

			// ooh -- it's possible this BB's state was changed on a previous iteration.

			let new_state = match self.mem.inst_state_change(state, bb.term_inst()) {
				StateChange::Static(new_state) => Some(new_state), // well that's easy.
				StateChange::Dynamic => {
					// ooh. more complicated.

					// 1. try interpreting just this BB; that might be enough.
					match self._interp_bb(bb, state) {
						Some((new_state, kind)) => {
							if kind == ValueKind::Immediate {
								log::warn!("OMFG IT WORKED {:?}", new_state);
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

				StateChange::None => unreachable!(),
			};

			// 4. now, propagate that state change to the successors.
			if let Some(new_state) = new_state {
				log::trace!("  new state: {:?}", new_state);
				match new_states.propagate(self, bb, new_state) {
					Ok(()) => {}
					Err(()) => todo!("oh noooooooo"),
				}
			} else {
				log::trace!("  could not determine bank");
			}
		}

		// 5. Finally, apply the changes.
		if new_states.made_changes() {
			let func = self.get_func_mut(fid);

			for (bb, new_state) in func.all_bbs_mut().zip(new_states.into_iter()) {
				if let Some(ns) = new_state {
					log::trace!("setting state of bb @ {} to {:?}", bb.loc(), ns);
					bb.set_mmu_state(ns);
				}
			}
		}
	}

	fn _interp_bb(&self, bb: &BasicBlock, state: MmuState) -> Option<(MmuState, ValueKind)> {
		use crate::arch::IInterpreter;
		let mut interpreter = self.plat.arch().new_interpreter();
		interpreter.interpret_bb(&self.mem, bb, Some(state));
		interpreter.last_mmu_state_change()
	}

	// ---------------------------------------------------------------------------------------------
	// Jump table analysis

	pub(crate) fn analyze_jump_table(&mut self, loc: Location) {
		trace!("there's a jumptable at {}", loc);
		// todo!()
	}
}

struct BBStateChanger {
	new_states: Vec<Option<MmuState>>,
	visited:    Vec<bool>,
}

impl BBStateChanger {
	fn new(num_bbs: usize) -> Self {
		Self {
			new_states: vec![None;  num_bbs],
			visited:    vec![false; num_bbs],
		}
	}

	fn made_changes(&self) -> bool {
		self.new_states.iter().any(|s| s.is_some())
	}

	fn new_state_for(&self, bb: &BasicBlock) -> Option<MmuState> {
		self.new_states[bb.id().idx()]
	}

	fn into_iter(self) -> impl Iterator<Item = Option<MmuState>> {
		self.new_states.into_iter()
	}

	fn propagate(&mut self, prog: &Program, from: &BasicBlock, new_state: MmuState)
	-> Result<(), ()> {
		self.visited.iter_mut().for_each(|v| *v = false);
		self._propagate_succ(prog, from, new_state)
	}

	fn _propagate_walk(&mut self, prog: &Program, from: &BasicBlock, new_state: MmuState)
	-> Result<(), ()> {
		let idx = from.id().idx();

		if self.visited[idx] {
			Ok(())
		} else {
			self.visited[idx] = true;

			if let Some(ns) = self.new_states[idx] {
				// uh oh. conflict
				log::trace!("conflicting states @ {} (changing to both {:?} and {:?})",
					from.loc(), ns, new_state);
				Err(())
			} else {
				self.new_states[idx] = Some(new_state);
				self._propagate_succ(prog, from, new_state)
			}
		}
	}

	fn _propagate_succ(&mut self, prog: &Program, from: &BasicBlock, new_state: MmuState)
	-> Result<(), ()> {
		let fid = from.id().func();

		for succ in from.successors() {
			match prog.span_at_loc(*succ).bb() {
				Some(succ) if succ.func() == fid => {
					let succ = prog.get_func(fid).get_bb(succ);
					if let Err(()) = self._propagate_walk(prog, succ, new_state) {
						return Err(());
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