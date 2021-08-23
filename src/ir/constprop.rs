
use std::collections::{ BTreeMap, VecDeque, HashSet };

use super::*;

// ------------------------------------------------------------------------------------------------
// JoinSemiLattice
// ------------------------------------------------------------------------------------------------

// From rustc!
trait JoinSemiLattice: Eq {
	fn join(&mut self, other: &Self) -> bool;
}

// ------------------------------------------------------------------------------------------------
// Info
// ------------------------------------------------------------------------------------------------

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
enum Info {
	Unk,       // ??? dunno
	Some(u64), // some constant
	Any,       // could be anything
}

impl Info {
	fn to_option(&self) -> Option<u64> {
		match self {
			Info::Some(val) => Some(*val),
			_               => None,
		}
	}
}

impl JoinSemiLattice for Info {
	fn join(&mut self, other: &Self) -> bool {
		use Info::*;

		let new = match (&self, &other) {
			(Unk, x)                     => **x,
			(x, Unk)                     => **x,
			(Any, _) | (_, Any)          => Any,
			(Some(a), Some(b)) if a == b => Some(*a),
			_                            => Any,
		};

		if *self != new {
			*self = new;
			true
		} else {
			false
		}
	}
}

// ------------------------------------------------------------------------------------------------
// State
// ------------------------------------------------------------------------------------------------

type State = BTreeMap<IrReg, Info>;

fn new_state(regs: impl Iterator<Item = IrReg>) -> State {
	regs.map(|r| (r, Info::Unk)).collect()
}

// ------------------------------------------------------------------------------------------------
// WorkList
// ------------------------------------------------------------------------------------------------

struct WorkList {
	list: VecDeque<IrBBId>,
	set:  HashSet<IrBBId>,
}

impl WorkList {
	fn new() -> Self {
		Self {
			list: VecDeque::new(),
			set: HashSet::new(),
		}
	}

	fn new_filled(n: usize) -> Self {
		let mut ret = Self::new();

		for i in 0 .. n {
			ret.enqueue(i);
		}

		ret
	}

	fn enqueue(&mut self, id: IrBBId) {
		if self.set.insert(id) {
			self.list.push_back(id);
		}
	}

	fn dequeue(&mut self) -> Option<IrBBId> {
		if let Some(id) = self.list.pop_front() {
			self.set.remove(&id);
			Some(id)
		} else {
			None
		}
	}
}

// ------------------------------------------------------------------------------------------------
// Propagator
// ------------------------------------------------------------------------------------------------

pub(crate) fn propagate_constants(bbs: &[IrBasicBlock], cfg: &IrCfg) -> BTreeMap<IrReg, u64> {
	// since each variable is only assigned once, there's no need to track changing state -
	// the state of a variable is determined at its def.
	let mut prop = Propagator::new(bbs, cfg);
	prop.run();
	prop.finish()
}

struct Propagator<'bb, 'cf> {
	bbs:   &'bb [IrBasicBlock],
	cfg:   &'cf IrCfg,
	state: State,
	work:  WorkList,
}

impl<'bb, 'cf> Propagator<'bb, 'cf> {
	fn new(bbs: &'bb [IrBasicBlock], cfg: &'cf IrCfg) -> Self {
		Self {
			state: new_state(find_all_regs(bbs).into_iter()),
			bbs,
			cfg,
			// TODO: seeding it with RPO would give faster iteration to fixpoint.
			work: WorkList::new_filled(bbs.len()),
		}
	}

	fn finish(self) -> BTreeMap<IrReg, u64> {
		self.state
			.into_iter()
			.filter_map(|(reg, info)|
				info.to_option().map(|val| (reg, val)))
			.collect()
	}

	fn run(&mut self) {
		while let Some(id) = self.work.dequeue() {
			self.visit(id);
		}
	}

	fn visit(&mut self, bbid: IrBBId) {
		let mut changed = false;

		for phi in self.bbs[bbid].phis() {
			changed |= phi_join(phi, &mut self.state);
		}

		for inst in self.bbs[bbid].insts() {
			changed |= transfer(inst, &mut self.state);
		}

		if changed {
			for (_, succ, _) in self.cfg.edges(bbid) {
				self.work.enqueue(succ);
			}
		}
	}
}

// ------------------------------------------------------------------------------------------------
// Phi join function
// ------------------------------------------------------------------------------------------------

fn phi_join(phi: &IrPhi, state: &mut State) -> bool {
	let mut reg_state = state[phi.dst_reg()];
	let mut changed = false;

	for arg in phi.args() {
		changed |= reg_state.join(&state[arg]);
	}

	state.insert(*phi.dst_reg(), reg_state);
	changed
}

// ------------------------------------------------------------------------------------------------
// Transfer function
// ------------------------------------------------------------------------------------------------

fn transfer(inst: &IrInst, state: &mut State) -> bool {
	use IrInstKind::*;

	let src_to_info = |src: IrSrc, state: &State| {
		match src {
			IrSrc::Reg(reg)   => state[&reg],
			IrSrc::Const(c)   => Info::Some(c.val()),
			IrSrc::Return(..) => Info::Any,
		}
	};

	let thing = match inst.kind() {
		// no change!
		Nop | Use { .. } | Store { .. } | Branch { .. } | CBranch { .. } | IBranch { .. }
		| Call { .. } | ICall { .. } | Ret { .. } => None,

		Assign { dst, src } => {
			Some((dst, src_to_info(src, state)))
		}

		Load { dst, .. } => {
			Some((dst, Info::Any))
		}

		Unary { dst, op, src } => {
			let src_info = src_to_info(src, state);
			let new_info = match src_info {
				Info::Some(val) => Info::Some(do_unop(op, val)),
				_               => Info::Any,
			};

			Some((dst, new_info))
		}

		Binary { dst, src1, op, src2 } => {
			let src1_info = src_to_info(src1, state);
			let src2_info = src_to_info(src2, state);

			let new_info = match (src1_info, src2_info) {
				(Info::Some(val1), Info::Some(val2)) => {
					match do_binop(op, val1, val2, src1.size()) {
						Some(new_val) => Info::Some(new_val),
						None          => Info::Any,
					}
				}
				_ => Info::Any,
			};

			Some((dst, new_info))
		}

		Ternary { dst, src1, op, src2, src3 } => {
			let src1_info = src_to_info(src1, state);
			let src2_info = src_to_info(src2, state);
			let src3_info = src_to_info(src3, state);

			let new_info = match (src1_info, src2_info, src3_info) {
				(Info::Some(val1), Info::Some(val2), Info::Some(val3)) =>
					Info::Some(do_ternop(op, val1, val2, val3, src1.size())),
				_ => Info::Any,
			};

			Some((dst, new_info))
		}
	};

	match thing {
		Some((var, new_info)) => {
			let changed = state[&var] != new_info;
			state.insert(var, new_info);
			changed
		}
		_ => false
	}
}

fn do_unop(op: IrUnOp, val: u64) -> u64 {
	use IrUnOp::*;

	match op {
		IntZxt | IntSxt => val,
		IntNeg          => !val + 1, // unsigned...
		IntNot          => !val,
		BoolNot         => if val == 0 { 1 } else { 0 },
	}
}

fn do_binop(op: IrBinOp, val1: u64, val2: u64, size: ValSize) -> Option<u64> {
	use IrBinOp::*;

	let val = match op {
		IntEq  => if val1 == val2 { 1 } else { 0 },
		IntNe  => if val1 != val2 { 1 } else { 0 },
		IntSlt => if (val1 as i64) < val2 as i64 { 1 } else { 0 },
		IntSle => if val1 as i64 <= val2 as i64 { 1 } else { 0 },
		IntUlt => if val1 < val2 { 1 } else { 0 },
		IntUle => if val1 <= val2 { 1 } else { 0 },

		IntUAdd => match size {
			ValSize::_8  => (val1 as u8).wrapping_add(val2 as u8) as u64,
			ValSize::_16 => (val1 as u16).wrapping_add(val2 as u16) as u64,
			ValSize::_32 => (val1 as u32).wrapping_add(val2 as u32) as u64,
			ValSize::_64 => (val1 as u64).wrapping_add(val2 as u64) as u64,
		}
		IntUSub  => match size {
			ValSize::_8  => (val1 as u8).wrapping_sub(val2 as u8) as u64,
			ValSize::_16 => (val1 as u16).wrapping_sub(val2 as u16) as u64,
			ValSize::_32 => (val1 as u32).wrapping_sub(val2 as u32) as u64,
			ValSize::_64 => (val1 as u64).wrapping_sub(val2 as u64) as u64,
		}
		IntCarry => match size {
			ValSize::_8  => (val1 as u8).overflowing_add(val2 as u8).1 as u64,
			ValSize::_16 => (val1 as u16).overflowing_add(val2 as u16).1 as u64,
			ValSize::_32 => (val1 as u32).overflowing_add(val2 as u32).1 as u64,
			ValSize::_64 => (val1 as u64).overflowing_add(val2 as u64).1 as u64,
		}
		IntSCarry => match size {
			ValSize::_8  => (val1 as i8).overflowing_add(val2 as i8).1 as u64,
			ValSize::_16 => (val1 as i16).overflowing_add(val2 as i16).1 as u64,
			ValSize::_32 => (val1 as i32).overflowing_add(val2 as i32).1 as u64,
			ValSize::_64 => (val1 as i64).overflowing_add(val2 as i64).1 as u64,
		}
		IntSBorrow => match size {
			ValSize::_8  => (val1 as i8).overflowing_sub(val2 as i8).1 as u64,
			ValSize::_16 => (val1 as i16).overflowing_sub(val2 as i16).1 as u64,
			ValSize::_32 => (val1 as i32).overflowing_sub(val2 as i32).1 as u64,
			ValSize::_64 => (val1 as i64).overflowing_sub(val2 as i64).1 as u64,
		}
		IntMul => match size {
			ValSize::_8  => (val1 as u8).wrapping_mul(val2 as u8) as u64,
			ValSize::_16 => (val1 as u16).wrapping_mul(val2 as u16) as u64,
			ValSize::_32 => (val1 as u32).wrapping_mul(val2 as u32) as u64,
			ValSize::_64 => (val1 as u64).wrapping_mul(val2 as u64) as u64,
		}
		IntUDiv => {
			// not using checked_div et.al. because the result has to be u64, and this is
			// less awkward imo
			if val2 == 0 {
				return None;
			} else {
				match size {
					ValSize::_8  => (val1 as u8 / val2 as u8) as u64,
					ValSize::_16 => (val1 as u16 / val2 as u16) as u64,
					ValSize::_32 => (val1 as u32 / val2 as u32) as u64,
					ValSize::_64 => (val1 as u64 / val2 as u64) as u64,
				}
			}
		}
		IntSDiv => {
			if val2 == 0 {
				return None;
			} else {
				match size {
					ValSize::_8  => (val1 as i8 / val2 as i8) as u64,
					ValSize::_16 => (val1 as i16 / val2 as i16) as u64,
					ValSize::_32 => (val1 as i32 / val2 as i32) as u64,
					ValSize::_64 => (val1 as i64 / val2 as i64) as u64,
				}
			}
		}
		IntUMod => {
			if val2 == 0 {
				return None;
			} else {
				match size {
					ValSize::_8  => (val1 as u8 % val2 as u8) as u64,
					ValSize::_16 => (val1 as u16 % val2 as u16) as u64,
					ValSize::_32 => (val1 as u32 % val2 as u32) as u64,
					ValSize::_64 => (val1 as u64 % val2 as u64) as u64,
				}
			}
		}
		// TODO: modulo on signed numbers is poorly-defined! aaaah!!!!!
		IntSMod => {
			if val2 == 0 {
				return None;
			} else {
				match size {
					ValSize::_8  => (val1 as i8 % val2 as i8) as u64,
					ValSize::_16 => (val1 as i16 % val2 as i16) as u64,
					ValSize::_32 => (val1 as i32 % val2 as i32) as u64,
					ValSize::_64 => (val1 as i64 % val2 as i64) as u64,
				}
			}
		}

		IntXor => val1 ^ val2,
		IntAnd => val1 & val2,
		IntOr =>  val1 | val2,

		IntShl => match size {
			ValSize::_8  => (val1 as u8).wrapping_shl(val2 as u32) as u64,
			ValSize::_16 => (val1 as u16).wrapping_shl(val2 as u32) as u64,
			ValSize::_32 => (val1 as u32).wrapping_shl(val2 as u32) as u64,
			ValSize::_64 => (val1 as u64).wrapping_shl(val2 as u32) as u64,
		}
		IntUShr => match size {
			ValSize::_8  => (val1 as u8).wrapping_shr(val2 as u32) as u64,
			ValSize::_16 => (val1 as u16).wrapping_shr(val2 as u32) as u64,
			ValSize::_32 => (val1 as u32).wrapping_shr(val2 as u32) as u64,
			ValSize::_64 => (val1 as u64).wrapping_shr(val2 as u32) as u64,
		}
		// TODO: what if val2 is negative?
		IntSShr => match size {
			ValSize::_8  => (val1 as i8).wrapping_shr(val2 as u32) as u64,
			ValSize::_16 => (val1 as i16).wrapping_shr(val2 as u32) as u64,
			ValSize::_32 => (val1 as i32).wrapping_shr(val2 as u32) as u64,
			ValSize::_64 => (val1 as i64).wrapping_shr(val2 as u32) as u64,
		}

		IntPair => (val1 << size as u32) | val2,
		BoolXor => if val1 != val2 { 1 } else { 0 },
		BoolAnd => if val1 != 0 && val2 != 0 { 1 } else { 0 },
		BoolOr =>  if val1 != 0 || val2 != 0 { 1 } else { 0 },
	};

	Some(val)
}

fn do_ternop(op: IrTernOp, val1: u64, val2: u64, val3: u64, size: ValSize) -> u64 {
	use IrTernOp::*;

	match op {
		IntUAddC => match size {
			ValSize::_8 => (val1 as u8).wrapping_add(val2 as u8).wrapping_add(val3 as u8) as u64,
			ValSize::_16 => (val1 as u16).wrapping_add(val2 as u16).wrapping_add(val3 as u16) as u64,
			ValSize::_32 => (val1 as u32).wrapping_add(val2 as u32).wrapping_add(val3 as u32) as u64,
			ValSize::_64 => (val1 as u64).wrapping_add(val2 as u64).wrapping_add(val3 as u64) as u64,
		},
		IntUSubB => match size {
			ValSize::_8 => (val1 as u8).wrapping_sub(val2 as u8).wrapping_sub(val3 as u8) as u64,
			ValSize::_16 => (val1 as u16).wrapping_sub(val2 as u16).wrapping_sub(val3 as u16) as u64,
			ValSize::_32 => (val1 as u32).wrapping_sub(val2 as u32).wrapping_sub(val3 as u32) as u64,
			ValSize::_64 => (val1 as u64).wrapping_sub(val2 as u64).wrapping_sub(val3 as u64) as u64,
		},
		IntCarryC => {
			let (sum, carry) = match size {
				ValSize::_8 => {
					let (sum, carry) = (val1 as u8).overflowing_add(val2 as u8);
					(sum as u64, carry)
				}
				ValSize::_16 => {
					let (sum, carry) = (val1 as u16).overflowing_add(val2 as u16);
					(sum as u64, carry)
				}
				ValSize::_32 => {
					let (sum, carry) = (val1 as u32).overflowing_add(val2 as u32);
					(sum as u64, carry)
				}
				ValSize::_64 => {
					let (sum, carry) = (val1 as u64).overflowing_add(val2 as u64);
					(sum as u64, carry)
				}
			};

			if carry {
				1
			} else {
				match size {
					ValSize::_8  => (sum as u8).overflowing_add(val3 as u8).1 as u64,
					ValSize::_16 => (sum as u16).overflowing_add(val3 as u16).1 as u64,
					ValSize::_32 => (sum as u32).overflowing_add(val3 as u32).1 as u64,
					ValSize::_64 => (sum as u64).overflowing_add(val3 as u64).1 as u64,
				}
			}
		}
		IntSCarryC => {
			let (sum, carry) = match size {
				ValSize::_8 => {
					let (sum, carry) = (val1 as i8).overflowing_add(val2 as i8);
					(sum as u64, carry)
				}
				ValSize::_16 => {
					let (sum, carry) = (val1 as i16).overflowing_add(val2 as i16);
					(sum as u64, carry)
				}
				ValSize::_32 => {
					let (sum, carry) = (val1 as i32).overflowing_add(val2 as i32);
					(sum as u64, carry)
				}
				ValSize::_64 => {
					let (sum, carry) = (val1 as i64).overflowing_add(val2 as i64);
					(sum as u64, carry)
				}
			};

			if carry {
				1
			} else {
				match size {
					ValSize::_8  => (sum as i8).overflowing_add(val3 as i8).1 as u64,
					ValSize::_16 => (sum as i16).overflowing_add(val3 as i16).1 as u64,
					ValSize::_32 => (sum as i32).overflowing_add(val3 as i32).1 as u64,
					ValSize::_64 => (sum as i64).overflowing_add(val3 as i64).1 as u64,
				}
			}
		}
		IntSBorrowC => {
			let (sum, borrow) = match size {
				ValSize::_8 => {
					let (sum, borrow) = (val1 as i8).overflowing_sub(val2 as i8);
					(sum as u64, borrow)
				}
				ValSize::_16 => {
					let (sum, borrow) = (val1 as i16).overflowing_sub(val2 as i16);
					(sum as u64, borrow)
				}
				ValSize::_32 => {
					let (sum, borrow) = (val1 as i32).overflowing_sub(val2 as i32);
					(sum as u64, borrow)
				}
				ValSize::_64 => {
					let (sum, borrow) = (val1 as i64).overflowing_sub(val2 as i64);
					(sum as u64, borrow)
				}
			};

			if borrow {
				1
			} else {
				match size {
					ValSize::_8  => (sum as i8).overflowing_sub(val3 as i8).1 as u64,
					ValSize::_16 => (sum as i16).overflowing_sub(val3 as i16).1 as u64,
					ValSize::_32 => (sum as i32).overflowing_sub(val3 as i32).1 as u64,
					ValSize::_64 => (sum as i64).overflowing_sub(val3 as i64).1 as u64,
				}
			}
		}
	}
}