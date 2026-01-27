
use std::collections::{ BTreeMap, VecDeque, HashSet };

use super::*;

// TODO: abstract some of this stuff (JoinSemiLattice, WorkList, some of Propagator) out into
// a separate library for use by other passes

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
	Some {     // some constant, including where we got that value (up to 3 sources)
		val: u64,
		from: [Option<IrSrc>; 3],
	},
	Any,       // could be anything
}

impl Info {
	fn to_option(&self) -> Option<u64> {
		match self {
			Info::Some { val, .. } => Some(*val),
			_                      => None,
		}
	}

	fn some1(val: u64, src1: IrSrc) -> Self {
		Self::Some { val, from: [Some(src1), None, None] }
	}

	fn some2(val: u64, src1: IrSrc, src2: IrSrc) -> Self {
		Self::Some { val, from: [Some(src1), Some(src2), None] }
	}

	fn some3(val: u64, src1: IrSrc, src2: IrSrc, src3: IrSrc) -> Self {
		Self::Some { val, from: [Some(src1), Some(src2), Some(src3)] }
	}
}

impl JoinSemiLattice for Info {
	fn join(&mut self, other: &Self) -> bool {
		use Info::*;

		let new = match (&self, &other) {
			(Unk, x)                     => **x,
			(x, Unk)                     => **x,
			(Any, _) | (_, Any)          => Any,
			(Some { val: a, from: from1 }, Some { val: b, from: _from2 }) if a == b => {
				// TODO: uhhhh how DO we handle this? just pick from1 or from2 or merge
				// them somehow?
				Some { val: *a, from: *from1 }
			}
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

/// Results of constant propagation. It maps from IR Registers to a tuple of:
///
/// - The determined constant value for that register
/// - A list of up to 3 sources from which that constant was computed
///
/// The sources can be used to propagate information backwards, such as in cases
/// where a constant address is computed by combining two smaller pieces, and those
/// smaller pieces need to be marked as references to that address.
pub(crate) type ConstPropResults = BTreeMap<IrReg, (u64, [Option<IrSrc>; 3])>;

pub(crate) fn propagate_constants(bbs: &[IrBasicBlock], cfg: &IrCfg) -> ConstPropResults {
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

	fn finish(self) -> BTreeMap<IrReg, (u64, [Option<IrSrc>; 3])> {
		self.state
			.into_iter()
			.filter_map(|(reg, info)|
				match info {
					Info::Unk | Info::Any => None,
					Info::Some { val, from } => Some((reg, (val, from))),
				})
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
			IrSrc::Const(c)   => Info::some1(c.val(), src),
			IrSrc::Return(..) => Info::Any,
		}
	};

	let thing = match inst.kind() {
		// no change!
		Nop | Use { .. } | Store { .. } | Branch { .. } | CBranch { .. } | IBranch { .. }
		| Call { .. } | ICall { .. } | Ret { .. } => None,

		Assign { dst, src, .. } => Some((dst, src_to_info(src, state))),
		Load   { dst, .. }      => Some((dst, Info::Any)),

		Unary { dst, op, src, .. } => {
			let src_info = src_to_info(src, state);
			let new_info = match src_info {
				Info::Some { val, .. } => {
					Info::some1(do_unop(op, val, src.size(), dst.size()), src)
				},
				_ => Info::Any,
			};

			Some((dst, new_info))
		}

		Binary { dst, src1, op, src2, .. } => {
			let src1_info = src_to_info(src1, state);
			let src2_info = src_to_info(src2, state);

			let new_info = match (src1_info, src2_info) {
				(Info::Some { val: val1, .. }, Info::Some { val: val2, .. }) => {
					match do_binop(op, val1, val2, src1.size()) {
						Some(new_val) => Info::some2(new_val, src1, src2),
						None          => Info::Any,
					}
				}
				_ => Info::Any,
			};

			Some((dst, new_info))
		}

		Ternary { dst, src1, op, src2, src3, .. } => {
			let src1_info = src_to_info(src1, state);
			let src2_info = src_to_info(src2, state);
			let src3_info = src_to_info(src3, state);

			let new_info = match (src1_info, src2_info, src3_info) {
				(	Info::Some{ val: val1, .. },
					Info::Some{ val: val2, .. },
					Info::Some{ val: val3, .. }) =>

					Info::some3(do_ternop(op, val1, val2, val3, src1.size()), src1, src2, src3),
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

fn do_unop(op: IrUnOp, val: u64, src_size: ValSize, dst_size: ValSize) -> u64 {
	use IrUnOp::*;

	match op {
		IntZxt => val,
		// IrInst::isxt ensures that src_size < dst_size
		IntSxt => match src_size {
			ValSize::_8 =>  match dst_size {
				ValSize::_16 => val as u8 as i8 as i16 as u16 as u64,
				ValSize::_32 => val as u8 as i8 as i32 as u32 as u64,
				ValSize::_64 => val as u8 as i8 as i64 as u64,
				_ => unreachable!(),
			}
			ValSize::_16 => match dst_size {
				ValSize::_32 => val as u16 as i16 as i32 as u32 as u64,
				ValSize::_64 => val as u16 as i16 as i64 as u64,
				_ => unreachable!(),
			}
			ValSize::_32 => match dst_size {
				ValSize::_64 => val as u32 as i32 as i64 as u64,
				_ => unreachable!(),
			}
			ValSize::_64 => unreachable!(),
		},
		IntNeg => match src_size {
			ValSize::_8 =>  (-(val as i8 )) as u8 as u64,
			ValSize::_16 => (-(val as i16)) as u16 as u64,
			ValSize::_32 => (-(val as i32)) as u32 as u64,
			ValSize::_64 => (-(val as i64)) as u64,
		},
		IntNot => match src_size {
			ValSize::_8 =>  (!(val as i8 )) as u8 as u64,
			ValSize::_16 => (!(val as i16)) as u16 as u64,
			ValSize::_32 => (!(val as i32)) as u32 as u64,
			ValSize::_64 => (!(val as i64)) as u64,
		},
		BoolNot => (val == 0) as u64,
	}
}

fn do_binop(op: IrBinOp, val1: u64, val2: u64, size: ValSize) -> Option<u64> {
	use IrBinOp::*;

	let val = match op {
		IntEq  => (val1 == val2) as u64,
		IntNe  => (val1 != val2) as u64,

		IntSlt => match size {
			ValSize::_8  => ((val1 as i8)  < (val2 as i8)) as u64,
			ValSize::_16 => ((val1 as i16) < (val2 as i16)) as u64,
			ValSize::_32 => ((val1 as i32) < (val2 as i32)) as u64,
			ValSize::_64 => ((val1 as i64) < (val2 as i64)) as u64,
		},
		IntSle => match size {
			ValSize::_8  => ((val1 as i8)  <= (val2 as i8)) as u64,
			ValSize::_16 => ((val1 as i16) <= (val2 as i16)) as u64,
			ValSize::_32 => ((val1 as i32) <= (val2 as i32)) as u64,
			ValSize::_64 => ((val1 as i64) <= (val2 as i64)) as u64,
		},

		IntUlt => (val1 < val2) as u64,
		IntUle => (val1 <= val2) as u64,

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
		// TODO: this is also poorly-defined. would it make more sense to have an n*n=>2n
		// multiplication operation? and then you'd need some kind of unpair operation...
		// well we'll punt for now cause I don't forsee implementing arches with multiplication
		// any time soon.
		IntMul => match size {
			ValSize::_8  => (val1 as u8).wrapping_mul(val2 as u8) as u64,
			ValSize::_16 => (val1 as u16).wrapping_mul(val2 as u16) as u64,
			ValSize::_32 => (val1 as u32).wrapping_mul(val2 as u32) as u64,
			ValSize::_64 => (val1 as u64).wrapping_mul(val2 as u64) as u64,
		}
		IntUDiv => {
			// not using checked_div et al. because the result has to be u64, and this is
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
					ValSize::_8  => (val1 as i8 / val2 as i8) as u8 as u64,
					ValSize::_16 => (val1 as i16 / val2 as i16) as u16 as u64,
					ValSize::_32 => (val1 as i32 / val2 as i32) as u32 as u64,
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
					ValSize::_8  => (val1 as i8 % val2 as i8) as u8 as u64,
					ValSize::_16 => (val1 as i16 % val2 as i16) as u16 as u64,
					ValSize::_32 => (val1 as i32 % val2 as i32) as u32 as u64,
					ValSize::_64 => (val1 as i64 % val2 as i64) as u64,
				}
			}
		}

		IntXor => val1 ^ val2,
		IntAnd => val1 & val2,
		IntOr =>  val1 | val2,

		// TODO: for all shifts, what if shift distance exceeds bits? checked_shx().unwrap_or(0)
		// treats it as "all bits shifted off end" but some architectures instead shift only by
		// lower bits (so e.g. if it's a 16-bit arch, and you shift by 17, it treats it as shifting
		// by 1). Should that be an option? or give an error? or...?
		IntShl => match size {
			ValSize::_8  => (val1 as u8).checked_shl(val2 as u32).unwrap_or(0) as u64,
			ValSize::_16 => (val1 as u16).checked_shl(val2 as u32).unwrap_or(0) as u64,
			ValSize::_32 => (val1 as u32).checked_shl(val2 as u32).unwrap_or(0) as u64,
			ValSize::_64 => (val1 as u64).checked_shl(val2 as u32).unwrap_or(0) as u64,
		}
		IntUShr => match size {
			ValSize::_8  => (val1 as u8).checked_shr(val2 as u32).unwrap_or(0) as u64,
			ValSize::_16 => (val1 as u16).checked_shr(val2 as u32).unwrap_or(0) as u64,
			ValSize::_32 => (val1 as u32).checked_shr(val2 as u32).unwrap_or(0) as u64,
			ValSize::_64 => (val1 as u64).checked_shr(val2 as u32).unwrap_or(0) as u64,
		}
		// TODO: what if val2 is negative?
		IntSShr => match size {
			ValSize::_8  => (val1 as i8).checked_shr(val2 as u32)
				.unwrap_or_else(|| if (val1 as i8) < 0 { -1 } else { 0 }) as u8 as u64,
			ValSize::_16 => (val1 as i16).checked_shr(val2 as u32)
				.unwrap_or_else(|| if (val1 as i16) < 0 { -1 } else { 0 }) as u16 as u64,
			ValSize::_32 => (val1 as i32).checked_shr(val2 as u32)
				.unwrap_or_else(|| if (val1 as i32) < 0 { -1 } else { 0 }) as u32 as u64,
			ValSize::_64 => (val1 as i64).checked_shr(val2 as u32)
				.unwrap_or_else(|| if (val1 as i64) < 0 { -1 } else { 0 }) as u64,
		}

		IntPair => (val1 << size as u32) | val2,
		BoolXor => (val1 != val2) as u64,
		BoolAnd => (val1 != 0 && val2 != 0) as u64,
		BoolOr =>  (val1 != 0 || val2 != 0) as u64,
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

#[cfg(test)]
mod tests {
	use super::*;

	#[track_caller]
	fn test_unop(expected: u64, op: IrUnOp, val: u64, src_size: ValSize) {
		let actual = do_unop(op, val, src_size, src_size);
		assert_eq!(expected, actual);
	}

	#[track_caller]
	fn test_ext(expected: u64, dst_size: ValSize, op: IrUnOp, val: u64, src_size: ValSize) {
		let actual = do_unop(op, val, src_size, dst_size);
		assert_eq!(expected, actual);
	}

	#[track_caller]
	fn test_binop(expected: u64, op: IrBinOp, val1: u64, val2: u64, size: ValSize) {
		let actual = do_binop(op, val1, val2, size);
		assert_eq!(Some(expected), actual);
	}

	#[track_caller]
	fn test_binop_none(op: IrBinOp, val1: u64, val2: u64, size: ValSize) {
		let actual = do_binop(op, val1, val2, size);
		assert_eq!(None, actual);
	}

	#[track_caller]
	fn test_ternop(expected: u64, op: IrTernOp, val1: u64, val2: u64, val3: u64, size: ValSize) {
		let actual = do_ternop(op, val1, val2, val3, size);
		assert_eq!(expected, actual);
	}

	#[test]
	fn test_zxt() {
		use { IrUnOp::*, ValSize::* };
		test_ext(0x34, _16, IntZxt, 0x34, _8);
		test_ext(0x34, _32, IntZxt, 0x34, _8);
		test_ext(0x34, _64, IntZxt, 0x34, _8);
		test_ext(0x34, _32, IntZxt, 0x34, _16);
		test_ext(0x34, _64, IntZxt, 0x34, _16);
		test_ext(0x34, _64, IntZxt, 0x34, _32);
		test_ext(0xFF, _16, IntZxt, 0xFF, _8);
		test_ext(0xFF, _32, IntZxt, 0xFF, _8);
		test_ext(0xFF, _64, IntZxt, 0xFF, _8);
		test_ext(0xFF, _32, IntZxt, 0xFF, _16);
		test_ext(0xFF, _64, IntZxt, 0xFF, _16);
		test_ext(0xFF, _64, IntZxt, 0xFF, _32);
	}

	#[test]
	fn test_sxt() {
		use { IrUnOp::*, ValSize::* };
		test_ext(0x34, _16, IntSxt, 0x34, _8);
		test_ext(0x34, _32, IntSxt, 0x34, _8);
		test_ext(0x34, _64, IntSxt, 0x34, _8);
		test_ext(0x34, _32, IntSxt, 0x34, _16);
		test_ext(0x34, _64, IntSxt, 0x34, _16);
		test_ext(0x34, _64, IntSxt, 0x34, _32);
		test_ext(0xFFFF,              _16, IntSxt, 0xFF,       _8);
		test_ext(0xFFFFFFFF,          _32, IntSxt, 0xFF,       _8);
		test_ext(0xFFFFFFFF_FFFFFFFF, _64, IntSxt, 0xFF,       _8);
		test_ext(0xFFFFFFFF,          _32, IntSxt, 0xFFFF,     _16);
		test_ext(0xFFFFFFFF_FFFFFFFF, _64, IntSxt, 0xFFFF,     _16);
		test_ext(0xFFFFFFFF_FFFFFFFF, _64, IntSxt, 0xFFFFFFFF, _32);
	}

	#[test]
	fn test_neg() {
		use { IrUnOp::*, ValSize::* };
		test_unop(0xFE, IntNeg, 0x02, _8);
		test_unop(0x02, IntNeg, 0xFE, _8);
		test_unop(0xFFFE, IntNeg, 0x0002, _16);
		test_unop(0x0002, IntNeg, 0xFFFE, _16);
		test_unop(0xFFFFFFFE, IntNeg, 0x00000002, _32);
		test_unop(0x00000002, IntNeg, 0xFFFFFFFE, _32);
		test_unop(0xFFFFFFFF_FFFFFFFE, IntNeg, 0x00000000_00000002, _64);
		test_unop(0x00000000_00000002, IntNeg, 0xFFFFFFFF_FFFFFFFE, _64);
	}

	#[test]
	fn test_inot() {
		use { IrUnOp::*, ValSize::* };
		test_unop(0xFD, IntNot, 0x02, _8);
		test_unop(0x02, IntNot, 0xFD, _8);
		test_unop(0xFFFD, IntNot, 0x0002, _16);
		test_unop(0x0002, IntNot, 0xFFFD, _16);
		test_unop(0xFFFFFFFD, IntNot, 0x00000002, _32);
		test_unop(0x00000002, IntNot, 0xFFFFFFFD, _32);
		test_unop(0xFFFFFFFF_FFFFFFFD, IntNot, 0x00000000_00000002, _64);
		test_unop(0x00000000_00000002, IntNot, 0xFFFFFFFF_FFFFFFFD, _64);
	}

	#[test]
	fn test_bnot() {
		use { IrUnOp::*, ValSize::* };
		test_unop(1, BoolNot, 0, _8);
		test_unop(0, BoolNot, 1, _8);
		test_unop(0, BoolNot, 100, _8);
	}

	#[test]
	fn test_ieq_ine() {
		use { IrBinOp::*, ValSize::* };
		test_binop(1, IntEq,  0x34, 0x34, _8);
		test_binop(0, IntEq,  0x34, 0x9E, _8);
		test_binop(0, IntNe,  0x34, 0x34, _8);
		test_binop(1, IntNe,  0x34, 0x9E, _8);
	}

	#[test]
	fn test_islt() {
		use { IrBinOp::*, ValSize::* };
		test_binop(1, IntSlt, 0xFF, 0x01, _8);
		test_binop(1, IntSlt, 0x03, 0x05, _8);
		test_binop(0, IntSlt, 0x01, 0xFF, _8);
		test_binop(0, IntSlt, 0x05, 0x03, _8);
		test_binop(0, IntSlt, 0x04, 0x04, _8);
		test_binop(1, IntSlt, 0xFFFF, 0x0001, _16);
		test_binop(1, IntSlt, 0x0003, 0x0005, _16);
		test_binop(0, IntSlt, 0x0001, 0xFFFF, _16);
		test_binop(0, IntSlt, 0x0005, 0x0003, _16);
		test_binop(0, IntSlt, 0x0004, 0x0004, _16);
		test_binop(1, IntSlt, 0xFFFFFFFF, 0x00000001, _32);
		test_binop(1, IntSlt, 0x00000003, 0x00000005, _32);
		test_binop(0, IntSlt, 0x00000001, 0xFFFFFFFF, _32);
		test_binop(0, IntSlt, 0x00000005, 0x00000003, _32);
		test_binop(0, IntSlt, 0x00000004, 0x00000004, _32);
		test_binop(1, IntSlt, 0xFFFFFFFF_FFFFFFFF, 0x00000000_00000001, _64);
		test_binop(1, IntSlt, 0x00000000_00000003, 0x00000000_00000005, _64);
		test_binop(0, IntSlt, 0x00000000_00000001, 0xFFFFFFFF_FFFFFFFF, _64);
		test_binop(0, IntSlt, 0x00000000_00000005, 0x00000000_00000003, _64);
		test_binop(0, IntSlt, 0x00000000_00000004, 0x00000000_00000004, _64);
	}

	#[test]
	fn test_isle() {
		use { IrBinOp::*, ValSize::* };
		test_binop(1, IntSle, 0xFF, 0x01, _8);
		test_binop(1, IntSle, 0x03, 0x05, _8);
		test_binop(0, IntSle, 0x01, 0xFF, _8);
		test_binop(0, IntSle, 0x05, 0x03, _8);
		test_binop(1, IntSle, 0x04, 0x04, _8);
		test_binop(1, IntSle, 0xFFFF, 0x0001, _16);
		test_binop(1, IntSle, 0x0003, 0x0005, _16);
		test_binop(0, IntSle, 0x0001, 0xFFFF, _16);
		test_binop(0, IntSle, 0x0005, 0x0003, _16);
		test_binop(1, IntSle, 0x0004, 0x0004, _16);
		test_binop(1, IntSle, 0xFFFFFFFF, 0x00000001, _32);
		test_binop(1, IntSle, 0x00000003, 0x00000005, _32);
		test_binop(0, IntSle, 0x00000001, 0xFFFFFFFF, _32);
		test_binop(0, IntSle, 0x00000005, 0x00000003, _32);
		test_binop(1, IntSle, 0x00000004, 0x00000004, _32);
		test_binop(1, IntSle, 0xFFFFFFFF_FFFFFFFF, 0x00000000_00000001, _64);
		test_binop(1, IntSle, 0x00000000_00000003, 0x00000000_00000005, _64);
		test_binop(0, IntSle, 0x00000000_00000001, 0xFFFFFFFF_FFFFFFFF, _64);
		test_binop(0, IntSle, 0x00000000_00000005, 0x00000000_00000003, _64);
		test_binop(1, IntSle, 0x00000000_00000004, 0x00000000_00000004, _64);
	}

	#[test]
	fn test_iult() {
		use { IrBinOp::*, ValSize::* };
		test_binop(0, IntUlt, 0xFF, 0x01, _8);
		test_binop(1, IntUlt, 0x03, 0x05, _8);
		test_binop(1, IntUlt, 0x01, 0xFF, _8);
		test_binop(0, IntUlt, 0x05, 0x03, _8);
		test_binop(0, IntUlt, 0x04, 0x04, _8);
		test_binop(0, IntUlt, 0xFFFF, 0x0001, _16);
		test_binop(1, IntUlt, 0x0003, 0x0005, _16);
		test_binop(1, IntUlt, 0x0001, 0xFFFF, _16);
		test_binop(0, IntUlt, 0x0005, 0x0003, _16);
		test_binop(0, IntUlt, 0x0004, 0x0004, _16);
		test_binop(0, IntUlt, 0xFFFFFFFF, 0x00000001, _32);
		test_binop(1, IntUlt, 0x00000003, 0x00000005, _32);
		test_binop(1, IntUlt, 0x00000001, 0xFFFFFFFF, _32);
		test_binop(0, IntUlt, 0x00000005, 0x00000003, _32);
		test_binop(0, IntUlt, 0x00000004, 0x00000004, _32);
		test_binop(0, IntUlt, 0xFFFFFFFF_FFFFFFFF, 0x00000000_00000001, _64);
		test_binop(1, IntUlt, 0x00000000_00000003, 0x00000000_00000005, _64);
		test_binop(1, IntUlt, 0x00000000_00000001, 0xFFFFFFFF_FFFFFFFF, _64);
		test_binop(0, IntUlt, 0x00000000_00000005, 0x00000000_00000003, _64);
		test_binop(0, IntUlt, 0x00000000_00000004, 0x00000000_00000004, _64);
	}

	#[test]
	fn test_iule() {
		use { IrBinOp::*, ValSize::* };
		test_binop(0, IntUle, 0xFF, 0x01, _8);
		test_binop(1, IntUle, 0x03, 0x05, _8);
		test_binop(1, IntUle, 0x01, 0xFF, _8);
		test_binop(0, IntUle, 0x05, 0x03, _8);
		test_binop(1, IntUle, 0x04, 0x04, _8);
		test_binop(0, IntUle, 0xFFFF, 0x0001, _16);
		test_binop(1, IntUle, 0x0003, 0x0005, _16);
		test_binop(1, IntUle, 0x0001, 0xFFFF, _16);
		test_binop(0, IntUle, 0x0005, 0x0003, _16);
		test_binop(1, IntUle, 0x0004, 0x0004, _16);
		test_binop(0, IntUle, 0xFFFFFFFF, 0x00000001, _32);
		test_binop(1, IntUle, 0x00000003, 0x00000005, _32);
		test_binop(1, IntUle, 0x00000001, 0xFFFFFFFF, _32);
		test_binop(0, IntUle, 0x00000005, 0x00000003, _32);
		test_binop(1, IntUle, 0x00000004, 0x00000004, _32);
		test_binop(0, IntUle, 0xFFFFFFFF_FFFFFFFF, 0x00000000_00000001, _64);
		test_binop(1, IntUle, 0x00000000_00000003, 0x00000000_00000005, _64);
		test_binop(1, IntUle, 0x00000000_00000001, 0xFFFFFFFF_FFFFFFFF, _64);
		test_binop(0, IntUle, 0x00000000_00000005, 0x00000000_00000003, _64);
		test_binop(1, IntUle, 0x00000000_00000004, 0x00000000_00000004, _64);
	}

	#[test]
	fn test_iuadd() {
		use { IrBinOp::*, ValSize::* };
		test_binop(0x08, IntUAdd, 0x03, 0x05, _8);
		test_binop(0x0008, IntUAdd, 0x0003, 0x0005, _16);
		test_binop(0x00000008, IntUAdd, 0x00000003, 0x00000005, _32);
		test_binop(0x00000000_00000008, IntUAdd, 0x00000000_00000003, 0x00000000_00000005, _64);

		test_binop(0x02, IntUAdd, 0xFD, 0x05, _8);
		test_binop(0x0002, IntUAdd, 0xFFFD, 0x0005, _16);
		test_binop(0x00000002, IntUAdd, 0xFFFFFFFD, 0x00000005, _32);
		test_binop(0x00000000_00000002, IntUAdd, 0xFFFFFFFF_FFFFFFFD, 0x00000000_00000005, _64);
	}

	#[test]
	fn test_iusub() {
		use { IrBinOp::*, ValSize::* };
		test_binop(0x03, IntUSub, 0x08, 0x05, _8);
		test_binop(0x0003, IntUSub, 0x0008, 0x0005, _16);
		test_binop(0x00000003, IntUSub, 0x00000008, 0x00000005, _32);
		test_binop(0x00000000_00000003, IntUSub, 0x00000000_00000008, 0x00000000_00000005, _64);

		test_binop(0xFE, IntUSub, 0x03, 0x05, _8);
		test_binop(0xFFFE, IntUSub, 0x0003, 0x0005, _16);
		test_binop(0xFFFFFFFE, IntUSub, 0x00000003, 0x00000005, _32);
		test_binop(0xFFFFFFFF_FFFFFFFE, IntUSub, 0x00000000_00000003, 0x00000000_00000005, _64);
	}

	#[test]
	fn test_iucarry() {
		use { IrBinOp::*, ValSize::* };
		test_binop(0, IntCarry, 0x08, 0x05, _8);
		test_binop(0, IntCarry, 0x0008, 0x0005, _16);
		test_binop(0, IntCarry, 0x00000008, 0x00000005, _32);
		test_binop(0, IntCarry, 0x00000000_00000008, 0x00000000_00000005, _64);

		test_binop(0, IntCarry, 0x7F, 0x01, _8);
		test_binop(1, IntCarry, 0xFF, 0x01, _8);
		test_binop(0, IntCarry, 0x00FF, 0x0001, _16);
		test_binop(0, IntCarry, 0x000000FF, 0x00000001, _32);
		test_binop(0, IntCarry, 0x00000000_000000FF, 0x00000000_00000001, _64);

		test_binop(0, IntCarry, 0x7FFF, 0x0001, _16);
		test_binop(1, IntCarry, 0xFFFF, 0x0001, _16);
		test_binop(0, IntCarry, 0x0000FFFF, 0x00000001, _32);
		test_binop(0, IntCarry, 0x00000000_0000FFFF, 0x00000000_00000001, _64);

		test_binop(0, IntCarry, 0x7FFFFFFF, 0x00000001, _32);
		test_binop(1, IntCarry, 0xFFFFFFFF, 0x00000001, _32);
		test_binop(0, IntCarry, 0x00000000_FFFFFFFF, 0x00000000_00000001, _64);

		test_binop(0, IntCarry, 0x7FFFFFFF_FFFFFFFF, 0x00000000_00000001, _64);
		test_binop(1, IntCarry, 0xFFFFFFFF_FFFFFFFF, 0x00000000_00000001, _64);
	}

	#[test]
	fn test_iscarry() {
		use { IrBinOp::*, ValSize::* };
		test_binop(0, IntSCarry, 0x08, 0x05, _8);
		test_binop(0, IntSCarry, 0x0008, 0x0005, _16);
		test_binop(0, IntSCarry, 0x00000008, 0x00000005, _32);
		test_binop(0, IntSCarry, 0x00000000_00000008, 0x00000000_00000005, _64);

		test_binop(0, IntSCarry, 0xFE, 0x05, _8);
		test_binop(0, IntSCarry, 0xFFFE, 0x0005, _16);
		test_binop(0, IntSCarry, 0xFFFFFFFE, 0x00000005, _32);
		test_binop(0, IntSCarry, 0xFFFFFFFF_FFFFFFFE, 0x00000000_00000005, _64);

		test_binop(1, IntSCarry, 0x7F, 0x01, _8);
		test_binop(1, IntSCarry, 0x80, 0xFF, _8);
		test_binop(0, IntSCarry, 0xFF, 0x01, _8);
		test_binop(0, IntSCarry, 0x00FF, 0x0001, _16);
		test_binop(0, IntSCarry, 0x000000FF, 0x00000001, _32);
		test_binop(0, IntSCarry, 0x00000000_000000FF, 0x00000000_00000001, _64);

		test_binop(1, IntSCarry, 0x7FFF, 0x0001, _16);
		test_binop(1, IntSCarry, 0x8000, 0xFFFF, _16);
		test_binop(0, IntSCarry, 0xFFFF, 0x0001, _16);
		test_binop(0, IntSCarry, 0x0000FFFF, 0x00000001, _32);
		test_binop(0, IntSCarry, 0x00000000_0000FFFF, 0x00000000_00000001, _64);

		test_binop(1, IntSCarry, 0x7FFFFFFF, 0x00000001, _32);
		test_binop(1, IntSCarry, 0x80000000, 0xFFFFFFFF, _32);
		test_binop(0, IntSCarry, 0xFFFFFFFF, 0x00000001, _32);
		test_binop(0, IntSCarry, 0x00000000_FFFFFFFF, 0x00000000_00000001, _64);

		test_binop(1, IntSCarry, 0x7FFFFFFF_FFFFFFFF, 0x00000000_00000001, _64);
		test_binop(1, IntSCarry, 0x80000000_00000000, 0xFFFFFFFF_FFFFFFFF, _64);
		test_binop(0, IntSCarry, 0xFFFFFFFF_FFFFFFFF, 0x00000000_00000001, _64);
	}

	#[test]
	fn test_isborrow() {
		use { IrBinOp::*, ValSize::* };
		test_binop(0, IntSBorrow, 0x08, 0x05, _8);
		test_binop(0, IntSBorrow, 0x0008, 0x0005, _16);
		test_binop(0, IntSBorrow, 0x00000008, 0x00000005, _32);
		test_binop(0, IntSBorrow, 0x00000000_00000008, 0x00000000_00000005, _64);

		test_binop(0, IntSBorrow, 0xFE, 0x05, _8);
		test_binop(0, IntSBorrow, 0xFFFE, 0x0005, _16);
		test_binop(0, IntSBorrow, 0xFFFFFFFE, 0x00000005, _32);
		test_binop(0, IntSBorrow, 0xFFFFFFFF_FFFFFFFE, 0x00000000_00000005, _64);

		test_binop(1, IntSBorrow, 0x7F, 0xFF, _8);
		test_binop(1, IntSBorrow, 0x80, 0x01, _8);
		test_binop(0, IntSBorrow, 0xFF, 0xFF, _8);
		test_binop(0, IntSBorrow, 0x00FF, 0x00FF, _16);
		test_binop(0, IntSBorrow, 0x000000FF, 0x000000FF, _32);
		test_binop(0, IntSBorrow, 0x00000000_000000FF, 0x00000000_000000FF, _64);

		test_binop(1, IntSBorrow, 0x7FFF, 0xFFFF, _16);
		test_binop(1, IntSBorrow, 0x8000, 0x0001, _16);
		test_binop(0, IntSBorrow, 0xFFFF, 0xFFFF, _16);
		test_binop(0, IntSBorrow, 0x0000FFFF, 0x0000FFFF, _32);
		test_binop(0, IntSBorrow, 0x00000000_0000FFFF, 0x00000000_0000FFFF, _64);

		test_binop(1, IntSBorrow, 0x7FFFFFFF, 0xFFFFFFFF, _32);
		test_binop(1, IntSBorrow, 0x80000000, 0x00000001, _32);
		test_binop(0, IntSBorrow, 0xFFFFFFFF, 0xFFFFFFFF, _32);
		test_binop(0, IntSBorrow, 0x00000000_FFFFFFFF, 0x00000000_FFFFFFFF, _64);

		test_binop(1, IntSBorrow, 0x7FFFFFFF_FFFFFFFF, 0xFFFFFFFF_FFFFFFFF, _64);
		test_binop(1, IntSBorrow, 0x80000000_00000000, 0x00000000_00000001, _64);
		test_binop(0, IntSBorrow, 0xFFFFFFFF_FFFFFFFF, 0xFFFFFFFF_FFFFFFFF, _64);
	}

	#[test]
	fn test_imul() {
		use { IrBinOp::*, ValSize::* };

		test_binop(33, IntMul, 11, 3, _8);
		test_binop(33, IntMul, 11, 3, _16);
		test_binop(33, IntMul, 11, 3, _32);
		test_binop(33, IntMul, 11, 3, _64);

		//          -15            -5  x  3
		test_binop(0xF1, IntMul, 0xFB, 0x03, _8);
		test_binop(0xFFF1, IntMul, 0xFFFB, 0x0003, _16);
		test_binop(0xFFFFFFF1, IntMul, 0xFFFFFFFB, 0x00000003, _32);
		test_binop(0xFFFFFFFF_FFFFFFF1, IntMul, 0xFFFFFFFF_FFFFFFFB, 0x00000000_00000003, _64);

		//           15            -5  x -3
		test_binop(0x0F, IntMul, 0xFB, 0xFD, _8);
		test_binop(0x000F, IntMul, 0xFFFB, 0xFFFD, _16);
		test_binop(0x0000000F, IntMul, 0xFFFFFFFB, 0xFFFFFFFD, _32);
		test_binop(0x00000000_0000000F, IntMul, 0xFFFFFFFF_FFFFFFFB, 0xFFFFFFFF_FFFFFFFD, _64);
	}

	#[test]
	fn test_iudiv() {
		use { IrBinOp::*, ValSize::* };

		test_binop(3, IntUDiv, 33, 11, _8);
		test_binop(3, IntUDiv, 33, 11, _16);
		test_binop(3, IntUDiv, 33, 11, _32);
		test_binop(3, IntUDiv, 33, 11, _64);

		test_binop_none(IntUDiv, 33, 0, _8);
		test_binop_none(IntUDiv, 33, 0, _16);
		test_binop_none(IntUDiv, 33, 0, _32);
		test_binop_none(IntUDiv, 33, 0, _64);

		test_binop(0x0F, IntUDiv, 0xFF, 0x11, _8);
		test_binop(0x0F, IntUDiv, 0xFFFF, 0x1111, _16);
		test_binop(0x0F, IntUDiv, 0xFFFFFFFF, 0x11111111, _32);
		test_binop(0x0F, IntUDiv, 0xFFFFFFFF_FFFFFFFF, 0x11111111_11111111, _64);
	}

	#[test]
	fn test_isdiv() {
		use { IrBinOp::*, ValSize::* };

		test_binop(3, IntSDiv, 33, 11, _8);
		test_binop(3, IntSDiv, 33, 11, _16);
		test_binop(3, IntSDiv, 33, 11, _32);
		test_binop(3, IntSDiv, 33, 11, _64);

		//           -3            -33
		test_binop(0xFD, IntSDiv, 0xDF, 11, _8);
		test_binop(0xFFFD, IntSDiv, 0xFFDF, 11, _16);
		test_binop(0xFFFFFFFD, IntSDiv, 0xFFFFFFDF, 11, _32);
		test_binop(0xFFFFFFFF_FFFFFFFD, IntSDiv, 0xFFFFFFFF_FFFFFFDF, 11, _64);

		//           -3                -11
		test_binop(0xFD, IntSDiv, 33, 0xF5, _8);
		test_binop(0xFFFD, IntSDiv, 33, 0xFFF5, _16);
		test_binop(0xFFFFFFFD, IntSDiv, 33, 0xFFFFFFF5, _32);
		test_binop(0xFFFFFFFF_FFFFFFFD, IntSDiv, 33, 0xFFFFFFFF_FFFFFFF5, _64);

		//                    -33    -11
		test_binop(3, IntSDiv, 0xDF, 0xF5, _8);
		test_binop(3, IntSDiv, 0xFFDF, 0xFFF5, _16);
		test_binop(3, IntSDiv, 0xFFFFFFDF, 0xFFFFFFF5, _32);
		test_binop(3, IntSDiv, 0xFFFFFFFF_FFFFFFDF, 0xFFFFFFFF_FFFFFFF5, _64);

		test_binop_none(IntSDiv, 33, 0, _8);
		test_binop_none(IntSDiv, 33, 0, _16);
		test_binop_none(IntSDiv, 33, 0, _32);
		test_binop_none(IntSDiv, 33, 0, _64);

		test_binop(0, IntSDiv, 0xFF, 0x11, _8);
		test_binop(0, IntSDiv, 0xFFFF, 0x1111, _16);
		test_binop(0, IntSDiv, 0xFFFFFFFF, 0x11111111, _32);
		test_binop(0, IntSDiv, 0xFFFFFFFF_FFFFFFFF, 0x11111111_11111111, _64);
	}

	#[test]
	fn test_iumod() {
		use { IrBinOp::*, ValSize::* };

		test_binop(0, IntUMod, 33, 11, _8);
		test_binop(0, IntUMod, 33, 11, _16);
		test_binop(0, IntUMod, 33, 11, _32);
		test_binop(0, IntUMod, 33, 11, _64);

		test_binop(0, IntUMod, 0, 11, _64);
		test_binop(1, IntUMod, 1, 11, _64);
		test_binop(2, IntUMod, 2, 11, _64);
		test_binop(3, IntUMod, 3, 11, _64);
		test_binop(4, IntUMod, 4, 11, _64);
		test_binop(5, IntUMod, 5, 11, _64);
		test_binop(6, IntUMod, 6, 11, _64);
		test_binop(7, IntUMod, 7, 11, _64);
		test_binop(8, IntUMod, 8, 11, _64);
		test_binop(9, IntUMod, 9, 11, _64);
		test_binop(10, IntUMod, 10, 11, _64);
		test_binop(0, IntUMod, 11, 11, _64);

		test_binop_none(IntUMod, 33, 0, _8);
		test_binop_none(IntUMod, 33, 0, _16);
		test_binop_none(IntUMod, 33, 0, _32);
		test_binop_none(IntUMod, 33, 0, _64);

		test_binop(0, IntUMod, 0xFF, 0x11, _8);
		test_binop(0, IntUMod, 0xFFFF, 0x1111, _16);
		test_binop(0, IntUMod, 0xFFFFFFFF, 0x11111111, _32);
		test_binop(0, IntUMod, 0xFFFFFFFF_FFFFFFFF, 0x11111111_11111111, _64);
	}

	#[test]
	fn test_ismod() {
		use { IrBinOp::*, ValSize::* };

		// on positives it should behave the same as iumod
		test_binop(0, IntSMod, 33, 11, _8);
		test_binop(0, IntSMod, 33, 11, _16);
		test_binop(0, IntSMod, 33, 11, _32);
		test_binop(0, IntSMod, 33, 11, _64);

		test_binop(0, IntSMod, 0, 11, _64);
		test_binop(1, IntSMod, 1, 11, _64);
		test_binop(2, IntSMod, 2, 11, _64);
		test_binop(3, IntSMod, 3, 11, _64);
		test_binop(4, IntSMod, 4, 11, _64);
		test_binop(5, IntSMod, 5, 11, _64);
		test_binop(6, IntSMod, 6, 11, _64);
		test_binop(7, IntSMod, 7, 11, _64);
		test_binop(8, IntSMod, 8, 11, _64);
		test_binop(9, IntSMod, 9, 11, _64);
		test_binop(10, IntSMod, 10, 11, _64);
		test_binop(0, IntSMod, 11, 11, _64);

		test_binop_none(IntSMod, 33, 0, _8);
		test_binop_none(IntSMod, 33, 0, _16);
		test_binop_none(IntSMod, 33, 0, _32);
		test_binop_none(IntSMod, 33, 0, _64);

		// -1 % whatever == -1
		test_binop(0xFF, IntSMod, 0xFF, 0x11, _8);
		test_binop(0xFFFF, IntSMod, 0xFFFF, 0x1111, _16);
		test_binop(0xFFFFFFFF, IntSMod, 0xFFFFFFFF, 0x11111111, _32);
		test_binop(0xFFFFFFFF_FFFFFFFF, IntSMod, 0xFFFFFFFF_FFFFFFFF, 0x11111111_11111111, _64);

		test_binop(0xFF, IntSMod, 0xFF, 11, _8); // -1  % 11
		test_binop(0xFE, IntSMod, 0xFE, 11, _8); // -2  % 11
		test_binop(0xFD, IntSMod, 0xFD, 11, _8); // -3  % 11
		test_binop(0xFC, IntSMod, 0xFC, 11, _8); // -4  % 11
		test_binop(0xFB, IntSMod, 0xFB, 11, _8); // -5  % 11
		test_binop(0xFA, IntSMod, 0xFA, 11, _8); // -6  % 11
		test_binop(0xF9, IntSMod, 0xF9, 11, _8); // -7  % 11
		test_binop(0xF8, IntSMod, 0xF8, 11, _8); // -8  % 11
		test_binop(0xF7, IntSMod, 0xF7, 11, _8); // -9  % 11
		test_binop(0xF6, IntSMod, 0xF6, 11, _8); // -10 % 11
		test_binop(0x00, IntSMod, 0xF5, 11, _8); // -11 % 11

		test_binop(1, IntSMod, 1, 0xF5, _8);   // 1  % -11
		test_binop(2, IntSMod, 2, 0xF5, _8);   // 2  % -11
		test_binop(3, IntSMod, 3, 0xF5, _8);   // 3  % -11
		test_binop(4, IntSMod, 4, 0xF5, _8);   // 4  % -11
		test_binop(5, IntSMod, 5, 0xF5, _8);   // 5  % -11
		test_binop(6, IntSMod, 6, 0xF5, _8);   // 6  % -11
		test_binop(7, IntSMod, 7, 0xF5, _8);   // 7  % -11
		test_binop(8, IntSMod, 8, 0xF5, _8);   // 8  % -11
		test_binop(9, IntSMod, 9, 0xF5, _8);   // 9  % -11
		test_binop(10, IntSMod, 10, 0xF5, _8); // 10 % -11
		test_binop(0, IntSMod, 11, 0xF5, _8);  // 11 % -11

		test_binop(0xFF, IntSMod, 0xFF, 0xF5, _8); // -1  % -11
		test_binop(0xFE, IntSMod, 0xFE, 0xF5, _8); // -2  % -11
		test_binop(0xFD, IntSMod, 0xFD, 0xF5, _8); // -3  % -11
		test_binop(0xFC, IntSMod, 0xFC, 0xF5, _8); // -4  % -11
		test_binop(0xFB, IntSMod, 0xFB, 0xF5, _8); // -5  % -11
		test_binop(0xFA, IntSMod, 0xFA, 0xF5, _8); // -6  % -11
		test_binop(0xF9, IntSMod, 0xF9, 0xF5, _8); // -7  % -11
		test_binop(0xF8, IntSMod, 0xF8, 0xF5, _8); // -8  % -11
		test_binop(0xF7, IntSMod, 0xF7, 0xF5, _8); // -9  % -11
		test_binop(0xF6, IntSMod, 0xF6, 0xF5, _8); // -10 % -11
		test_binop(0x00, IntSMod, 0xF5, 0xF5, _8); // -11 % -11
	}

	#[test]
	fn test_ixor_iand_ior() {
		use { IrBinOp::*, ValSize::* };

		test_binop(0b10101100, IntXor, 0b11001010, 0b01100110, _8);
		test_binop(0b10101100, IntXor, 0b11001010, 0b01100110, _16);
		test_binop(0b10101100, IntXor, 0b11001010, 0b01100110, _32);
		test_binop(0b10101100, IntXor, 0b11001010, 0b01100110, _64);

		test_binop(0b01000010, IntAnd, 0b11001010, 0b01100110, _8);
		test_binop(0b01000010, IntAnd, 0b11001010, 0b01100110, _16);
		test_binop(0b01000010, IntAnd, 0b11001010, 0b01100110, _32);
		test_binop(0b01000010, IntAnd, 0b11001010, 0b01100110, _64);

		test_binop(0b11101110, IntOr,  0b11001010, 0b01100110, _8);
		test_binop(0b11101110, IntOr,  0b11001010, 0b01100110, _16);
		test_binop(0b11101110, IntOr,  0b11001010, 0b01100110, _32);
		test_binop(0b11101110, IntOr,  0b11001010, 0b01100110, _64);
	}

	#[test]
	fn test_ishl() {
		use { IrBinOp::*, ValSize::* };

		test_binop(0x01, IntShl, 0x01, 0x00, _8);
		test_binop(0x02, IntShl, 0x01, 0x01, _8);
		test_binop(0x80, IntShl, 0x01, 0x07, _8);
		test_binop(0x00, IntShl, 0x01, 0x08, _8);
		test_binop(0xF0, IntShl, 0x3F, 0x04, _8);

		test_binop(0x0001, IntShl, 0x01, 0x00, _16);
		test_binop(0x0100, IntShl, 0x01, 0x08, _16);
		test_binop(0x8000, IntShl, 0x01, 0x0F, _16);
		test_binop(0x0000, IntShl, 0x01, 0x10, _16);
		test_binop(0xFF00, IntShl, 0x3FFF, 0x08, _16);

		test_binop(0x00000001, IntShl, 0x01, 0x00, _32);
		test_binop(0x00010000, IntShl, 0x01, 0x10, _32);
		test_binop(0x80000000, IntShl, 0x01, 0x1F, _32);
		test_binop(0x00000000, IntShl, 0x01, 0x20, _32);
		test_binop(0xABCD0000, IntShl, 0x3FFFABCD, 0x10, _32);

		test_binop(0x00000000_00000001, IntShl, 0x01, 0x00, _64);
		test_binop(0x00000001_00000000, IntShl, 0x01, 0x20, _64);
		test_binop(0x80000000_00000000, IntShl, 0x01, 0x3F, _64);
		test_binop(0x00000000_00000000, IntShl, 0x01, 0x40, _64);
		test_binop(0xBEEFFACE_00000000, IntShl, 0xABCD1234_BEEFFACE, 0x20, _64);
	}

	#[test]
	fn test_iushr() {
		use { IrBinOp::*, ValSize::* };

		test_binop(0x01, IntUShr, 0x01, 0x00, _8);
		test_binop(0x01, IntUShr, 0x02, 0x01, _8);
		test_binop(0x01, IntUShr, 0x80, 0x07, _8);
		test_binop(0x00, IntUShr, 0xFF, 0x08, _8);
		test_binop(0x03, IntUShr, 0x3F, 0x04, _8);

		test_binop(0x0001, IntUShr, 0x0001, 0x00, _16);
		test_binop(0x0001, IntUShr, 0x0100, 0x08, _16);
		test_binop(0x0001, IntUShr, 0x8000, 0x0F, _16);
		test_binop(0x0000, IntUShr, 0xFFFF, 0x10, _16);
		test_binop(0x003F, IntUShr, 0x3F0B, 0x08, _16);

		test_binop(0x00000001, IntUShr, 0x00000001, 0x00, _32);
		test_binop(0x00000001, IntUShr, 0x00010000, 0x10, _32);
		test_binop(0x00000001, IntUShr, 0x80000000, 0x1F, _32);
		test_binop(0x00000000, IntUShr, 0xFFFFFFFF, 0x20, _32);
		test_binop(0x00001234, IntUShr, 0x1234ABCD, 0x10, _32);

		test_binop(0x00000000_00000001, IntUShr, 0x00000000_00000001, 0x00, _64);
		test_binop(0x00000000_00000001, IntUShr, 0x00000001_00000000, 0x20, _64);
		test_binop(0x00000000_00000001, IntUShr, 0x80000000_00000000, 0x3F, _64);
		test_binop(0x00000000_00000000, IntUShr, 0xFFFFFFFF_FFFFFFFF, 0x40, _64);
		test_binop(0x00000000_C0DEBEEF, IntUShr, 0xC0DEBEEF_FACECACE, 0x20, _64);
	}

	#[test]
	fn test_isshr() {
		use { IrBinOp::*, ValSize::* };

		test_binop(0x01, IntSShr, 0x01, 0x00, _8);
		test_binop(0x01, IntSShr, 0x02, 0x01, _8);
		test_binop(0x00, IntSShr, 0x7F, 0x08, _8);
		test_binop(0xFF, IntSShr, 0x80, 0x07, _8);
		test_binop(0xFF, IntSShr, 0xFF, 0x08, _8);
		test_binop(0x03, IntSShr, 0x3F, 0x04, _8);

		test_binop(0x0001, IntSShr, 0x0001, 0x00, _16);
		test_binop(0x0001, IntSShr, 0x0100, 0x08, _16);
		test_binop(0xFFFF, IntSShr, 0x8000, 0x0F, _16);
		test_binop(0x0000, IntSShr, 0x7FFF, 0x10, _16);
		test_binop(0xFFFF, IntSShr, 0xFFFF, 0x10, _16);
		test_binop(0x003F, IntSShr, 0x3F0B, 0x08, _16);

		test_binop(0x00000001, IntSShr, 0x00000001, 0x00, _32);
		test_binop(0x00000001, IntSShr, 0x00010000, 0x10, _32);
		test_binop(0xFFFFFFFF, IntSShr, 0x80000000, 0x1F, _32);
		test_binop(0x00000000, IntSShr, 0x7FFFFFFF, 0x20, _32);
		test_binop(0xFFFFFFFF, IntSShr, 0xFFFFFFFF, 0x20, _32);
		test_binop(0x00001234, IntSShr, 0x1234ABCD, 0x10, _32);

		test_binop(0x00000000_00000001, IntSShr, 0x00000000_00000001, 0x00, _64);
		test_binop(0x00000000_00000001, IntSShr, 0x00000001_00000000, 0x20, _64);
		test_binop(0xFFFFFFFF_FFFFFFFF, IntSShr, 0x80000000_00000000, 0x3F, _64);
		test_binop(0x00000000_00000000, IntSShr, 0x7FFFFFFF_FFFFFFFF, 0x40, _64);
		test_binop(0xFFFFFFFF_FFFFFFFF, IntSShr, 0xFFFFFFFF_FFFFFFFF, 0x40, _64);
		test_binop(0xFFFFFFFF_C0DEBEEF, IntSShr, 0xC0DEBEEF_FACECACE, 0x20, _64);
	}

	#[test]
	fn test_ipair() {
		use { IrBinOp::*, ValSize::* };

		test_binop(0x1234,              IntPair, 0x12,       0x34,       _8);
		test_binop(0x12345678,          IntPair, 0x1234,     0x5678,     _16);
		test_binop(0x12345678_ABCDEF97, IntPair, 0x12345678, 0xABCDEF97, _32);
	}

	#[test]
	fn test_bxor_band_bor() {
		use { IrBinOp::*, ValSize::* };

		test_binop(0, BoolXor, 0, 0, _8);
		test_binop(1, BoolXor, 0, 1, _8);
		test_binop(1, BoolXor, 1, 0, _8);
		test_binop(0, BoolXor, 1, 1, _8);

		test_binop(0, BoolAnd, 0, 0, _8);
		test_binop(0, BoolAnd, 0, 1, _8);
		test_binop(0, BoolAnd, 1, 0, _8);
		test_binop(1, BoolAnd, 1, 1, _8);

		test_binop(0, BoolOr, 0, 0, _8);
		test_binop(1, BoolOr, 0, 1, _8);
		test_binop(1, BoolOr, 1, 0, _8);
		test_binop(1, BoolOr, 1, 1, _8);
	}

	#[test]
	fn test_iuaddc() {
		use { IrTernOp::*, ValSize::* };
		test_ternop(0x08, IntUAddC, 0x03, 0x05, 0, _8);
		test_ternop(0x09, IntUAddC, 0x03, 0x05, 1, _8);
		test_ternop(0xFF, IntUAddC, 0xFF, 0x00, 0, _8);
		test_ternop(0x00, IntUAddC, 0xFF, 0x00, 1, _8);
		test_ternop(0x0008, IntUAddC, 0x0003, 0x0005, 0, _16);
		test_ternop(0x0009, IntUAddC, 0x0003, 0x0005, 1, _16);
		test_ternop(0x00000008, IntUAddC, 0x00000003, 0x00000005, 0, _32);
		test_ternop(0x00000009, IntUAddC, 0x00000003, 0x00000005, 1, _32);
		test_ternop(0x00000000_00000008, IntUAddC, 0x00000000_00000003, 0x00000000_00000005, 0, _64);
		test_ternop(0x00000000_00000009, IntUAddC, 0x00000000_00000003, 0x00000000_00000005, 1, _64);

		test_ternop(0x02, IntUAddC, 0xFD, 0x05, 0, _8);
		test_ternop(0x03, IntUAddC, 0xFD, 0x05, 1, _8);
		test_ternop(0x0002, IntUAddC, 0xFFFD, 0x0005, 0, _16);
		test_ternop(0x0003, IntUAddC, 0xFFFD, 0x0005, 1, _16);
		test_ternop(0x00000002, IntUAddC, 0xFFFFFFFD, 0x00000005, 0, _32);
		test_ternop(0x00000003, IntUAddC, 0xFFFFFFFD, 0x00000005, 1, _32);
		test_ternop(0x00000000_00000002, IntUAddC, 0xFFFFFFFF_FFFFFFFD, 0x00000000_00000005, 0, _64);
		test_ternop(0x00000000_00000003, IntUAddC, 0xFFFFFFFF_FFFFFFFD, 0x00000000_00000005, 1, _64);
	}

	#[test]
	fn test_iusubb() {
		use { IrTernOp::*, ValSize::* };
		test_ternop(0x03, IntUSubB, 0x08, 0x05, 0, _8);
		test_ternop(0x02, IntUSubB, 0x08, 0x05, 1, _8);
		test_ternop(0x0003, IntUSubB, 0x0008, 0x0005, 0, _16);
		test_ternop(0x0002, IntUSubB, 0x0008, 0x0005, 1, _16);
		test_ternop(0x00000003, IntUSubB, 0x00000008, 0x00000005, 0, _32);
		test_ternop(0x00000002, IntUSubB, 0x00000008, 0x00000005, 1, _32);
		test_ternop(0x00000000_00000003, IntUSubB, 0x00000000_00000008, 0x00000000_00000005, 0, _64);
		test_ternop(0x00000000_00000002, IntUSubB, 0x00000000_00000008, 0x00000000_00000005, 1, _64);

		test_ternop(0xFE, IntUSubB, 0x03, 0x05, 0, _8);
		test_ternop(0xFD, IntUSubB, 0x03, 0x05, 1, _8);
		test_ternop(0xFFFE, IntUSubB, 0x0003, 0x0005, 0, _16);
		test_ternop(0xFFFD, IntUSubB, 0x0003, 0x0005, 1, _16);
		test_ternop(0xFFFFFFFE, IntUSubB, 0x00000003, 0x00000005, 0, _32);
		test_ternop(0xFFFFFFFD, IntUSubB, 0x00000003, 0x00000005, 1, _32);
		test_ternop(0xFFFFFFFF_FFFFFFFE, IntUSubB, 0x00000000_00000003, 0x00000000_00000005, 0, _64);
		test_ternop(0xFFFFFFFF_FFFFFFFD, IntUSubB, 0x00000000_00000003, 0x00000000_00000005, 1, _64);
	}

	#[test]
	fn test_iucarryc() {
		use { IrTernOp::*, ValSize::* };
		test_ternop(0, IntCarryC, 0x08, 0x05, 0, _8);
		test_ternop(0, IntCarryC, 0x0008, 0x0005, 0, _16);
		test_ternop(0, IntCarryC, 0x00000008, 0x00000005, 0, _32);
		test_ternop(0, IntCarryC, 0x00000000_00000008, 0x00000000_00000005, 0, _64);

		test_ternop(0, IntCarryC, 0x7F, 0x01, 0, _8);
		test_ternop(0, IntCarryC, 0x7F, 0x00, 1, _8);
		test_ternop(0, IntCarryC, 0x7F, 0x01, 1, _8);

		test_ternop(0, IntCarryC, 0xFF, 0x00, 0, _8);
		test_ternop(1, IntCarryC, 0xFF, 0x01, 0, _8);
		test_ternop(1, IntCarryC, 0xFF, 0x00, 1, _8);
		test_ternop(1, IntCarryC, 0xFF, 0x01, 1, _8);

		test_ternop(0, IntCarryC, 0xFE, 0x00, 0, _8);
		test_ternop(0, IntCarryC, 0xFE, 0x00, 1, _8);
		test_ternop(0, IntCarryC, 0xFE, 0x01, 0, _8);
		test_ternop(1, IntCarryC, 0xFE, 0x01, 1, _8);

		test_ternop(0, IntCarryC, 0x00FF, 0x0001, 0, _16);
		test_ternop(0, IntCarryC, 0x000000FF, 0x00000001, 0, _32);
		test_ternop(0, IntCarryC, 0x00000000_000000FF, 0x00000000_00000001, 0, _64);

		test_ternop(0, IntCarryC, 0x7FFF, 0x0001, 0, _16);
		test_ternop(0, IntCarryC, 0x7FFF, 0x0000, 1, _16);
		test_ternop(0, IntCarryC, 0x7FFF, 0x0001, 1, _16);

		test_ternop(0, IntCarryC, 0xFFFF, 0x0000, 0, _16);
		test_ternop(1, IntCarryC, 0xFFFF, 0x0001, 0, _16);
		test_ternop(1, IntCarryC, 0xFFFF, 0x0000, 1, _16);
		test_ternop(1, IntCarryC, 0xFFFF, 0x0001, 1, _16);

		test_ternop(0, IntCarryC, 0xFFFE, 0x0000, 0, _16);
		test_ternop(0, IntCarryC, 0xFFFE, 0x0001, 0, _16);
		test_ternop(0, IntCarryC, 0xFFFE, 0x0000, 1, _16);
		test_ternop(1, IntCarryC, 0xFFFE, 0x0001, 1, _16);

		test_ternop(0, IntCarryC, 0x0000FFFF, 0x00000001, 0, _32);
		test_ternop(0, IntCarryC, 0x00000000_0000FFFF, 0x00000000_00000001, 0, _64);

		test_ternop(0, IntCarryC, 0x7FFFFFFF, 0x00000001, 0, _32);
		test_ternop(0, IntCarryC, 0x7FFFFFFF, 0x00000000, 1, _32);
		test_ternop(0, IntCarryC, 0x7FFFFFFF, 0x00000001, 1, _32);

		test_ternop(0, IntCarryC, 0xFFFFFFFF, 0x00000000, 0, _32);
		test_ternop(1, IntCarryC, 0xFFFFFFFF, 0x00000001, 0, _32);
		test_ternop(1, IntCarryC, 0xFFFFFFFF, 0x00000000, 1, _32);
		test_ternop(1, IntCarryC, 0xFFFFFFFF, 0x00000001, 1, _32);

		test_ternop(0, IntCarryC, 0xFFFFFFFE, 0x00000000, 0, _32);
		test_ternop(0, IntCarryC, 0xFFFFFFFE, 0x00000001, 0, _32);
		test_ternop(0, IntCarryC, 0xFFFFFFFE, 0x00000000, 1, _32);
		test_ternop(1, IntCarryC, 0xFFFFFFFE, 0x00000001, 1, _32);

		test_ternop(0, IntCarryC, 0x00000000_FFFFFFFF, 0x00000000_00000001, 0, _64);

		test_ternop(0, IntCarryC, 0x7FFFFFFF_FFFFFFFF, 0x00000000_00000001, 0, _64);
		test_ternop(0, IntCarryC, 0x7FFFFFFF_FFFFFFFF, 0x00000000_00000000, 1, _64);
		test_ternop(0, IntCarryC, 0x7FFFFFFF_FFFFFFFF, 0x00000000_00000001, 1, _64);

		test_ternop(0, IntCarryC, 0xFFFFFFFF_FFFFFFFF, 0x00000000_00000000, 0, _64);
		test_ternop(1, IntCarryC, 0xFFFFFFFF_FFFFFFFF, 0x00000000_00000001, 0, _64);
		test_ternop(1, IntCarryC, 0xFFFFFFFF_FFFFFFFF, 0x00000000_00000000, 1, _64);
		test_ternop(1, IntCarryC, 0xFFFFFFFF_FFFFFFFF, 0x00000000_00000001, 1, _64);

		test_ternop(0, IntCarryC, 0xFFFFFFFF_FFFFFFFE, 0x00000000_00000000, 0, _64);
		test_ternop(0, IntCarryC, 0xFFFFFFFF_FFFFFFFE, 0x00000000_00000001, 0, _64);
		test_ternop(0, IntCarryC, 0xFFFFFFFF_FFFFFFFE, 0x00000000_00000000, 1, _64);
		test_ternop(1, IntCarryC, 0xFFFFFFFF_FFFFFFFE, 0x00000000_00000001, 1, _64);
	}

	#[test]
	fn test_iscarryc() {
		use { IrTernOp::*, ValSize::* };
		test_ternop(0, IntSCarryC, 0x08, 0x05, 0, _8);
		test_ternop(0, IntSCarryC, 0x0008, 0x0005, 0, _16);
		test_ternop(0, IntSCarryC, 0x00000008, 0x00000005, 0, _32);
		test_ternop(0, IntSCarryC, 0x00000000_00000008, 0x00000000_00000005, 0, _64);

		test_ternop(0, IntSCarryC, 0x7F, 0x00, 0, _8);
		test_ternop(1, IntSCarryC, 0x7F, 0x01, 0, _8);
		test_ternop(1, IntSCarryC, 0x7F, 0x00, 1, _8);
		test_ternop(1, IntSCarryC, 0x7E, 0x01, 1, _8);

		test_ternop(0, IntSCarryC, 0x80, 0x00, 0, _8);
		test_ternop(0, IntSCarryC, 0x80, 0x00, 1, _8);
		test_ternop(1, IntSCarryC, 0x80, 0xFF, 0, _8);
		// despite it doing "x + -1 + 1" there IS still a carry here. the 6502 at least agrees.
		test_ternop(1, IntSCarryC, 0x80, 0xFF, 1, _8);

		test_ternop(0, IntSCarryC, 0x80, 0xFF, 0, _16);
		test_ternop(0, IntSCarryC, 0x80, 0xFF, 1, _16);
		test_ternop(1, IntSCarryC, 0x8000, 0xFFFF, 0, _16);
		test_ternop(1, IntSCarryC, 0x8000, 0xFFFF, 1, _16);
		test_ternop(0, IntSCarryC, 0x8000, 0xFFFF, 0, _32);
		test_ternop(0, IntSCarryC, 0x8000, 0xFFFF, 1, _32);
		test_ternop(1, IntSCarryC, 0x80000000, 0xFFFFFFFF, 0, _32);
		test_ternop(1, IntSCarryC, 0x80000000, 0xFFFFFFFF, 1, _32);
		test_ternop(0, IntSCarryC, 0x80000000, 0xFFFFFFFF, 0, _64);
		test_ternop(0, IntSCarryC, 0x80000000, 0xFFFFFFFF, 1, _64);
		test_ternop(1, IntSCarryC, 0x80000000_00000000, 0xFFFFFFFF_FFFFFFFF, 0, _64);
		test_ternop(1, IntSCarryC, 0x80000000_00000000, 0xFFFFFFFF_FFFFFFFF, 1, _64);

		test_ternop(0, IntSCarryC, 0xFE, 0x05, 0, _8);
		test_ternop(0, IntSCarryC, 0xFFFE, 0x0005, 0, _16);
		test_ternop(0, IntSCarryC, 0xFFFFFFFE, 0x00000005, 0, _32);
		test_ternop(0, IntSCarryC, 0xFFFFFFFF_FFFFFFFE, 0x00000000_00000005, 0, _64);

		test_ternop(1, IntSCarryC, 0x7F, 0x01, 0, _8);
		test_ternop(1, IntSCarryC, 0x7F, 0x00, 1, _8);
		test_ternop(1, IntSCarryC, 0x80, 0xFF, 0, _8);
		test_ternop(0, IntSCarryC, 0xFF, 0x01, 0, _8);
		test_ternop(0, IntSCarryC, 0xFF, 0x00, 1, _8);
		test_ternop(0, IntSCarryC, 0x00FF, 0x0001, 0, _16);
		test_ternop(0, IntSCarryC, 0x00FF, 0x0000, 1, _16);
		test_ternop(0, IntSCarryC, 0x000000FF, 0x00000001, 0, _32);
		test_ternop(0, IntSCarryC, 0x000000FF, 0x00000000, 1, _32);
		test_ternop(0, IntSCarryC, 0x00000000_000000FF, 0x00000000_00000001, 0, _64);
		test_ternop(0, IntSCarryC, 0x00000000_000000FF, 0x00000000_00000000, 1, _64);

		test_ternop(1, IntSCarryC, 0x7FFF, 0x0001, 0, _16);
		test_ternop(1, IntSCarryC, 0x7FFF, 0x0000, 1, _16);
		test_ternop(1, IntSCarryC, 0x8000, 0xFFFF, 0, _16);
		test_ternop(0, IntSCarryC, 0xFFFF, 0x0001, 0, _16);
		test_ternop(0, IntSCarryC, 0xFFFF, 0x0000, 1, _16);
		test_ternop(0, IntSCarryC, 0x0000FFFF, 0x00000001, 0, _32);
		test_ternop(0, IntSCarryC, 0x0000FFFF, 0x00000000, 1, _32);
		test_ternop(0, IntSCarryC, 0x00000000_0000FFFF, 0x00000000_00000001, 0, _64);
		test_ternop(0, IntSCarryC, 0x00000000_0000FFFF, 0x00000000_00000000, 1, _64);

		test_ternop(1, IntSCarryC, 0x7FFFFFFF, 0x00000001, 0, _32);
		test_ternop(1, IntSCarryC, 0x7FFFFFFF, 0x00000000, 1, _32);
		test_ternop(1, IntSCarryC, 0x80000000, 0xFFFFFFFF, 0, _32);
		test_ternop(0, IntSCarryC, 0xFFFFFFFF, 0x00000001, 0, _32);
		test_ternop(0, IntSCarryC, 0xFFFFFFFF, 0x00000000, 1, _32);
		test_ternop(0, IntSCarryC, 0x00000000_FFFFFFFF, 0x00000000_00000001, 0, _64);
		test_ternop(0, IntSCarryC, 0x00000000_FFFFFFFF, 0x00000000_00000000, 1, _64);

		test_ternop(1, IntSCarryC, 0x7FFFFFFF_FFFFFFFF, 0x00000000_00000001, 0, _64);
		test_ternop(1, IntSCarryC, 0x7FFFFFFF_FFFFFFFF, 0x00000000_00000000, 1, _64);
		test_ternop(1, IntSCarryC, 0x80000000_00000000, 0xFFFFFFFF_FFFFFFFF, 0, _64);
		test_ternop(0, IntSCarryC, 0xFFFFFFFF_FFFFFFFF, 0x00000000_00000001, 0, _64);
		test_ternop(0, IntSCarryC, 0xFFFFFFFF_FFFFFFFF, 0x00000000_00000000, 1, _64);
	}

	// I'll be honest I'm running out of steam here

	#[test]
	fn test_isborrowc() {
		use { IrTernOp::*, ValSize::* };
		test_ternop(0, IntSBorrowC, 0x08, 0x05, 0, _8);
		test_ternop(0, IntSBorrowC, 0x08, 0x05, 1, _8);
		test_ternop(0, IntSBorrowC, 0x0008, 0x0005, 0, _16);
		test_ternop(0, IntSBorrowC, 0x0008, 0x0005, 1, _16);
		test_ternop(0, IntSBorrowC, 0x00000008, 0x00000005, 0, _32);
		test_ternop(0, IntSBorrowC, 0x00000008, 0x00000005, 1, _32);
		test_ternop(0, IntSBorrowC, 0x00000000_00000008, 0x00000000_00000005, 0, _64);
		test_ternop(0, IntSBorrowC, 0x00000000_00000008, 0x00000000_00000005, 1, _64);

		test_ternop(0, IntSBorrowC, 0xFE, 0x05, 0, _8);
		test_ternop(0, IntSBorrowC, 0xFE, 0x05, 1, _8);
		test_ternop(0, IntSBorrowC, 0xFFFE, 0x0005, 0, _16);
		test_ternop(0, IntSBorrowC, 0xFFFE, 0x0005, 1, _16);
		test_ternop(0, IntSBorrowC, 0xFFFFFFFE, 0x00000005, 0, _32);
		test_ternop(0, IntSBorrowC, 0xFFFFFFFE, 0x00000005, 1, _32);
		test_ternop(0, IntSBorrowC, 0xFFFFFFFF_FFFFFFFE, 0x00000000_00000005, 0, _64);
		test_ternop(0, IntSBorrowC, 0xFFFFFFFF_FFFFFFFE, 0x00000000_00000005, 1, _64);

		test_ternop(1, IntSBorrowC, 0x7F, 0xFF, 0, _8);
		test_ternop(1, IntSBorrowC, 0x80, 0x01, 0, _8);
		test_ternop(1, IntSBorrowC, 0x80, 0x00, 1, _8);
		test_ternop(0, IntSBorrowC, 0xFF, 0xFF, 0, _8);
		test_ternop(0, IntSBorrowC, 0x00FF, 0x00FF, 0, _16);
		test_ternop(0, IntSBorrowC, 0x000000FF, 0x000000FF, 0, _32);
		test_ternop(0, IntSBorrowC, 0x00000000_000000FF, 0x00000000_000000FF, 0, _64);

		test_ternop(1, IntSBorrowC, 0x7FFF, 0xFFFF, 0, _16);
		test_ternop(1, IntSBorrowC, 0x8000, 0x0001, 0, _16);
		test_ternop(1, IntSBorrowC, 0x8000, 0x0000, 1, _16);
		test_ternop(0, IntSBorrowC, 0xFFFF, 0xFFFF, 0, _16);
		test_ternop(0, IntSBorrowC, 0x0000FFFF, 0x0000FFFF, 0, _32);
		test_ternop(0, IntSBorrowC, 0x00000000_0000FFFF, 0x00000000_0000FFFF, 0, _64);

		test_ternop(1, IntSBorrowC, 0x7FFFFFFF, 0xFFFFFFFF, 0, _32);
		test_ternop(1, IntSBorrowC, 0x80000000, 0x00000001, 0, _32);
		test_ternop(1, IntSBorrowC, 0x80000000, 0x00000000, 1, _32);
		test_ternop(0, IntSBorrowC, 0xFFFFFFFF, 0xFFFFFFFF, 0, _32);
		test_ternop(0, IntSBorrowC, 0x00000000_FFFFFFFF, 0x00000000_FFFFFFFF, 0, _64);

		test_ternop(1, IntSBorrowC, 0x7FFFFFFF_FFFFFFFF, 0xFFFFFFFF_FFFFFFFF, 0, _64);
		test_ternop(1, IntSBorrowC, 0x80000000_00000000, 0x00000000_00000001, 0, _64);
		test_ternop(1, IntSBorrowC, 0x80000000_00000000, 0x00000000_00000000, 1, _64);
		test_ternop(0, IntSBorrowC, 0xFFFFFFFF_FFFFFFFF, 0xFFFFFFFF_FFFFFFFF, 0, _64);
	}
}