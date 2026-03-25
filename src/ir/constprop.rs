
use std::collections::{ BTreeMap };

use petgraph::visit::{ DfsPostOrder };

use crate::dataflow::{ JoinSemiLattice, DataflowCfg, DataflowAlgorithm };

use super::*;

// ------------------------------------------------------------------------------------------------
// Submodules
// ------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests;

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
				// TODO: how DO we handle this? just pick from1 or from2 or merge them somehow?
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
// Propagator
// ------------------------------------------------------------------------------------------------

impl DataflowCfg<IrBBId> for IrCfg {
	fn num_nodes(&self) -> usize {
		self.node_count()
	}

	fn initial_order(&self) -> impl Iterator<Item = IrBBId> {
		let mut rpo = Vec::<IrBBId>::with_capacity(self.num_nodes());
		let mut postorder = DfsPostOrder::new(self, 0);
		while let Some(id) = postorder.next(self) {
			rpo.push(id);
		}

		rpo.into_iter().rev()
	}

	fn successors(&self, id: IrBBId) -> impl Iterator<Item = IrBBId> {
		self.edges(id).map(|(_, succ, _)| succ)
	}
}

/// Results of constant propagation. It maps from IR Registers to a tuple of:
///
/// - The determined constant value for that register
/// - A list of up to 3 sources from which that constant was computed
///
/// The sources can be used to propagate information backwards, such as in cases
/// where a constant address is computed by combining two smaller pieces, and those
/// smaller pieces need to be marked as references to that address.
pub(crate) type ConstPropResults = BTreeMap<IrReg, (u64, [Option<IrSrc>; 3])>;

type ConstPropState = BTreeMap<IrReg, Info>;

/// Runs constant propagation on the given IR code and CFG.
pub(crate) fn propagate_constants(bbs: &[IrBasicBlock], cfg: &IrCfg) -> ConstPropResults {
	// since each variable is only assigned once, there's no need to track changing state -
	// the state of a variable is determined at its def.
	let mut prop = Propagator::new(bbs);
	prop.run(cfg);
	prop.finish()
}

struct Propagator<'bb> {
	bbs:   &'bb [IrBasicBlock],
	state: ConstPropState,
}

impl<'bb> Propagator<'bb> {
	fn new(bbs: &'bb [IrBasicBlock]) -> Self {
		Self {
			state: find_all_regs(bbs)
				.into_iter()
				.map(|r| (r, Info::Unk))
				.collect(),
			bbs,
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
}

impl<'bb> DataflowAlgorithm for Propagator<'bb> {
	type ID = IrBBId;

	fn visit(&mut self, bbid: IrBBId) -> bool {
		let mut changed = false;

		for phi in self.bbs[bbid].phis() {
			changed |= phi_join(phi, &mut self.state);
		}

		for inst in self.bbs[bbid].insts() {
			changed |= transfer(inst, &mut self.state);
		}

		changed
	}
}

// ------------------------------------------------------------------------------------------------
// Phi join function
// ------------------------------------------------------------------------------------------------

fn phi_join(phi: &IrPhi, state: &mut ConstPropState) -> bool {
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

fn transfer(inst: &IrInst, state: &mut ConstPropState) -> bool {
	use IrInstKind::*;

	let src_to_info = |src: IrSrc, state: &ConstPropState| {
		match src {
			IrSrc::Reg(reg)   => state[&reg],
			IrSrc::Const(c)   => Info::some1(c.val(), src),
			IrSrc::Return(..) => Info::Any,
		}
	};

	let thing = match inst.kind() {
		// no change!
		Nop | Use { .. } | Store { .. } | Branch { .. } | CBranch { .. } | IBranch { .. }
		| Call { .. } | ICall { .. } | Ret { .. } | Halt => None,

		Mov  { dst, src, .. } => Some((dst, src_to_info(src, state))),
		Load { dst, .. }      => Some((dst, Info::Any)),

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
		Zxt => val,
		// IrInst::sxt ensures that src_size < dst_size
		Sxt => match src_size {
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
		Lo => match src_size {
			ValSize::_8  => unreachable!(),
			ValSize::_16 => val & 0xFF,
			ValSize::_32 => val & 0xFFFF,
			ValSize::_64 => val & 0xFFFFFFFF,
		},
		Hi => match src_size {
			ValSize::_8  => unreachable!(),
			ValSize::_16 => (val >>  8) & 0xFF,
			ValSize::_32 => (val >> 16) & 0xFFFF,
			ValSize::_64 => (val >> 32) & 0xFFFFFFFF,
		},
		Neg => match src_size {
			ValSize::_8 =>  (-(val as i8 )) as u8 as u64,
			ValSize::_16 => (-(val as i16)) as u16 as u64,
			ValSize::_32 => (-(val as i32)) as u32 as u64,
			ValSize::_64 => (-(val as i64)) as u64,
		},
		INot => match src_size {
			ValSize::_8 =>  (!(val as i8 )) as u8 as u64,
			ValSize::_16 => (!(val as i16)) as u16 as u64,
			ValSize::_32 => (!(val as i32)) as u32 as u64,
			ValSize::_64 => (!(val as i64)) as u64,
		},
		BNot => (val == 0) as u64,
	}
}

/// Wraps the given value to `NBITS` bits. (Just masks off any higher bits)
fn mask_to<const NBITS: usize>(v: u64) -> u64 {
	v & ((1 << NBITS) - 1)
}

/// Computes the carry-outs of all places in the unsigned addition `a + b + ci_0`. `ci_0` is meant
/// to be the carry in to bit 0, and should be 0 or 1. (This is untested for values of `ci_0` other
/// than 0 or 1.)
///
/// This is worst-case linear time in the number of bits, but has early-out if the carries stabilize
/// sooner so can complete in as little as 1 iteration.
fn carries<const NBITS: usize>(a: u64, b: u64, ci_0: u64) -> u64 {
	let mut ci = ci_0;
	let mut co = 0;
	let mut old_co = co;
	for _ in 0 .. NBITS {
		co = ((a ^ b) & ci) | (a & b);
		// early out if it stabilized
		if co == old_co {
			break;
		}
		old_co = co;
		ci = (co << 1) | ci_0;
	}

	// expression below was for testing, should be equal to a+b+ci_0 (and it was, for all
	// combinations of ci_0 in {0, 1}, a in {0, 65535}, and b in {0, 65535}
	co //, mask_to::<NBITS>(a ^ b ^ ci))
}

/// Computes the borrow-outs of all places in the unsigned subtraction `a - b - bi_0`. `bi_0` is
/// meant to be the borrow in to bit 0, and should be 0 or 1. (This is untested for values of
/// `bi_0` other than 0 or 1.)
///
/// This is worst-case linear time in the number of bits, but has early-out if the borrows stabilize
/// sooner so can complete in as little as 1 iteration.
fn borrows<const NBITS: usize>(a: u64, b: u64, bi_0: u64) -> u64 {
	let mut bi = bi_0;
	let mut bo = 0;
	let mut old_bo = bo;
	for _ in 0 .. NBITS {
		bo = (mask_to::<NBITS>(!(a ^ b)) & bi) | (mask_to::<NBITS>(!a) & b);
		// early out if it stabilized
		if bo == old_bo {
			break;
		}
		old_bo = bo;
		bi = (bo << 1) | bi_0;
	}
	// expression below was for testing, should be equal to a-b-bi_0 (and it was, for all
	// combinations of bi_0 in {0, 1}, a in {0, 65535}, and b in {0, 65535}
	bo //, mask_to::<NBITS>(a ^ b ^ bi))
}

fn do_binop(op: IrBinOp, val1: u64, val2: u64, size: ValSize) -> Option<u64> {
	use IrBinOp::*;

	let val = match op {
		Eq  => (val1 == val2) as u64,
		Ne  => (val1 != val2) as u64,

		Slt => match size {
			ValSize::_8  => ((val1 as i8)  < (val2 as i8)) as u64,
			ValSize::_16 => ((val1 as i16) < (val2 as i16)) as u64,
			ValSize::_32 => ((val1 as i32) < (val2 as i32)) as u64,
			ValSize::_64 => ((val1 as i64) < (val2 as i64)) as u64,
		},
		Sle => match size {
			ValSize::_8  => ((val1 as i8)  <= (val2 as i8)) as u64,
			ValSize::_16 => ((val1 as i16) <= (val2 as i16)) as u64,
			ValSize::_32 => ((val1 as i32) <= (val2 as i32)) as u64,
			ValSize::_64 => ((val1 as i64) <= (val2 as i64)) as u64,
		},

		Ult => (val1 < val2) as u64,
		Ule => (val1 <= val2) as u64,

		Add => match size {
			ValSize::_8  => (val1 as u8).wrapping_add(val2 as u8) as u64,
			ValSize::_16 => (val1 as u16).wrapping_add(val2 as u16) as u64,
			ValSize::_32 => (val1 as u32).wrapping_add(val2 as u32) as u64,
			ValSize::_64 => val1.wrapping_add(val2),
		}
		Sub  => match size {
			ValSize::_8  => (val1 as u8).wrapping_sub(val2 as u8) as u64,
			ValSize::_16 => (val1 as u16).wrapping_sub(val2 as u16) as u64,
			ValSize::_32 => (val1 as u32).wrapping_sub(val2 as u32) as u64,
			ValSize::_64 => val1.wrapping_sub(val2),
		}
		UCarry => match size {
			ValSize::_8  => (val1 as u8).overflowing_add(val2 as u8).1 as u64,
			ValSize::_16 => (val1 as u16).overflowing_add(val2 as u16).1 as u64,
			ValSize::_32 => (val1 as u32).overflowing_add(val2 as u32).1 as u64,
			ValSize::_64 => val1.overflowing_add(val2).1 as u64,
		}
		SCarry => match size {
			ValSize::_8  => (val1 as i8).overflowing_add(val2 as i8).1 as u64,
			ValSize::_16 => (val1 as i16).overflowing_add(val2 as i16).1 as u64,
			ValSize::_32 => (val1 as i32).overflowing_add(val2 as i32).1 as u64,
			ValSize::_64 => (val1 as i64).overflowing_add(val2 as i64).1 as u64,
		}
		SBorrow => match size {
			ValSize::_8  => (val1 as i8).overflowing_sub(val2 as i8).1 as u64,
			ValSize::_16 => (val1 as i16).overflowing_sub(val2 as i16).1 as u64,
			ValSize::_32 => (val1 as i32).overflowing_sub(val2 as i32).1 as u64,
			ValSize::_64 => (val1 as i64).overflowing_sub(val2 as i64).1 as u64,
		}
		Carries => match size {
			ValSize::_8  => carries::< 8>(val1, val2, 0),
			ValSize::_16 => carries::<16>(val1, val2, 0),
			ValSize::_32 => carries::<32>(val1, val2, 0),
			ValSize::_64 => carries::<64>(val1, val2, 0),
		}
		Borrows => match size {
			ValSize::_8  => borrows::< 8>(val1, val2, 0),
			ValSize::_16 => borrows::<16>(val1, val2, 0),
			ValSize::_32 => borrows::<32>(val1, val2, 0),
			ValSize::_64 => borrows::<64>(val1, val2, 0),
		}
		//
		// : this is poorly-defined. would it make more sense to have an n*n=>2n multiplication
		// operation?  well we'll punt for now cause I don't forsee implementing arches with
		// multiplication any time soon.
		Mul => match size {
			ValSize::_8  => (val1 as u8).wrapping_mul(val2 as u8) as u64,
			ValSize::_16 => (val1 as u16).wrapping_mul(val2 as u16) as u64,
			ValSize::_32 => (val1 as u32).wrapping_mul(val2 as u32) as u64,
			ValSize::_64 => val1.wrapping_mul(val2),
		}
		UDiv => {
			// not using checked_div et al. because the result has to be u64, and this is
			// less awkward imo
			if val2 == 0 {
				return None;
			} else {
				match size {
					ValSize::_8  => (val1 as u8 / val2 as u8) as u64,
					ValSize::_16 => (val1 as u16 / val2 as u16) as u64,
					ValSize::_32 => (val1 as u32 / val2 as u32) as u64,
					ValSize::_64 => val1 / val2,
				}
			}
		}
		SDiv => {
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
		UMod => {
			if val2 == 0 {
				return None;
			} else {
				match size {
					ValSize::_8  => (val1 as u8 % val2 as u8) as u64,
					ValSize::_16 => (val1 as u16 % val2 as u16) as u64,
					ValSize::_32 => (val1 as u32 % val2 as u32) as u64,
					ValSize::_64 => val1 % val2,
				}
			}
		}
		// TODO: modulo on signed numbers is poorly-defined! aaaah!!!!!
		SMod => {
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

		IXor => val1 ^ val2,
		IAnd => val1 & val2,
		IOr =>  val1 | val2,

		// TODO: for all shifts, what if shift distance exceeds bits? checked_shx().unwrap_or
		// (0) treats it as "all bits shifted off end" but some architectures instead shift only by
		// lower bits (so e.g. if it's a 16-bit arch, and you shift by 17, it treats it as shifting
		// by 1). Should that be an option? or give an error? or force arches to mask off the
		// distance before passing it to a shift? or...?
		Shl => match size {
			ValSize::_8  => (val1 as u8).checked_shl(val2 as u32).unwrap_or(0) as u64,
			ValSize::_16 => (val1 as u16).checked_shl(val2 as u32).unwrap_or(0) as u64,
			ValSize::_32 => (val1 as u32).checked_shl(val2 as u32).unwrap_or(0) as u64,
			ValSize::_64 => val1.checked_shl(val2 as u32).unwrap_or(0),
		}
		UShr => match size {
			ValSize::_8  => (val1 as u8).checked_shr(val2 as u32).unwrap_or(0) as u64,
			ValSize::_16 => (val1 as u16).checked_shr(val2 as u32).unwrap_or(0) as u64,
			ValSize::_32 => (val1 as u32).checked_shr(val2 as u32).unwrap_or(0) as u64,
			ValSize::_64 => val1.checked_shr(val2 as u32).unwrap_or(0),
		}
		// TODO: what if val2 is negative?
		SShr => match size {
			ValSize::_8  => (val1 as i8).checked_shr(val2 as u32)
				.unwrap_or(if (val1 as i8) < 0 { -1 } else { 0 }) as u8 as u64,
			ValSize::_16 => (val1 as i16).checked_shr(val2 as u32)
				.unwrap_or(if (val1 as i16) < 0 { -1 } else { 0 }) as u16 as u64,
			ValSize::_32 => (val1 as i32).checked_shr(val2 as u32)
				.unwrap_or(if (val1 as i32) < 0 { -1 } else { 0 }) as u32 as u64,
			ValSize::_64 => (val1 as i64).checked_shr(val2 as u32)
				.unwrap_or(if (val1 as i64) < 0 { -1 } else { 0 }) as u64,
		}

		// TODO: all rotates interpret distance modulo number of bits in source, so e.g. for an
		// 8-bit value, rotating left by 0, 8, 16, 24 etc. all give the same value. I don't think
		// this is really a problem, but it's something to be aware of/specify.
		Rol => match size {
			ValSize::_8  => (val1 as u8).rotate_left(val2 as u32) as u64,
			ValSize::_16 => (val1 as u16).rotate_left(val2 as u32) as u64,
			ValSize::_32 => (val1 as u32).rotate_left(val2 as u32) as u64,
			ValSize::_64 => val1.rotate_left(val2 as u32),
		}
		Ror => match size {
			ValSize::_8  => (val1 as u8).rotate_right(val2 as u32) as u64,
			ValSize::_16 => (val1 as u16).rotate_right(val2 as u32) as u64,
			ValSize::_32 => (val1 as u32).rotate_right(val2 as u32) as u64,
			ValSize::_64 => val1.rotate_right(val2 as u32),
		}

		Pair => (val1 << size as u32) | val2,

		Bit => {
			let num_bits = size.bytes() as u64 * 8;
			assert!(val2 < num_bits, "bit position {} exceeds number of bits {}", val2, num_bits);
			if (val1 & (1 << val2)) != 0 { 1 } else { 0 }
		}

		BXor => (val1 != val2) as u64,
		BAnd => (val1 != 0 && val2 != 0) as u64,
		BOr =>  (val1 != 0 || val2 != 0) as u64,
	};

	Some(val)
}

fn do_ternop(op: IrTernOp, val1: u64, val2: u64, val3: u64, size: ValSize) -> u64 {
	use IrTernOp::*;

	match op {
		AddC => match size {
			ValSize::_8 => (val1 as u8).wrapping_add(val2 as u8).wrapping_add(val3 as u8) as u64,
			ValSize::_16 => (val1 as u16).wrapping_add(val2 as u16).wrapping_add(val3 as u16) as u64,
			ValSize::_32 => (val1 as u32).wrapping_add(val2 as u32).wrapping_add(val3 as u32) as u64,
			ValSize::_64 => val1.wrapping_add(val2).wrapping_add(val3),
		},
		SubB => match size {
			ValSize::_8 => (val1 as u8).wrapping_sub(val2 as u8).wrapping_sub(val3 as u8) as u64,
			ValSize::_16 => (val1 as u16).wrapping_sub(val2 as u16).wrapping_sub(val3 as u16) as u64,
			ValSize::_32 => (val1 as u32).wrapping_sub(val2 as u32).wrapping_sub(val3 as u32) as u64,
			ValSize::_64 => val1.wrapping_sub(val2).wrapping_sub(val3),
		},
		UCarryC => {
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
					let (sum, carry) = val1.overflowing_add(val2);
					(sum, carry)
				}
			};

			if carry {
				1
			} else {
				match size {
					ValSize::_8  => (sum as u8).overflowing_add(val3 as u8).1 as u64,
					ValSize::_16 => (sum as u16).overflowing_add(val3 as u16).1 as u64,
					ValSize::_32 => (sum as u32).overflowing_add(val3 as u32).1 as u64,
					ValSize::_64 => sum.overflowing_add(val3).1 as u64,
				}
			}
		}
		SCarryC => {
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
		SBorrowB => {
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
		CarriesC => match size {
			ValSize::_8  => carries::< 8>(val1, val2, val3),
			ValSize::_16 => carries::<16>(val1, val2, val3),
			ValSize::_32 => carries::<32>(val1, val2, val3),
			ValSize::_64 => carries::<64>(val1, val2, val3),
		}
		BorrowsB => match size {
			ValSize::_8  => borrows::< 8>(val1, val2, val3),
			ValSize::_16 => borrows::<16>(val1, val2, val3),
			ValSize::_32 => borrows::<32>(val1, val2, val3),
			ValSize::_64 => borrows::<64>(val1, val2, val3),
		}
		BSet => {
			let num_bits = size.bytes() as u64 * 8;
			assert!(val2 < num_bits, "bit position {} exceeds number of bits {}", val2, num_bits);
			assert!(val3 == 0 || val3 == 1, "src3 must be 0 or 1");
			(val1 & !(1 << val2)) | (val3 << val2)
		}
	}
}