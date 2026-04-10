
use std::iter::{ FusedIterator };
use std::fmt::{ Debug, Formatter, Result as FmtResult };

use crate::ir::{ IrRegSetType, IrReg };

/// Efficient bitset implementation of a set of [`IrReg`] indices.
#[derive(Copy, Clone, PartialEq, Eq)]
pub(crate) struct RegSet {
	bits: IrRegSetType,
}

impl Debug for RegSet {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		write!(f, "{{")?;

		for (i, reg) in self.iter().enumerate() {
			if i != 0 {
				write!(f, " ")?;
			}

			write!(f, "r{}", reg)?;
		}

		write!(f, "}}")
	}
}

impl RegSet {
	pub(crate) const EMPTY: RegSet = RegSet { bits: 0 };

	/// New empty set.
	pub(crate) fn new() -> Self {
		Self { bits: 0 }
	}

	/// New set with the given `bits`.
	pub(crate) fn new_from(bits: IrRegSetType) -> Self {
		Self { bits }
	}

	/// How many registers are in this set?
	pub(crate) fn len(&self) -> usize {
		self.bits.count_ones() as usize
	}

	/// Is this set empty?
	pub(crate) fn is_empty(&self) -> bool {
		self.bits == 0
	}

	/// Does this set contain `reg`?
	pub(crate) fn contains(&self, reg: u8) -> bool {
		assert!(reg <= IrReg::MAX);
		(self.bits & (1 << reg)) != 0
	}

	/// Set difference `A - B`; returns new set containing regs in `self` and not in `other`.
	pub(crate) fn difference(&self, other: RegSet) -> RegSet {
		RegSet { bits: self.bits & !(other.bits) }
	}

	/// Symmetric set difference `A \ B`; returns new set containing regs in `self` or `other` but
	/// not in both.
	pub(crate) fn symmetric_difference(&self, other: RegSet) -> RegSet {
		RegSet { bits: self.bits ^ other.bits }
	}

	/// Set union `A ∪ B`; returns new set containing regs in either `self` or `other`.
	pub(crate) fn union(&self, other: RegSet) -> RegSet {
		RegSet { bits: self.bits | other.bits }
	}

	/// Set intersection `A ∩ B`; returns new set containing regs in both `self` and `other`.
	pub(crate) fn intersection(&self, other: RegSet) -> RegSet {
		RegSet { bits: self.bits & other.bits }
	}

	/// Insert `reg` into this set. Returns `true` if `reg` was not in the set already, or false if
	/// it already existed.
	pub(crate) fn insert(&mut self, reg: u8) -> bool {
		assert!(reg <= IrReg::MAX);
		let mask = 1 << reg;
		if self.bits & mask != 0 {
			false
		} else {
			self.bits |= mask;
			true
		}
	}

	/// Remove `reg` from this set. Returns `true` if `reg` was in the set, or false if it wasn't.
	pub(crate) fn remove(&mut self, reg: u8) -> bool {
		assert!(reg <= IrReg::MAX);
		let mask = 1 << reg;
		if self.bits & mask != 0 {
			self.bits &= !mask;
			true
		} else {
			false
		}
	}

	/// Iterator over the register indices in this set.
	pub(crate) fn iter(&self) -> impl Iterator<Item = u8> + ExactSizeIterator + FusedIterator {
		struct RegSetIter {
			bits: IrRegSetType,
			idx: u8,
		}

		impl Iterator for RegSetIter {
			type Item = u8;

			fn next(&mut self) -> Option<u8> {
				if self.bits == 0 {
					None
				} else {
					let shift_dist = self.bits.trailing_zeros();
					self.idx += shift_dist as u8 + 1;
					self.bits >>= shift_dist + 1;
					Some(self.idx - 1)
				}
			}

			fn size_hint(&self) -> (usize, Option<usize>) {
				let len = self.bits.count_ones() as usize;
				(len, Some(len))
			}
		}

		impl ExactSizeIterator for RegSetIter {
			fn len(&self) -> usize {
				// eh why not, I'm sure the default impl of calling size_hint would optimize down
				// to the same thing anyway but whatever
				self.bits.count_ones() as usize
			}
		}
		impl FusedIterator for RegSetIter {}

		RegSetIter { bits: self.bits, idx: 0 }
	}
}

impl std::ops::BitAnd<RegSet> for RegSet {
	type Output = RegSet;

	/// Shorthand for `self.intersection(rhs)`.
	fn bitand(self, rhs: RegSet) -> RegSet {
		self.intersection(rhs)
	}
}

impl std::ops::BitAndAssign<RegSet> for RegSet {
	/// Shorthand for `self = self.intersection(rhs)`.
	fn bitand_assign(&mut self, rhs: RegSet) {
		*self = self.intersection(rhs);
	}
}

impl std::ops::BitOr<RegSet> for RegSet {
	type Output = RegSet;

	/// Shorthand for `self.union(rhs)`.
	fn bitor(self, rhs: RegSet) -> RegSet {
		self.union(rhs)
	}
}

impl std::ops::BitOrAssign<RegSet> for RegSet {
	/// Shorthand for `self = self.union(rhs)`.
	fn bitor_assign(&mut self, rhs: RegSet) {
		*self = self.union(rhs);
	}
}

impl std::ops::BitXor<RegSet> for RegSet {
	type Output = RegSet;

	/// Shorthand for `self.symmetric_difference(rhs)`.
	fn bitxor(self, rhs: RegSet) -> RegSet {
		self.symmetric_difference(rhs)
	}
}

impl std::ops::Sub<RegSet> for RegSet {
	type Output = RegSet;

	/// Shorthand for `self.difference(rhs)`.
	fn sub(self, rhs: RegSet) -> RegSet {
		self.difference(rhs)
	}
}

impl std::ops::SubAssign<RegSet> for RegSet {
	/// Shorthand for `self = self.difference(rhs)`.
	fn sub_assign(&mut self, rhs: RegSet) {
		*self = self.difference(rhs);
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn test_iter() {
		let set = RegSet::new_from(0b10010011);
		let bits = set.iter().collect::<Vec<u8>>();
		assert_eq!(bits, &[0, 1, 4, 7]);
	}

	#[test]
	fn test_iter2() {
		let set = RegSet::new_from(0b100100110);
		let bits = set.iter().collect::<Vec<u8>>();
		assert_eq!(bits, &[1, 2, 5, 8]);
	}

	#[test]
	fn test_ops() {
		let a =           RegSet::new_from(0b10100000);
		let b =           RegSet::new_from(0b11000011);
		assert_eq!(a & b, RegSet::new_from(0b10000000));
		assert_eq!(a | b, RegSet::new_from(0b11100011));
		assert_eq!(a ^ b, RegSet::new_from(0b01100011));
		assert_eq!(a - b, RegSet::new_from(0b00100000));
	}
}