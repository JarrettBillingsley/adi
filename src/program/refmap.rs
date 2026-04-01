
use std::collections::{
	BTreeMap,
	BTreeSet,

	btree_map::Iter as BTreeIter,
};
use std::ops::{ RangeBounds, Bound };

use crate::memory::EA;

// ------------------------------------------------------------------------------------------------
// RefMap
// ------------------------------------------------------------------------------------------------

pub type RefSet = BTreeSet<EA>;

/// A many-to-many mapping of references - "arrows" pointing from one EA to another.
/// Every EA can have multiple "outrefs" - references *to* other EAs and multiple "inrefs" -
/// references *from* other EAs.
pub struct RefMap {
	inrefs:  BTreeMap<EA, RefSet>,
	outrefs: BTreeMap<EA, RefSet>,
}

impl RefMap {
	#[allow(clippy::new_without_default)]
	pub fn new() -> Self {
		Self { inrefs: BTreeMap::new(), outrefs: BTreeMap::new() }
	}

	/// Add a reference from `src` to `dst`.
	///
	/// Panics if `src` is unresolved.
	pub fn add(&mut self, src: EA, dst: EA) {
		// unresolved dst is fine
		assert!(src.is_resolved());
		self._add_outref(src, dst);
		self._add_inref(src, dst);
	}

	/// Remove a reference.
	///
	/// Panics if `src` is unresolved.
	pub fn remove(&mut self, src: EA, dst: EA) {
		assert!(src.is_resolved());
		self._remove_outref(src, dst);
		self._remove_inref(src, dst);
	}

	/// Remove all outrefs from the given EA.
	pub fn remove_all_outrefs(&mut self, src: EA) {
		let set = self.outrefs.remove(&src).unwrap_or_else(|| panic!("no refs from {}", src));

		for dst in set {
			self._remove_inref(src, dst);
		}
	}

	/// Remove all inrefs to the given EA.
	pub fn remove_all_inrefs(&mut self, dst: EA) {
		let set = self.inrefs.remove(&dst).unwrap_or_else(|| panic!("no refs to {}", dst));

		for src in set {
			self._remove_outref(src, dst);
		}
	}

	/// Get all inrefs to a given EA, or None if there aren't any.
	pub fn get_inrefs(&self, dst: EA) -> Option<&RefSet> {
		self.inrefs.get(&dst)
	}

	/// Get all outrefs from a given EA, or None if there aren't any.
	///
	/// Panics if `src` is unresolved.
	pub fn get_outrefs(&self, src: EA) -> Option<&RefSet> {
		assert!(src.is_resolved());
		self.outrefs.get(&src)
	}

	/// Get all outrefs from a range of EAs, in order of referrers. Only ranges of the form `a .. b`
	/// (inclusive start, exclusive end) are allowed. The end EA is never accessed, only used as a
	/// bound, so it's okay if it's the first byte past the end of a segment.
	///
	/// Panics if `range` is not a valid kind of range, or if either end is unresolved.
	pub fn outrefs_in_range(&self, range: impl RangeBounds<EA>) -> impl Iterator<Item = (EA, EA)> {
		let start = match range.start_bound() {
			Bound::Included(&s) => s,
			_                   => panic!("only ranges of the form `a .. b` are allowed"),
		};

		let end = match range.end_bound() {
			Bound::Excluded(&e) => e,
			_                   => panic!("only ranges of the form `a .. b` are allowed"),
		};

		assert!(start.is_resolved(), "start unresolved");
		assert!(end.is_resolved(),   "end unresolved");

		self.outrefs.range(start .. end).map(|(src, dsts): (&EA, &RefSet)| {
			dsts.iter().map(|dst| (*src, *dst))
		}).flatten()
	}

	/// Iterator over all outrefs in the entire map.
	pub fn all_outrefs(&self) -> BTreeIter<'_, EA, RefSet> {
		self.outrefs.iter()
	}

	fn _add_outref(&mut self, src: EA, dst: EA) {
		self.outrefs.entry(src).or_default().insert(dst);
	}

	fn _add_inref(&mut self, src: EA, dst: EA) {
		self.inrefs.entry(dst).or_default().insert(src);
	}

	fn _remove_outref(&mut self, src: EA, dst: EA) {
		let set = self.outrefs.get_mut(&src).unwrap_or_else(|| panic!("no outrefs from {}", src));
		assert!(set.remove(&dst));
		if set.is_empty() {
			self.outrefs.remove(&src);
		}
	}

	fn _remove_inref(&mut self, src: EA, dst: EA) {
		let set = self.inrefs.get_mut(&dst).unwrap_or_else(|| panic!("no inrefs to {}", dst));
		assert!(set.remove(&src));
		if set.is_empty() {
			self.inrefs.remove(&dst);
		}
	}
}

#[cfg(test)]
mod tests {
	use super::*;
	use std::iter::FromIterator;
	use crate::memory::SegId;

	#[test]
	fn basic() {
		let mut ref_map = RefMap::new();
		let a = EA::new(SegId(0), 0x00);
		let b = EA::new(SegId(0), 0x10);
		let c = EA::new(SegId(0), 0x20);
		let d = EA::new(SegId(0), 0x30);
		let e = EA::new(SegId(0), 0x40);

		// one-to-many
		ref_map.add(a, b);
		ref_map.add(a, c);

		// many-to-one
		ref_map.add(b, d);
		ref_map.add(c, d);
		ref_map.add(e, d);

		// self-reference
		ref_map.add(e, e);

		// test it all out
		assert_eq!(ref_map.get_outrefs(a), Some(&RefSet::from_iter(vec![b, c])));
		assert_eq!(ref_map.get_inrefs(b),  Some(&RefSet::from_iter(vec![a])));
		assert_eq!(ref_map.get_inrefs(c),  Some(&RefSet::from_iter(vec![a])));

		assert_eq!(ref_map.get_inrefs(a),  None);
		assert_eq!(ref_map.get_outrefs(d), None);
		assert_eq!(ref_map.get_inrefs(d),  Some(&RefSet::from_iter(vec![b, c, e])));

		// now remove all outrefs from a
		ref_map.remove_all_outrefs(a);

		assert_eq!(ref_map.get_outrefs(a), None);
		assert_eq!(ref_map.get_inrefs(b),  None);
		assert_eq!(ref_map.get_inrefs(c),  None);

		assert!(ref_map.get_inrefs(e).unwrap().contains(&e));
		assert!(ref_map.get_outrefs(e).unwrap().contains(&e));

		// and remove all inrefs to d
		ref_map.remove_all_inrefs(d);

		assert_eq!(ref_map.get_inrefs(d), None);
		assert_eq!(ref_map.get_outrefs(e), Some(&RefSet::from_iter(vec![e])));

		// and finally remove e's ref to itself, leaving nothing
		ref_map.remove(e, e);

		assert_eq!(ref_map.get_inrefs(e), None);
		assert_eq!(ref_map.get_outrefs(e), None);
		assert_eq!(ref_map.all_outrefs().count(), 0);
	}

	#[test]
	#[allow(unused)]
	fn outrefs_in_range() {
		let mut ref_map = RefMap::new();
		fn ea(offs: u64) -> EA { EA::new(SegId(0), offs) }

		let f1_a   = EA::new(SegId(0), 0x00);
		let f1_b   = EA::new(SegId(0), 0x01);
		let f1_c   = EA::new(SegId(0), 0x02);
		let f1_d   = EA::new(SegId(0), 0x03);
		let f1_e   = EA::new(SegId(0), 0x04);
		let f1_end = EA::new(SegId(0), 0x05);

		let f2_a   = EA::new(SegId(0), 0x10);
		let f2_b   = EA::new(SegId(0), 0x11);
		let f2_c   = EA::new(SegId(0), 0x12);
		let f2_d   = EA::new(SegId(0), 0x13);
		let f2_e   = EA::new(SegId(0), 0x14);
		let f2_end = EA::new(SegId(0), 0x15);

		ref_map.add(f1_a, f1_c);
		ref_map.add(f1_b, f2_a);
		ref_map.add(f1_d, f1_a);
		ref_map.add(f2_a, f2_b);
		ref_map.add(f2_a, f2_c);
		ref_map.add(f2_e, f2_d);

		assert_eq!(
			ref_map.outrefs_in_range(f1_a .. f1_end).collect::<Vec<_>>(),
			&[(f1_a, f1_c), (f1_b, f2_a), (f1_d, f1_a)]);

		assert_eq!(
			ref_map.outrefs_in_range(f2_a .. f2_end).collect::<Vec<_>>(),
			&[(f2_a, f2_b), (f2_a, f2_c), (f2_e, f2_d)]);
	}
}
