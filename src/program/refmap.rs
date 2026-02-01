
use std::collections::{
	BTreeMap,
	BTreeSet,

	btree_map::Iter as BTreeIter,
};

use crate::memory::EA;

// ------------------------------------------------------------------------------------------------
// RefMap
// ------------------------------------------------------------------------------------------------

pub type RefSet = BTreeSet<EA>;

/// A many-to-many mapping of references - "arrows" pointing from one EA to another.
/// Every EA can have multiple "outrefs" - references *to* other EAs and multiple "inrefs" -
/// references *from* other EAs.
#[derive(Default)]
pub struct RefMap {
	inrefs:  BTreeMap<EA, RefSet>,
	outrefs: BTreeMap<EA, RefSet>,
}

impl RefMap {
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

	/// Iterator over all outrefs in the entire map.
	pub fn all_outrefs(&self) -> BTreeIter<'_, EA, RefSet> {
		self.outrefs.iter()
	}

	fn _add_outref(&mut self, src: EA, dst: EA) {
		self.outrefs.entry(src).or_insert_with(BTreeSet::new).insert(dst);
	}

	fn _add_inref(&mut self, src: EA, dst: EA) {
		self.inrefs.entry(dst).or_insert_with(BTreeSet::new).insert(src);
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
}