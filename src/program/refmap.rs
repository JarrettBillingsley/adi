
use derive_new::new;

use std::collections::{
	BTreeMap,
	BTreeSet,

	btree_map::Iter as BTreeIter,
};

use crate::memory::Location;

// ------------------------------------------------------------------------------------------------
// RefMap
// ------------------------------------------------------------------------------------------------

pub type RefSet = BTreeSet<Location>;

/// A many-to-many mapping of references - "arrows" pointing from one location to another.
/// Every location can have multiple "outrefs" - references *to* other locations and
/// multiple "inrefs" - references *from* other locations. Looking up these refs is
#[derive(Default)]
#[derive(new)]
pub struct RefMap {
	#[new(value = "BTreeMap::new()")]
	inrefs:  BTreeMap<Location, RefSet>,
	#[new(value = "BTreeMap::new()")]
	outrefs: BTreeMap<Location, RefSet>,
}

impl RefMap {
	/// Add a reference from `src` to `dst`. Returns that reference object.
	pub fn add(&mut self, src: Location, dst: Location) {
		self._add_outref(src, dst);
		self._add_inref(src, dst);
	}

	/// Remove a reference.
	pub fn remove(&mut self, src: Location, dst: Location) {
		self._remove_outref(src, dst);
		self._remove_inref(src, dst);
	}

	/// Remove all outrefs from the given location.
	pub fn remove_all_outrefs(&mut self, src: Location) {
		let set = self.outrefs.remove(&src).unwrap_or_else(|| panic!("no refs from {}", src));

		for dst in set {
			self._remove_inref(src, dst);
		}
	}

	/// Remove all inrefs to the given location.
	pub fn remove_all_inrefs(&mut self, dst: Location) {
		let set = self.inrefs.remove(&dst).unwrap_or_else(|| panic!("no refs to {}", dst));

		for src in set {
			self._remove_outref(src, dst);
		}
	}

	/// Get all inrefs to a given location, or None if there aren't any.
	pub fn get_inrefs(&mut self, dst: Location) -> Option<&RefSet> {
		self.inrefs.get(&dst)
	}

	/// Get all outrefs from a given location, or None if there aren't any.
	pub fn get_outrefs(&mut self, src: Location) -> Option<&RefSet> {
		self.outrefs.get(&src)
	}

	pub fn all_outrefs(&self) -> BTreeIter<'_, Location, RefSet> {
		self.outrefs.iter()
	}

	fn _add_outref(&mut self, src: Location, dst: Location) {
		self.outrefs.entry(src).or_insert_with(BTreeSet::new).insert(dst);
	}

	fn _add_inref(&mut self, src: Location, dst: Location) {
		self.inrefs.entry(dst).or_insert_with(BTreeSet::new).insert(src);
	}

	fn _remove_outref(&mut self, src: Location, dst: Location) {
		let set = self.outrefs.get_mut(&src).unwrap_or_else(|| panic!("no outrefs from {}", src));
		assert!(set.remove(&dst));
		if set.is_empty() {
			self.outrefs.remove(&src);
		}
	}

	fn _remove_inref(&mut self, src: Location, dst: Location) {
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
		let a = Location::new(SegId(0), 0x00);
		let b = Location::new(SegId(0), 0x10);
		let c = Location::new(SegId(0), 0x20);
		let d = Location::new(SegId(0), 0x30);
		let e = Location::new(SegId(0), 0x40);

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