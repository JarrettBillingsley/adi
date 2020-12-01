use std::collections::{
	BTreeMap,
	HashMap,

	btree_map::Iter as BTreeIter,
	btree_map::Range as BTreeRange,
	hash_map::Iter as HashIter,
};

use super::newtypes::*;

// ------------------------------------------------------------------------------------------------
// NameMap
// ------------------------------------------------------------------------------------------------

/// A bidirectional mapping between names and virtual addresses.
pub struct NameMap<'a> {
	names_to_vas: HashMap<&'a str, VAddr>,
	vas_to_names: BTreeMap<VAddr, &'a str>,
}

impl<'a> NameMap<'a> {
	/// Makes a new empty map.
	pub fn new() -> Self {
		Self {
			names_to_vas: HashMap::new(),
			vas_to_names: BTreeMap::new(),
		}
	}

	/// Assigns a name to a given VA. The VA must not already have a name.
	pub fn add(&mut self, name: &'a str, va: VAddr) {
		assert!(!self.names_to_vas.contains_key(&name));
		self.names_to_vas.insert(name, va);
		self.vas_to_names.insert(va, name);
	}

	/// Removes a mapping by name.
	pub fn remove_name(&mut self, name: &'a str) {
		let va = *self.names_to_vas.get(&name).unwrap();
		self.names_to_vas.remove(name);
		self.vas_to_names.remove(&va);
	}

	/// Removes a mapping by VA.
	pub fn remove_va(&mut self, va: VAddr) {
		let name = *self.vas_to_names.get(&va).unwrap();
		self.names_to_vas.remove(name);
		self.vas_to_names.remove(&va);
	}

	/// Gets the VA for a name, if one of that name exists.
	pub fn va_for_name(&self, name: &'a str) -> Option<VAddr> {
		self.names_to_vas.get(name).copied()
	}

	/// Gets the name for an VA, if there is one.
	pub fn name_for_va(&self, va: VAddr) -> Option<&'a str> {
		self.vas_to_names.get(&va).copied()
	}

	/// Whether or not the given name exists.
	pub fn has_name(&self, name: &'a str) -> bool {
		self.names_to_vas.contains_key(name)
	}

	/// Whether or not there is a name for the given VA.
	pub fn has_va(&self, va: VAddr) -> bool {
		self.vas_to_names.contains_key(&va)
	}

	/// All (name, VA) pairs in arbitrary order.
	pub fn names(&self) -> HashIter<'a, &str, VAddr> {
		self.names_to_vas.iter()
	}

	/// All (VA, name) pairs in VA order.
	pub fn vas(&self) -> BTreeIter<'a, VAddr, &str> {
		self.vas_to_names.iter()
	}

	/// All (VA, name) pairs in a given range of VAs, in VA order.
	pub fn names_in_range<R>(&self, range: R) -> BTreeRange<'a, VAddr, &str>
	where
		R: std::ops::RangeBounds<VAddr>
	{
		self.vas_to_names.range(range)
	}
}