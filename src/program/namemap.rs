use std::collections::{
	BTreeMap,
	HashMap,

	btree_map::Iter as BTreeIter,
	btree_map::Range as BTreeRange,
	hash_map::Iter as HashIter,
};
use std::ops::RangeBounds;

use crate::memory::EA;

// TODO: make this parameterizable
pub const AUTOGEN_NAME_PREFIX: &str = "loc";

// ------------------------------------------------------------------------------------------------
// NameMap
// ------------------------------------------------------------------------------------------------

/// A bidirectional mapping between names and EAs.
#[derive(Default)]
pub struct NameMap {
	names_to_eas: HashMap<String, EA>,
	eas_to_names: BTreeMap<EA, String>,
}

impl NameMap {
	pub fn new() -> Self {
		Self { names_to_eas: HashMap::new(), eas_to_names: BTreeMap::new() }
	}

	/// Assigns a name to a given EA.
	pub fn add(&mut self, name: &str, ea: EA) {
		self.names_to_eas.insert(name.into(), ea);
		self.eas_to_names.insert(ea, name.into());
	}

	/// Removes a mapping by name.
	pub fn remove_name(&mut self, name: &str) {
		let ea = *self.names_to_eas.get(name).unwrap();
		self.names_to_eas.remove(name);
		self.eas_to_names.remove(&ea);
	}

	/// Removes a mapping by EA.
	pub fn remove_ea(&mut self, ea: EA) {
		let name = self.eas_to_names.get(&ea).unwrap();
		self.names_to_eas.remove(name);
		self.eas_to_names.remove(&ea);
	}

	/// Gets the EA for a name, if one of that name exists.
	pub fn ea_for_name(&self, name: &str) -> Option<EA> {
		self.names_to_eas.get(name).copied()
	}

	/// Gets the name for an EA, if there is one.
	pub fn name_for_ea(&self, ea: EA) -> Option<&String> {
		self.eas_to_names.get(&ea)
	}

	/// Whether or not the given name exists.
	pub fn has_name(&self, name: &str) -> bool {
		self.names_to_eas.contains_key(name)
	}

	/// Whether or not there is a name for the given EA.
	pub fn has_ea(&self, ea: EA) -> bool {
		self.eas_to_names.contains_key(&ea)
	}

	/// All (name, EA) pairs in arbitrary order.
	pub fn names(&self) -> HashIter<'_, String, EA> {
		self.names_to_eas.iter()
	}

	/// All (EA, name) pairs in EA order.
	pub fn eas(&self) -> BTreeIter<'_, EA, String> {
		self.eas_to_names.iter()
	}

	/// All (EA, name) pairs in a given range of EAs, in EA order.
	pub fn names_in_range(&self, range: impl RangeBounds<EA>)
	-> BTreeRange<'_, EA, String> {
		self.eas_to_names.range(range)
	}
}