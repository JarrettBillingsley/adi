use std::collections::{
	BTreeMap,
	HashMap,

	btree_map::Iter as BTreeIter,
	btree_map::Range as BTreeRange,
	hash_map::Iter as HashIter,
};
use std::ops::RangeBounds;

use crate::memory::types::*;

// TODO: make this parameterizable
pub const AUTOGEN_NAME_PREFIX: &str = "loc";

// ------------------------------------------------------------------------------------------------
// NameMap
// ------------------------------------------------------------------------------------------------

/// A bidirectional mapping between names and locations.
#[derive(Default)]
pub struct NameMap<'a> {
	names_to_locs: HashMap<&'a str, Location>,
	locs_to_names: BTreeMap<Location, &'a str>,
}

impl<'a> NameMap<'a> {
	/// Makes a new empty map.
	pub fn new() -> Self {
		Self {
			names_to_locs: HashMap::new(),
			locs_to_names: BTreeMap::new(),
		}
	}

	/// Assigns a name to a given Location.
	pub fn add(&mut self, name: &'a str, loc: Location) {
		self.names_to_locs.insert(name, loc);
		self.locs_to_names.insert(loc, name);
	}

	/// Removes a mapping by name.
	pub fn remove_name(&mut self, name: &'a str) {
		let loc = *self.names_to_locs.get(&name).unwrap();
		self.names_to_locs.remove(name);
		self.locs_to_names.remove(&loc);
	}

	/// Removes a mapping by Location.
	pub fn remove_loc(&mut self, loc: Location) {
		let name = *self.locs_to_names.get(&loc).unwrap();
		self.names_to_locs.remove(name);
		self.locs_to_names.remove(&loc);
	}

	/// Gets the Location for a name, if one of that name exists.
	pub fn loc_for_name(&self, name: &'a str) -> Option<Location> {
		self.names_to_locs.get(name).copied()
	}

	/// Gets the name for an Location, if there is one.
	pub fn name_for_loc(&self, loc: Location) -> Option<&'a str> {
		self.locs_to_names.get(&loc).copied()
	}

	/// Whether or not the given name exists.
	pub fn has_name(&self, name: &'a str) -> bool {
		self.names_to_locs.contains_key(name)
	}

	/// Whether or not there is a name for the given Location.
	pub fn has_loc(&self, loc: Location) -> bool {
		self.locs_to_names.contains_key(&loc)
	}

	/// All (name, Location) pairs in arbitrary order.
	pub fn names(&self) -> HashIter<'a, &str, Location> {
		self.names_to_locs.iter()
	}

	/// All (Location, name) pairs in Location order.
	pub fn locations(&self) -> BTreeIter<'a, Location, &str> {
		self.locs_to_names.iter()
	}

	/// All (Location, name) pairs in a given range of Locations, in Location order.
	pub fn names_in_range(&self, range: impl RangeBounds<Location>)
	-> BTreeRange<'a, Location, &str> {
		self.locs_to_names.range(range)
	}
}