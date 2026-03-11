
use std::borrow::{ Cow };
use std::collections::{
	BTreeMap,
	HashMap,
	hash_map::Iter as HashIter,
};
use std::fmt::{ Display, Formatter, Result as FmtResult };
use std::ops::RangeBounds;

use crate::memory::EA;

// TODO: make these parameterizable
pub const AUTOGEN_NAME_PREFIX: &str = "loc";
pub const AUTOGEN_FUNC_PREFIX: &str = "func";

// ------------------------------------------------------------------------------------------------
// Name, NameKind
// ------------------------------------------------------------------------------------------------

/// Different kinds of names.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum NameKind {
	/// An automatically-generated name.
	AutoGen,
	/// The name of a hardware register or similar. Created automatically by platforms.
	Hardware,
	/// A name assigned by the user.
	User,
	/// A name that is local to a function. Can only be user-assigned.
	Local,
}

impl NameKind {
	/// Is this name user-defined?
	pub fn is_user(&self) -> bool {
		matches!(self, NameKind::User | NameKind::Local)
	}
}

/// A name.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Name<'a> {
	/// The name.
	pub name: Cow<'a, String>,
	/// What kind of name it is.
	pub kind: NameKind,
}

impl<'a> Name<'a> {
	/// Is this name user-defined?
	pub fn is_user(&self) -> bool {
		self.kind.is_user()
	}
}

impl<'a> Display for Name<'a> {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		write!(f, "{}", self.name)
	}
}

// ------------------------------------------------------------------------------------------------
// NameMap
// ------------------------------------------------------------------------------------------------

/// A bidirectional mapping between names and EAs.
#[derive(Default)]
pub struct NameMap {
	names_to_eas: HashMap<String, (EA, NameKind)>,
	eas_to_names: BTreeMap<EA, (String, NameKind)>,
}

impl NameMap {
	pub fn new() -> Self {
		Self {
			names_to_eas: HashMap::new(),
			eas_to_names: BTreeMap::new(),
		}
	}

	// --------------------------------------------------------------------------------------------
	// Generating names

	/// Given a `base` (usually a segment name) and an `addr` (an address represented as a string),
	/// returns a name of the form `BASE_loc_ADDR`.
	pub fn generate_name(&self, base: impl Into<String>, addr: impl Into<String>) -> String {
		format!("{}_{}_{}", base.into(), AUTOGEN_NAME_PREFIX, addr.into())
	}

	/// Like `generate_name`, but in the form `BASE_func_ADDR`.
	pub fn generate_func_name(&self, base: impl Into<String>, addr: impl Into<String>) -> String {
		format!("{}_{}_{}", base.into(), AUTOGEN_FUNC_PREFIX, addr.into())
	}

	// --------------------------------------------------------------------------------------------
	// Adding and removing names

	/// Assigns a name to a given EA.
	pub fn add(&mut self, name: impl Into<String>, ea: EA, kind: NameKind) {
		let name = name.into();
		self.names_to_eas.insert(name.clone(), (ea, kind));
		self.eas_to_names.insert(ea, (name, kind));
	}

	/// Removes a mapping by name. Panics if the name doesn't exist.
	pub fn remove_name(&mut self, name: impl Into<String>) {
		let name = name.into();
		let (ea, _) = *self.names_to_eas.get(&name).unwrap();
		self.names_to_eas.remove(&name);
		self.eas_to_names.remove(&ea);
	}

	/// Removes a mapping by EA. Panics if the EA isn't in the map.
	pub fn remove_ea(&mut self, ea: EA) {
		let (name, _) = self.eas_to_names.get(&ea).unwrap();
		self.names_to_eas.remove(name);
		self.eas_to_names.remove(&ea);
	}

	// --------------------------------------------------------------------------------------------
	// Queries

	/// Gets both the EA and kind of a name, if that name exists.
	pub fn ea_and_kind_for_name(&self, name: impl Into<String>) -> Option<(EA, NameKind)> {
		self.names_to_eas.get(&name.into()).copied()
	}

	/// Gets the name for an EA, if there is one.
	pub fn name_for_ea(&self, ea: EA) -> Option<Name<'_>> {
		self.eas_to_names.get(&ea).map(|(name, kind)| Name {
			name: Cow::Borrowed(name),
			kind: *kind,
		})
	}

	/// Whether or not the given name exists.
	pub fn has_name(&self, name: impl Into<String>) -> bool {
		self.names_to_eas.contains_key(&name.into())
	}

	/// Whether or not there is a name for the given EA.
	pub fn has_ea(&self, ea: EA) -> bool {
		self.eas_to_names.contains_key(&ea)
	}

	// --------------------------------------------------------------------------------------------
	// Iterators

	/// All `(name, (EA, NameKind))` pairs in arbitrary order.
	pub fn names(&self) -> HashIter<'_, String, (EA, NameKind)> {
		self.names_to_eas.iter()
	}

	/// All `(EA, Name)` pairs in EA order.
	pub fn eas(&self) -> impl Iterator<Item = (EA, Name<'_>)> {
		self.eas_to_names.iter().map(|(ea, (name, kind))| (*ea, Name {
			name: Cow::Borrowed(name),
			kind: *kind,
		}))
	}

	/// All `(EA, Name)` pairs in a given range of EAs, in EA order.
	pub fn names_in_range(&self, range: impl RangeBounds<EA>)
	-> impl Iterator<Item = (EA, Name<'_>)> {
		self.eas_to_names.range(range).map(|(ea, (name, kind))| (*ea, Name {
			name: Cow::Borrowed(name),
			kind: *kind,
		}))
	}
}