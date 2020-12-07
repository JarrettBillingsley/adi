use std::collections::{
	HashMap,
	hash_map::Iter as HashIter,
};
use std::iter::IntoIterator;
use std::iter::FromIterator;

// ------------------------------------------------------------------------------------------------
// Memory configuration
// ------------------------------------------------------------------------------------------------

/// A collection of assignments of segments to areas in the memory map.
///
/// This just maps from region names to segment names. No hard references to objects.
///
/// You can create MemoryConfigs from any iterable that gives (name, name) pairs.
/// For example:
///
/// ```
/// use adi::MemoryConfig;
/// use std::iter::FromIterator;
/// let config = MemoryConfig::from_iter(&[
///     ("a", "b"),
///     ("c", "d"),
/// ]);
/// assert_eq!(config.segment_for_region("a"), Some(&"b".to_string()));
/// assert_eq!(config.segment_for_region("x"), None);
/// ```
#[derive(Debug, PartialEq, Eq)]
pub struct MemoryConfig {
	assignments: HashMap<String, String>,
}

type IterItem<'itr> = &'itr (&'itr str, &'itr str);

impl<'src> FromIterator<IterItem<'src>> for MemoryConfig {
	fn from_iter<I>(iter: I) -> Self
	where I: IntoIterator<Item = IterItem<'src>> {
		MemoryConfig::new(iter.into_iter().map(|&(k, v)| (k.into(), v.into())).collect())
	}
}

impl MemoryConfig {
	/// ctor
	// can't use derive_new cause needs to be private
	fn new(assignments: HashMap<String, String>) -> Self {
		Self {
			assignments
		}
	}

	/// Gets the segment mapped to the given region (if any).
	pub fn segment_for_region(&self, region_name: &str) -> Option<&String> {
		self.assignments.get(region_name)
	}

	/// Iterates over the mappings (region name, segment name).
	pub fn iter(&self) -> HashIter<'_, String, String> {
		self.assignments.iter()
	}

	/// Construct a new `MemoryConfig` using this one as a base, and the items in `iter`
	/// will be added to the mappings (and will replace any existing mappings).
	pub fn derive<'src, I>(&self, src: I) -> Self
	where I: IntoIterator<Item = IterItem<'src>> {
		let mut ret = Self::new(self.assignments.clone());
		ret.assignments.extend(src.into_iter().map(|&(k, v)| (k.into(), v.into())));
		ret
	}
}