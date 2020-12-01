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
/// use adi::memory::MemoryConfig;
/// use std::iter::FromIterator;
/// let config = MemoryConfig::from_iter(&[
///     ("a", "b"),
///     ("c", "d"),
/// ]);
/// assert_eq!(config.segment_for_region("a"), Some("b"));
/// assert_eq!(config.segment_for_region("x"), None);
/// ```
#[derive(Debug, PartialEq, Eq)]
pub struct MemoryConfig<'a> {
	assignments: HashMap<&'a str, &'a str>,
}

type MemoryConfigIterItem<'a> = &'a (&'a str, &'a str);

impl<'a> FromIterator<MemoryConfigIterItem<'a>> for MemoryConfig<'a> {
	fn from_iter<I: IntoIterator<Item=MemoryConfigIterItem<'a>>>(iter: I) -> Self {
		MemoryConfig::new(iter.into_iter().copied().collect())
	}
}

impl<'a> MemoryConfig<'a> {
	/// ctor
	fn new(assignments: HashMap<&'a str, &'a str>) -> Self {
		Self {
			assignments
		}
	}

	/// Gets the segment mapped to the given region (if any).
	pub fn segment_for_region(&self, region_name: &str) -> Option<&str> {
		self.assignments.get(region_name).copied()
	}

	/// Iterates over the mappings (region name, segment name).
	pub fn iter(&self) -> HashIter<'a, &str, &str> {
		self.assignments.iter()
	}

	/// Construct a new `MemoryConfig` using this one as a base, and the items in `iter`
	/// will be added to the mappings (and will replace any existing mappings).
	pub fn derive<I: IntoIterator<Item=MemoryConfigIterItem<'a>>>(&self, iter: I) -> Self {
		let mut ret = Self::new(self.assignments.clone());
		ret.assignments.extend(iter.into_iter().copied());
		ret
	}
}