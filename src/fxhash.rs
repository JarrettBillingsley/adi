
pub use rustc_hash::*;

pub(crate) trait FxHashSetEx {
	fn new() -> Self;
	fn with_capacity(cap: usize) -> Self;
}

impl<T> FxHashSetEx for FxHashSet<T> {
	fn new() -> Self {
		Self::default()
	}

	fn with_capacity(cap: usize) -> Self {
		Self::with_capacity_and_hasher(cap, FxBuildHasher::default())
	}
}

pub(crate) trait FxHashMapEx {
	fn new() -> Self;
}

impl<K, V> FxHashMapEx for FxHashMap<K, V> {
	fn new() -> Self {
		Self::default()
	}
}