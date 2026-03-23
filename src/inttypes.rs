
/// The type of "offsets," unsigned distances into some piece of memory.
///
/// This is not `usize` but `u64`, since `usize` could be smaller than `u64` but we may need to
/// represent 64-bit offsets.
pub type Offs = u64;

/// Convert an [`offs_t`] to a `usize`, panicking if it doesn't fit.
pub fn to_usize(o: Offs) -> usize {
	o.try_into().unwrap()
}

/// The type of "sizes," counts of things.
pub type Size = u64;