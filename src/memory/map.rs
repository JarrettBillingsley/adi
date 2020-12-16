
use crate::memory::{ VA, IMmu, SegId };

// ------------------------------------------------------------------------------------------------
// Memory map
// ------------------------------------------------------------------------------------------------

#[allow(clippy::len_without_is_empty)]
pub trait IMemoryMap {
	/// how many bits an address is.
	fn bits(&self) -> usize;
	/// how many digits in a formatted address.
	fn digits(&self) -> usize;
	/// the first invalid address, and the size of the virtual address space.
	fn end(&self) -> VA;
	/// The length of the address space.
	fn len(&self) -> usize;
	/// Given a VA, get the ID of the segment that (currently) contains it, or None if none does.
	fn segid_for_va(&self, va: VA) -> Option<SegId>;
	fn name_prefix_for_va(&self, va: VA) -> String;
}

/// Describes a CPU's entire memory map.
///
/// Once created, the memory map cannot change.
#[derive(Debug)]
pub struct MemoryMap<TMmu: IMmu> {
	bits:     usize,
	digits:   usize,
	end:      VA,
	mmu:      TMmu,
}

impl<TMmu: IMmu> MemoryMap<TMmu> {
	/// given a number of bits in the address and an MMU, constructs a new MemoryMap.
	pub fn new(bits: usize, mmu: TMmu) -> Self {
		Self {
			bits,
			digits: ((bits + 3) & !3) >> 2, // round up to next multiple of 4, divide by 4
			end: VA(2_usize.pow(bits as u32)),
			mmu,
		}
	}
}

impl<TMmu: IMmu> IMemoryMap for MemoryMap<TMmu> {
	/// how many bits an address is.
	fn bits(&self) -> usize { self.bits }

	/// how many digits in a formatted address.
	fn digits(&self) -> usize { self.digits }

	/// the first invalid address, and the size of the virtual address space.
	fn end(&self) -> VA { self.end }

	/// The length of the address space.
	fn len(&self) -> usize { self.end.0 }

	fn segid_for_va(&self, va: VA) -> Option<SegId> {
		let state = self.mmu.initial_state();
		self.mmu.segid_for_va(state, va)
	}

	fn name_prefix_for_va(&self, va: VA) -> String {
		let state = self.mmu.initial_state();
		self.mmu.name_prefix_for_va(state, va)
	}
}