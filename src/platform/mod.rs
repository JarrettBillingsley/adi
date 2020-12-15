
use crate::memory::{ Image };
use crate::arch::{ IArchitecture };

// ------------------------------------------------------------------------------------------------
// Sub-modules
// ------------------------------------------------------------------------------------------------

mod nes;

pub use nes::NesPlatform;

// ------------------------------------------------------------------------------------------------
// IPlatform
// ------------------------------------------------------------------------------------------------

pub trait IPlatform: Sized + Sync + Send {
	/// The architecture used by the CPU.
	type TCpuArchitecture: IArchitecture;

	// TODO: there can be more than one processor in a platform...

	fn can_parse(img: &Image) -> bool;
}