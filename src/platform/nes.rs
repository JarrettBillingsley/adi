use std::io::{ BufReader, Cursor };

use nes_rom::ines::{ Ines };

use crate::platform::{ IPlatform, PlatformResult, PlatformError };
// use crate::arch::mos65xx::{ Mos65xxArchitecture };
use crate::memory::{ Image, MemoryRegion };
use crate::program::{ Program };

// ------------------------------------------------------------------------------------------------
// NesPlatform
// ------------------------------------------------------------------------------------------------

pub struct NesPlatform;

impl IPlatform for NesPlatform {
	fn can_parse(&self, img: &Image) -> bool {
		let reader = BufReader::new(Cursor::new(img.data()));
		match Ines::from_rom(reader) {
			Ok(..)  => true,
			Err(..) => false,
		}
	}

	fn program_from_image(&self, _img: Image) -> PlatformResult<Program> {
		// let reader = BufReader::new(Cursor::new(img.data()));
		// let cart = Ines::from_rom(reader)?;

		// 1. determine/create MMU
		// 2. create segments
		// 3. create Memory
		// 4. create Program
		// 5. setup default names


		// /// The base memory map - determined by the system architecture and nothing else.
		// fn base_memory_map(&self) -> MemoryMap {
		// 	use MemoryRegionKind::*;

		// 	MemoryMap::new(16, &[
		// 		MemoryRegion::new("RAM".into(),     VA(0x0000), VA(0x0800),  true, Ram   ),
		// 		MemoryRegion::new("RAMECHO".into(), VA(0x0800), VA(0x2000),  true, Mirror),
		// 		MemoryRegion::new("PPU".into(),     VA(0x2000), VA(0x2008),  true, Mmio  ),
		// 		MemoryRegion::new("PPUECHO".into(), VA(0x2008), VA(0x4000),  true, Mirror),
		// 		MemoryRegion::new("IOREG".into(),   VA(0x4000), VA(0x4020),  true, Mmio  ),
		// 	])
		// }

		// /// The base memory configuration - determined by the system architecture and nothing else.
		// fn base_memory_config(&self) -> MemoryConfig {
		// 	MemoryConfig::from_iter(&[
		// 		// Region, Segment
		// 		("RAM",    "RAM"),
		// 		("PPU",    "PPU"),
		// 		("IOREG",  "IOREG"),
		// 	])
		// }

		Err(PlatformError::invalid_image("ono".into()))
	}
}

// ------------------------------------------------------------------------------------------------
// NesMmu
// ------------------------------------------------------------------------------------------------

pub struct NesMmu {
	mapper: Mapper,
}

impl NesMmu {
	fn new(mapper: Mapper) -> Self {
		Self { mapper }
	}
}

// impl IMmu for NesMmu {
// 	type TState = NesMmuState;

// 	fn initial_state(&self) -> Self::TState {
// 		NesMmuState::default()
// 	}

// 	fn segid_for_va(&self, state: Self::TState, va: VA) -> Option<SegId> {

// 	}

// 	fn generate_name_for_va(&self, va: VA) -> String;
// }

// ------------------------------------------------------------------------------------------------
// Mapper
// ------------------------------------------------------------------------------------------------

pub enum Mapper {
	INes000 = 0,
}

impl Mapper {
	fn mem_regions(&self) -> Vec<MemoryRegion> {
		use Mapper::*;
		// use MemoryRegionKind::*;
		match self {
			INes000 => vec![
				//MemoryRegion::new("PRGROM".into(), VA(0x8000), VA(0x10000), false, Rom, false)
			],
		}
	}

	fn mem_config(&self) -> Vec<(String, String)> {
		use Mapper::*;
		match self {
			INes000 => vec![("PRGROM".into(), "PRG0".into())],
		}
	}
}