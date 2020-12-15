use std::io::{ BufReader, Cursor };

use nes_rom::ines::{ Ines };

use crate::platform::{ IPlatform };
use crate::arch::mos65xx::{ Mos65xxArchitecture };
use crate::memory::{ VA, MemoryRegion, MemoryRegionKind, Image };

pub struct NesPlatform {
	mapper: Mapper,
}

impl NesPlatform {
	fn new(mapper: Mapper) -> Self {
		Self { mapper }
	}
}

impl IPlatform for NesPlatform {
	type TCpuArchitecture = Mos65xxArchitecture;

	fn can_parse(img: &Image) -> bool {
		let reader = BufReader::new(Cursor::new(img.data()));
		match Ines::from_rom(reader) {
			Ok(..)  => true,
			Err(..) => false,
		}
	}
}

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

pub enum Mapper {
	INes000 = 0,
}

impl Mapper {
	fn mem_regions(&self) -> Vec<MemoryRegion> {
		use Mapper::*;
		use MemoryRegionKind::*;
		match self {
			INes000 => vec![MemoryRegion::new("PRGROM".into(), VA(0x8000), VA(0x10000), false, Rom)],
		}
	}

	fn mem_config(&self) -> Vec<(String, String)> {
		use Mapper::*;
		match self {
			INes000 => vec![("PRGROM".into(), "PRG0".into())],
		}
	}
}