
use std::iter::FromIterator;
use adi::memory::*;

fn main() -> std::io::Result<()> {
	test_nes()
}

fn test_nes() -> std::io::Result<()> {
	let regions = &[
		// default
		MemoryRegion::new("RAM",      0x0000,  0x0800, true, MemoryRegionKind::Ram   ),
		MemoryRegion::new("RAMECHO",  0x0800,  0x2000, true, MemoryRegionKind::Mirror),
		MemoryRegion::new("PPU",      0x2000,  0x2008, true, MemoryRegionKind::Mmio  ),
		MemoryRegion::new("PPUECHO",  0x2008,  0x4000, true, MemoryRegionKind::Mirror),
		MemoryRegion::new("IOREG",    0x4000,  0x4020, true, MemoryRegionKind::Mmio  ),

		// ROM-specific
		MemoryRegion::new("PRGROM",   0x8000, 0x10000, false, MemoryRegionKind::Rom),
	];

	let segments = &mut [
		// default
		Segment::new("RAM",   VAddr(0x0000), VAddr(0x0800), None),
		Segment::new("PPU",   VAddr(0x2000), VAddr(0x2008), None),
		Segment::new("IOREG", VAddr(0x4000), VAddr(0x4020), None),

		// ROM-specific
		Segment::new("PRG0",  VAddr(0x8000), VAddr(0x10000), Some(PAddr(0))),
	];

	// default
	let nes_config = MemoryConfig::from_iter(&[
		// Region, Segment
		("RAM",    "RAM"),
		("PPU",    "PPU"),
		("IOREG",  "IOREG"),
	]);

	// ROM-specific
	let config = nes_config.derive(&[
		("PRGROM", "PRG0"),
	]);

	let img_data = std::fs::read("tests/data/smb.prg")?;
	let img = RomImage::new("smb.prg", &img_data);
	let map = MemoryMap::new(16, regions);
	let mem = Memory::new(img, segments, map, config);
	println!("{}", mem);

	Ok(())
}