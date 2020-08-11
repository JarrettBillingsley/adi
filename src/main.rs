
use std::iter::FromIterator;
use adi::memory::*;

fn main() {
	test_nes();
}

fn test_nes() {
	let regions = &[
		//default
		MemoryRegion::new("RAM",      0x0000,  0x0800, true, MemoryRegionKind::Ram   ),
		MemoryRegion::new("RAMECHO",  0x0800,  0x2000, true, MemoryRegionKind::Mirror),
		MemoryRegion::new("PPU",      0x2000,  0x2008, true, MemoryRegionKind::Mmio  ),
		MemoryRegion::new("PPUECHO",  0x2008,  0x4000, true, MemoryRegionKind::Mirror),
		MemoryRegion::new("IOREG",    0x4000,  0x4020, true, MemoryRegionKind::Mmio  ),

		// ROM-specific
		MemoryRegion::new("PRGROM",   0x8000, 0x10000, false, MemoryRegionKind::Rom),
	];

	let mmap = MemoryMap::new(16, regions);

	// Region: Segment
	let config = MemoryConfig::from_iter(&[
		// default
		("RAM",    "RAM"),
		("PPU",    "PPU"),
		("IOREG",  "IOREG"),

		// ROM-specific
		("PRGROM", "PRG0"),
	]);

	println!("{:?}", mmap);
	println!("{:?}", config);

	/*	let segments = &[
			// default
			FakeSegment ("RAM",   vbase = 0x0000, vend =  0x0800, type = SegType.DATA),
			FakeSegment ("PPU",   vbase = 0x2000, vend =  0x2008, type = SegType.DATA),
			FakeSegment ("IOREG", vbase = 0x4000, vend =  0x4020, type = SegType.DATA),

			// ROM-specific
			ImageSegment("PRG0",  vbase = 0x8000, vend = 0x10000, pbase = 0, type = SegType.CODE|SegType.DATA),
		];
	*/
}