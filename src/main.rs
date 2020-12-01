
use std::iter::FromIterator;
use adi::memory::*;

fn main() -> std::io::Result<()> {
	test_nes()
}

fn test_nes() -> std::io::Result<()> {
	let regions = &[
		// default
		MemoryRegion::new("RAM",     VAddr(0x0000), VAddr(0x0800),  true,  MemoryRegionKind::Ram   ),
		MemoryRegion::new("RAMECHO", VAddr(0x0800), VAddr(0x2000),  true,  MemoryRegionKind::Mirror),
		MemoryRegion::new("PPU",     VAddr(0x2000), VAddr(0x2008),  true,  MemoryRegionKind::Mmio  ),
		MemoryRegion::new("PPUECHO", VAddr(0x2008), VAddr(0x4000),  true,  MemoryRegionKind::Mirror),
		MemoryRegion::new("IOREG",   VAddr(0x4000), VAddr(0x4020),  true,  MemoryRegionKind::Mmio  ),
		MemoryRegion::new("WEIRD",   VAddr(0x5000), VAddr(0x6000), true, MemoryRegionKind::RamBank),

		// ROM-specific
		MemoryRegion::new("PRGROM",  VAddr(0x8000), VAddr(0x10000), false, MemoryRegionKind::Rom),
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

	// let's set it up
	let img_data = std::fs::read("tests/data/smb.prg")?;
	let img = RomImage::new("smb.prg", &img_data);
	let map = MemoryMap::new(16, regions);
	let mut mem = MemoryBuilder::new(Endian::Big, img, map, config);
		// default segments
		mem.segment("RAM",   VAddr(0x0000), VAddr(0x0800), None);
		mem.segment("PPU",   VAddr(0x2000), VAddr(0x2008), None);
		mem.segment("IOREG", VAddr(0x4000), VAddr(0x4020), None);

		// ROM-specific segments
		mem.segment("PRG0",  VAddr(0x8000), VAddr(0x10000), Some(PAddr(0)));
	let mut mem = mem.build();

	for StdName { name, addr } in NES_STD_NAMES {
		mem.add_name_va(name, VAddr(*addr));
	}

	println!("{}", mem);

	println!("0x2000: {}", mem.name_of_va(VAddr(0x2000)));
	println!("0x2001: {}", mem.name_of_va(VAddr(0x2001)));
	println!("0x2008: {}", mem.name_of_va(VAddr(0x2008)));
	println!("0x0400: {}", mem.name_of_va(VAddr(0x0400)));
	println!("0x5000: {}", mem.name_of_va(VAddr(0x5000)));
	println!("0x8000: {}", mem.name_of_va(VAddr(0x8000)));

	// for (name, va) in mem.all_names() { println!("{}: 0x{:04x}", name, va); }
	// println!();
	// for (va, name) in mem.all_names_by_va() { println!("0x{:04x}: {}", va, name); }
	// println!();
	// for (va, name) in mem.names_in_range(..VAddr(0x2004)) { println!("0x{:04x}: {}", va, name); }

	println!();

	println!("location for 0x0000: {:?}", mem.va_to_loc(VAddr(0x0000)));
	println!("location for 0x2000: {:?}", mem.va_to_loc(VAddr(0x2000)));
	println!("location for 0x2001: {:?}", mem.va_to_loc(VAddr(0x2001)));
	println!("location for 0x2008: {:?}", mem.va_to_loc(VAddr(0x2008)));
	println!("location for 0x0400: {:?}", mem.va_to_loc(VAddr(0x0400)));
	println!("location for 0x5000: {:?}", mem.va_to_loc(VAddr(0x5000)));
	println!("location for 0x8000: {:?}", mem.va_to_loc(VAddr(0x8000)));

	Ok(())
}

struct StdName {
	name: &'static str,
	addr: usize,
}

const NES_STD_NAMES: &[StdName] = &[
	StdName { name: "PPU_CTRL_REG1",         addr: 0x2000 },
	StdName { name: "PPU_CTRL_REG2",         addr: 0x2001 },
	StdName { name: "PPU_STATUS",            addr: 0x2002 },
	StdName { name: "PPU_SPR_ADDR",          addr: 0x2003 },
	StdName { name: "PPU_SPR_DATA",          addr: 0x2004 },
	StdName { name: "PPU_SCROLL_REG",        addr: 0x2005 },
	StdName { name: "PPU_ADDRESS",           addr: 0x2006 },
	StdName { name: "PPU_DATA",              addr: 0x2007 },

	StdName { name: "SND_PULSE1_CTRL1",      addr: 0x4000 },
	StdName { name: "SND_PULSE1_CTRL2",      addr: 0x4001 },
	StdName { name: "SND_PULSE1_TIMER",      addr: 0x4002 },
	StdName { name: "SND_PULSE1_LENGTH",     addr: 0x4003 },
	StdName { name: "SND_PULSE2_CTRL1",      addr: 0x4004 },
	StdName { name: "SND_PULSE2_CTRL2",      addr: 0x4005 },
	StdName { name: "SND_PULSE2_TIMER",      addr: 0x4006 },
	StdName { name: "SND_PULSE2_LENGTH",     addr: 0x4007 },
	StdName { name: "SND_TRI_CTRL",          addr: 0x4008 },
	StdName { name: "SND_TRI_TIMER",         addr: 0x400A },
	StdName { name: "SND_TRI_LENGTH",        addr: 0x400B },
	StdName { name: "SND_NOISE_CTRL",        addr: 0x400c },
	StdName { name: "SND_NOISE_PERIOD",      addr: 0x400e },
	StdName { name: "SND_NOISE_LENGTH",      addr: 0x400f },
	StdName { name: "SND_DMC_CTRL",          addr: 0x4010 },
	StdName { name: "SND_DMC_COUNTER",       addr: 0x4011 },
	StdName { name: "SND_DMC_ADDR",          addr: 0x4012 },
	StdName { name: "SND_DMC_LEN",           addr: 0x4013 },
	StdName { name: "SPR_DMA",               addr: 0x4014 },
	StdName { name: "SND_MASTER_CTRL",       addr: 0x4015 },
	StdName { name: "JOYPAD_PORT1",          addr: 0x4016 },
	StdName { name: "JOYPAD_PORT2",          addr: 0x4017 },
	StdName { name: "TESTMODE_PULSE_DAC",    addr: 0x4018 },
	StdName { name: "TESTMODE_TRINOISE_DAC", addr: 0x4019 },
	StdName { name: "TESTMODE_DPCM_DAC",     addr: 0x401A },
	StdName { name: "UNUSED_TIMER_LO",       addr: 0x401C },
	StdName { name: "UNUSED_TIMER_MID",      addr: 0x401D },
	StdName { name: "UNUSED_TIMER_HI",       addr: 0x401E },
	StdName { name: "UNUSED_TIMER_CTRL",     addr: 0x401F },

	StdName { name: "VEC_NMI",               addr: 0xFFFA },
	StdName { name: "VEC_RESET",             addr: 0xFFFC },
	StdName { name: "VEC_IRQ",               addr: 0xFFFE },
];