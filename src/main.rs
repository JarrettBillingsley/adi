
use better_panic::{ Settings, Verbosity };
use std::iter::FromIterator;
use adi::*;

fn main() -> std::io::Result<()> {
	Settings::new()
		.lineno_suffix(true)
		// .most_recent_first(false)
		.verbosity(Verbosity::Full)
		.install();

	test_nes()
}

fn test_nes() -> std::io::Result<()> {
	use MemoryRegionKind::*;

	// let's set it up
	let img = RomImage::new("smb.prg".into(), std::fs::read("tests/data/smb.prg")?);

	let map = MemoryMap::new(16, &[
		// default
		MemoryRegion::new("RAM".into(),     VAddr(0x0000), VAddr(0x0800),  true, Ram   ),
		MemoryRegion::new("RAMECHO".into(), VAddr(0x0800), VAddr(0x2000),  true, Mirror),
		MemoryRegion::new("PPU".into(),     VAddr(0x2000), VAddr(0x2008),  true, Mmio  ),
		MemoryRegion::new("PPUECHO".into(), VAddr(0x2008), VAddr(0x4000),  true, Mirror),
		MemoryRegion::new("IOREG".into(),   VAddr(0x4000), VAddr(0x4020),  true, Mmio  ),
		// ROM-specific
		MemoryRegion::new("PRGROM".into(),  VAddr(0x8000), VAddr(0x10000), false, Rom),
	]);

	let config = MemoryConfig::from_iter(&[
		// Region, Segment
		// default
		("RAM",    "RAM"),
		("PPU",    "PPU"),
		("IOREG",  "IOREG"),
	]).derive(&[
		// ROM-specific (TODO: bankable regions can change the mem config; eesh)
		("PRGROM", "PRG0"),
	]);

	let mut mem = Memory::new(Endian::Little, map, config, img);

	// default
	mem.add_segment("RAM",   VAddr(0x0000), VAddr(0x0800), None);
	mem.add_segment("PPU",   VAddr(0x2000), VAddr(0x2008), None);
	mem.add_segment("IOREG", VAddr(0x4000), VAddr(0x4020), None);
	// ROM-specific
	mem.add_segment("PRG0",  VAddr(0x8000), VAddr(0x10000), Some(PAddr(0)));

	let mut prog = Program::new(mem);
	setup_nes_labels(&mut prog);

	let mem = prog.mem();
	println!("{}", mem);

	for (loc, name) in prog.all_names_by_loc() {
		println!("{}: {}", loc, name);
	}

	println!();

	for (loc, name) in prog.names_in_va_range(..VAddr(0x2004)) {
		println!("{}: {}", loc, name);
	}

	println!();

	println!("name for 0x2000: {}", prog.name_of_va(VAddr(0x2000)));
	println!("name for 0x2001: {}", prog.name_of_va(VAddr(0x2001)));
	println!("name for 0x2008: {}", prog.name_of_va(VAddr(0x2008)));
	println!("name for 0x0400: {}", prog.name_of_va(VAddr(0x0400)));
	println!("name for 0x5000: {}", prog.name_of_va(VAddr(0x5000)));
	println!("name for 0x8000: {}", prog.name_of_va(VAddr(0x8000)));

	println!();

	println!("location for 0x0000: {:?}", mem.va_to_loc(VAddr(0x0000)));
	println!("location for 0x2000: {:?}", mem.va_to_loc(VAddr(0x2000)));
	println!("location for 0x2001: {:?}", mem.va_to_loc(VAddr(0x2001)));
	println!("location for 0x2008: {:?}", mem.va_to_loc(VAddr(0x2008)));
	println!("location for 0x0400: {:?}", mem.va_to_loc(VAddr(0x0400)));
	println!("location for 0x5000: {:?}", mem.va_to_loc(VAddr(0x5000)));
	println!("location for 0x8000: {:?}", mem.va_to_loc(VAddr(0x8000)));

	println!();

	let prg0 = mem.segment_for_name("PRG0").unwrap();
	let prg0 = mem.image_slice_from_segment(prg0);

	// Disassembly/printing!
	use mos65xx::{ Disassembler, Printer, SyntaxFlavor };
	let disas = Disassembler;
	let print = Printer::new(SyntaxFlavor::New);

	let mut iter = disas.disas_all(&prg0[..10], VAddr(0x8000));

	for inst in &mut iter {
		print!("0x{:4X}  ", inst.va());
		let b = inst.bytes();

		match b.len() {
			1 => print!("{:02X}      ",         b[0]),
			2 => print!("{:02X} {:02X}   ",     b[0], b[1]),
			3 => print!("{:02X} {:02X} {:02X}", b[0], b[1], b[2]),
			_ => unreachable!()
		}

		println!("  {}", print.fmt_instr(&inst, &prog));
	}

	if let Some(err) = iter.err() {
		println!("{}", err);
	}

	println!();

	println!("{:04X}", mem.read_le_16_loc(prog.loc_from_name("VEC_NMI")).unwrap());
	println!("{:04X}", mem.read_le_16_loc(prog.loc_from_name("VEC_RESET")).unwrap());
	println!("{:04X}", mem.read_le_16_loc(prog.loc_from_name("VEC_IRQ")).unwrap());

	Ok(())
}

fn setup_nes_labels(prog: &mut Program) {
	for StdName(name, addr) in NES_STD_NAMES {
		prog.add_name_va(name, VAddr(*addr));
	}
}

struct StdName(&'static str, usize);

const NES_STD_NAMES: &[StdName] = &[
	StdName("PPU_CTRL_REG1",         0x2000),
	StdName("PPU_CTRL_REG2",         0x2001),
	StdName("PPU_STATUS",            0x2002),
	StdName("PPU_SPR_ADDR",          0x2003),
	StdName("PPU_SPR_DATA",          0x2004),
	StdName("PPU_SCROLL_REG",        0x2005),
	StdName("PPU_ADDRESS",           0x2006),
	StdName("PPU_DATA",              0x2007),

	StdName("SND_PULSE1_CTRL1",      0x4000),
	StdName("SND_PULSE1_CTRL2",      0x4001),
	StdName("SND_PULSE1_TIMER",      0x4002),
	StdName("SND_PULSE1_LENGTH",     0x4003),
	StdName("SND_PULSE2_CTRL1",      0x4004),
	StdName("SND_PULSE2_CTRL2",      0x4005),
	StdName("SND_PULSE2_TIMER",      0x4006),
	StdName("SND_PULSE2_LENGTH",     0x4007),
	StdName("SND_TRI_CTRL",          0x4008),
	StdName("SND_TRI_TIMER",         0x400A),
	StdName("SND_TRI_LENGTH",        0x400B),
	StdName("SND_NOISE_CTRL",        0x400c),
	StdName("SND_NOISE_PERIOD",      0x400e),
	StdName("SND_NOISE_LENGTH",      0x400f),
	StdName("SND_DMC_CTRL",          0x4010),
	StdName("SND_DMC_COUNTER",       0x4011),
	StdName("SND_DMC_ADDR",          0x4012),
	StdName("SND_DMC_LEN",           0x4013),
	StdName("SPR_DMA",               0x4014),
	StdName("SND_MASTER_CTRL",       0x4015),
	StdName("JOYPAD_PORT1",          0x4016),
	StdName("JOYPAD_PORT2",          0x4017),
	StdName("TESTMODE_PULSE_DAC",    0x4018),
	StdName("TESTMODE_TRINOISE_DAC", 0x4019),
	StdName("TESTMODE_DPCM_DAC",     0x401A),
	StdName("UNUSED_TIMER_LO",       0x401C),
	StdName("UNUSED_TIMER_MID",      0x401D),
	StdName("UNUSED_TIMER_HI",       0x401E),
	StdName("UNUSED_TIMER_CTRL",     0x401F),

	StdName("VEC_NMI",               0xFFFA),
	StdName("VEC_RESET",             0xFFFC),
	StdName("VEC_IRQ",               0xFFFE),
];