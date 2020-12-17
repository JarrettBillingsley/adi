use std::io::{ BufReader, Cursor };
use std::error::{ Error };
use std::fmt::{ Formatter, Result as FmtResult };

use nes_rom::ines::{ Ines };
use parse_display::Display;

use crate::platform::{ IPlatform, PlatformResult, PlatformError };
use crate::arch::{ IArchitecture };
use crate::arch::mos65xx::{ Mos65xxArchitecture };
use crate::memory::{ Memory, SegCollection, VA, IMmu, MmuState, Image, SegId, Location };
use crate::program::{ Program };

// ------------------------------------------------------------------------------------------------
// NesPlatform
// ------------------------------------------------------------------------------------------------

#[derive(Copy, Clone, Display)]
#[display("NES/Famicom")]
pub struct NesPlatform;

impl IPlatform for NesPlatform {
	fn can_parse(&self, img: &Image) -> bool {
		let reader = BufReader::new(Cursor::new(img.data()));
		match Ines::from_rom(reader) {
			Ok(..)  => true,
			Err(..) => false,
		}
	}

	#[allow(deprecated)]
	fn program_from_image(&self, img: Image) -> PlatformResult<Program> {
		let reader = BufReader::new(Cursor::new(img.data()));
		let cart = match Ines::from_rom(reader) {
			Ok(cart) => cart,
			// ines::RomError has a buggy Display impl, can't call to_string
			Err(e) => return Err(PlatformError::invalid_image(e.description().into())),
		};

		let mut segs = SegCollection::new();
		let mmu = setup_mmu(&img, &mut segs, &cart)?;

		let mem = Memory::new(
			Mos65xxArchitecture.addr_bits(),
			Mos65xxArchitecture.endianness(),
			segs,
			mmu
		);

		// 4. create Program
		let mut prog = Program::new(Box::new(mem), Box::new(*self));

		// 5. setup default names
		setup_nes_labels(&mut prog);

		Ok(prog)
	}
}

fn setup_mmu(img: &Image, segs: &mut SegCollection, cart: &Ines)
-> PlatformResult<NesMmu<impl IMmu>> {
	let ram = segs.add_segment("RAM",   VA(0x0000), VA(0x0800), None);
	let ppu = segs.add_segment("PPU",   VA(0x2000), VA(0x2008), None);
	let io  = segs.add_segment("IOREG", VA(0x4000), VA(0x4020), None);

	match cart.mapper {
		// Most common in descending order: 1, 4, 2, 0, 3, 7, 206, 11, 5, 19

		0 => {
			let prg0_img = Image::new(img.name(), &cart.prg_data);
			let prg0 = segs.add_segment("PRG0", VA(0x8000), VA(0x10000), Some(prg0_img));
			Ok(NesMmu { ram, ppu, io, mapper: mappers::NRom { prg0 } })
		}

		_ => Err(PlatformError::invalid_image(format!("mapper {} unsupported", cart.mapper))),
	}
}

fn setup_nes_labels(prog: &mut Program) {
	for StdName(name, addr) in NES_STD_NAMES {
		prog.add_name_va(name, VA(*addr));
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

// ------------------------------------------------------------------------------------------------
// NesMmu
// ------------------------------------------------------------------------------------------------

#[derive(Debug)]
pub struct NesMmu<Mapper: IMmu> {
	ram:    SegId,
	ppu:    SegId,
	io:     SegId,
	mapper: Mapper,
}

impl<Mapper: IMmu> std::fmt::Display for NesMmu<Mapper> {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		write!(f, "{}", self.mapper)
	}
}

impl<Mapper: IMmu> IMmu for NesMmu<Mapper> {
	fn initial_state(&self) -> MmuState {
		self.mapper.initial_state()
	}

	fn loc_for_va(&self, state: MmuState, va: VA) -> Option<Location> {
		match va.0 {
			0x0000 ..= 0x1FFF => Some(Location::new(self.ram, va.0 & 0x7FF)),
			0x2000 ..= 0x3FFF => Some(Location::new(self.ppu, va.0 & 0x7)),
			0x4000 ..= 0x401F => Some(Location::new(self.io,  va.0 & 0x1F)),
			_                 => self.mapper.loc_for_va(state, va),
		}
	}

	fn name_prefix_for_va(&self, state: MmuState, va: VA) -> String {
		match va.0 {
			0x0000 ..= 0x07FF => "RAM".into(),
			0x0800 ..= 0x1FFF => "RAMECHO".into(),
			0x2000 ..= 0x2007 => "PPU".into(),
			0x2008 ..= 0x3FFF => "PPUECHO".into(),
			0x4000 ..= 0x401F => "IOREG".into(),
			_                 => self.mapper.name_prefix_for_va(state, va),
		}
	}
}

// ------------------------------------------------------------------------------------------------
// Mappers
// ------------------------------------------------------------------------------------------------

mod mappers {
	use super::*;

	// ---------------------------------------------------------------------------------------------
	// iNes 0 (NROM, no mapper)

	#[derive(Debug, Display)]
	#[display("<no mapper>")]
	pub(crate) struct NRom { pub prg0: SegId }

	impl IMmu for NRom {
		fn initial_state(&self) -> MmuState {
			Default::default()
		}

		fn loc_for_va(&self, _state: MmuState, va: VA) -> Option<Location> {
			match va.0 {
				0x8000 ..= 0xFFFF => Some(Location::new(self.prg0, va.0 - 0x8000)),
				_                 => None,
			}
		}

		fn name_prefix_for_va(&self, _state: MmuState, va: VA) -> String {
			match va.0 {
				0x8000 ..= 0xFFFF => "PRG0".into(),
				_                 => "UNK".into(),
			}
		}
	}
}