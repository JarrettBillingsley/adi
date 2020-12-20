use std::marker::PhantomData;
use std::io::{ BufReader, Cursor };
use std::error::{ Error };
use std::fmt::{ Display, Formatter, Result as FmtResult };

use nes_rom::ines::{ Ines };
use parse_display::Display;

use crate::platform::{ IPlatform, ILoader, PlatformResult, PlatformError };
use crate::arch::{ IArchitecture };
use crate::arch::mos65xx::{ Mos65xxArchitecture };
use crate::memory::{ ImageRead, Memory, SegCollection, VA, IMmu, MmuState, Image, SegId, Location };
use crate::program::{ IProgram, Program };

// ------------------------------------------------------------------------------------------------
// NesPlatform
// ------------------------------------------------------------------------------------------------

struct NesPlatform<Mapper> {
	_m: PhantomData<Mapper>
}

impl<Mapper: IMmu> NesPlatform<Mapper> {
	fn new() -> Self {
		Self { _m: PhantomData }
	}
}

impl<Mapper: IMmu> IPlatform for NesPlatform<Mapper> {
	type TArchitecture = Mos65xxArchitecture;
	type TMmu = NesMmu<Mapper>;

	fn arch(&self) -> Self::TArchitecture {
		Mos65xxArchitecture
	}
}

impl<Mapper: IMmu> Display for NesPlatform<Mapper> {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		write!(f, "NES/Famicom")
	}
}

// ------------------------------------------------------------------------------------------------
// NesLoader
// ------------------------------------------------------------------------------------------------

pub struct NesLoader;

impl ILoader for NesLoader {
	fn can_parse(&self, img: &Image) -> bool {
		let reader = BufReader::new(Cursor::new(img.data()));
		match Ines::from_rom(reader) {
			Ok(..)  => true,
			Err(..) => false,
		}
	}

	#[allow(deprecated)]
	fn program_from_image(&self, img: Image) -> PlatformResult<Box<dyn IProgram>> {
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
		let mut prog = Program::new(mem, NesPlatform::new());

		// 5. setup default names
		setup_nes_labels(&mut prog);

		Ok(Box::new(prog))
	}
}

fn setup_mmu(img: &Image, segs: &mut SegCollection, cart: &Ines)
-> PlatformResult<NesMmu<impl IMmu>> {
	let ram = segs.add_segment("RAM",   0x800, None);
	let ppu = segs.add_segment("PPU",   0x008, None);
	let io  = segs.add_segment("IOREG", 0x020, None);

	match cart.mapper {
		// Most common in descending order: 1, 4, 2, 0, 3, 7, 206, 11, 5, 19

		0 => {
			let prg0_img = Image::new(img.name(), &cart.prg_data);
			let prg0_len = prg0_img.len();
			let prg0 = segs.add_segment("PRG0", prg0_len, Some(prg0_img));
			Ok(NesMmu { ram, ppu, io, mapper: mappers::NRom::new(prg0, prg0_len)})
		}

		_ => Err(PlatformError::invalid_image(format!("mapper {} unsupported", cart.mapper))),
	}
}

fn setup_nes_labels<Plat: IPlatform>(prog: &mut Program<Plat>) {
	let state = prog.initial_mmu_state();

	for StdName(name, addr) in NES_STD_NAMES {
		prog.add_name_va(name, state, VA(*addr));
	}

	for StdName(name, addr) in NES_INT_VECS {
		let src_loc = prog.loc_from_va(state, VA(*addr));
		let seg     = prog.segment_from_loc(src_loc);
		let dst_va  = VA(seg.read_le_u16(src_loc) as usize);
		let dst_loc = prog.loc_from_va(state, dst_va);

		prog.add_name_va(name, state, dst_va);

		// TODO: add a data item for each of these locations
		prog.add_ref(src_loc, dst_loc);
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
];

const NES_INT_VECS: &[StdName] = &[
	StdName("VEC_NMI",   0xFFFA),
	StdName("VEC_RESET", 0xFFFC),
	StdName("VEC_IRQ",   0xFFFE),
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

	fn va_for_loc(&self, state: MmuState, loc: Location) -> Option<VA> {
		match loc.seg {
			seg if seg == self.ram => Some(VA(loc.offs & 0x7FFF)),
			seg if seg == self.ppu => Some(VA(0x2000 + (loc.offs & 0x7))),
			seg if seg == self.io  => Some(VA(0x4000 + (loc.offs & 0x1F))),
			_                      => self.mapper.va_for_loc(state, loc),
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
	pub(super) struct NRom {
		prg0:      SegId,
		prg0_mask: usize,
	}

	impl NRom {
		pub(super) fn new(prg0: SegId, prg0_len: usize) -> Self {
			Self { prg0, prg0_mask: prg0_len - 1 }
		}
	}

	impl IMmu for NRom {
		fn initial_state(&self) -> MmuState {
			Default::default()
		}

		fn loc_for_va(&self, _state: MmuState, va: VA) -> Option<Location> {
			match va.0 {
				0x8000 ..= 0xFFFF => Some(Location::new(self.prg0, va.0 & self.prg0_mask)),
				_                 => None,
			}
		}

		fn va_for_loc(&self, _state: MmuState, loc: Location) -> Option<VA> {
			match loc.seg {
				seg if seg == self.prg0 => Some(VA(0x8000 + (loc.offs & self.prg0_mask))),
				_                       => None,
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

// TODO: when implementing bankswitching, have to somehow keep track of which banks
// are mapped into which frames/windows, so that we can detect the case that the same
// bank is mapped into different windows (which really shouldn't happen I don't think, but)