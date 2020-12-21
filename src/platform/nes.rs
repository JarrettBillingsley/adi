use std::io::{ BufReader, Cursor };
use std::error::{ Error };
use std::fmt::{ Display, Formatter, Result as FmtResult };

use nes_rom::ines::{ Ines };
use parse_display::Display;
use enum_dispatch::enum_dispatch;

use crate::platform::{ IPlatform, ILoader, PlatformResult, PlatformError };
use crate::arch::{ IArchitecture };
use crate::arch::mos65xx::{ Mos65xxArchitecture };
use crate::memory::{ ImageRead, Memory, SegCollection, VA, IMmu, MmuState, Image, SegId, Location };
use crate::program::{ IProgram, Program };

// ------------------------------------------------------------------------------------------------
// NesPlatform
// ------------------------------------------------------------------------------------------------

struct NesPlatform;

impl NesPlatform {
	fn new() -> Self { Self }
}

impl IPlatform for NesPlatform {
	type TArchitecture = Mos65xxArchitecture;
	type TMmu = NesMmu;

	fn arch(&self) -> Self::TArchitecture {
		Mos65xxArchitecture
	}
}

impl Display for NesPlatform {
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
			Err(e) => return PlatformError::invalid_image(e.description().into()),
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
-> PlatformResult<NesMmu> {
	let ram = segs.add_segment("RAM",   0x800, None);
	let ppu = segs.add_segment("PPU",   0x008, None);
	let io  = segs.add_segment("IOREG", 0x020, None);

	// TODO: PRG RAM and RAM banking
	let mapper = match cart.mapper {
		// Most common in descending order: 1, 4, 2, 0, 3, 7, 206, 11, 5, 19

		0 | 3 => {
			if (cart.prg_data.len() != 0x4000) && (cart.prg_data.len() != 0x8000) {
				return PlatformError::invalid_image(
					format!("PRG ROM length (0x{:X}) must be 8 or 16KB", cart.prg_data.len()))
			}

			// TODO: this sets "orig_offs" to 0 every time.
			let prg0_img = Image::new(img.name(), &cart.prg_data);
			let prg0_len = prg0_img.len();
			let prg0 = segs.add_segment("PRG0", prg0_len, Some(prg0_img));
			let ctor = if cart.mapper == 0 { NRom::new } else { NRom::new_cnrom };
			ctor(prg0, prg0_len)
		}

		2 => {
			let prg_rom_len = cart.prg_data.len();

			// data must be a multiple of 16KB (0x8000)
			if (prg_rom_len % 0x8000) != 0 {
				return PlatformError::invalid_image(
					format!("PRG ROM length (0x{:X}) not a multiple of 16KB", cart.prg_data.len()))
			}

			let mut all = Vec::new();
			let mut iter = cart.prg_data.chunks_exact(0x4000);

			while let Some(chunk) = iter.next() {
				let prg_img = Image::new(img.name(), chunk);
				let seg_id = segs.add_segment(&format!("PRG{}", all.len()), 0x4000, Some(prg_img));
				all.push(seg_id);
			}

			assert_eq!(iter.remainder().len(), 0);

			UXRom::new(all)
		}

		_ => return PlatformError::invalid_image(format!("mapper {} unsupported", cart.mapper)),
	};

	Ok(NesMmu { ram, ppu, io, mapper })
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

		// sometimes two+ vectors can be pointing at the same location.
		if !prog.has_name_for_loc(dst_loc) {
			prog.add_name_va(name, state, dst_va);
		}

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
	StdName("SND_NOISE_CTRL",        0x400C),
	StdName("SND_NOISE_PERIOD",      0x400E),
	StdName("SND_NOISE_LENGTH",      0x400F),
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

#[enum_dispatch]
#[derive(Debug)]
enum Mapper {
	NRom,
	UXRom,
}

impl Display for Mapper {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		match self {
			Mapper::NRom(m) => m.fmt(f),
			Mapper::UXRom(m) => m.fmt(f),
		}
	}
}

#[enum_dispatch(Mapper)]
trait IMapper {
	fn initial_state(&self) -> MmuState;
	fn loc_for_va(&self, state: MmuState, va: VA) -> Option<Location>;
	fn va_for_loc(&self, state: MmuState, loc: Location) -> Option<VA>;
	fn name_prefix_for_va(&self, state: MmuState, va: VA) -> String;
}

#[derive(Debug)]
pub struct NesMmu {
	ram:    SegId,
	ppu:    SegId,
	io:     SegId,
	mapper: Mapper,
}

impl std::fmt::Display for NesMmu {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		write!(f, "{}", self.mapper)
	}
}

impl IMmu for NesMmu {
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

// ---------------------------------------------------------------------------------------------
// iNes 0 (NROM, no mapper)
// iNes 3 (CNROM, no PRG banking)

#[derive(Debug, Display)]
#[display("{name}")]
struct NRom {
	name:      &'static str,
	prg0:      SegId,
	prg0_mask: usize,
	prg0_base: usize,
}

impl NRom {
	fn new(prg0: SegId, prg0_len: usize) -> Mapper {
		Self::_new("<no mapper>", prg0, prg0_len)
	}

	fn new_cnrom(prg0: SegId, prg0_len: usize) -> Mapper {
		Self::_new("CNROM", prg0, prg0_len)
	}

	fn _new(name: &'static str, prg0: SegId, prg0_len: usize) -> Mapper {
		let prg0_base = 0x10000 - prg0_len;
		Self { name, prg0, prg0_mask: prg0_len - 1, prg0_base }.into()
	}
}

impl IMapper for NRom {
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
			seg if seg == self.prg0 => Some(VA(self.prg0_base + (loc.offs & self.prg0_mask))),
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

// ---------------------------------------------------------------------------------------------
// iNes 2 (UXROM)

#[derive(Debug, Display)]
#[display("UXROM")]
struct UXRom {
	all:  Vec<SegId>,
	last: SegId,
}

impl UXRom {
	fn new(all: Vec<SegId>) -> Mapper {
		let last = *all.last().unwrap();
		Self { all, last }.into()
	}

	fn seg_idx_for_loc(&self, seg: SegId) -> Option<usize> {
		// TODO: linear... problem?
		for (i, &s) in self.all.iter().enumerate() {
			if s == seg {
				return Some(i);
			}
		}

		None
	}

	fn contains_seg(&self, seg: SegId) -> bool {
		self.seg_idx_for_loc(seg).is_some()
	}
}

impl IMapper for UXRom {
	fn initial_state(&self) -> MmuState {
		Default::default()
	}

	fn loc_for_va(&self, state: MmuState, va: VA) -> Option<Location> {
		let offs = va.0 & 0x3FFF;

		match va.0 {
			0x8000 ..= 0xBFFF => Some(Location::new(self.all[state.to_usize()], offs)),
			0xC000 ..= 0xFFFF => Some(Location::new(self.last,                  offs)),
			_                 => None,
		}
	}

	fn va_for_loc(&self, _state: MmuState, loc: Location) -> Option<VA> {
		let offs = loc.offs & 0x3FFF;

		// every segment except the last has a virtual base of 0x8000.
		if loc.seg == self.last {
			Some(VA(0xC000 + offs))
		} else if self.contains_seg(loc.seg) {
			Some(VA(0x8000 + offs))
		} else {
			None
		}
	}

	fn name_prefix_for_va(&self, state: MmuState, va: VA) -> String {
		match va.0 {
			0x8000 ..= 0xBFFF => format!("PRG{}", state.to_usize()),
			0xC000 ..= 0xFFFF => format!("PRG{}", self.all.len() - 1),
			_                 => "UNK".into(),
		}
	}
}
