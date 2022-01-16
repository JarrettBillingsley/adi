use std::fmt::{ Display, Formatter, Result as FmtResult };

use parse_display::{ Display };
use enum_dispatch::enum_dispatch;

use crate::platform::{ IPlatform, ILoader, PlatformResult, PlatformError };
use crate::arch::{ Architecture, IArchitecture };
use crate::arch::gb::{ GBArchitecture };
use crate::memory::{ Memory, SegCollection, VA, IMmu, MmuState, StateChange, Image, SegId,
	EA };
use crate::program::{ Program, Instruction, Operand };

// ------------------------------------------------------------------------------------------------
// GBPlatform
// ------------------------------------------------------------------------------------------------

pub struct GBPlatform;

impl GBPlatform {
	fn new() -> Self { Self }
}

impl IPlatform for GBPlatform {
	fn arch(&self) -> Architecture {
		GBArchitecture.into()
	}
}

impl Display for GBPlatform {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		write!(f, "Game Boy")
	}
}

// ------------------------------------------------------------------------------------------------
// GBHeader
// ------------------------------------------------------------------------------------------------

// n*nt*ndo can eat my entire ass if they have a problem with this number
const GB_LOGO: [u8; 48] = [
	0xCE, 0xED, 0x66, 0x66, 0xCC, 0x0D, 0x00, 0x0B, 0x03, 0x73, 0x00, 0x83, 0x00, 0x0C, 0x00, 0x0D,
	0x00, 0x08, 0x11, 0x1F, 0x88, 0x89, 0x00, 0x0E, 0xDC, 0xCC, 0x6E, 0xE6, 0xDD, 0xDD, 0xD9, 0x99,
	0xBB, 0xBB, 0x67, 0x63, 0x6E, 0x0E, 0xEC, 0xCC, 0xDD, 0xDC, 0x99, 0x9F, 0xBB, 0xB9, 0x33, 0x3E,
];

#[derive(Debug, Display, PartialEq, Eq, Copy, Clone)]
enum MbcType {
	#[display("<no MBC>")]      None,
	#[display("MBC1")]          Mbc1,
	#[display("MBC2")]          Mbc2,
	#[display("MMM01")]         Mmm01,
	#[display("MBC3")]          Mbc3,
	#[display("MBC5")]          Mbc5,
	#[display("MBC6")]          Mbc6,
	#[display("MBC7")]          Mbc7,
	#[display("Pocket Camera")] PocketCamera,
	#[display("Bandai TAMA5")]  Tama5,
	#[display("HuC-3")]         Huc3,
	#[display("HuC-1")]         Huc1,
	#[display("{:?}")]          Other(u8),
}

impl From<u8> for MbcType {
	fn from(x: u8) -> Self {
		use MbcType::*;

		match x {
			0x00 | 0x08 | 0x09 => None,
			0x01 | 0x02 | 0x03 => Mbc1,
			0x05 | 0x06        => Mbc2,
			0x0B | 0x0C | 0x0D => Mmm01,
			0x0F ..= 0x13      => Mbc3,
			0x19 ..= 0x1E      => Mbc5,
			0x20               => Mbc6,
			0x22               => Mbc7,
			0xFC               => PocketCamera,
			0xFD               => Tama5,
			0xFE               => Huc3,
			0xFF               => Huc1,
			x                  => Other(x),
		}
	}
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
enum SgbFlags {
	Regular,
	Sgb,
	Other(u8),
}

impl From<u8> for SgbFlags {
	fn from(x: u8) -> Self {
		use SgbFlags::*;

		match x {
			0 => Regular,
			3 => Sgb,
			x => Other(x),
		}
	}
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
enum CgbFlags {
	Regular,
	BackwardsCompatible,
	CgbOnly,
	Other(u8),
}

impl CgbFlags {
	fn is_cgb(&self) -> bool {
		matches!(self, CgbFlags::BackwardsCompatible | CgbFlags::CgbOnly)
	}
}

impl From<u8> for CgbFlags {
	fn from(x: u8) -> Self {
		use CgbFlags::*;

		match x {
			0    => Regular,
			0x80 => BackwardsCompatible,
			0xC0 => CgbOnly,
			x    => Other(x),
		}
	}
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
struct CartOptions {
	battery: bool, // has battery-backed RAM?
	timer:   bool, // has RTC?
	rumble:  bool, // has rumble motor?
	accel:   bool, // has 2-axis accelerometer?
	ir:      bool, // has infrared port?
}

impl From<u8> for CartOptions {
	fn from(x: u8) -> Self {
		Self {
			battery: matches!(x, 0x03 | 0x06 | 0x09 | 0x0D | 0x0F | 0x10 | 0x13 | 0x1B | 0x1E | 0xFF),
			timer:   matches!(x, 0x0F | 0x10 | 0xFD | 0xFE),
			rumble:  matches!(x, 0x1C | 0x1D | 0x1E | 0x22),
			accel:   matches!(x, 0x22),
			ir:      matches!(x, 0xFE | 0xFF),
		}
	}
}

#[repr(C, packed)]
struct UnparsedGBHeader {
	entry:        [u8; 4],
	logo:         [u8; 48],
	title_manuf:  [u8; 15],
	cgb:          u8,
	new_licensee: [u8; 2],
	sgb:          u8,
	cart_type:    u8,
	rom_size:     u8,
	ram_size:     u8,
	dest:         u8,
	old_licensee: u8,
	rom_version:  u8,
	hdr_checksum: u8,
	checksum:     [u8; 2],
}

impl UnparsedGBHeader {
	fn check_logo(&self) -> PlatformResult<()> {
		if self.logo == GB_LOGO { Ok(()) } else {
			PlatformError::invalid_image("bad logo data".into())
		}
	}

	fn check_sgb(&self) -> PlatformResult<()> {
		if self.sgb != 0 && self.old_licensee != 0x33 {
			PlatformError::invalid_image(format!("bad SGB licensee 0x{:02X}", self.old_licensee))
		} else { Ok(()) }
	}

	fn check_checksum(&self, img: &[u8]) -> PlatformResult<()> {
		let mut cs: u8 = 0;

		for byte in &img[0x0134 ..= 0x014C] {
			cs = cs.wrapping_sub(*byte).wrapping_sub(1);
		}

		if cs == self.hdr_checksum {
			Ok(())
		} else {
			PlatformError::invalid_image(format!(
				"bad header checksum (expected 0x{:02X}, got 0x{:02X})", self.hdr_checksum, cs))
		}
	}

	fn check_rom_ram(&self) -> PlatformResult<()> {
		match self.rom_size {
			0 ..= 8 | 0x52 ..= 0x54 => {}
			_ => return PlatformError::invalid_image("bad rom size".into())
		}

		match self.ram_size {
			0 ..= 5 => Ok(()),
			_ => PlatformError::invalid_image("bad ram size".into())
		}
	}
}

struct GBHeader {
	entry:         [u8; 4],
	title:         String,
	manufacturer:  String,
	cgb:           CgbFlags,
	new_licensee:  String,
	sgb:           SgbFlags,
	mbc_type:      MbcType,
	options:       CartOptions,
	rom_bank_size: usize,
	rom_num_banks: usize,
	ram_bank_size: usize,
	ram_num_banks: usize,
	is_japanese:   bool,
	old_licensee:  u8,
	rom_version:   u8,
	hdr_checksum:  u8,
	checksum:      u16,
}

impl GBHeader {
	fn rom_size(&self) -> usize {
		self.rom_bank_size * self.rom_num_banks
	}

	fn ram_size(&self) -> usize {
		self.ram_bank_size * self.ram_num_banks
	}
}

impl From<&UnparsedGBHeader> for GBHeader {
	fn from(h: &UnparsedGBHeader) -> Self {
		let (title, manufacturer) = if h.title_manuf[10] == 0 && h.title_manuf[11] != 0 {
			(
				std::str::from_utf8(&h.title_manuf[..10]).unwrap_or("").trim_end_matches(0 as char),
				std::str::from_utf8(&h.title_manuf[11..]).unwrap_or("").trim_end_matches(0 as char),
			)
		} else {
			(
				std::str::from_utf8(&h.title_manuf).unwrap_or("").trim_end_matches(0 as char),
				"",
			)
		};

		let new_licensee = std::str::from_utf8(&h.new_licensee).unwrap_or("")
			.trim_end_matches(0 as char);

		// NOTE: apparently there is a Wisdom Tree mapper that has 0x8000-byte banks?
		let (rom_bank_size, rom_num_banks) = match h.rom_size {
			0 ..= 8 => (0x4000, 1 << (h.rom_size + 1)),
			0x52    => (0x4000, 72),
			0x53    => (0x4000, 80),
			0x54    => (0x4000, 96),
			_       => unreachable!(),
		};

		let (ram_bank_size, ram_num_banks) = match h.ram_size {
			0 => {
				// MBC2 is a weirdo... and technically even this is wrong
				// because it's only 4-bit ram!
				if matches!(h.cart_type, 5 | 6) {
					(0x200, 1)
				} else {
					(0, 0)
				}
			}

			1 => ( 0x800, 1), // 2KB
			2 => (0x2000, 1), // 8KB
			3 => (0x2000, 4),
			4 => (0x2000, 16),
			5 => (0x2000, 8),
			_ => unreachable!()
		};

		Self {
			entry:         h.entry,
			title:         title.into(),
			manufacturer:  manufacturer.into(),
			cgb:           CgbFlags::from(h.cgb),
			new_licensee:  new_licensee.into(),
			sgb:           SgbFlags::from(h.sgb),
			mbc_type:      MbcType::from(h.cart_type),
			options:       CartOptions::from(h.cart_type),
			rom_bank_size,
			rom_num_banks,
			ram_bank_size,
			ram_num_banks,
			is_japanese:   h.dest == 0,
			old_licensee:  h.old_licensee,
			rom_version:   h.rom_version,
			hdr_checksum:  h.hdr_checksum,
			// according to pandocs, this is big-endian
			checksum:      u16::from_be_bytes(h.checksum),
		}
	}
}

// ------------------------------------------------------------------------------------------------
// GBCart
// ------------------------------------------------------------------------------------------------

struct GBCart {
	header:    GBHeader,
	rom_banks: Vec<Image>,
}

// ------------------------------------------------------------------------------------------------
// GBLoader
// ------------------------------------------------------------------------------------------------

pub struct GBLoader;

impl GBLoader {
	fn parse_header(&self, img: &[u8]) -> PlatformResult<GBHeader> {
		if img.len() < 0x150 {
			return PlatformError::invalid_image("not enough data for header".into());
		}

		let (head, body, _) = unsafe { img[0x100 .. 0x150].align_to::<UnparsedGBHeader>() };
		assert!(head.is_empty());
		let hdr = &body[0];

		hdr.check_logo()?;
		hdr.check_sgb()?;
		hdr.check_checksum(img)?;
		hdr.check_rom_ram()?;
		Ok(hdr.into())
	}

	fn parse_image(&self, img: &Image) -> PlatformResult<GBCart> {
		let header = self.parse_header(img.data())?;

		if img.len() != header.rom_size() {
			return PlatformError::invalid_image(format!(
				"image size ({}) does not match expected size ({})", img.len(), header.rom_size()));
		}

		let mut rom_banks = Vec::new();

		for i in 0 .. header.rom_num_banks {
			let offs = i * header.rom_bank_size;
			let bank = img.new_from_range(offs .. offs + header.rom_bank_size);
			rom_banks.push(bank);
		}

		Ok(GBCart{ header, rom_banks })
	}
}

impl ILoader for GBLoader {
	fn can_parse(&self, img: &Image) -> bool {
		GBLoader.parse_header(img.data()).is_ok()
	}

	fn program_from_image(&self, img: Image) -> PlatformResult<Program> {
		let cart = self.parse_image(&img)?;
		let mut segs = SegCollection::new();
		let mmu = setup_mmu(&mut segs, cart)?;

		let mem = Memory::new(
			GBArchitecture.addr_bits(),
			GBArchitecture.endianness(),
			segs,
			mmu.into()
		);

		let mut prog = Program::new(mem, GBPlatform::new().into());
		setup_gb_labels(&mut prog);
		Ok(prog)
	}
}

fn setup_mmu(segs: &mut SegCollection, cart: GBCart) -> PlatformResult<GBMmu> {
	// TODO: CGB memory map
	let vram = segs.add_segment_with_va("VRAM",  0x2000, None, VA(0x8000));
	let ram  = segs.add_segment_with_va("RAM",   0x2000, None, VA(0xC000));
	let oam  = segs.add_segment_with_va("OAM",     0xA0, None, VA(0xFE00));
	let io   = segs.add_segment_with_va("IOREG",   0x80, None, VA(0xFF00));
	let hram = segs.add_segment_with_va("HRAM",    0x7F, None, VA(0xFF80));
	let ie   = segs.add_segment_with_va("IE",         1, None, VA(0xFFFF));

	let mbc = match cart.header.mbc_type {
		MbcType::None => {
			if cart.header.rom_num_banks != 2 {
				return PlatformError::invalid_image("invalid number of ROM banks".into());
			} else if cart.header.rom_bank_size != 0x4000 {
				return PlatformError::invalid_image("invalid ROM bank size".into());
			}

			// done this way because of MOVING
			// (into_iter lets us take ownership of the values; indexing[] does not)
			let segids = cart.rom_banks
				.into_iter()
				.enumerate()
				.map(|(i, bank)| segs.add_segment_with_va(
					&format!("ROM{}", i), 0x4000, Some(bank), VA(0x4000 * i)))
				.collect::<Vec<_>>();

			// TODO: cart RAM

			NoMbc::new(segids[0], segids[1])
		}
		_ => return PlatformError::invalid_image(
			format!("unsupported MBC: {}", cart.header.mbc_type)),
	};

	Ok(GBMmu { vram, ram, oam, io, hram, ie, mbc })
}

fn setup_gb_labels(prog: &mut Program) {
	let state = prog.initial_mmu_state();

	for StdName(name, addr) in STD_NAMES {
		prog.add_name_va(name, state, VA(*addr));
	}

	// TODO: CGB names (if it's a CGB cart)
}

struct StdName(&'static str, usize);

const STD_NAMES: &[StdName] = &[
	StdName("INT_RST_00",  0x0000),
	StdName("INT_RST_08",  0x0008),
	StdName("INT_RST_10",  0x0010),
	StdName("INT_RST_18",  0x0018),
	StdName("INT_RST_20",  0x0020),
	StdName("INT_RST_28",  0x0028),
	StdName("INT_RST_30",  0x0030),
	StdName("INT_RST_38",  0x0038),
	StdName("INT_VBLANK",  0x0040),
	StdName("INT_LCDC",    0x0048),
	StdName("INT_TIMER",   0x0050),
	StdName("INT_SERIAL",  0x0058),
	StdName("INT_JOYPAD",  0x0060),
	StdName("RESET",       0x0100),
	StdName("CART_HEADER", 0x0104),
	StdName("JOYP",        0xFF00),
	StdName("SB",          0xFF01),
	StdName("DIV",         0xFF04),
	StdName("TIMA",        0xFF05),
	StdName("TMA",         0xFF06),
	StdName("TAC",         0xFF07),
	StdName("IF",          0xFF0F),
	StdName("NR10",        0xFF10),
	StdName("NR11",        0xFF11),
	StdName("NR12",        0xFF12),
	StdName("NR13",        0xFF13),
	StdName("NR14",        0xFF14),
	StdName("NR21",        0xFF16),
	StdName("NR22",        0xFF17),
	StdName("NR23",        0xFF18),
	StdName("NR24",        0xFF19),
	StdName("NR30",        0xFF1A),
	StdName("NR31",        0xFF1B),
	StdName("NR32",        0xFF1C),
	StdName("NR33",        0xFF1D),
	StdName("NR34",        0xFF1E),
	StdName("NR41",        0xFF20),
	StdName("NR42",        0xFF21),
	StdName("NR43",        0xFF22),
	StdName("NR44",        0xFF23),
	StdName("NR50",        0xFF24),
	StdName("NR51",        0xFF25),
	StdName("NR52",        0xFF26),
	StdName("WAVE_RAM",    0xFF30),
	StdName("LCDC",        0xFF40),
	StdName("STAT",        0xFF41),
	StdName("SCY",         0xFF42),
	StdName("SCX",         0xFF43),
	StdName("LY",          0xFF44),
	StdName("LYC",         0xFF45),
	StdName("DMA",         0xFF46),
	StdName("BGP",         0xFF47),
	StdName("OBP0",        0xFF48),
	StdName("OBP1",        0xFF49),
	StdName("WY",          0xFF4A),
	StdName("WX",          0xFF4B),
	StdName("IE",          0xFFFF),
];

const CGB_STD_NAMES: &[StdName] = &[
	StdName("KEY1",        0xFF4D),
	StdName("RP",          0xFF56),
	StdName("OPRI",        0xFF6C),
	StdName("SVBK",        0xFF70),
];

// ------------------------------------------------------------------------------------------------
// GBMmu
// ------------------------------------------------------------------------------------------------

#[derive(Debug)]
pub struct GBMmu {
	vram: SegId,
	ram:  SegId,
	oam:  SegId,
	io:   SegId,
	hram: SegId,
	ie:   SegId,
	mbc:  Mbc,
}

impl std::fmt::Display for GBMmu {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		write!(f, "{}", self.mbc)
	}
}

impl IMmu for GBMmu {
	fn initial_state(&self) -> MmuState {
		self.mbc.initial_state()
	}

	fn ea_for_va(&self, state: MmuState, va: VA) -> Option<EA> {
		match va.0 {
			0x0000 ..= 0x7FFF |
			0xA000 ..= 0xBFFF => self.mbc.ea_for_va(state, va),
			0x8000 ..= 0x9FFF => Some(EA::new(self.vram, va.0 & 0x1FFF)),
			0xC000 ..= 0xFDFF => Some(EA::new(self.ram,  va.0 & 0x1FFF)),
			0xFE00 ..= 0xFE9F => Some(EA::new(self.oam,  va.0 % 0xA0)),
			0xFEA0 ..= 0xFEFF => None,
			0xFF00 ..= 0xFF7F => Some(EA::new(self.io,   va.0 & 0x7F)),
			0xFF80 ..= 0xFFFE => Some(EA::new(self.hram, va.0 & 0x7F)),
			0xFFFF            => Some(EA::new(self.ie,   0)),
			_                 => panic!()
		}
	}

	fn va_for_ea(&self, state: MmuState, ea: EA) -> Option<VA> {
		match ea.seg() {
			seg if seg == self.vram => Some(VA(0x8000 + (ea.offs() & 0x1FFF))),
			seg if seg == self.ram  => Some(VA(0xC000 + (ea.offs() & 0x1FFF))),
			seg if seg == self.oam  => Some(VA(0xFE00 + (ea.offs() % 0xA0))),
			seg if seg == self.io   => Some(VA(0xFF00 + (ea.offs() & 0x7F))),
			seg if seg == self.hram => Some(VA(0xFF80 + (ea.offs() & 0x7F))),
			seg if seg == self.ie   => Some(VA(0xFFFF)),
			_                       => self.mbc.va_for_ea(state, ea),
		}
	}

	fn name_prefix_for_va(&self, state: MmuState, va: VA) -> String {
		match va.0 {
			0x0000 ..= 0x7FFF |
			0xA000 ..= 0xBFFF => self.mbc.name_prefix_for_va(state, va),
			0x8000 ..= 0x9FFF => "VRAM".into(),
			0xC000 ..= 0xDFFF => "RAM".into(),
			0xE000 ..= 0xFDFF => "RAMECHO".into(),
			0xFE00 ..= 0xFE9F => "OAM".into(),
			0xFEA0 ..= 0xFEFF => "UNDEFINED".into(),
			0xFF00 ..= 0xFF7F => "IOREG".into(),
			0xFF80 ..= 0xFFFE => "HRAM".into(),
			0xFFFF            => "IE".into(),
			_                 => panic!()
		}
	}

	fn inst_state_change(&self, state: MmuState, inst: &Instruction) -> StateChange {
		for op in inst.ops() {
			match op {
				Operand::Mem(va, acc) if acc.writes_mem() => {
					match self.mbc.state_change(state, *va) {
						StateChange::None => {},
						something         => return something,
					}
				}

				// TODO: indirect mem accesses *could* change state, but they're so
				// ridiculously common in this arch that saying "Maybe!" on every one
				// is prohibitive

				_ => {}
			}
		}

		StateChange::None
	}

	fn write(&self, old: MmuState, addr: VA, val: usize) -> MmuState {
		self.mbc.write(old, addr, val)
	}
}

// ------------------------------------------------------------------------------------------------
// MBCs
// ------------------------------------------------------------------------------------------------

#[enum_dispatch]
#[derive(Debug)]
enum Mbc {
	NoMbc,
}

impl Display for Mbc {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		match self {
			Mbc::NoMbc(m) => m.fmt(f),
		}
	}
}

#[enum_dispatch(Mbc)]
trait IMbc {
	fn initial_state(&self) -> MmuState;
	fn ea_for_va(&self, state: MmuState, va: VA) -> Option<EA>;
	fn va_for_ea(&self, state: MmuState, ea: EA) -> Option<VA>;
	fn name_prefix_for_va(&self, state: MmuState, va: VA) -> String;
	fn state_change(&self, state: MmuState, va: VA) -> StateChange;
	fn write(&self, old: MmuState, addr: VA, val: usize) -> MmuState;
}

// ---------------------------------------------------------------------------------------------
// No MBC

#[derive(Debug, Display)]
#[display("<no MBC>")]
struct NoMbc {
	rom0: SegId,
	rom1: SegId,
}

impl NoMbc {
	fn new(rom0: SegId, rom1: SegId) -> Mbc {
		Self { rom0, rom1 }.into()
	}
}

impl IMbc for NoMbc {
	fn initial_state(&self) -> MmuState {
		Default::default()
	}

	fn ea_for_va(&self, _state: MmuState, va: VA) -> Option<EA> {
		match va.0 {
			0x0000 ..= 0x3FFF => Some(EA::new(self.rom0, va.0 & 0x3FFF)),
			0x4000 ..= 0x7FFF => Some(EA::new(self.rom1, va.0 & 0x3FFF)),
			_                 => None,
		}
	}

	fn va_for_ea(&self, _state: MmuState, ea: EA) -> Option<VA> {
		match ea.seg() {
			seg if seg == self.rom0 => Some(VA(ea.offs() & 0x3FFF)),
			seg if seg == self.rom1 => Some(VA(0x4000 + (ea.offs() & 0x3FFF))),
			_                       => None,
		}
	}

	fn name_prefix_for_va(&self, _state: MmuState, va: VA) -> String {
		match va.0 {
			0x0000 ..= 0x3FFF => "ROM0".into(),
			0x4000 ..= 0x7FFF => "ROM1".into(),
			_                 => "UNK".into(),
		}
	}

	fn state_change(&self, _state: MmuState, _va: VA) -> StateChange {
		StateChange::None
	}

	fn write(&self, old: MmuState, _addr: VA, _val: usize) -> MmuState {
		// state... state never changes...
		old
	}
}