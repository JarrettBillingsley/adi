use std::fmt::{ Display, Formatter, Result as FmtResult };

use crate::platform::{ IPlatform, ILoader, PlatformResult, PlatformError };
use crate::arch::{ Architecture, IArchitecture };
use crate::arch::toy::{ ToyArchitecture };
use crate::memory::{ Memory, SegCollection, VA, IMmu, MmuState, StateChange, Image, SegId,
	EA };
use crate::program::{ Program };

// ------------------------------------------------------------------------------------------------
// ToyPlatform
// ------------------------------------------------------------------------------------------------

pub struct ToyPlatform;

impl ToyPlatform {
	fn new() -> Self { Self }
}

impl IPlatform for ToyPlatform {
	fn arch(&self) -> Architecture {
		ToyArchitecture.into()
	}
}

impl Display for ToyPlatform {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		write!(f, "Toy")
	}
}

// ------------------------------------------------------------------------------------------------
// ToyLoader
// ------------------------------------------------------------------------------------------------

pub struct ToyLoader;

impl ToyLoader {
	fn parse_header(&self, img: &[u8]) -> PlatformResult<()> {
		if img.len() < 3 {
			return PlatformError::invalid_image("not enough data for header".into());
		}

		if &img[0 .. 3] == b"TOY" {
			Ok(())
		} else {
			PlatformError::invalid_image("bad header magic number".into())
		}
	}

	fn parse_image(&self, img: &Image) -> PlatformResult<Image> {
		self.parse_header(img.data())?;
		Ok(img.new_from_range(3 .. img.len()))
	}
}

impl ILoader for ToyLoader {
	fn can_parse(&self, img: &Image) -> bool {
		ToyLoader.parse_header(img.data()).is_ok()
	}

	fn program_from_image(&self, img: Image) -> PlatformResult<Program> {
		let rom_img = self.parse_image(&img)?;
		let mut segs = SegCollection::new();
		let mmu = setup_mmu(&mut segs, rom_img);

		let mem = Memory::new(
			ToyArchitecture.addr_bits(),
			ToyArchitecture.endianness(),
			segs,
			mmu.into()
		);

		let mut prog = Program::new(mem, ToyPlatform::new().into());

		let state = prog.initial_mmu_state();
		prog.add_name_va("BANK", state, VA(0xFFFF));
		prog.add_name_va("WEIRDBANK", state, VA(0xFFFE));

		Ok(prog)
	}
}

fn setup_mmu(segs: &mut SegCollection, rom_img: Image) -> ToyMmu {
	let rom = segs.add_segment_with_va("ROM", 0x8000, Some(rom_img), VA(0x0000));
	let ram = segs.add_segment_with_va("RAM", 0x8000, None,          VA(0x8000));
	ToyMmu { rom, ram }
}

// ------------------------------------------------------------------------------------------------
// ToyMmu
// ------------------------------------------------------------------------------------------------

// For now, just a very simple/stupid MMU. It has a "state" but the changing state doesn't actually
// do anything/remap memory; it's just there to ensure the state change propagation algorithm is
// working properly.
//
// The state is changed by writing to VA 0xFFFF, or by *reading* from VA 0xFFFE. Writing to 0xFFFE
// causes a "Maybe" change, just to test that part.
#[derive(Debug)]
pub struct ToyMmu {
	rom: SegId,
	ram: SegId,
}

impl Display for ToyMmu {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		write!(f, "<toy>")
	}
}

const INITIAL_STATE: u128 = 0;
const STATE_MASK: u64 = 31;
const READ_FFFE_STATE: u128 = 69420;

impl IMmu for ToyMmu {
	fn initial_state(&self) -> MmuState {
		MmuState::new(INITIAL_STATE)
	}

	fn ea_for_va(&self, _state: MmuState, va: VA) -> Option<EA> {
		match va.0 {
			0x0000 ..= 0x7FFF => Some(EA::new(self.rom,  va.0)),
			0x8000 ..= 0xFFFF => Some(EA::new(self.ram,  va.0 & 0x7FFF)),
			_                 => panic!()
		}
	}

	fn va_for_ea(&self, _state: MmuState, ea: EA) -> Option<VA> {
		match ea.seg() {
			seg if seg == self.rom => Some(VA(0x0000 + (ea.offs() & 0x7FFF))),
			seg if seg == self.ram => Some(VA(0x8000 + (ea.offs() & 0x7FFF))),
			_                      => panic!(),
		}
	}

	fn name_prefix_for_va(&self, _state: MmuState, va: VA) -> String {
		match va.0 {
			0x0000 ..= 0x7FFF => "ROM".into(),
			0x8000 ..= 0xFFFF => "RAM".into(),
			_                 => panic!()
		}
	}

	// The state is changed by writing to VA 0xFFFF, or by *reading* from VA 0xFFFE.
	fn state_change(&self, state: MmuState, va: VA, val: Option<u64>, load: bool) -> StateChange {
		if load {
			match va.0 {
				0xFFFE => state.change(MmuState::new(READ_FFFE_STATE)),
				_      => StateChange::None,
			}
		}
		else {
			match (va.0, val) {
				(0xFFFF, Some(val)) => state.change(MmuState::from_u64(val & STATE_MASK)),
				(0xFFFF, None)      => StateChange::Dynamic,
				_                   => StateChange::None,
			}
		}
	}
}