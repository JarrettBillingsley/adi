
use std::error::Error;
use std::fmt::Display;

use parse_display::Display;
use lazy_static::lazy_static;

use crate::arch::{ IArchitecture };
use crate::memory::{ Image, IMmu };
use crate::program::{ IProgram };

// ------------------------------------------------------------------------------------------------
// Sub-modules
// ------------------------------------------------------------------------------------------------

mod nes;

use nes::{ NesLoader };

// ------------------------------------------------------------------------------------------------
// IPlatform
// ------------------------------------------------------------------------------------------------

pub trait IPlatform: Display + Sized {
	type TMmu: IMmu;
	type TArchitecture: IArchitecture;

	fn arch(&self) -> Self::TArchitecture;
}

pub type MmuTypeOf  <Plat> = <Plat as IPlatform>::TMmu;
pub type ArchTypeOf <Plat> = <Plat as IPlatform>::TArchitecture;
pub type DisasTypeOf<Plat> = <ArchTypeOf<Plat> as IArchitecture>::TDisassembler;
pub type InstTypeOf <Plat> = <ArchTypeOf<Plat> as IArchitecture>::TInstruction;

// ------------------------------------------------------------------------------------------------
// ILoader
// ------------------------------------------------------------------------------------------------

pub trait ILoader: Sync + Send {
	fn can_parse(&self, img: &Image) -> bool;
	fn program_from_image(&self, img: Image) -> PlatformResult<Box<dyn IProgram>>;
}

lazy_static! {
	static ref ALL_LOADERS: Vec<Box<dyn ILoader>> = {
		vec![
			Box::new(NesLoader)
		]
	};
}

fn loader_for_image(img: &Image) -> Option<&'static Box<dyn ILoader>> {
	for loader in ALL_LOADERS.iter() {
		if loader.can_parse(img) {
			return Some(loader);
		}
	}

	None
}

pub fn program_from_image(img: Image) -> PlatformResult<Box<dyn IProgram>> {
	match loader_for_image(&img) {
		Some(loader) => loader.program_from_image(img),
		None         => Err(PlatformError::unknown_platform()),
	}
}

// ------------------------------------------------------------------------------------------------
// PlatformErrorKind
// ------------------------------------------------------------------------------------------------

#[derive(Debug, Display, PartialEq, Eq, Clone)]
pub enum PlatformErrorKind {
	#[display("could not determine platform automatically")]
	UnknownPlatform,
	#[display("invalid image: {msg}")]
	InvalidImage { msg: String },
}

// ------------------------------------------------------------------------------------------------
// PlatformError
// ------------------------------------------------------------------------------------------------

#[derive(Debug, Display, PartialEq, Eq, Clone)]
#[display("platform error: {kind}")]
pub struct PlatformError {
	kind: PlatformErrorKind,
}

impl Error for PlatformError {}

impl PlatformError {
	pub fn unknown_platform() -> Self {
		Self { kind: PlatformErrorKind::UnknownPlatform }
	}

	pub fn invalid_image(msg: String) -> Self {
		Self { kind: PlatformErrorKind::InvalidImage { msg } }
	}
}

// ------------------------------------------------------------------------------------------------
// PlatformResult
// ------------------------------------------------------------------------------------------------

pub type PlatformResult<T> = Result<T, PlatformError>;
