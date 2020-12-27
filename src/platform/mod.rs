
use std::error::Error;
use std::fmt::Display;

use parse_display::Display;
use lazy_static::lazy_static;
use enum_dispatch::enum_dispatch;

use crate::arch::{ IArchitecture };
use crate::memory::{ Image };
use crate::program::{ Program };

// ------------------------------------------------------------------------------------------------
// Sub-modules
// ------------------------------------------------------------------------------------------------

mod nes;

use nes::{ NesLoader };
pub use nes::{ NesPlatform, NesMmu };

// ------------------------------------------------------------------------------------------------
// IPlatform
// ------------------------------------------------------------------------------------------------

pub trait IPlatform: Display + Sized {
	type TArchitecture: IArchitecture;

	fn arch(&self) -> Self::TArchitecture;
}

pub type ArchTypeOf <Plat> = <Plat as IPlatform>::TArchitecture;
pub type DisasTypeOf<Plat> = <ArchTypeOf<Plat> as IArchitecture>::TDisassembler;

// ------------------------------------------------------------------------------------------------
// ILoader
// ------------------------------------------------------------------------------------------------

#[enum_dispatch(Loader)]
pub trait ILoader: Sync + Send {
	fn can_parse(&self, img: &Image) -> bool;
	fn program_from_image(&self, img: Image) -> PlatformResult<Program>;
}

#[enum_dispatch]
pub enum Loader {
	NesLoader
}

lazy_static! {
	static ref ALL_LOADERS: Vec<Loader> = {
		vec![
			NesLoader.into(),
		]
	};
}

pub fn program_from_image(img: Image) -> PlatformResult<Program> {
	for loader in ALL_LOADERS.iter() {
		if loader.can_parse(&img) {
			return loader.program_from_image(img);
		}
	}

	PlatformError::unknown_platform()
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
	pub fn unknown_platform<T>() -> PlatformResult<T> {
		Err(Self { kind: PlatformErrorKind::UnknownPlatform })
	}

	pub fn invalid_image<T>(msg: String) -> PlatformResult<T> {
		Err(Self { kind: PlatformErrorKind::InvalidImage { msg } })
	}
}

// ------------------------------------------------------------------------------------------------
// PlatformResult
// ------------------------------------------------------------------------------------------------

pub type PlatformResult<T> = Result<T, PlatformError>;
