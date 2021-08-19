
use std::error::Error;
use std::fmt::Display;

use parse_display::Display;
use lazy_static::lazy_static;
use enum_dispatch::enum_dispatch;

use crate::arch::{ Architecture };
use crate::memory::{ Image };
use crate::program::{ Program };

// ------------------------------------------------------------------------------------------------
// Sub-modules
// ------------------------------------------------------------------------------------------------

mod nes;
mod gb;
mod toy;

pub use gb::{ GBMmu };
pub use nes::{ NesMmu };
pub use toy::{ ToyMmu };

// ------------------------------------------------------------------------------------------------
// IPlatform
// ------------------------------------------------------------------------------------------------

use gb::{ GBPlatform };
use nes::{ NesPlatform };
use toy::{ ToyPlatform };

#[enum_dispatch]
#[derive(Display)]
pub enum Platform {
	#[display("{0}")]
	GBPlatform,
	#[display("{0}")]
	NesPlatform,
	#[display("{0}")]
	ToyPlatform,
}

#[enum_dispatch(Platform)]
pub trait IPlatform: Display + Sized {
	fn arch(&self) -> Architecture;
}

// ------------------------------------------------------------------------------------------------
// ILoader
// ------------------------------------------------------------------------------------------------

use gb::{ GBLoader };
use nes::{ NesLoader };
use toy::{ ToyLoader };

#[enum_dispatch]
pub enum Loader {
	GBLoader,
	NesLoader,
	ToyLoader,
}

#[enum_dispatch(Loader)]
pub trait ILoader: Sync + Send {
	fn can_parse(&self, img: &Image) -> bool;
	fn program_from_image(&self, img: Image) -> PlatformResult<Program>;
}

lazy_static! {
	static ref ALL_LOADERS: Vec<Loader> = {
		vec![
			GBLoader.into(),
			NesLoader.into(),
			ToyLoader.into(),
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
