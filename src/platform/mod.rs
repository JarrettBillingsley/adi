
use std::error::Error;
use std::fmt::Display;

use parse_display::Display;
use lazy_static::lazy_static;

use crate::memory::{ Image };
use crate::program::{ Program };

// ------------------------------------------------------------------------------------------------
// Sub-modules
// ------------------------------------------------------------------------------------------------

mod nes;

pub use nes::{ NesPlatform };

// ------------------------------------------------------------------------------------------------
// IPlatform
// ------------------------------------------------------------------------------------------------

pub trait IPlatform: Display + Sync {
	fn can_parse(&self, img: &Image) -> bool;
	fn program_from_image(&self, img: Image) -> PlatformResult<Program>;
}

// ------------------------------------------------------------------------------------------------
// Functions
// ------------------------------------------------------------------------------------------------

lazy_static! {
	static ref ALL_PLATFORMS: Vec<Box<dyn IPlatform>> = {
		vec![
			Box::new(NesPlatform)
		]
	};
}

fn platform_for_image(img: &Image) -> Option<&'static Box<dyn IPlatform>> {
	for plat in ALL_PLATFORMS.iter() {
		if plat.can_parse(img) {
			return Some(plat);
		}
	}

	None
}

pub fn program_from_image(img: Image) -> PlatformResult<Program> {
	match platform_for_image(&img) {
		Some(plat) => plat.program_from_image(img),
		None       => Err(PlatformError::unknown_platform()),
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
