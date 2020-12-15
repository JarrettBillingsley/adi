
use std::error::Error;

use parse_display::Display;
use lazy_static::lazy_static;

use crate::memory::{ Image };
// use crate::arch::{ IArchitecture };
use crate::program::{ Program };

// ------------------------------------------------------------------------------------------------
// Sub-modules
// ------------------------------------------------------------------------------------------------

mod nes;

pub use nes::NesPlatform;

// ------------------------------------------------------------------------------------------------
// IPlatform
// ------------------------------------------------------------------------------------------------

pub trait IPlatform: Sync {
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

pub fn platform_for_image(img: &Image) -> Option<&Box<dyn IPlatform>> {
	for plat in ALL_PLATFORMS.iter() {
		if plat.can_parse(img) {
			return Some(plat);
		}
	}

	None
}

// ------------------------------------------------------------------------------------------------
// PlatformErrorKind
// ------------------------------------------------------------------------------------------------

#[derive(Debug, Display, PartialEq, Eq, Clone)]
pub enum PlatformErrorKind {
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
	pub fn invalid_image(msg: String) -> Self {
		Self { kind: PlatformErrorKind::InvalidImage { msg } }
	}
}

// ------------------------------------------------------------------------------------------------
// PlatformResult
// ------------------------------------------------------------------------------------------------

pub type PlatformResult<T> = Result<T, PlatformError>;
