
use std::convert::{ TryFrom };
use std::ops::{ Add, AddAssign, Sub, SubAssign };
use std::fmt::{ Debug, UpperHex, Formatter, Result as FmtResult };

use parse_display::Display;

use crate::memory::{ EA };

// ------------------------------------------------------------------------------------------------
// VA
// ------------------------------------------------------------------------------------------------

/// newtype for virtual addresses.
#[derive(Debug, Display, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct VA(pub usize);

impl TryFrom<EA> for VA {
	type Error = ();

	fn try_from(ea: EA) -> Result<VA, ()> {
		if ea.is_unresolved() {
			Ok(VA(ea.offs()))
		} else {
			Err(())
		}
	}
}

impl UpperHex for VA {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		UpperHex::fmt(&self.0, f)
	}
}

impl Add<usize> for VA {
	type Output = Self;
	#[inline] fn add(self, other: usize) -> Self {
		VA(self.0 + other)
	}
}

impl Add<isize> for VA {
	type Output = Self;
	#[inline] fn add(self, other: isize) -> Self {
		VA(((self.0 as isize) + other) as usize)
	}
}

impl AddAssign<usize> for VA {
	#[inline] fn add_assign(&mut self, other: usize) {
		self.0 += other;
	}
}

impl Sub<VA> for VA {
	type Output = usize;
	#[inline] fn sub(self, other: Self) -> usize {
		self.0 - other.0
	}
}

impl Sub<usize> for VA {
	type Output = Self;
	#[inline] fn sub(self, other: usize) -> Self {
		VA(self.0 - other)
	}
}

impl SubAssign<usize> for VA {
	#[inline] fn sub_assign(&mut self, other: usize) {
		self.0 -= other;
	}
}
