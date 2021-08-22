
use std::cell::{ RefCell };
use std::collections::{ HashMap };

use crate::program::{ BBId };
use crate::memory::{ VA, EA, MmuState, Endian };
use crate::ir::{ IrInst };

// ------------------------------------------------------------------------------------------------
// InterpCompiler
// ------------------------------------------------------------------------------------------------

/// Trait for objects which can compile BBs to IR instructions, used by the interpreter.
pub(crate) trait InterpCompiler {
	/// Given a BBId, compile it to IR and return it.
	fn compile_bb(&self, bbid: BBId) -> Vec<IrInst>;

	/// Get the size of the register "segment" needed for this IR code to work properly.
	fn regs_size(&self) -> usize;

	/// Get the endianness used by the IR code.
	fn endianness(&self) -> Endian;
}

// ------------------------------------------------------------------------------------------------
// InterpResolver
// ------------------------------------------------------------------------------------------------

/// Trait for objects which can resolve VAs to EAs, and EAs to BBIds, used by the interpreter.
/// The intention is for this to be implemented by things which want to monitor the execution
/// and log things like referenced addresses.
pub(crate) trait InterpResolver {
	/// Given a VA, resolve it to an EA if possible; otherwise return `None`.
	fn ea_for_va(&self, state: MmuState, va: VA) -> Option<EA>;

	/// Given an EA, if it refers to a BB, give its BBId; otherwise return `None`.
	fn bbid_for_ea(&self, ea: EA) -> Option<BBId>;
}

// ------------------------------------------------------------------------------------------------
// InterpMem
// ------------------------------------------------------------------------------------------------

const PAGE_SIZE: usize = 4096;

struct InterpMemPage {
	bytes: [u8; PAGE_SIZE],
}

struct InterpMem {
	pages: Vec<Box<InterpMemPage>>,
}

/*
if the EA maps to an Image segment:
	on reads, use the ImageRead methods to get the data.
	on writes... use the MMU
else if it maps to a RAM segment (??? how to determine?)
	do the lazily-created page thingggg
else
	... buh buh??
	address fault handler?
*/

// ------------------------------------------------------------------------------------------------
// IrInterp
// ------------------------------------------------------------------------------------------------

const MAX_BB_INSTRUCTIONS: usize = 5000;
type BBCache = HashMap<BBId, Vec<IrInst>>;

pub(crate) enum InterpResult {
	/// All instructions completed and no control flow was found.
	FallThru,

	/// Hit a return instruction.
	Return,

	/// Hit a call (either direct, or indirect but resolved).
	Call(EA),

	/// Hit a branch (either direct, or indirect but resolved).
	Branch(EA),

	/// Control flow to a VA that could not be resolved.
	Unk(VA),
}

/// Interprets IR instructions.
pub(crate) struct IrInterp<'c, C: InterpCompiler> {
	comp:  &'c C,
	cache: RefCell<BBCache>,
}

impl<'c, C: InterpCompiler> IrInterp<'c, C> {
	/// Constructor. Takes and maintains a reference to a compiler object used to compile
	/// BBs during interpretation.
	pub(crate) fn new(comp: &'c C) -> Self {
		Self {
			comp,
			cache: RefCell::new(BBCache::new()),
		}
	}

	/// Interpret a single BB. Any control flow out of the BB will halt interpretation.
	pub(crate) fn interp_bb(&self, bbid: BBId, resolve: &impl InterpResolver) -> InterpResult {
		self.with_compiled_bb(bbid, |insts| {
			self.interp_bb_impl(bbid, resolve, insts)
		})
	}

	fn interp_bb_impl(&self, bbid: BBId, _resolve: &impl InterpResolver, _insts: &Vec<IrInst>)
	-> InterpResult {
		log::trace!("Beginning interpretation of {:?}", bbid);

		/*
		let mut irpc = 0;
		let mut iters = 0;
		let end = insts.len();

		while irpc < end {
			iters += 1;

			if iters > MAX_BB_INSTRUCTIONS {
				panic!("BB {:?} has been running for {} iterations...", bbid, iters);
			}

			use IrInst::*;

			match insts[irpc] {
				Nop => irpc += 1,

				Assign { dst, src } => {
					self.set_val(dst, self.get_val(src));
					irpc += 1;
				}

				Load { dst, addr } => {
					irpc += 1;
				}

				Store { addr, src } => {
					irpc += 1;
				}

				IrBranch { offs } => {
					self.irbranch(&mut irpc, offs);
				}

				IrCBranch { cond, offs } => {

				}

				Branch { target } => {
					return InterpResult::Branch(target);
				}

				CBranch { cond, target } => {

				}

				IBranch { target } => {

				}

				Call { target } => {
					return InterpResult::Call(target);
				}

				ICall { target } => {

				}

				Ret { target: _ } => { // TODO: target target!
					return InterpResult::Return;
				}

				Unary { dst, op, src } => {
					irpc += 1;
				}

				Binary { dst, src1, op, src2 } => {
					irpc += 1;
				}
			}
		}*/

		InterpResult::FallThru
	}

	/*
	fn irbranch(&self, irpc: &mut usize, offs: i32, end: usize) {
		assert!(offs != 0);

		let next_irpc = *irpc as isize + offs as isize;

		if offs < 0 {
			assert!(next_irpc > 0);
		} else {
			// yes, <= because branching to end is fine, it just ends the loop
			assert!(next_irpc as usize <= end);
		}

		*irpc = next_irpc as usize;
	}
	*/

	// doin it like this because returning a reference to the contents of a RefCell is no good
	fn with_compiled_bb<T>(&self, bbid: BBId, f: impl Fn(&Vec<IrInst>) -> T) -> T {
		if !self.cache.borrow().contains_key(&bbid) {
			let insts = self.comp.compile_bb(bbid);
			self.cache.borrow_mut().insert(bbid, insts);
		}

		let cache = self.cache.borrow();
		f(cache.get(&bbid).unwrap())
	}
}