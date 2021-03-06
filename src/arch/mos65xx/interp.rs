
use crate::arch::{ IInterpreter, IPrinter, Value, ValueKind };
use crate::memory::{ Memory, ImageRead, Location, MmuState, VA };
use crate::program::{ BasicBlock, BBTerm, Instruction, MemIndir };

use super::{ AddrMode, MetaOp, SyntaxFlavor, Operand, Mos65xxPrinter, InstDesc, lookup_desc };

// ------------------------------------------------------------------------------------------------
// InterpRegs
// ------------------------------------------------------------------------------------------------

use super::Reg;

type Vu8 = Value<u8>;
type Vu16 = Value<u16>;

#[allow(non_snake_case)]
#[derive(Default)]
struct InterpRegs {
	A:  Vu8,
	X:  Vu8,
	Y:  Vu8,
	S:  u8,
	P:  u8,
	PC: u16,
}

impl InterpRegs {
	fn new() -> Self {
		let mut ret: Self = Default::default();
		ret.reset();
		ret
	}

	fn reset(&mut self) {
		self.A = Vu8::ind(0);
		self.X = Vu8::ind(0);
		self.Y = Vu8::ind(0);
		self.S = 0xFD;
		self.P = 0x24;
		self.PC = 0;
	}
}

// ------------------------------------------------------------------------------------------------
// Interpreter
// ------------------------------------------------------------------------------------------------

pub struct Mos65xxInterpreter {
	regs:      InterpRegs,
	print:     Mos65xxPrinter,
	ret_stack: Vec<Location>,
	// TODO: external PRG-RAM
	ram:       Vec<Vu8>,
	jmp_dst:   Option<Location>,
	new_state: Option<(MmuState, ValueKind)>,
}

impl Mos65xxInterpreter {
	const N: usize = 7;
	const V: usize = 6;
	const B: usize = 4;
	const D: usize = 3;
	const I: usize = 2;
	const Z: usize = 1;
	const C: usize = 0;

	pub(super) fn new() -> Self {
		let mut ret = Self {
			regs:      InterpRegs::new(),
			print:     Mos65xxPrinter::new(SyntaxFlavor::New),
			ret_stack: Vec::new(),
			ram:       vec![Vu8::ind(0); 0x800],
			jmp_dst:   None,
			new_state: None,
		};
		ret.reset();
		ret
	}

	// TODO: this is a NES memory map but this is the generic 65xx module
	fn read_byte(&self, mem: &Memory, state: MmuState, addr: u16) -> Vu8 {
		let ret = match addr {
			0x0000 ..= 0x07FF => {
				let val = self.ram[addr as usize];

				match val.kind {
					ValueKind::Indeterminate | ValueKind::Loaded(..) => val.val,
					_ => return val,
				}
			}
			0x2002 => 0xFF,
			_ => {
				if let Some(loc) = mem.loc_for_va(state, VA(addr.into())) {
					let seg = mem.segment_from_loc(loc);

					if seg.is_real() {
						seg.read_u8(loc)
					} else {
						0
					}
				} else {
					0
				}
			}
		};

		Vu8::load(ret, Some(VA(addr as usize)))
	}

	fn write_byte(&mut self, mem: &Memory, state: MmuState, addr: u16, val: Vu8) {
		match addr {
			0x0000 ..= 0x07FF => self.ram[addr as usize] = val,
			0x0800 ..= 0x401F => {} // ignore
			0x4020 ..= 0xFFFF => {
				let new_state = mem.mmu_write(state, VA(addr as usize), val.val as usize);

				if new_state != state {
					assert!(self.new_state.is_none());
					self.new_state = Some((new_state, val.kind))
				} else {
					log::warn!("wuh oh. old {:?} new {:?} {:04X} {:02X}",
						state, new_state, addr, val.val);
				}
			}
		}
	}

	fn op_addr(&self, i: &Instruction) -> u16 {
		match i.ops.first() {
			Some(Operand::Mem(a, ..)) => *a as u16,
			Some(Operand::Indir(MemIndir::RegDisp { disp, .. }, ..)) => *disp as u16,
			_ => panic!("bad operands: {:?}", i.ops),
		}
	}

	fn get_addr(&self, desc: InstDesc, state: MmuState, mem: &Memory, i: &Instruction) -> u16 {
		use AddrMode::*;

		match desc.addr_mode {
			IMP | REL | LAB => 0, // not used, doesn't matter

			IMM       => (i.va().0 as u16).wrapping_add(1),
			ZPG | ABS => self.op_addr(i),
			ZPX       => self.op_addr(i).wrapping_add(self.regs.X.val.into()) & 0xFF,
			ZPY       => self.op_addr(i).wrapping_add(self.regs.Y.val.into()) & 0xFF,
			ABX       => self.op_addr(i).wrapping_add(self.regs.X.val.into()),
			ABY       => self.op_addr(i).wrapping_add(self.regs.Y.val.into()),

			IND => {
				let ptr_addr = self.op_addr(i);
				let lo = self.read_byte(mem, state, ptr_addr).val as u16;

				let hi = if ptr_addr & 0xFF == 0xFF {
					// bug!
					self.read_byte(mem, state, ptr_addr & 0xFF00).val as u16
				} else {
					self.read_byte(mem, state, ptr_addr + 1).val as u16
				};

				(hi << 8) | lo
			}
			IZX => { // [[{} + x]]
				let ptr_addr = self.op_addr(i).wrapping_add(self.regs.X.val.into());
				let lo = self.read_byte(mem, state, ptr_addr).val as u16;
				let hi = self.read_byte(mem, state, ptr_addr.wrapping_add(1) & 0xFF).val as u16;
				(hi << 8) | lo
			}
			IZY => { // [[{}] + y]
				let ptr_addr = self.op_addr(i);
				let lo = self.read_byte(mem, state, ptr_addr).val as u16;
				let hi = self.read_byte(mem, state, ptr_addr.wrapping_add(1) & 0xFF).val as u16;
				((hi << 8) | lo).wrapping_add(self.regs.Y.val.into())
			}
		}
	}

	fn get_flag(&self, flag: usize) -> u8 {
		if (self.regs.P & (1 << flag)) != 0 { 1 } else { 0 }
	}

	fn set_flag(&mut self, flag: usize, f: bool) {
		if f {
			self.regs.P |= 1 << flag;
		} else {
			self.regs.P &= !(1 << flag);
		}
	}

	fn set_flags_zn(&mut self, val: u8) {
		self.set_flag(Self::Z, val == 0x00);
		self.set_flag(Self::N, val & 0x80 == 0x80);
	}

	fn push(&mut self, mem: &Memory, state: MmuState, val: Vu8) {
		self.write_byte(mem, state, 0x0100 | (self.regs.S as u16), val);
		self.regs.S = self.regs.S.wrapping_sub(1);
	}

	fn pop(&mut self, mem: &Memory, state: MmuState) -> Vu8 {
		self.regs.S = self.regs.S.wrapping_add(1);
		self.read_byte(mem, state, 0x0100 | (self.regs.S as u16))
	}

	fn pop_n(&mut self, mem: &Memory, state: MmuState, n: usize) {
		for _ in 0 .. n {
			self.pop(mem, state);
		}
	}

	fn interpret_inst(&mut self, state: MmuState, mem: &Memory, i: &Instruction) {
		let desc = lookup_desc(i.bytes[0]);
		let addr = self.get_addr(desc, state, mem, i);
		let inst_display = self.print.fmt_instr(i, state, &crate::arch::NullLookup);

		log::info!("[A={:16} X={:16} Y={:16} S={:02X} P={:08b}] {:04X} {}",
			format!("{:?}", self.regs.A), format!("{:?}", self.regs.X), format!("{:?}", self.regs.Y),
			self.regs.S, self.regs.P, i.va(), inst_display);

		use MetaOp::*;
		match desc.meta_op {
			BRK => { todo!("BRK instruction @ {} (VA: {:04X})", i.loc(), i.va()) }
			UNK => { todo!("unknown instruction @ {} (VA: {:04X})", i.loc(), i.va()) }
			NOP | DOP => {}
			// handled by interpret_branch.
			BCC | BCS | BEQ | BMI | BNE | BPL | BVC | BVS => {}
			// handled (partly) by interpret_bb.
			JMP => {
				if desc.addr_mode == AddrMode::IND {
					self.jmp_dst = Some(mem.loc_from_va(state, VA(addr as usize)));
				}
			}
			RTI => self.pop_n(mem, state, 3),

			JSR => {
				// yes, really, -1
				let ret = i.next_va().0 - 1;
				// TODO: not really indeterminate
				self.push(mem, state, Vu8::ind((ret >> 8) as u8));
				self.push(mem, state, Vu8::ind((ret & 0xFF) as u8));
			}
			RTS => {
				let lo = self.pop(mem, state).val as usize;
				let hi = self.pop(mem, state).val as usize;
				// yes, really, +1
				self.jmp_dst = Some(mem.loc_from_va(state, VA(((hi << 8) | lo) + 1)));
			}
			PHA => self.push(mem, state, self.regs.A),
			PHP => self.push(mem, state, Vu8::ind(self.regs.P)), // TODO: not really indeterminate
			PLA => { self.regs.A = self.pop(mem, state); self.set_flags_zn(self.regs.A.val); }
			PLP => self.regs.P = self.pop(mem, state).val,

			ADC => {
				let data = self.read_byte(mem, state, addr).val;
				let a = self.regs.A.val;
				let (x1, o1) = data.overflowing_add(a);
				let (x2, o2) = x1.overflowing_add(self.get_flag(Self::C));
				self.regs.A = Vu8::comp(x2);
				self.set_flag(Self::C, o1 | o2);
				self.set_flag(Self::V, (a ^ data) & 0x80 == 0 && (a ^ self.regs.A.val) & 0x80 != 0);
				self.set_flags_zn(self.regs.A.val);
			}
			SBC => {
				let data = self.read_byte(mem, state, addr).val;
				let a = self.regs.A.val;
				let (x1, o1) = a.overflowing_sub(data);
				let (x2, o2) = x1.overflowing_sub(1 - self.get_flag(Self::C));
				self.regs.A = Vu8::comp(x2);
				self.set_flag(Self::C, !(o1 | o2));
				self.set_flag(Self::V, (a ^ data) & 0x80 != 0 && (a ^ self.regs.A.val) & 0x80 != 0);
				self.set_flags_zn(self.regs.A.val);
			}
			CMP => {
				let data = self.read_byte(mem, state, addr).val;
				self.set_flags_zn(self.regs.A.val.wrapping_sub(data));
				self.set_flag(Self::C, self.regs.A.val >= data);
			}
			CPX => {
				let data = self.read_byte(mem, state, addr).val;
				self.set_flags_zn(self.regs.X.val.wrapping_sub(data));
				self.set_flag(Self::C, self.regs.X.val >= data);
			}
			CPY => {
				let data = self.read_byte(mem, state, addr).val;
				self.set_flags_zn(self.regs.Y.val.wrapping_sub(data));
				self.set_flag(Self::C, self.regs.Y.val >= data);
			}

			INC => {
				let data = self.read_byte(mem, state, addr).val;
				let val = Vu8::comp(data.wrapping_add(1));
				self.set_flags_zn(val.val);
				self.write_byte(mem, state, addr, val);
			}
			DEC => {
				let data = self.read_byte(mem, state, addr).val;
				let val = Vu8::comp(data.wrapping_sub(1));
				self.set_flags_zn(val.val);
				self.write_byte(mem, state, addr, val);
			}
			INX => {
				self.regs.X = Vu8::comp(self.regs.X.val.wrapping_add(1));
				self.set_flags_zn(self.regs.X.val);
			}
			INY => {
				self.regs.Y = Vu8::comp(self.regs.Y.val.wrapping_add(1));
				self.set_flags_zn(self.regs.Y.val);
			}
			DEX => {
				self.regs.X = Vu8::comp(self.regs.X.val.wrapping_sub(1));
				self.set_flags_zn(self.regs.X.val);
			}
			DEY => {
				self.regs.Y = Vu8::comp(self.regs.Y.val.wrapping_sub(1));
				self.set_flags_zn(self.regs.Y.val);
			}

			AND => {
				let data = self.read_byte(mem, state, addr).val;
				self.regs.A = Vu8::comp(self.regs.A.val & data);
				self.set_flags_zn(self.regs.A.val);
			}
			EOR => {
				let data = self.read_byte(mem, state, addr).val;
				self.regs.A = Vu8::comp(self.regs.A.val ^ data);
				self.set_flags_zn(self.regs.A.val);
			}
			ORA => {
				let data = self.read_byte(mem, state, addr).val;
				self.regs.A = Vu8::comp(self.regs.A.val | data);
				self.set_flags_zn(self.regs.A.val);
			}

			BIT => {
				let data = self.read_byte(mem, state, addr).val;
				let val = self.regs.A.val & data;
				self.set_flag(Self::Z, val == 0);
				self.set_flag(Self::N, (data & (1 << 7)) != 0);
				self.set_flag(Self::V, (data & (1 << 6)) != 0);
			}
			ASLA | ASL => {
				let data = if desc.meta_op == ASL {
					self.read_byte(mem, state, addr).val
				} else {
					self.regs.A.val
				};

				self.set_flag(Self::C, ((data >> 7) & 1) != 0);
				let val = Vu8::comp(data.wrapping_shl(1));
				self.set_flags_zn(val.val);

				if desc.meta_op == ASL {
					self.write_byte(mem, state, addr, val);
				} else {
					self.regs.A = val;
				}
			}
			LSRA | LSR => {
				let data = if desc.meta_op == LSR {
					self.read_byte(mem, state, addr).val
				} else {
					self.regs.A.val
				};

				self.set_flag(Self::C, ((data >> 7) & 1) != 0);
				let val = Vu8::comp(data.wrapping_shr(1));
				self.set_flags_zn(val.val);

				if desc.meta_op == LSR {
					self.write_byte(mem, state, addr, val);
				} else {
					self.regs.A = val;
				}
			}
			ROLA | ROL => {
				let data = if desc.meta_op == ROL {
					self.read_byte(mem, state, addr).val
				} else {
					self.regs.A.val
				};

				let old_c = self.get_flag(Self::C);
				self.set_flag(Self::C, ((data >> 7) & 1) != 0);
				let val = Vu8::comp((data << 1) | old_c);
				self.set_flags_zn(val.val);

				if desc.meta_op == ROL {
					self.write_byte(mem, state, addr, val);
				} else {
					self.regs.A = val;
				}
			}
			RORA | ROR => {
				let data = if desc.meta_op == ROR {
					self.read_byte(mem, state, addr).val
				} else {
					self.regs.A.val
				};

				let mut val = data.rotate_right(1);
				if self.get_flag(Self::C) == 1 {
				    val |= 1 << 7;
				} else {
				    val &= !(1 << 7);
				}
				let val = Vu8::comp(val);
				self.set_flag(Self::C, (data & 1) > 0);
				self.set_flags_zn(val.val);

				if desc.meta_op == ROR {
					self.write_byte(mem, state, addr, val);
				} else {
					self.regs.A = val;
				}
			}

			CLC => self.set_flag(Self::C, false),
			CLD => self.set_flag(Self::D, false),
			CLI => self.set_flag(Self::I, false),
			CLV => self.set_flag(Self::V, false),
			SEC => self.set_flag(Self::C, true),
			SED => self.set_flag(Self::D, true),
			SEI => self.set_flag(Self::I, true),

			LDA | LDAI => {
				let data = self.read_byte(mem, state, addr);

				if desc.meta_op == LDAI {
					self.regs.A = Vu8::imm(data.val);
				} else {
					self.regs.A = data;
				}

				self.set_flags_zn(self.regs.A.val);
			}
			LDX | LDXI => {
				let data = self.read_byte(mem, state, addr);

				if desc.meta_op == LDXI {
					self.regs.X = Vu8::imm(data.val);
				} else {
					self.regs.X = data;
				}

				self.set_flags_zn(self.regs.X.val);
			}
			LDY | LDYI => {
				let data = self.read_byte(mem, state, addr);

				if desc.meta_op == LDYI {
					self.regs.Y = Vu8::imm(data.val);
				} else {
					self.regs.Y = data;
				}

				self.set_flags_zn(self.regs.Y.val);
			}

			STA => self.write_byte(mem, state, addr, self.regs.A),
			STX => self.write_byte(mem, state, addr, self.regs.X),
			STY => self.write_byte(mem, state, addr, self.regs.Y),
			TAX => { self.regs.X = self.regs.A; self.set_flags_zn(self.regs.X.val); }
			TXA => { self.regs.A = self.regs.X; self.set_flags_zn(self.regs.A.val); }
			TAY => { self.regs.Y = self.regs.A; self.set_flags_zn(self.regs.Y.val); }
			TYA => { self.regs.A = self.regs.Y; self.set_flags_zn(self.regs.A.val); }
			TSX => { self.regs.X = Vu8::ind(self.regs.S); self.set_flags_zn(self.regs.X.val); }
			TXS => { self.regs.S = self.regs.X.val; self.set_flags_zn(self.regs.S); }
		}
	}

	fn should_branch(&mut self, i: &Instruction) -> bool {
		use MetaOp::*;
		let desc = lookup_desc(i.bytes[0]);
		match desc.meta_op {
			BCC => self.get_flag(Self::C) == 0,
			BCS => self.get_flag(Self::C) == 1,
			BNE => self.get_flag(Self::Z) == 0,
			BEQ => self.get_flag(Self::Z) == 1,
			BPL => self.get_flag(Self::N) == 0,
			BMI => self.get_flag(Self::N) == 1,
			BVC => self.get_flag(Self::V) == 0,
			BVS => self.get_flag(Self::V) == 1,
			_   => unreachable!(),
		}
	}
}

impl IInterpreter for Mos65xxInterpreter {
	fn reset(&mut self) {
		self.regs.reset();
		self.ret_stack.clear();
		self.ram.iter_mut().for_each(|x| *x = Vu8::ind(0));
		self.jmp_dst = None;
		self.new_state = None;
	}

	fn interpret_bb(&mut self, mem: &Memory, bb: &BasicBlock, state: Option<MmuState>)
	-> Option<Location> {
		let insts = bb.insts();
		let last = insts.len() - 1;
		let state = state.unwrap_or(bb.mmu_state());

		self.new_state = None;

		for i in insts {
			self.interpret_inst(state, mem, i);
		}

		use BBTerm::*;
		// then the terminator
		match bb.term() {
			DeadEnd | Halt          => None,
			FallThru(to) | Jump(to) | BankChange(to) => Some(*to),

			Return => {
				if let Some(expected) = self.ret_stack.pop()	{
					self.regs.A.kind = ValueKind::Return;
					self.regs.X.kind = ValueKind::Return;
					self.regs.Y.kind = ValueKind::Return;

					let jmp_dst = self.jmp_dst.expect("return instr has to set this");

					if jmp_dst == expected {
						Some(expected)
					} else {
						log::warn!("ret addr shenanigans at {:04X} (expected {}, found {})",
							insts[last].va(), expected, jmp_dst);
						None
					}
				} else {
					None
				}
			}

			Call { dst, ret } => {
				self.regs.A.kind = ValueKind::Argument(Reg::A as usize);
				self.regs.X.kind = ValueKind::Argument(Reg::X as usize);
				self.regs.Y.kind = ValueKind::Argument(Reg::Y as usize);
				self.ret_stack.push(*ret);
				Some(*dst)
			}

			Cond { t, f } => {
				if self.should_branch(&insts[last]) {
					Some(*t)
				} else {
					Some(*f)
				}
			}

			JumpTbl(_targets) => {
				// TODO: check that jmp_dst is in the targets
				log::warn!("halfassed jumptable implementation");
				self.jmp_dst
			}
		}
	}

	fn last_mmu_state_change(&self) -> Option<(MmuState, ValueKind)> {
		self.new_state
	}
}
