use crate::memory::{ IMemory, ImageRead };
use crate::program::{ BasicBlock, BBTerm };

use super::*;

// ------------------------------------------------------------------------------------------------
// InterpRegs
// ------------------------------------------------------------------------------------------------

#[allow(non_snake_case)]
struct InterpRegs {
	A:  u8,
	X:  u8,
	Y:  u8,
	S:  u8,
	P:  u8,
	PC: u16,
}

impl InterpRegs {
	fn new() -> Self {
		Self { A: 0, X: 0, Y: 0, S: 0xFD, P: 0x24, PC: 0 }
	}

	fn reset(&mut self) {
		self.A = 0;
		self.X = 0;
		self.Y = 0;
		self.S = 0xFD;
		self.P = 0x24;
		self.PC = 0;
	}
}

// ------------------------------------------------------------------------------------------------
// Interpreter
// ------------------------------------------------------------------------------------------------

pub struct Interpreter {
	regs:      InterpRegs,
	print:     Printer,
	ret_stack: Vec<Location>,
	// TODO: external PRG-RAM
	ram:       Vec<u8>,
	jmp_dst:   Location,
}

impl Interpreter {
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
			print:     Printer::new(SyntaxFlavor::New),
			ret_stack: Vec::new(),
			ram:       vec![0; 0x800],
			jmp_dst:   Location::invalid(0),
		};
		ret.reset();
		ret
	}

	// TODO: this is a NES memory map but this is the generic 65xx module
	fn read_byte(&self, mem: &dyn IMemory, state: MmuState, addr: u16) -> u8 {
		match addr {
			0x0000 ..= 0x07FF => self.ram[addr as usize],
			0x2002 => 0xFF,
			_ => {
				if let Some(loc) = mem.loc_for_va(state, VA(addr.into())) {
					let seg = mem.segment_from_loc(loc);

					if seg.is_real() {
						return seg.read_u8(loc);
					}
				}

				0
			}
		}
	}

	fn write_byte(&mut self, _mem: &dyn IMemory, _state: MmuState, addr: u16, val: u8) {
		// TODO: *THIS* is where we detect bank changes!!
		match addr {
			0x0000 ..= 0x07FF => self.ram[addr as usize] = val,
			_ => {}
		}
	}

	fn op_addr(&self, i: &Instruction) -> u16 {
		if let Some(Operand::Mem16(a, ..)) = i.ops.first() { *a } else { panic!() }
	}

	fn get_addr(&self, desc: InstDesc, state: MmuState, mem: &dyn IMemory, i: &Instruction) -> u16 {
		use AddrMode::*;

		match desc.addr_mode {
			IMP | REL | LAB => 0, // not used, doesn't matter

			IMM       => (i.va().0 as u16).wrapping_add(1),
			ZPG | ABS => self.op_addr(i),
			ZPX       => self.op_addr(i).wrapping_add(self.regs.X.into()) & 0xFF,
			ZPY       => self.op_addr(i).wrapping_add(self.regs.Y.into()) & 0xFF,
			ABX       => self.op_addr(i).wrapping_add(self.regs.X.into()),
			ABY       => self.op_addr(i).wrapping_add(self.regs.Y.into()),

			IND => {
				let ptr_addr = self.op_addr(i);
				let lo = self.read_byte(mem, state, ptr_addr) as u16;

				let hi = if ptr_addr & 0xFF == 0xFF {
					// bug!
					self.read_byte(mem, state, ptr_addr & 0xFF00) as u16
				} else {
					self.read_byte(mem, state, ptr_addr + 1) as u16
				};

				(hi << 8) | lo
			}
			IZX => { // [[{} + x]]
				let ptr_addr = self.op_addr(i).wrapping_add(self.regs.X.into());
				let lo = self.read_byte(mem, state, ptr_addr) as u16;
				let hi = self.read_byte(mem, state, ptr_addr.wrapping_add(1) & 0xFF) as u16;
				(hi << 8) | lo
			}
			IZY => { // [[{}] + y]
				let ptr_addr = self.op_addr(i);
				let lo = self.read_byte(mem, state, ptr_addr) as u16;
				let hi = self.read_byte(mem, state, ptr_addr.wrapping_add(1) & 0xFF) as u16;
				((hi << 8) | lo).wrapping_add(self.regs.Y.into())
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

	fn push(&mut self, mem: &dyn IMemory, state: MmuState, val: u8) {
		self.write_byte(mem, state, 0x0100 | (self.regs.S as u16), val);
		self.regs.S = self.regs.S.wrapping_sub(1);
	}

	fn pop(&mut self, mem: &dyn IMemory, state: MmuState) -> u8 {
		self.regs.S = self.regs.S.wrapping_add(1);
		self.read_byte(mem, state, 0x0100 | (self.regs.S as u16))
	}

	fn pop_n(&mut self, mem: &dyn IMemory, state: MmuState, n: usize) {
		for _ in 0 .. n {
			self.pop(mem, state);
		}
	}

	fn interpret_inst(&mut self, state: MmuState, mem: &dyn IMemory, i: &Instruction) {
		let desc = lookup_desc(i.bytes[0]);
		let addr = self.get_addr(desc, state, mem, i);
		let inst_display = self.print.fmt_instr(i, state, &crate::disasm::NullLookup);

		log::info!("[A={:02X} X={:02X} Y={:02X} S={:02X} P={:08b}] {:04X} {}",
			self.regs.A, self.regs.X, self.regs.Y, self.regs.S,
			self.regs.P,
			i.va(), inst_display);

		use MetaOp::*;
		match desc.meta_op {
			BRK => { todo!("BRK instruction @ {} (VA: {:04X})", i.loc(), i.va()) }
			UNK => { todo!("unknown instruction @ {} (VA: {:04X})", i.loc(), i.va()) }
			NOP => {}
			// handled by interpret_branch.
			BCC | BCS | BEQ | BMI | BNE | BPL | BVC | BVS => {}
			// handled (partly) by interpret_bb.
			JMP => {
				if desc.addr_mode == AddrMode::IND {
					self.jmp_dst = mem.loc_from_va(state, VA(addr as usize));
				}
			}
			RTI => self.pop_n(mem, state, 3),

			JSR => {
				// yes, really, -1
				let ret = i.next_va().0 - 1;
				self.push(mem, state, (ret >> 8) as u8);
				self.push(mem, state, (ret & 0xFF) as u8);
			}
			RTS => {
				let lo = self.pop(mem, state) as usize;
				let hi = self.pop(mem, state) as usize;
				// yes, really, +1
				self.jmp_dst = mem.loc_from_va(state, VA(((hi << 8) | lo) + 1));
			}
			PHA => self.push(mem, state, self.regs.A),
			PHP => self.push(mem, state, self.regs.P),
			PLA => { self.regs.A = self.pop(mem, state); self.set_flags_zn(self.regs.A); }
			PLP => self.regs.P = self.pop(mem, state),

			ADC => {
				let data = self.read_byte(mem, state, addr);
				let a = self.regs.A;
				let (x1, o1) = data.overflowing_add(a);
				let (x2, o2) = x1.overflowing_add(self.get_flag(Self::C));
				self.regs.A = x2;
				self.set_flag(Self::C, o1 | o2);
				self.set_flag(Self::V, (a ^ data) & 0x80 == 0 && (a ^ self.regs.A) & 0x80 != 0);
				self.set_flags_zn(self.regs.A);
			}
			SBC => {
				let data = self.read_byte(mem, state, addr);
				let a = self.regs.A;
				let (x1, o1) = a.overflowing_sub(data);
				let (x2, o2) = x1.overflowing_sub(1 - self.get_flag(Self::C));
				self.regs.A = x2;
				self.set_flag(Self::C, !(o1 | o2));
				self.set_flag(Self::V, (a ^ data) & 0x80 != 0 && (a ^ self.regs.A) & 0x80 != 0);
				self.set_flags_zn(self.regs.A);
			}
			CMP => {
				let data = self.read_byte(mem, state, addr);
				self.set_flags_zn(self.regs.A.wrapping_sub(data));
				self.set_flag(Self::C, self.regs.A >= data);
			}
			CPX => {
				let data = self.read_byte(mem, state, addr);
				self.set_flags_zn(self.regs.X.wrapping_sub(data));
				self.set_flag(Self::C, self.regs.X >= data);
			}
			CPY => {
				let data = self.read_byte(mem, state, addr);
				self.set_flags_zn(self.regs.Y.wrapping_sub(data));
				self.set_flag(Self::C, self.regs.Y >= data);
			}

			INC => {
				let data = self.read_byte(mem, state, addr);
				let val = data.wrapping_add(1);
				self.set_flags_zn(val);
				self.write_byte(mem, state, addr, val);
			}
			DEC => {
				let data = self.read_byte(mem, state, addr);
				let val = data.wrapping_sub(1);
				self.set_flags_zn(val);
				self.write_byte(mem, state, addr, val);
			}
			INX => { self.regs.X = self.regs.X.wrapping_add(1); self.set_flags_zn(self.regs.X); }
			INY => { self.regs.Y = self.regs.Y.wrapping_add(1); self.set_flags_zn(self.regs.Y); }
			DEX => { self.regs.X = self.regs.X.wrapping_sub(1); self.set_flags_zn(self.regs.X); }
			DEY => { self.regs.Y = self.regs.Y.wrapping_sub(1); self.set_flags_zn(self.regs.Y); }

			AND => {
				let data = self.read_byte(mem, state, addr);
				self.regs.A &= data;
				self.set_flags_zn(self.regs.A);
			}
			EOR => {
				let data = self.read_byte(mem, state, addr);
				self.regs.A ^= data;
				self.set_flags_zn(self.regs.A);
			}
			ORA => {
				let data = self.read_byte(mem, state, addr);
				self.regs.A |= data;
				self.set_flags_zn(self.regs.A);
			}

			BIT => {
				let data = self.read_byte(mem, state, addr);
				let val = self.regs.A & data;
				self.set_flag(Self::Z, val == 0);
				self.set_flag(Self::N, (data & (1 << 7)) != 0);
				self.set_flag(Self::V, (data & (1 << 6)) != 0);
			}
			ASLA | ASL => {
				let data = self.read_byte(mem, state, addr);
				self.set_flag(Self::C, ((data >> 7) & 1) != 0);
				let val = data.wrapping_shl(1);
				self.set_flags_zn(val);

				if desc.meta_op == ASL {
					self.write_byte(mem, state, addr, val);
				}
			}
			LSRA | LSR => {
				let data = self.read_byte(mem, state, addr);
				self.set_flag(Self::C, ((data >> 7) & 1) != 0);
				let val = data.wrapping_shr(1);
				self.set_flags_zn(val);

				if desc.meta_op == LSR {
					self.write_byte(mem, state, addr, val);
				}
			}
			ROLA | ROL => {
				let data = self.read_byte(mem, state, addr);
				let old_c = self.get_flag(Self::C);
				self.set_flag(Self::C, ((data >> 7) & 1) != 0);
				let val = (data << 1) | old_c;
				self.set_flags_zn(val);

				if desc.meta_op == ROL {
					self.write_byte(mem, state, addr, val);
				}
			}
			RORA | ROR => {
				let data = self.read_byte(mem, state, addr);
				let mut ret = data.rotate_right(1);
				if self.get_flag(Self::C) == 1 {
				    ret |= 1 << 7;
				} else {
				    ret &= !(1 << 7);
				}
				self.set_flag(Self::C, (data & 1) > 0);
				self.set_flags_zn(ret);

				if desc.meta_op == ROR {
					self.write_byte(mem, state, addr, ret);
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
				self.regs.A = data;
				self.set_flags_zn(self.regs.A);
			}
			LDX | LDXI => {
				let data = self.read_byte(mem, state, addr);
				self.regs.X = data;
				self.set_flags_zn(self.regs.X);
			}
			LDY | LDYI => {
				let data = self.read_byte(mem, state, addr);
				self.regs.Y = data;
				self.set_flags_zn(self.regs.Y);
			}

			STA => self.write_byte(mem, state, addr, self.regs.A),
			STX => self.write_byte(mem, state, addr, self.regs.X),
			STY => self.write_byte(mem, state, addr, self.regs.Y),
			TAX => { self.regs.X = self.regs.A; self.set_flags_zn(self.regs.X); }
			TAY => { self.regs.Y = self.regs.A; self.set_flags_zn(self.regs.Y); }
			TSX => { self.regs.X = self.regs.S; self.set_flags_zn(self.regs.X); }
			TXA => { self.regs.A = self.regs.X; self.set_flags_zn(self.regs.A); }
			TXS => { self.regs.S = self.regs.X; self.set_flags_zn(self.regs.S); }
			TYA => { self.regs.A = self.regs.Y; self.set_flags_zn(self.regs.A); }
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

impl IInterpreter for Interpreter {
	fn reset(&mut self) {
		self.regs.reset();
		self.ret_stack.clear();
		self.ram.iter_mut().for_each(|x| *x = 0);
		self.jmp_dst = Location::invalid(0);
	}

	fn interpret_bb(&mut self, mem: &dyn IMemory, bb: &BasicBlock)
	-> Option<Location> {
		let insts = bb.insts();
		let last = insts.len() - 1;
		let state = bb.mmu_state();

		for i in insts {
			self.interpret_inst(state, mem, i);
		}

		use BBTerm::*;
		// then the terminator
		match bb.term() {
			DeadEnd | Halt          => None,
			FallThru(to) | Jump(to) => Some(*to),

			Return => {
				if let Some(expected) = self.ret_stack.pop()	{
					if self.jmp_dst == expected {
						Some(expected)
					} else {
						log::warn!("ret addr shenanigans at {:04X} (expected {}, found {})",
							insts[last].va(), expected, self.jmp_dst);
						None
					}
				} else {
					None
				}
			}

			Call { dst, ret } => {
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
				Some(self.jmp_dst)
			}
		}
	}
}
