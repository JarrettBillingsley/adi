
use super::*;

// ------------------------------------------------------------------------------------------------
// ToyBuilder
// ------------------------------------------------------------------------------------------------

fn encode_op(op: Opcode) -> u8 {
	op as u8
}

fn encode_opr0(op: Opcode, r0: Reg) -> u8 {
	check_8bit_reg(r0);
	op as u8 | ((r0 as u8) << 6)
}

fn encode_r1_8bit(r1: Reg) -> u8 {
	check_8bit_reg(r1);
	r1 as u8
}

fn encode_r1_any(r1: Reg) -> u8 {
	match r1 {
		Reg::A | Reg::B | Reg::C | Reg::D | Reg::DC => r1 as u8,
		_ => panic!("cannot use register {:?}", r1),
	}
}

fn check_8bit_reg(reg: Reg) {
	match reg {
		Reg::A | Reg::B | Reg::C | Reg::D => {},
		_ => panic!("cannot use register {:?}", reg),
	}
}

fn calc_branch_offset(from: usize, to: usize) -> u8 {
	let diff = (to as isize) - (from as isize) - 2;
	let diff: i8 = diff.try_into().expect("branch offset too far");
	diff as u8
}

fn encode_16bit_addr(target: usize) -> [u8; 2] {
	assert!(matches!(target, 0 ..= 0xFFFF), "invalid 16-bit address");
	[
		(target & 0xFF) as u8,
		((target >> 8) & 0xFF) as u8
	]
}

pub struct ToyBuilder {
	bytes: Vec<u8>,
}

impl ToyBuilder {
	#[allow(clippy::new_without_default)]
	pub fn new() -> Self {
		Self { bytes: vec![] }
	}

	pub fn finish(self) -> Vec<u8> {
		let mut insts = self.bytes;
		insts.resize(0x8000, 0);
		let mut ret = vec![b'T', b'O', b'Y'];
		ret.append(&mut insts);
		ret
	}

	pub fn org(&mut self, addr: usize) {
		assert!(addr >= self.bytes.len(), "can only move origin up");
		assert!(addr <= 0x7FFF, "invalid address {:X} (valid addresses 0..=0x7FFF)", addr);
		self.bytes.resize(addr, 0);
	}

	pub fn append(&mut self, bytes: &[u8]) -> usize {
		let ret = self.bytes.len();
		assert!(ret + bytes.len() <= 0x8000, "ROM segment too big");
		self.bytes.extend_from_slice(bytes);
		ret
	}

	pub fn branch_here(&mut self, from: usize) -> usize {
		assert!(from < self.bytes.len(), "bad source address {:X}", from);
		let opc = self.bytes[from];
		if opc != Opcode::BLT_S8 as u8 && opc != Opcode::BLE_S8 as u8 &&
			opc != Opcode::BEQ_S8 as u8 && opc != Opcode::BNE_S8 as u8 {
			panic!("patching something that isn't a branch at {:X}", from);
		}

		self.bytes[from + 1] = calc_branch_offset(from, self.bytes.len());
		self.bytes.len()
	}

	pub fn jump_here(&mut self, from: usize) -> usize {
		assert!(from < self.bytes.len(), "bad source address {:X}", from);
		let opc = self.bytes[from];
		if opc != Opcode::JMP_I16 as u8 && opc != Opcode::CALL_I16 as u8 {
			panic!("patching something that isn't a jump/call at {:X}", from);
		}

		let target = encode_16bit_addr(self.bytes.len());
		self.bytes[from + 1] = target[0];
		self.bytes[from + 2] = target[1];
		self.bytes.len()
	}

	pub fn mov(&mut self, dst: Reg, src: Reg) -> usize {
		self.append(&[encode_opr0(Opcode::MOV_RR, dst), encode_r1_8bit(src)])
	}

	pub fn movi(&mut self, dst: Reg, src: impl Into<u8>) -> usize {
		self.append(&[encode_opr0(Opcode::MOV_RI8, dst), src.into()])
	}

	pub fn add(&mut self, dst: Reg, src: Reg) -> usize {
		self.append(&[encode_opr0(Opcode::ADD_RR, dst), encode_r1_8bit(src)])
	}

	pub fn addi(&mut self, dst: Reg, src: impl Into<u8>) -> usize {
		self.append(&[encode_opr0(Opcode::ADD_RI8, dst), src.into()])
	}

	pub fn adc(&mut self, dst: Reg, src: Reg) -> usize {
		self.append(&[encode_opr0(Opcode::ADC_RR, dst), encode_r1_8bit(src)])
	}

	pub fn adci(&mut self, dst: Reg, src: impl Into<u8>) -> usize {
		self.append(&[encode_opr0(Opcode::ADC_RI8, dst), src.into()])
	}

	pub fn sub(&mut self, dst: Reg, src: Reg) -> usize {
		self.append(&[encode_opr0(Opcode::SUB_RR, dst), encode_r1_8bit(src)])
	}

	pub fn subi(&mut self, dst: Reg, src: impl Into<u8>) -> usize {
		self.append(&[encode_opr0(Opcode::SUB_RI8, dst), src.into()])
	}

	pub fn sbc(&mut self, dst: Reg, src: Reg) -> usize {
		self.append(&[encode_opr0(Opcode::SBC_RR, dst), encode_r1_8bit(src)])
	}

	pub fn sbci(&mut self, dst: Reg, src: impl Into<u8>) -> usize {
		self.append(&[encode_opr0(Opcode::SBC_RI8, dst), src.into()])
	}

	pub fn and(&mut self, dst: Reg, src: Reg) -> usize {
		self.append(&[encode_opr0(Opcode::AND_RR, dst), encode_r1_8bit(src)])
	}

	pub fn andi(&mut self, dst: Reg, src: impl Into<u8>) -> usize {
		self.append(&[encode_opr0(Opcode::AND_RI8, dst), src.into()])
	}

	pub fn or(&mut self, dst: Reg, src: Reg) -> usize {
		self.append(&[encode_opr0(Opcode::OR_RR, dst), encode_r1_8bit(src)])
	}

	pub fn ori(&mut self, dst: Reg, src: impl Into<u8>) -> usize {
		self.append(&[encode_opr0(Opcode::OR_RI8, dst), src.into()])
	}

	pub fn xor(&mut self, dst: Reg, src: Reg) -> usize {
		self.append(&[encode_opr0(Opcode::XOR_RR, dst), encode_r1_8bit(src)])
	}

	pub fn xori(&mut self, dst: Reg, src: impl Into<u8>) -> usize {
		self.append(&[encode_opr0(Opcode::XOR_RI8, dst), src.into()])
	}

	pub fn not(&mut self, dst: Reg, src: Reg) -> usize {
		self.append(&[encode_opr0(Opcode::NOT_RR, dst), encode_r1_8bit(src)])
	}

	pub fn noti(&mut self, dst: Reg, src: impl Into<u8>) -> usize {
		self.append(&[encode_opr0(Opcode::NOT_RI8, dst), src.into()])
	}

	pub fn cmp(&mut self, dst: Reg, src: Reg) -> usize {
		self.append(&[encode_opr0(Opcode::CMP_RR, dst), encode_r1_8bit(src)])
	}

	pub fn cmpi(&mut self, dst: Reg, src: impl Into<u8>) -> usize {
		self.append(&[encode_opr0(Opcode::CMP_RI8, dst), src.into()])
	}

	pub fn cmc(&mut self, dst: Reg, src: Reg) -> usize {
		self.append(&[encode_opr0(Opcode::CMC_RR, dst), encode_r1_8bit(src)])
	}

	pub fn cmci(&mut self, dst: Reg, src: impl Into<u8>) -> usize {
		self.append(&[encode_opr0(Opcode::CMC_RI8, dst), src.into()])
	}

	pub fn blt(&mut self) -> usize {
		self.append(&[encode_op(Opcode::BLT_S8), 0])
	}

	pub fn blt_to(&mut self, to: usize) -> usize {
		self.append(&[encode_op(Opcode::BLT_S8), calc_branch_offset(self.bytes.len(), to)])
	}

	pub fn ble(&mut self) -> usize {
		self.append(&[encode_op(Opcode::BLE_S8), 0])
	}

	pub fn ble_to(&mut self, to: usize) -> usize {
		self.append(&[encode_op(Opcode::BLE_S8), calc_branch_offset(self.bytes.len(), to)])
	}

	pub fn beq(&mut self) -> usize {
		self.append(&[encode_op(Opcode::BEQ_S8), 0])
	}

	pub fn beq_to(&mut self, to: usize) -> usize {
		self.append(&[encode_op(Opcode::BEQ_S8), calc_branch_offset(self.bytes.len(), to)])
	}

	pub fn bne(&mut self) -> usize {
		self.append(&[encode_op(Opcode::BNE_S8), 0])
	}

	pub fn bne_to(&mut self, to: usize) -> usize {
		self.append(&[encode_op(Opcode::BNE_S8), calc_branch_offset(self.bytes.len(), to)])
	}

	pub fn jmp(&mut self) -> usize {
		self.append(&[encode_op(Opcode::JMP_I16), 0, 0])
	}

	pub fn jmp_to(&mut self, target: usize) -> usize {
		let target = encode_16bit_addr(target);
		self.append(&[encode_op(Opcode::JMP_I16), target[0], target[1]])
	}

	pub fn jmpi(&mut self) -> usize {
		self.append(&[encode_op(Opcode::JMPI_IMPDC)])
	}

	pub fn call(&mut self) -> usize {
		self.append(&[encode_op(Opcode::CALL_I16), 0, 0])
	}

	pub fn cali(&mut self) -> usize {
		self.append(&[encode_op(Opcode::CALI_IMPDC)])
	}

	pub fn call_to(&mut self, target: usize) -> usize {
		let target = encode_16bit_addr(target);
		self.append(&[encode_op(Opcode::CALL_I16), target[0], target[1]])
	}

	pub fn ret(&mut self) -> usize {
		self.append(&[encode_op(Opcode::RET_IMP)])
	}

	pub fn ld(&mut self, dst: Reg, src: Reg) -> usize {
		self.append(&[encode_opr0(Opcode::LD_RR, dst), encode_r1_any(src)])
	}

	pub fn ldi(&mut self, dst: Reg, addr: u16) -> usize {
		let addr = encode_16bit_addr(addr as usize);
		self.append(&[encode_opr0(Opcode::LD_RI16, dst), addr[0], addr[1]])
	}

	pub fn st(&mut self, dst: Reg, src: Reg) -> usize {
		self.append(&[encode_opr0(Opcode::ST_RR, dst), encode_r1_any(src)])
	}

	pub fn sti(&mut self, dst: Reg, addr: u16) -> usize {
		let addr = encode_16bit_addr(addr as usize);
		self.append(&[encode_opr0(Opcode::ST_RI16, dst), addr[0], addr[1]])
	}
}