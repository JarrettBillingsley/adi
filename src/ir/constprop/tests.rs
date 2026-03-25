use super::*;

#[track_caller]
fn test_unop(expected: u64, op: IrUnOp, val: u64, src_size: ValSize) {
	let actual = do_unop(op, val, src_size, src_size);
	assert_eq!(expected, actual);
}

#[track_caller]
fn test_unop_8(expected: u64, op: IrUnOp, val: u64) {
	test_unop(expected, op, val, ValSize::_8);
}

#[track_caller]
fn test_unop_16(expected: u64, op: IrUnOp, val: u64) {
	test_unop(expected, op, val, ValSize::_16);
}

#[track_caller]
fn test_unop_32(expected: u64, op: IrUnOp, val: u64) {
	test_unop(expected, op, val, ValSize::_32);
}

#[track_caller]
fn test_unop_64(expected: u64, op: IrUnOp, val: u64) {
	test_unop(expected, op, val, ValSize::_64);
}

#[track_caller]
fn test_ext(expected: u64, dst_size: ValSize, op: IrUnOp, val: u64, src_size: ValSize) {
	let actual = do_unop(op, val, src_size, dst_size);
	assert_eq!(expected, actual);
}

#[track_caller]
fn test_binop(expected: u64, op: IrBinOp, val1: u64, val2: u64, size: ValSize) {
	let actual = do_binop(op, val1, val2, size);
	assert_eq!(Some(expected), actual);
}

#[track_caller]
fn test_binop_8(expected: u64, op: IrBinOp, val1: u64, val2: u64) {
	test_binop(expected, op, val1, val2, ValSize::_8);
}

#[track_caller]
fn test_binop_16(expected: u64, op: IrBinOp, val1: u64, val2: u64) {
	test_binop(expected, op, val1, val2, ValSize::_16);
}

#[track_caller]
fn test_binop_32(expected: u64, op: IrBinOp, val1: u64, val2: u64) {
	test_binop(expected, op, val1, val2, ValSize::_32);
}

#[track_caller]
fn test_binop_64(expected: u64, op: IrBinOp, val1: u64, val2: u64) {
	test_binop(expected, op, val1, val2, ValSize::_64);
}

#[track_caller]
fn test_binop_none(op: IrBinOp, val1: u64, val2: u64, size: ValSize) {
	let actual = do_binop(op, val1, val2, size);
	assert_eq!(None, actual);
}

#[track_caller]
fn test_ternop(expected: u64, op: IrTernOp, val1: u64, val2: u64, val3: u64, size: ValSize) {
	let actual = do_ternop(op, val1, val2, val3, size);
	assert_eq!(expected, actual);
}

#[track_caller]
fn test_ternop_8(expected: u64, op: IrTernOp, val1: u64, val2: u64, val3: u64) {
	test_ternop(expected, op, val1, val2, val3, ValSize::_8);
}

#[track_caller]
fn test_ternop_16(expected: u64, op: IrTernOp, val1: u64, val2: u64, val3: u64) {
	test_ternop(expected, op, val1, val2, val3, ValSize::_16);
}

#[track_caller]
fn test_ternop_32(expected: u64, op: IrTernOp, val1: u64, val2: u64, val3: u64) {
	test_ternop(expected, op, val1, val2, val3, ValSize::_32);
}

#[track_caller]
fn test_ternop_64(expected: u64, op: IrTernOp, val1: u64, val2: u64, val3: u64) {
	test_ternop(expected, op, val1, val2, val3, ValSize::_64);
}

#[test]
fn test_zxt() {
	use { IrUnOp::*, ValSize::* };
	test_ext(0x34, _16, Zxt, 0x34, _8);
	test_ext(0x34, _32, Zxt, 0x34, _8);
	test_ext(0x34, _64, Zxt, 0x34, _8);
	test_ext(0x34, _32, Zxt, 0x34, _16);
	test_ext(0x34, _64, Zxt, 0x34, _16);
	test_ext(0x34, _64, Zxt, 0x34, _32);
	test_ext(0xFF, _16, Zxt, 0xFF, _8);
	test_ext(0xFF, _32, Zxt, 0xFF, _8);
	test_ext(0xFF, _64, Zxt, 0xFF, _8);
	test_ext(0xFF, _32, Zxt, 0xFF, _16);
	test_ext(0xFF, _64, Zxt, 0xFF, _16);
	test_ext(0xFF, _64, Zxt, 0xFF, _32);
}

#[test]
fn test_sxt() {
	use { IrUnOp::*, ValSize::* };
	test_ext(0x34, _16, Sxt, 0x34, _8);
	test_ext(0x34, _32, Sxt, 0x34, _8);
	test_ext(0x34, _64, Sxt, 0x34, _8);
	test_ext(0x34, _32, Sxt, 0x34, _16);
	test_ext(0x34, _64, Sxt, 0x34, _16);
	test_ext(0x34, _64, Sxt, 0x34, _32);
	test_ext(0xFFFF,              _16, Sxt, 0xFF,       _8);
	test_ext(0xFFFFFFFF,          _32, Sxt, 0xFF,       _8);
	test_ext(0xFFFFFFFF_FFFFFFFF, _64, Sxt, 0xFF,       _8);
	test_ext(0xFFFFFFFF,          _32, Sxt, 0xFFFF,     _16);
	test_ext(0xFFFFFFFF_FFFFFFFF, _64, Sxt, 0xFFFF,     _16);
	test_ext(0xFFFFFFFF_FFFFFFFF, _64, Sxt, 0xFFFFFFFF, _32);
}

#[test]
fn test_lo() {
	use IrUnOp::*;
	test_unop_16(      0x34, Lo, 0x1234);
	test_unop_32(    0x5678, Lo, 0x12345678);
	test_unop_64(0x9ABCDEF0, Lo, 0x12345678_9ABCDEF0);
}

#[test]
fn test_hi() {
	use IrUnOp::*;
	test_unop_16(      0x12, Hi, 0x1234);
	test_unop_32(    0x1234, Hi, 0x12345678);
	test_unop_64(0x12345678, Hi, 0x12345678_9ABCDEF0);
}

#[test]
fn test_neg() {
	use IrUnOp::*;
	test_unop_8 (0xFE, Neg, 0x02);
	test_unop_8 (0x02, Neg, 0xFE);
	test_unop_16(0xFFFE, Neg, 0x0002);
	test_unop_16(0x0002, Neg, 0xFFFE);
	test_unop_32(0xFFFFFFFE, Neg, 0x00000002);
	test_unop_32(0x00000002, Neg, 0xFFFFFFFE);
	test_unop_64(0xFFFFFFFF_FFFFFFFE, Neg, 0x00000000_00000002);
	test_unop_64(0x00000000_00000002, Neg, 0xFFFFFFFF_FFFFFFFE);
}

#[test]
fn test_inot() {
	use IrUnOp::*;
	test_unop_8 (0xFD, INot, 0x02);
	test_unop_8 (0x02, INot, 0xFD);
	test_unop_16(0xFFFD, INot, 0x0002);
	test_unop_16(0x0002, INot, 0xFFFD);
	test_unop_32(0xFFFFFFFD, INot, 0x00000002);
	test_unop_32(0x00000002, INot, 0xFFFFFFFD);
	test_unop_64(0xFFFFFFFF_FFFFFFFD, INot, 0x00000000_00000002);
	test_unop_64(0x00000000_00000002, INot, 0xFFFFFFFF_FFFFFFFD);
}

#[test]
fn test_bnot() {
	use IrUnOp::*;
	test_unop_8(1, BNot, 0);
	test_unop_8(0, BNot, 1);
	test_unop_8(0, BNot, 100);
}

#[test]
fn test_eq_ne() {
	use IrBinOp::*;
	test_binop_8(1, Eq,  0x34, 0x34);
	test_binop_8(0, Eq,  0x34, 0x9E);
	test_binop_8(0, Ne,  0x34, 0x34);
	test_binop_8(1, Ne,  0x34, 0x9E);
}

#[test]
fn test_slt() {
	use IrBinOp::*;
	test_binop_8 (1, Slt, 0xFF, 0x01);
	test_binop_8 (1, Slt, 0x03, 0x05);
	test_binop_8 (0, Slt, 0x01, 0xFF);
	test_binop_8 (0, Slt, 0x05, 0x03);
	test_binop_8 (0, Slt, 0x04, 0x04);
	test_binop_16(1, Slt, 0xFFFF, 0x0001);
	test_binop_16(1, Slt, 0x0003, 0x0005);
	test_binop_16(0, Slt, 0x0001, 0xFFFF);
	test_binop_16(0, Slt, 0x0005, 0x0003);
	test_binop_16(0, Slt, 0x0004, 0x0004);
	test_binop_32(1, Slt, 0xFFFFFFFF, 0x00000001);
	test_binop_32(1, Slt, 0x00000003, 0x00000005);
	test_binop_32(0, Slt, 0x00000001, 0xFFFFFFFF);
	test_binop_32(0, Slt, 0x00000005, 0x00000003);
	test_binop_32(0, Slt, 0x00000004, 0x00000004);
	test_binop_64(1, Slt, 0xFFFFFFFF_FFFFFFFF, 0x00000000_00000001);
	test_binop_64(1, Slt, 0x00000000_00000003, 0x00000000_00000005);
	test_binop_64(0, Slt, 0x00000000_00000001, 0xFFFFFFFF_FFFFFFFF);
	test_binop_64(0, Slt, 0x00000000_00000005, 0x00000000_00000003);
	test_binop_64(0, Slt, 0x00000000_00000004, 0x00000000_00000004);
}

#[test]
fn test_sle() {
	use IrBinOp::*;
	test_binop_8 (1, Sle, 0xFF, 0x01);
	test_binop_8 (1, Sle, 0x03, 0x05);
	test_binop_8 (0, Sle, 0x01, 0xFF);
	test_binop_8 (0, Sle, 0x05, 0x03);
	test_binop_8 (1, Sle, 0x04, 0x04);
	test_binop_16(1, Sle, 0xFFFF, 0x0001);
	test_binop_16(1, Sle, 0x0003, 0x0005);
	test_binop_16(0, Sle, 0x0001, 0xFFFF);
	test_binop_16(0, Sle, 0x0005, 0x0003);
	test_binop_16(1, Sle, 0x0004, 0x0004);
	test_binop_32(1, Sle, 0xFFFFFFFF, 0x00000001);
	test_binop_32(1, Sle, 0x00000003, 0x00000005);
	test_binop_32(0, Sle, 0x00000001, 0xFFFFFFFF);
	test_binop_32(0, Sle, 0x00000005, 0x00000003);
	test_binop_32(1, Sle, 0x00000004, 0x00000004);
	test_binop_64(1, Sle, 0xFFFFFFFF_FFFFFFFF, 0x00000000_00000001);
	test_binop_64(1, Sle, 0x00000000_00000003, 0x00000000_00000005);
	test_binop_64(0, Sle, 0x00000000_00000001, 0xFFFFFFFF_FFFFFFFF);
	test_binop_64(0, Sle, 0x00000000_00000005, 0x00000000_00000003);
	test_binop_64(1, Sle, 0x00000000_00000004, 0x00000000_00000004);
}

#[test]
fn test_ult() {
	use IrBinOp::*;
	test_binop_8 (0, Ult, 0xFF, 0x01);
	test_binop_8 (1, Ult, 0x03, 0x05);
	test_binop_8 (1, Ult, 0x01, 0xFF);
	test_binop_8 (0, Ult, 0x05, 0x03);
	test_binop_8 (0, Ult, 0x04, 0x04);
	test_binop_16(0, Ult, 0xFFFF, 0x0001);
	test_binop_16(1, Ult, 0x0003, 0x0005);
	test_binop_16(1, Ult, 0x0001, 0xFFFF);
	test_binop_16(0, Ult, 0x0005, 0x0003);
	test_binop_16(0, Ult, 0x0004, 0x0004);
	test_binop_32(0, Ult, 0xFFFFFFFF, 0x00000001);
	test_binop_32(1, Ult, 0x00000003, 0x00000005);
	test_binop_32(1, Ult, 0x00000001, 0xFFFFFFFF);
	test_binop_32(0, Ult, 0x00000005, 0x00000003);
	test_binop_32(0, Ult, 0x00000004, 0x00000004);
	test_binop_64(0, Ult, 0xFFFFFFFF_FFFFFFFF, 0x00000000_00000001);
	test_binop_64(1, Ult, 0x00000000_00000003, 0x00000000_00000005);
	test_binop_64(1, Ult, 0x00000000_00000001, 0xFFFFFFFF_FFFFFFFF);
	test_binop_64(0, Ult, 0x00000000_00000005, 0x00000000_00000003);
	test_binop_64(0, Ult, 0x00000000_00000004, 0x00000000_00000004);
}

#[test]
fn test_ule() {
	use IrBinOp::*;
	test_binop_8 (0, Ule, 0xFF, 0x01);
	test_binop_8 (1, Ule, 0x03, 0x05);
	test_binop_8 (1, Ule, 0x01, 0xFF);
	test_binop_8 (0, Ule, 0x05, 0x03);
	test_binop_8 (1, Ule, 0x04, 0x04);
	test_binop_16(0, Ule, 0xFFFF, 0x0001);
	test_binop_16(1, Ule, 0x0003, 0x0005);
	test_binop_16(1, Ule, 0x0001, 0xFFFF);
	test_binop_16(0, Ule, 0x0005, 0x0003);
	test_binop_16(1, Ule, 0x0004, 0x0004);
	test_binop_32(0, Ule, 0xFFFFFFFF, 0x00000001);
	test_binop_32(1, Ule, 0x00000003, 0x00000005);
	test_binop_32(1, Ule, 0x00000001, 0xFFFFFFFF);
	test_binop_32(0, Ule, 0x00000005, 0x00000003);
	test_binop_32(1, Ule, 0x00000004, 0x00000004);
	test_binop_64(0, Ule, 0xFFFFFFFF_FFFFFFFF, 0x00000000_00000001);
	test_binop_64(1, Ule, 0x00000000_00000003, 0x00000000_00000005);
	test_binop_64(1, Ule, 0x00000000_00000001, 0xFFFFFFFF_FFFFFFFF);
	test_binop_64(0, Ule, 0x00000000_00000005, 0x00000000_00000003);
	test_binop_64(1, Ule, 0x00000000_00000004, 0x00000000_00000004);
}

#[test]
fn test_add() {
	use IrBinOp::*;
	test_binop_8 (0x08, Add, 0x03, 0x05);
	test_binop_16(0x0008, Add, 0x0003, 0x0005);
	test_binop_32(0x00000008, Add, 0x00000003, 0x00000005);
	test_binop_64(0x00000000_00000008, Add, 0x00000000_00000003, 0x00000000_00000005);

	test_binop_8 (0x02, Add, 0xFD, 0x05);
	test_binop_16(0x0002, Add, 0xFFFD, 0x0005);
	test_binop_32(0x00000002, Add, 0xFFFFFFFD, 0x00000005);
	test_binop_64(0x00000000_00000002, Add, 0xFFFFFFFF_FFFFFFFD, 0x00000000_00000005);
}

#[test]
fn test_sub() {
	use IrBinOp::*;
	test_binop_8 (0x03, Sub, 0x08, 0x05);
	test_binop_16(0x0003, Sub, 0x0008, 0x0005);
	test_binop_32(0x00000003, Sub, 0x00000008, 0x00000005);
	test_binop_64(0x00000000_00000003, Sub, 0x00000000_00000008, 0x00000000_00000005);

	test_binop_8 (0xFE, Sub, 0x03, 0x05);
	test_binop_16(0xFFFE, Sub, 0x0003, 0x0005);
	test_binop_32(0xFFFFFFFE, Sub, 0x00000003, 0x00000005);
	test_binop_64(0xFFFFFFFF_FFFFFFFE, Sub, 0x00000000_00000003, 0x00000000_00000005);
}

#[test]
fn test_ucarry() {
	use IrBinOp::*;
	test_binop_8 (0, UCarry, 0x08, 0x05);
	test_binop_16(0, UCarry, 0x0008, 0x0005);
	test_binop_32(0, UCarry, 0x00000008, 0x00000005);
	test_binop_64(0, UCarry, 0x00000000_00000008, 0x00000000_00000005);

	test_binop_8 (0, UCarry, 0x7F, 0x01);
	test_binop_8 (1, UCarry, 0xFF, 0x01);
	test_binop_16(0, UCarry, 0x00FF, 0x0001);
	test_binop_32(0, UCarry, 0x000000FF, 0x00000001);
	test_binop_64(0, UCarry, 0x00000000_000000FF, 0x00000000_00000001);

	test_binop_16(0, UCarry, 0x7FFF, 0x0001);
	test_binop_16(1, UCarry, 0xFFFF, 0x0001);
	test_binop_32(0, UCarry, 0x0000FFFF, 0x00000001);
	test_binop_64(0, UCarry, 0x00000000_0000FFFF, 0x00000000_00000001);

	test_binop_32(0, UCarry, 0x7FFFFFFF, 0x00000001);
	test_binop_32(1, UCarry, 0xFFFFFFFF, 0x00000001);
	test_binop_64(0, UCarry, 0x00000000_FFFFFFFF, 0x00000000_00000001);

	test_binop_64(0, UCarry, 0x7FFFFFFF_FFFFFFFF, 0x00000000_00000001);
	test_binop_64(1, UCarry, 0xFFFFFFFF_FFFFFFFF, 0x00000000_00000001);
}

#[test]
fn test_scarry() {
	use IrBinOp::*;
	test_binop_8 (0, SCarry, 0x08, 0x05);
	test_binop_16(0, SCarry, 0x0008, 0x0005);
	test_binop_32(0, SCarry, 0x00000008, 0x00000005);
	test_binop_64(0, SCarry, 0x00000000_00000008, 0x00000000_00000005);

	test_binop_8 (0, SCarry, 0xFE, 0x05);
	test_binop_16(0, SCarry, 0xFFFE, 0x0005);
	test_binop_32(0, SCarry, 0xFFFFFFFE, 0x00000005);
	test_binop_64(0, SCarry, 0xFFFFFFFF_FFFFFFFE, 0x00000000_00000005);

	test_binop_8 (1, SCarry, 0x7F, 0x01);
	test_binop_8 (1, SCarry, 0x80, 0xFF);
	test_binop_8 (0, SCarry, 0xFF, 0x01);
	test_binop_16(0, SCarry, 0x00FF, 0x0001);
	test_binop_32(0, SCarry, 0x000000FF, 0x00000001);
	test_binop_64(0, SCarry, 0x00000000_000000FF, 0x00000000_00000001);

	test_binop_16(1, SCarry, 0x7FFF, 0x0001);
	test_binop_16(1, SCarry, 0x8000, 0xFFFF);
	test_binop_16(0, SCarry, 0xFFFF, 0x0001);
	test_binop_32(0, SCarry, 0x0000FFFF, 0x00000001);
	test_binop_64(0, SCarry, 0x00000000_0000FFFF, 0x00000000_00000001);

	test_binop_32(1, SCarry, 0x7FFFFFFF, 0x00000001);
	test_binop_32(1, SCarry, 0x80000000, 0xFFFFFFFF);
	test_binop_32(0, SCarry, 0xFFFFFFFF, 0x00000001);
	test_binop_64(0, SCarry, 0x00000000_FFFFFFFF, 0x00000000_00000001);

	test_binop_64(1, SCarry, 0x7FFFFFFF_FFFFFFFF, 0x00000000_00000001);
	test_binop_64(1, SCarry, 0x80000000_00000000, 0xFFFFFFFF_FFFFFFFF);
	test_binop_64(0, SCarry, 0xFFFFFFFF_FFFFFFFF, 0x00000000_00000001);
}

#[test]
fn test_sborrow() {
	use IrBinOp::*;
	test_binop_8 (0, SBorrow, 0x08, 0x05);
	test_binop_16(0, SBorrow, 0x0008, 0x0005);
	test_binop_32(0, SBorrow, 0x00000008, 0x00000005);
	test_binop_64(0, SBorrow, 0x00000000_00000008, 0x00000000_00000005);

	test_binop_8 (0, SBorrow, 0xFE, 0x05);
	test_binop_16(0, SBorrow, 0xFFFE, 0x0005);
	test_binop_32(0, SBorrow, 0xFFFFFFFE, 0x00000005);
	test_binop_64(0, SBorrow, 0xFFFFFFFF_FFFFFFFE, 0x00000000_00000005);

	test_binop_8 (1, SBorrow, 0x7F, 0xFF);
	test_binop_8 (1, SBorrow, 0x80, 0x01);
	test_binop_8 (0, SBorrow, 0xFF, 0xFF);
	test_binop_16(0, SBorrow, 0x00FF, 0x00FF);
	test_binop_32(0, SBorrow, 0x000000FF, 0x000000FF);
	test_binop_64(0, SBorrow, 0x00000000_000000FF, 0x00000000_000000FF);

	test_binop_16(1, SBorrow, 0x7FFF, 0xFFFF);
	test_binop_16(1, SBorrow, 0x8000, 0x0001);
	test_binop_16(0, SBorrow, 0xFFFF, 0xFFFF);
	test_binop_32(0, SBorrow, 0x0000FFFF, 0x0000FFFF);
	test_binop_64(0, SBorrow, 0x00000000_0000FFFF, 0x00000000_0000FFFF);

	test_binop_32(1, SBorrow, 0x7FFFFFFF, 0xFFFFFFFF);
	test_binop_32(1, SBorrow, 0x80000000, 0x00000001);
	test_binop_32(0, SBorrow, 0xFFFFFFFF, 0xFFFFFFFF);
	test_binop_64(0, SBorrow, 0x00000000_FFFFFFFF, 0x00000000_FFFFFFFF);

	test_binop_64(1, SBorrow, 0x7FFFFFFF_FFFFFFFF, 0xFFFFFFFF_FFFFFFFF);
	test_binop_64(1, SBorrow, 0x80000000_00000000, 0x00000000_00000001);
	test_binop_64(0, SBorrow, 0xFFFFFFFF_FFFFFFFF, 0xFFFFFFFF_FFFFFFFF);
}

#[test]
fn test_carries() {
	use IrBinOp::Carries;

	// as explained on the docs of [`carries`] I already validated the output for all possible
	// 16-bit ints so I'm not gonna be too thorough here.

	//             vvvvvvvvvv carries of...
	test_binop_8  (0b00000000, Carries,
					0b00000000,  //   this
					0b00000000); // + this
	test_binop_8  (0b00000000, Carries,
					0b00000001,
					0b00000000);
	test_binop_8  (0b00000000, Carries,
					0b00000000,
					0b00000001);
	test_binop_8  (0b00000001, Carries,
					0b00000001,
					0b00000001);
	test_binop_8  (0b11111111, Carries,
					0b11111111,
					0b00000001);
	test_binop_8  (0b11111111, Carries,
					0b11111111,
					0b11111111);
	test_binop_8  (0b10000000, Carries,
					0b10000000,
					0b10000000);
	test_binop_16 (0b00000000_11111111, Carries,
					0b00000000_11111111,
					0b00000000_11111111);
	test_binop_16 (0b10000000_11111111, Carries,
					0b10000000_11111111,
					0b10000000_11111111);
	test_binop_32 (0b00000000_00000000_10000000_11111111, Carries,
					0b00000000_00000000_10000000_11111111,
					0b00000000_00000000_10000000_11111111);
	test_binop_32 (0b10000000_00000000_10000000_11111111, Carries,
					0b10000000_00000000_10000000_11111111,
					0b10000000_00000000_10000000_11111111);
}

#[test]
fn test_borrows() {
	use IrBinOp::Borrows;

	// as explained on the docs of [`borrows`] I already validated the output for all possible
	// 16-bit ints so I'm not gonna be too thorough here.

	//             vvvvvvvvvv borrows of...
	test_binop_8  (0b00000000, Borrows,
					0b00000000,  //   this
					0b00000000); // - this
	test_binop_8  (0b00000000, Borrows,
					0b00000001,
					0b00000000);
	test_binop_8  (0b11111111, Borrows,
					0b00000000,
					0b00000001);
	test_binop_8  (0b00000000, Borrows,
					0b00000001,
					0b00000001);
}

#[test]
fn test_mul() {
	use IrBinOp::*;

	test_binop_8 (33, Mul, 11, 3);
	test_binop_16(33, Mul, 11, 3);
	test_binop_32(33, Mul, 11, 3);
	test_binop_64(33, Mul, 11, 3);

	//          -15            -5  x  3
	test_binop_8 (0xF1, Mul, 0xFB, 0x03);
	test_binop_16(0xFFF1, Mul, 0xFFFB, 0x0003);
	test_binop_32(0xFFFFFFF1, Mul, 0xFFFFFFFB, 0x00000003);
	test_binop_64(0xFFFFFFFF_FFFFFFF1, Mul, 0xFFFFFFFF_FFFFFFFB, 0x00000000_00000003);

	//           15            -5  x -3
	test_binop_8 (0x0F, Mul, 0xFB, 0xFD);
	test_binop_16(0x000F, Mul, 0xFFFB, 0xFFFD);
	test_binop_32(0x0000000F, Mul, 0xFFFFFFFB, 0xFFFFFFFD);
	test_binop_64(0x00000000_0000000F, Mul, 0xFFFFFFFF_FFFFFFFB, 0xFFFFFFFF_FFFFFFFD);
}

#[test]
fn test_udiv() {
	use IrBinOp::*;

	test_binop_8 (3, UDiv, 33, 11);
	test_binop_16(3, UDiv, 33, 11);
	test_binop_32(3, UDiv, 33, 11);
	test_binop_64(3, UDiv, 33, 11);

	test_binop_none(UDiv, 33, 0, ValSize::_8);
	test_binop_none(UDiv, 33, 0, ValSize::_16);
	test_binop_none(UDiv, 33, 0, ValSize::_32);
	test_binop_none(UDiv, 33, 0, ValSize::_64);

	test_binop_8 (0x0F, UDiv, 0xFF, 0x11);
	test_binop_16(0x0F, UDiv, 0xFFFF, 0x1111);
	test_binop_32(0x0F, UDiv, 0xFFFFFFFF, 0x11111111);
	test_binop_64(0x0F, UDiv, 0xFFFFFFFF_FFFFFFFF, 0x11111111_11111111);
}

#[test]
fn test_sdiv() {
	use IrBinOp::*;

	test_binop_8 (3, SDiv, 33, 11);
	test_binop_16(3, SDiv, 33, 11);
	test_binop_32(3, SDiv, 33, 11);
	test_binop_64(3, SDiv, 33, 11);

	//           -3            -33
	test_binop_8 (0xFD, SDiv, 0xDF, 11);
	test_binop_16(0xFFFD, SDiv, 0xFFDF, 11);
	test_binop_32(0xFFFFFFFD, SDiv, 0xFFFFFFDF, 11);
	test_binop_64(0xFFFFFFFF_FFFFFFFD, SDiv, 0xFFFFFFFF_FFFFFFDF, 11);

	//           -3                -11
	test_binop_8 (0xFD, SDiv, 33, 0xF5);
	test_binop_16(0xFFFD, SDiv, 33, 0xFFF5);
	test_binop_32(0xFFFFFFFD, SDiv, 33, 0xFFFFFFF5);
	test_binop_64(0xFFFFFFFF_FFFFFFFD, SDiv, 33, 0xFFFFFFFF_FFFFFFF5);

	//                    -33    -11
	test_binop_8 (3, SDiv, 0xDF, 0xF5);
	test_binop_16(3, SDiv, 0xFFDF, 0xFFF5);
	test_binop_32(3, SDiv, 0xFFFFFFDF, 0xFFFFFFF5);
	test_binop_64(3, SDiv, 0xFFFFFFFF_FFFFFFDF, 0xFFFFFFFF_FFFFFFF5);

	test_binop_none(SDiv, 33, 0, ValSize::_8);
	test_binop_none(SDiv, 33, 0, ValSize::_16);
	test_binop_none(SDiv, 33, 0, ValSize::_32);
	test_binop_none(SDiv, 33, 0, ValSize::_64);

	test_binop_8 (0, SDiv, 0xFF, 0x11);
	test_binop_16(0, SDiv, 0xFFFF, 0x1111);
	test_binop_32(0, SDiv, 0xFFFFFFFF, 0x11111111);
	test_binop_64(0, SDiv, 0xFFFFFFFF_FFFFFFFF, 0x11111111_11111111);
}

#[test]
fn test_umod() {
	use IrBinOp::*;

	test_binop_8 (0, UMod, 33, 11);
	test_binop_16(0, UMod, 33, 11);
	test_binop_32(0, UMod, 33, 11);
	test_binop_64(0, UMod, 33, 11);

	test_binop_64(0, UMod, 0, 11);
	test_binop_64(1, UMod, 1, 11);
	test_binop_64(2, UMod, 2, 11);
	test_binop_64(3, UMod, 3, 11);
	test_binop_64(4, UMod, 4, 11);
	test_binop_64(5, UMod, 5, 11);
	test_binop_64(6, UMod, 6, 11);
	test_binop_64(7, UMod, 7, 11);
	test_binop_64(8, UMod, 8, 11);
	test_binop_64(9, UMod, 9, 11);
	test_binop_64(10, UMod, 10, 11);
	test_binop_64(0, UMod, 11, 11);

	test_binop_none(UMod, 33, 0, ValSize::_8);
	test_binop_none(UMod, 33, 0, ValSize::_16);
	test_binop_none(UMod, 33, 0, ValSize::_32);
	test_binop_none(UMod, 33, 0, ValSize::_64);

	test_binop_8 (0, UMod, 0xFF, 0x11);
	test_binop_16(0, UMod, 0xFFFF, 0x1111);
	test_binop_32(0, UMod, 0xFFFFFFFF, 0x11111111);
	test_binop_64(0, UMod, 0xFFFFFFFF_FFFFFFFF, 0x11111111_11111111);
}

#[test]
fn test_smod() {
	use IrBinOp::*;

	// on positives it should behave the same as umod
	test_binop_8 (0, SMod, 33, 11);
	test_binop_16(0, SMod, 33, 11);
	test_binop_32(0, SMod, 33, 11);
	test_binop_64(0, SMod, 33, 11);

	test_binop_64(0, SMod, 0, 11);
	test_binop_64(1, SMod, 1, 11);
	test_binop_64(2, SMod, 2, 11);
	test_binop_64(3, SMod, 3, 11);
	test_binop_64(4, SMod, 4, 11);
	test_binop_64(5, SMod, 5, 11);
	test_binop_64(6, SMod, 6, 11);
	test_binop_64(7, SMod, 7, 11);
	test_binop_64(8, SMod, 8, 11);
	test_binop_64(9, SMod, 9, 11);
	test_binop_64(10, SMod, 10, 11);
	test_binop_64(0, SMod, 11, 11);

	test_binop_none(SMod, 33, 0, ValSize::_8);
	test_binop_none(SMod, 33, 0, ValSize::_16);
	test_binop_none(SMod, 33, 0, ValSize::_32);
	test_binop_none(SMod, 33, 0, ValSize::_64);

	// -1 % whatever == -1
	test_binop_8 (0xFF, SMod, 0xFF, 0x11);
	test_binop_16(0xFFFF, SMod, 0xFFFF, 0x1111);
	test_binop_32(0xFFFFFFFF, SMod, 0xFFFFFFFF, 0x11111111);
	test_binop_64(0xFFFFFFFF_FFFFFFFF, SMod, 0xFFFFFFFF_FFFFFFFF, 0x11111111_11111111);

	test_binop_8(0xFF, SMod, 0xFF, 11); // -1  % 11
	test_binop_8(0xFE, SMod, 0xFE, 11); // -2  % 11
	test_binop_8(0xFD, SMod, 0xFD, 11); // -3  % 11
	test_binop_8(0xFC, SMod, 0xFC, 11); // -4  % 11
	test_binop_8(0xFB, SMod, 0xFB, 11); // -5  % 11
	test_binop_8(0xFA, SMod, 0xFA, 11); // -6  % 11
	test_binop_8(0xF9, SMod, 0xF9, 11); // -7  % 11
	test_binop_8(0xF8, SMod, 0xF8, 11); // -8  % 11
	test_binop_8(0xF7, SMod, 0xF7, 11); // -9  % 11
	test_binop_8(0xF6, SMod, 0xF6, 11); // -10 % 11
	test_binop_8(0x00, SMod, 0xF5, 11); // -11 % 11

	test_binop_8(1, SMod, 1, 0xF5);   // 1  % -11
	test_binop_8(2, SMod, 2, 0xF5);   // 2  % -11
	test_binop_8(3, SMod, 3, 0xF5);   // 3  % -11
	test_binop_8(4, SMod, 4, 0xF5);   // 4  % -11
	test_binop_8(5, SMod, 5, 0xF5);   // 5  % -11
	test_binop_8(6, SMod, 6, 0xF5);   // 6  % -11
	test_binop_8(7, SMod, 7, 0xF5);   // 7  % -11
	test_binop_8(8, SMod, 8, 0xF5);   // 8  % -11
	test_binop_8(9, SMod, 9, 0xF5);   // 9  % -11
	test_binop_8(10, SMod, 10, 0xF5); // 10 % -11
	test_binop_8(0, SMod, 11, 0xF5);  // 11 % -11

	test_binop_8(0xFF, SMod, 0xFF, 0xF5); // -1  % -11
	test_binop_8(0xFE, SMod, 0xFE, 0xF5); // -2  % -11
	test_binop_8(0xFD, SMod, 0xFD, 0xF5); // -3  % -11
	test_binop_8(0xFC, SMod, 0xFC, 0xF5); // -4  % -11
	test_binop_8(0xFB, SMod, 0xFB, 0xF5); // -5  % -11
	test_binop_8(0xFA, SMod, 0xFA, 0xF5); // -6  % -11
	test_binop_8(0xF9, SMod, 0xF9, 0xF5); // -7  % -11
	test_binop_8(0xF8, SMod, 0xF8, 0xF5); // -8  % -11
	test_binop_8(0xF7, SMod, 0xF7, 0xF5); // -9  % -11
	test_binop_8(0xF6, SMod, 0xF6, 0xF5); // -10 % -11
	test_binop_8(0x00, SMod, 0xF5, 0xF5); // -11 % -11
}

#[test]
fn test_ixor_iand_ior() {
	use IrBinOp::*;

	test_binop_8 (0b10101100, IXor, 0b11001010, 0b01100110);
	test_binop_16(0b10101100, IXor, 0b11001010, 0b01100110);
	test_binop_32(0b10101100, IXor, 0b11001010, 0b01100110);
	test_binop_64(0b10101100, IXor, 0b11001010, 0b01100110);

	test_binop_8 (0b01000010, IAnd, 0b11001010, 0b01100110);
	test_binop_16(0b01000010, IAnd, 0b11001010, 0b01100110);
	test_binop_32(0b01000010, IAnd, 0b11001010, 0b01100110);
	test_binop_64(0b01000010, IAnd, 0b11001010, 0b01100110);

	test_binop_8 (0b11101110, IOr,  0b11001010, 0b01100110);
	test_binop_16(0b11101110, IOr,  0b11001010, 0b01100110);
	test_binop_32(0b11101110, IOr,  0b11001010, 0b01100110);
	test_binop_64(0b11101110, IOr,  0b11001010, 0b01100110);
}

#[test]
fn test_shl() {
	use IrBinOp::*;

	test_binop_8(0x01, Shl, 0x01, 0x00);
	test_binop_8(0x02, Shl, 0x01, 0x01);
	test_binop_8(0x80, Shl, 0x01, 0x07);
	test_binop_8(0x00, Shl, 0x01, 0x08);
	test_binop_8(0xF0, Shl, 0x3F, 0x04);

	test_binop_16(0x0001, Shl, 0x01, 0x00);
	test_binop_16(0x0100, Shl, 0x01, 0x08);
	test_binop_16(0x8000, Shl, 0x01, 0x0F);
	test_binop_16(0x0000, Shl, 0x01, 0x10);
	test_binop_16(0xFF00, Shl, 0x3FFF, 0x08);

	test_binop_32(0x00000001, Shl, 0x01, 0x00);
	test_binop_32(0x00010000, Shl, 0x01, 0x10);
	test_binop_32(0x80000000, Shl, 0x01, 0x1F);
	test_binop_32(0x00000000, Shl, 0x01, 0x20);
	test_binop_32(0xABCD0000, Shl, 0x3FFFABCD, 0x10);

	test_binop_64(0x00000000_00000001, Shl, 0x01, 0x00);
	test_binop_64(0x00000001_00000000, Shl, 0x01, 0x20);
	test_binop_64(0x80000000_00000000, Shl, 0x01, 0x3F);
	test_binop_64(0x00000000_00000000, Shl, 0x01, 0x40);
	test_binop_64(0xBEEFFACE_00000000, Shl, 0xABCD1234_BEEFFACE, 0x20);
}

#[test]
fn test_ushr() {
	use IrBinOp::*;

	test_binop_8(0x01, UShr, 0x01, 0x00);
	test_binop_8(0x01, UShr, 0x02, 0x01);
	test_binop_8(0x01, UShr, 0x80, 0x07);
	test_binop_8(0x00, UShr, 0xFF, 0x08);
	test_binop_8(0x03, UShr, 0x3F, 0x04);

	test_binop_16(0x0001, UShr, 0x0001, 0x00);
	test_binop_16(0x0001, UShr, 0x0100, 0x08);
	test_binop_16(0x0001, UShr, 0x8000, 0x0F);
	test_binop_16(0x0000, UShr, 0xFFFF, 0x10);
	test_binop_16(0x003F, UShr, 0x3F0B, 0x08);

	test_binop_32(0x00000001, UShr, 0x00000001, 0x00);
	test_binop_32(0x00000001, UShr, 0x00010000, 0x10);
	test_binop_32(0x00000001, UShr, 0x80000000, 0x1F);
	test_binop_32(0x00000000, UShr, 0xFFFFFFFF, 0x20);
	test_binop_32(0x00001234, UShr, 0x1234ABCD, 0x10);

	test_binop_64(0x00000000_00000001, UShr, 0x00000000_00000001, 0x00);
	test_binop_64(0x00000000_00000001, UShr, 0x00000001_00000000, 0x20);
	test_binop_64(0x00000000_00000001, UShr, 0x80000000_00000000, 0x3F);
	test_binop_64(0x00000000_00000000, UShr, 0xFFFFFFFF_FFFFFFFF, 0x40);
	test_binop_64(0x00000000_C0DEBEEF, UShr, 0xC0DEBEEF_FACECACE, 0x20);
}

#[test]
fn test_sshr() {
	use IrBinOp::*;

	test_binop_8(0x01, SShr, 0x01, 0x00);
	test_binop_8(0x01, SShr, 0x02, 0x01);
	test_binop_8(0x00, SShr, 0x7F, 0x08);
	test_binop_8(0xFF, SShr, 0x80, 0x07);
	test_binop_8(0xFF, SShr, 0xFF, 0x08);
	test_binop_8(0x03, SShr, 0x3F, 0x04);

	test_binop_16(0x0001, SShr, 0x0001, 0x00);
	test_binop_16(0x0001, SShr, 0x0100, 0x08);
	test_binop_16(0xFFFF, SShr, 0x8000, 0x0F);
	test_binop_16(0x0000, SShr, 0x7FFF, 0x10);
	test_binop_16(0xFFFF, SShr, 0xFFFF, 0x10);
	test_binop_16(0x003F, SShr, 0x3F0B, 0x08);

	test_binop_32(0x00000001, SShr, 0x00000001, 0x00);
	test_binop_32(0x00000001, SShr, 0x00010000, 0x10);
	test_binop_32(0xFFFFFFFF, SShr, 0x80000000, 0x1F);
	test_binop_32(0x00000000, SShr, 0x7FFFFFFF, 0x20);
	test_binop_32(0xFFFFFFFF, SShr, 0xFFFFFFFF, 0x20);
	test_binop_32(0x00001234, SShr, 0x1234ABCD, 0x10);

	test_binop_64(0x00000000_00000001, SShr, 0x00000000_00000001, 0x00);
	test_binop_64(0x00000000_00000001, SShr, 0x00000001_00000000, 0x20);
	test_binop_64(0xFFFFFFFF_FFFFFFFF, SShr, 0x80000000_00000000, 0x3F);
	test_binop_64(0x00000000_00000000, SShr, 0x7FFFFFFF_FFFFFFFF, 0x40);
	test_binop_64(0xFFFFFFFF_FFFFFFFF, SShr, 0xFFFFFFFF_FFFFFFFF, 0x40);
	test_binop_64(0xFFFFFFFF_C0DEBEEF, SShr, 0xC0DEBEEF_FACECACE, 0x20);
}

#[test]
fn test_rol() {
	use IrBinOp::*;
	test_binop_8(0b00010011, Rol, 0b00010011, 0);
	test_binop_8(0b00100110, Rol, 0b00010011, 1);
	test_binop_8(0b01001100, Rol, 0b00010011, 2);
	test_binop_8(0b10011000, Rol, 0b00010011, 3);
	test_binop_8(0b00110001, Rol, 0b00010011, 4);
	test_binop_8(0b01100010, Rol, 0b00010011, 5);
	test_binop_8(0b11000100, Rol, 0b00010011, 6);
	test_binop_8(0b10001001, Rol, 0b00010011, 7);
	test_binop_8(0b00010011, Rol, 0b00010011, 8);

	test_binop_16(0b1011000100010011, Rol, 0b1011000100010011,  0);
	test_binop_16(0b0110001000100111, Rol, 0b1011000100010011,  1);
	test_binop_16(0b1100010001001110, Rol, 0b1011000100010011,  2);
	test_binop_16(0b1000100010011101, Rol, 0b1011000100010011,  3);
	test_binop_16(0b0001000100111011, Rol, 0b1011000100010011,  4);
	test_binop_16(0b0010001001110110, Rol, 0b1011000100010011,  5);
	test_binop_16(0b0100010011101100, Rol, 0b1011000100010011,  6);
	test_binop_16(0b1000100111011000, Rol, 0b1011000100010011,  7);
	test_binop_16(0b0001001110110001, Rol, 0b1011000100010011,  8);
	test_binop_16(0b0010011101100010, Rol, 0b1011000100010011,  9);
	test_binop_16(0b0100111011000100, Rol, 0b1011000100010011, 10);
	test_binop_16(0b1001110110001000, Rol, 0b1011000100010011, 11);
	test_binop_16(0b0011101100010001, Rol, 0b1011000100010011, 12);
	test_binop_16(0b0111011000100010, Rol, 0b1011000100010011, 13);
	test_binop_16(0b1110110001000100, Rol, 0b1011000100010011, 14);
	test_binop_16(0b1101100010001001, Rol, 0b1011000100010011, 15);
	test_binop_16(0b1011000100010011, Rol, 0b1011000100010011, 16);

	// if you wanna add tests for 32- and 64-bit be my guest lol
}

#[test]
fn test_ror() {
	use IrBinOp::*;
	test_binop_8(0b00010011, Ror, 0b00010011, 0);
	test_binop_8(0b10001001, Ror, 0b00010011, 1);
	test_binop_8(0b11000100, Ror, 0b00010011, 2);
	test_binop_8(0b01100010, Ror, 0b00010011, 3);
	test_binop_8(0b00110001, Ror, 0b00010011, 4);
	test_binop_8(0b10011000, Ror, 0b00010011, 5);
	test_binop_8(0b01001100, Ror, 0b00010011, 6);
	test_binop_8(0b00100110, Ror, 0b00010011, 7);
	test_binop_8(0b00010011, Ror, 0b00010011, 8);

	test_binop_16(0b1011000100010011, Ror, 0b1011000100010011,  0);
	test_binop_16(0b1101100010001001, Ror, 0b1011000100010011,  1);
	test_binop_16(0b1110110001000100, Ror, 0b1011000100010011,  2);
	test_binop_16(0b0111011000100010, Ror, 0b1011000100010011,  3);
	test_binop_16(0b0011101100010001, Ror, 0b1011000100010011,  4);
	test_binop_16(0b1001110110001000, Ror, 0b1011000100010011,  5);
	test_binop_16(0b0100111011000100, Ror, 0b1011000100010011,  6);
	test_binop_16(0b0010011101100010, Ror, 0b1011000100010011,  7);
	test_binop_16(0b0001001110110001, Ror, 0b1011000100010011,  8);
	test_binop_16(0b1000100111011000, Ror, 0b1011000100010011,  9);
	test_binop_16(0b0100010011101100, Ror, 0b1011000100010011, 10);
	test_binop_16(0b0010001001110110, Ror, 0b1011000100010011, 11);
	test_binop_16(0b0001000100111011, Ror, 0b1011000100010011, 12);
	test_binop_16(0b1000100010011101, Ror, 0b1011000100010011, 13);
	test_binop_16(0b1100010001001110, Ror, 0b1011000100010011, 14);
	test_binop_16(0b0110001000100111, Ror, 0b1011000100010011, 15);
	test_binop_16(0b1011000100010011, Ror, 0b1011000100010011, 16);

	// if you wanna add tests for 32- and 64-bit be my guest lol
}

#[test]
fn test_pair() {
	use IrBinOp::*;

	test_binop_8 (0x1234,              Pair, 0x12,       0x34,     );
	test_binop_16(0x12345678,          Pair, 0x1234,     0x5678,   );
	test_binop_32(0x12345678_ABCDEF97, Pair, 0x12345678, 0xABCDEF97);
}

#[test]
fn test_bit() {
	use IrBinOp::*;
	test_binop_8 (1, Bit, 0x15, 0);
	test_binop_8 (0, Bit, 0x15, 1);
	test_binop_8 (1, Bit, 0x15, 2);
	test_binop_8 (0, Bit, 0x15, 3);
	test_binop_8 (1, Bit, 0x15, 4);
	test_binop_8 (0, Bit, 0x15, 5);
	test_binop_8 (0, Bit, 0x15, 6);
	test_binop_8 (0, Bit, 0x15, 7);
	test_binop_16(1, Bit, 0x1500, 8,);
	test_binop_16(0, Bit, 0x1500, 9,);
	test_binop_16(1, Bit, 0x1500, 10);
	test_binop_16(0, Bit, 0x1500, 11);
	test_binop_16(1, Bit, 0x1500, 12);
	test_binop_16(0, Bit, 0x1500, 13);
	test_binop_16(0, Bit, 0x1500, 14);
	test_binop_16(0, Bit, 0x1500, 15);
	test_binop_32(1, Bit, 0x15000000, 24);
	test_binop_32(0, Bit, 0x15000000, 25);
	test_binop_32(1, Bit, 0x15000000, 26);
	test_binop_32(0, Bit, 0x15000000, 27);
	test_binop_32(1, Bit, 0x15000000, 28);
	test_binop_32(0, Bit, 0x15000000, 29);
	test_binop_32(0, Bit, 0x15000000, 30);
	test_binop_32(0, Bit, 0x15000000, 31);
	test_binop_64(1, Bit, 0x15000000_00000000, 56);
	test_binop_64(0, Bit, 0x15000000_00000000, 57);
	test_binop_64(1, Bit, 0x15000000_00000000, 58);
	test_binop_64(0, Bit, 0x15000000_00000000, 59);
	test_binop_64(1, Bit, 0x15000000_00000000, 60);
	test_binop_64(0, Bit, 0x15000000_00000000, 61);
	test_binop_64(0, Bit, 0x15000000_00000000, 62);
	test_binop_64(0, Bit, 0x15000000_00000000, 63);
}

#[test] #[should_panic]
fn test_ibit_badpos_8 () { test_binop_8 (1, IrBinOp::Bit, 0, 8); }
#[test] #[should_panic]
fn test_ibit_badpos_16() { test_binop_16(1, IrBinOp::Bit, 0, 16); }
#[test] #[should_panic]
fn test_ibit_badpos_32() { test_binop_32(1, IrBinOp::Bit, 0, 32); }
#[test] #[should_panic]
fn test_ibit_badpos_64() { test_binop_64(1, IrBinOp::Bit, 0, 64); }

#[test]
fn test_bset() {
	use IrTernOp::*;
	test_ternop_8(0xF0, BSet, 0xF0, 0, 0);
	test_ternop_8(0xF0, BSet, 0xF0, 1, 0);
	test_ternop_8(0xF0, BSet, 0xF0, 2, 0);
	test_ternop_8(0xF0, BSet, 0xF0, 3, 0);
	test_ternop_8(0xE0, BSet, 0xF0, 4, 0);
	test_ternop_8(0xD0, BSet, 0xF0, 5, 0);
	test_ternop_8(0xB0, BSet, 0xF0, 6, 0);
	test_ternop_8(0x70, BSet, 0xF0, 7, 0);

	test_ternop_8(0xF1, BSet, 0xF0, 0, 1);
	test_ternop_8(0xF2, BSet, 0xF0, 1, 1);
	test_ternop_8(0xF4, BSet, 0xF0, 2, 1);
	test_ternop_8(0xF8, BSet, 0xF0, 3, 1);
	test_ternop_8(0xF0, BSet, 0xF0, 4, 1);
	test_ternop_8(0xF0, BSet, 0xF0, 5, 1);
	test_ternop_8(0xF0, BSet, 0xF0, 6, 1);
	test_ternop_8(0xF0, BSet, 0xF0, 7, 1);

	// man I don't feel like writing 16, 32, and 64-bit tests. it fuckin works lol
}

#[test] #[should_panic]
fn test_bset_badpos_8 () { test_ternop_8 (0, IrTernOp::BSet, 0, 8, 0); }
#[test] #[should_panic]
fn test_bset_badpos_16() { test_ternop_16(0, IrTernOp::BSet, 0, 16, 0); }
#[test] #[should_panic]
fn test_bset_badpos_32() { test_ternop_32(0, IrTernOp::BSet, 0, 32, 0); }
#[test] #[should_panic]
fn test_bset_badpos_64() { test_ternop_64(0, IrTernOp::BSet, 0, 64, 0); }
#[test] #[should_panic]
fn test_bset_badsrc   () { test_ternop_8 (0, IrTernOp::BSet, 0, 0, 2); }

#[test]
fn test_bxor_band_bor() {
	use IrBinOp::*;

	test_binop_8(0, BXor, 0, 0);
	test_binop_8(1, BXor, 0, 1);
	test_binop_8(1, BXor, 1, 0);
	test_binop_8(0, BXor, 1, 1);

	test_binop_8(0, BAnd, 0, 0);
	test_binop_8(0, BAnd, 0, 1);
	test_binop_8(0, BAnd, 1, 0);
	test_binop_8(1, BAnd, 1, 1);

	test_binop_8(0, BOr, 0, 0);
	test_binop_8(1, BOr, 0, 1);
	test_binop_8(1, BOr, 1, 0);
	test_binop_8(1, BOr, 1, 1);
}

#[test]
fn test_addc() {
	use IrTernOp::*;
	test_ternop_8 (0x08, AddC, 0x03, 0x05, 0);
	test_ternop_8 (0x09, AddC, 0x03, 0x05, 1);
	test_ternop_8 (0xFF, AddC, 0xFF, 0x00, 0);
	test_ternop_8 (0x00, AddC, 0xFF, 0x00, 1);
	test_ternop_16(0x0008, AddC, 0x0003, 0x0005, 0);
	test_ternop_16(0x0009, AddC, 0x0003, 0x0005, 1);
	test_ternop_32(0x00000008, AddC, 0x00000003, 0x00000005, 0);
	test_ternop_32(0x00000009, AddC, 0x00000003, 0x00000005, 1);
	test_ternop_64(0x00000000_00000008, AddC, 0x00000000_00000003, 0x00000000_00000005, 0);
	test_ternop_64(0x00000000_00000009, AddC, 0x00000000_00000003, 0x00000000_00000005, 1);

	test_ternop_8 (0x02, AddC, 0xFD, 0x05, 0);
	test_ternop_8 (0x03, AddC, 0xFD, 0x05, 1);
	test_ternop_16(0x0002, AddC, 0xFFFD, 0x0005, 0);
	test_ternop_16(0x0003, AddC, 0xFFFD, 0x0005, 1);
	test_ternop_32(0x00000002, AddC, 0xFFFFFFFD, 0x00000005, 0);
	test_ternop_32(0x00000003, AddC, 0xFFFFFFFD, 0x00000005, 1);
	test_ternop_64(0x00000000_00000002, AddC, 0xFFFFFFFF_FFFFFFFD, 0x00000000_00000005, 0);
	test_ternop_64(0x00000000_00000003, AddC, 0xFFFFFFFF_FFFFFFFD, 0x00000000_00000005, 1);
}

#[test]
fn test_subb() {
	use IrTernOp::*;
	test_ternop_8 (0x03, SubB, 0x08, 0x05, 0);
	test_ternop_8 (0x02, SubB, 0x08, 0x05, 1);
	test_ternop_16(0x0003, SubB, 0x0008, 0x0005, 0);
	test_ternop_16(0x0002, SubB, 0x0008, 0x0005, 1);
	test_ternop_32(0x00000003, SubB, 0x00000008, 0x00000005, 0);
	test_ternop_32(0x00000002, SubB, 0x00000008, 0x00000005, 1);
	test_ternop_64(0x00000000_00000003, SubB, 0x00000000_00000008, 0x00000000_00000005, 0);
	test_ternop_64(0x00000000_00000002, SubB, 0x00000000_00000008, 0x00000000_00000005, 1);

	test_ternop_8 (0xFE, SubB, 0x03, 0x05, 0);
	test_ternop_8 (0xFD, SubB, 0x03, 0x05, 1);
	test_ternop_16(0xFFFE, SubB, 0x0003, 0x0005, 0);
	test_ternop_16(0xFFFD, SubB, 0x0003, 0x0005, 1);
	test_ternop_32(0xFFFFFFFE, SubB, 0x00000003, 0x00000005, 0);
	test_ternop_32(0xFFFFFFFD, SubB, 0x00000003, 0x00000005, 1);
	test_ternop_64(0xFFFFFFFF_FFFFFFFE, SubB, 0x00000000_00000003, 0x00000000_00000005, 0);
	test_ternop_64(0xFFFFFFFF_FFFFFFFD, SubB, 0x00000000_00000003, 0x00000000_00000005, 1);
}

#[test]
fn test_ucarryc() {
	use IrTernOp::*;
	test_ternop_8 (0, UCarryC, 0x08, 0x05, 0);
	test_ternop_16(0, UCarryC, 0x0008, 0x0005, 0);
	test_ternop_32(0, UCarryC, 0x00000008, 0x00000005, 0);
	test_ternop_64(0, UCarryC, 0x00000000_00000008, 0x00000000_00000005, 0);

	test_ternop_8(0, UCarryC, 0x7F, 0x01, 0);
	test_ternop_8(0, UCarryC, 0x7F, 0x00, 1);
	test_ternop_8(0, UCarryC, 0x7F, 0x01, 1);

	test_ternop_8(0, UCarryC, 0xFF, 0x00, 0);
	test_ternop_8(1, UCarryC, 0xFF, 0x01, 0);
	test_ternop_8(1, UCarryC, 0xFF, 0x00, 1);
	test_ternop_8(1, UCarryC, 0xFF, 0x01, 1);

	test_ternop_8(0, UCarryC, 0xFE, 0x00, 0);
	test_ternop_8(0, UCarryC, 0xFE, 0x00, 1);
	test_ternop_8(0, UCarryC, 0xFE, 0x01, 0);
	test_ternop_8(1, UCarryC, 0xFE, 0x01, 1);

	test_ternop_16(0, UCarryC, 0x00FF, 0x0001, 0);
	test_ternop_32(0, UCarryC, 0x000000FF, 0x00000001, 0);
	test_ternop_64(0, UCarryC, 0x00000000_000000FF, 0x00000000_00000001, 0);

	test_ternop_16(0, UCarryC, 0x7FFF, 0x0001, 0);
	test_ternop_16(0, UCarryC, 0x7FFF, 0x0000, 1);
	test_ternop_16(0, UCarryC, 0x7FFF, 0x0001, 1);

	test_ternop_16(0, UCarryC, 0xFFFF, 0x0000, 0);
	test_ternop_16(1, UCarryC, 0xFFFF, 0x0001, 0);
	test_ternop_16(1, UCarryC, 0xFFFF, 0x0000, 1);
	test_ternop_16(1, UCarryC, 0xFFFF, 0x0001, 1);

	test_ternop_16(0, UCarryC, 0xFFFE, 0x0000, 0);
	test_ternop_16(0, UCarryC, 0xFFFE, 0x0001, 0);
	test_ternop_16(0, UCarryC, 0xFFFE, 0x0000, 1);
	test_ternop_16(1, UCarryC, 0xFFFE, 0x0001, 1);

	test_ternop_32(0, UCarryC, 0x0000FFFF, 0x00000001, 0);
	test_ternop_64(0, UCarryC, 0x00000000_0000FFFF, 0x00000000_00000001, 0);

	test_ternop_32(0, UCarryC, 0x7FFFFFFF, 0x00000001, 0);
	test_ternop_32(0, UCarryC, 0x7FFFFFFF, 0x00000000, 1);
	test_ternop_32(0, UCarryC, 0x7FFFFFFF, 0x00000001, 1);

	test_ternop_32(0, UCarryC, 0xFFFFFFFF, 0x00000000, 0);
	test_ternop_32(1, UCarryC, 0xFFFFFFFF, 0x00000001, 0);
	test_ternop_32(1, UCarryC, 0xFFFFFFFF, 0x00000000, 1);
	test_ternop_32(1, UCarryC, 0xFFFFFFFF, 0x00000001, 1);

	test_ternop_32(0, UCarryC, 0xFFFFFFFE, 0x00000000, 0);
	test_ternop_32(0, UCarryC, 0xFFFFFFFE, 0x00000001, 0);
	test_ternop_32(0, UCarryC, 0xFFFFFFFE, 0x00000000, 1);
	test_ternop_32(1, UCarryC, 0xFFFFFFFE, 0x00000001, 1);

	test_ternop_64(0, UCarryC, 0x00000000_FFFFFFFF, 0x00000000_00000001, 0);

	test_ternop_64(0, UCarryC, 0x7FFFFFFF_FFFFFFFF, 0x00000000_00000001, 0);
	test_ternop_64(0, UCarryC, 0x7FFFFFFF_FFFFFFFF, 0x00000000_00000000, 1);
	test_ternop_64(0, UCarryC, 0x7FFFFFFF_FFFFFFFF, 0x00000000_00000001, 1);

	test_ternop_64(0, UCarryC, 0xFFFFFFFF_FFFFFFFF, 0x00000000_00000000, 0);
	test_ternop_64(1, UCarryC, 0xFFFFFFFF_FFFFFFFF, 0x00000000_00000001, 0);
	test_ternop_64(1, UCarryC, 0xFFFFFFFF_FFFFFFFF, 0x00000000_00000000, 1);
	test_ternop_64(1, UCarryC, 0xFFFFFFFF_FFFFFFFF, 0x00000000_00000001, 1);

	test_ternop_64(0, UCarryC, 0xFFFFFFFF_FFFFFFFE, 0x00000000_00000000, 0);
	test_ternop_64(0, UCarryC, 0xFFFFFFFF_FFFFFFFE, 0x00000000_00000001, 0);
	test_ternop_64(0, UCarryC, 0xFFFFFFFF_FFFFFFFE, 0x00000000_00000000, 1);
	test_ternop_64(1, UCarryC, 0xFFFFFFFF_FFFFFFFE, 0x00000000_00000001, 1);
}

#[test]
fn test_scarryc() {
	use IrTernOp::*;
	test_ternop_8 (0, SCarryC, 0x08, 0x05, 0);
	test_ternop_16(0, SCarryC, 0x0008, 0x0005, 0);
	test_ternop_32(0, SCarryC, 0x00000008, 0x00000005, 0);
	test_ternop_64(0, SCarryC, 0x00000000_00000008, 0x00000000_00000005, 0);

	test_ternop_8(0, SCarryC, 0x7F, 0x00, 0);
	test_ternop_8(1, SCarryC, 0x7F, 0x01, 0);
	test_ternop_8(1, SCarryC, 0x7F, 0x00, 1);
	test_ternop_8(1, SCarryC, 0x7E, 0x01, 1);

	test_ternop_8(0, SCarryC, 0x80, 0x00, 0);
	test_ternop_8(0, SCarryC, 0x80, 0x00, 1);
	test_ternop_8(1, SCarryC, 0x80, 0xFF, 0);
	// despite it doing "x + -1 + 1" there IS still a carry here. the 6502 at least agrees.
	test_ternop_8(1, SCarryC, 0x80, 0xFF, 1);

	test_ternop_16(0, SCarryC, 0x80, 0xFF, 0);
	test_ternop_16(0, SCarryC, 0x80, 0xFF, 1);
	test_ternop_16(1, SCarryC, 0x8000, 0xFFFF, 0);
	test_ternop_16(1, SCarryC, 0x8000, 0xFFFF, 1);
	test_ternop_32(0, SCarryC, 0x8000, 0xFFFF, 0);
	test_ternop_32(0, SCarryC, 0x8000, 0xFFFF, 1);
	test_ternop_32(1, SCarryC, 0x80000000, 0xFFFFFFFF, 0);
	test_ternop_32(1, SCarryC, 0x80000000, 0xFFFFFFFF, 1);
	test_ternop_64(0, SCarryC, 0x80000000, 0xFFFFFFFF, 0);
	test_ternop_64(0, SCarryC, 0x80000000, 0xFFFFFFFF, 1);
	test_ternop_64(1, SCarryC, 0x80000000_00000000, 0xFFFFFFFF_FFFFFFFF, 0);
	test_ternop_64(1, SCarryC, 0x80000000_00000000, 0xFFFFFFFF_FFFFFFFF, 1);

	test_ternop_8 (0, SCarryC, 0xFE, 0x05, 0);
	test_ternop_16(0, SCarryC, 0xFFFE, 0x0005, 0);
	test_ternop_32(0, SCarryC, 0xFFFFFFFE, 0x00000005, 0);
	test_ternop_64(0, SCarryC, 0xFFFFFFFF_FFFFFFFE, 0x00000000_00000005, 0);

	test_ternop_8 (1, SCarryC, 0x7F, 0x01, 0);
	test_ternop_8 (1, SCarryC, 0x7F, 0x00, 1);
	test_ternop_8 (1, SCarryC, 0x80, 0xFF, 0);
	test_ternop_8 (0, SCarryC, 0xFF, 0x01, 0);
	test_ternop_8 (0, SCarryC, 0xFF, 0x00, 1);
	test_ternop_16(0, SCarryC, 0x00FF, 0x0001, 0);
	test_ternop_16(0, SCarryC, 0x00FF, 0x0000, 1);
	test_ternop_32(0, SCarryC, 0x000000FF, 0x00000001, 0);
	test_ternop_32(0, SCarryC, 0x000000FF, 0x00000000, 1);
	test_ternop_64(0, SCarryC, 0x00000000_000000FF, 0x00000000_00000001, 0);
	test_ternop_64(0, SCarryC, 0x00000000_000000FF, 0x00000000_00000000, 1);

	test_ternop_16(1, SCarryC, 0x7FFF, 0x0001, 0);
	test_ternop_16(1, SCarryC, 0x7FFF, 0x0000, 1);
	test_ternop_16(1, SCarryC, 0x8000, 0xFFFF, 0);
	test_ternop_16(0, SCarryC, 0xFFFF, 0x0001, 0);
	test_ternop_16(0, SCarryC, 0xFFFF, 0x0000, 1);
	test_ternop_32(0, SCarryC, 0x0000FFFF, 0x00000001, 0);
	test_ternop_32(0, SCarryC, 0x0000FFFF, 0x00000000, 1);
	test_ternop_64(0, SCarryC, 0x00000000_0000FFFF, 0x00000000_00000001, 0);
	test_ternop_64(0, SCarryC, 0x00000000_0000FFFF, 0x00000000_00000000, 1);

	test_ternop_32(1, SCarryC, 0x7FFFFFFF, 0x00000001, 0);
	test_ternop_32(1, SCarryC, 0x7FFFFFFF, 0x00000000, 1);
	test_ternop_32(1, SCarryC, 0x80000000, 0xFFFFFFFF, 0);
	test_ternop_32(0, SCarryC, 0xFFFFFFFF, 0x00000001, 0);
	test_ternop_32(0, SCarryC, 0xFFFFFFFF, 0x00000000, 1);
	test_ternop_64(0, SCarryC, 0x00000000_FFFFFFFF, 0x00000000_00000001, 0);
	test_ternop_64(0, SCarryC, 0x00000000_FFFFFFFF, 0x00000000_00000000, 1);

	test_ternop_64(1, SCarryC, 0x7FFFFFFF_FFFFFFFF, 0x00000000_00000001, 0);
	test_ternop_64(1, SCarryC, 0x7FFFFFFF_FFFFFFFF, 0x00000000_00000000, 1);
	test_ternop_64(1, SCarryC, 0x80000000_00000000, 0xFFFFFFFF_FFFFFFFF, 0);
	test_ternop_64(0, SCarryC, 0xFFFFFFFF_FFFFFFFF, 0x00000000_00000001, 0);
	test_ternop_64(0, SCarryC, 0xFFFFFFFF_FFFFFFFF, 0x00000000_00000000, 1);
}

// I'll be honest I'm running out of steam here

#[test]
fn test_sborrowb() {
	use IrTernOp::*;
	test_ternop_8 (0, SBorrowB, 0x08, 0x05, 0);
	test_ternop_8 (0, SBorrowB, 0x08, 0x05, 1);
	test_ternop_16(0, SBorrowB, 0x0008, 0x0005, 0);
	test_ternop_16(0, SBorrowB, 0x0008, 0x0005, 1);
	test_ternop_32(0, SBorrowB, 0x00000008, 0x00000005, 0);
	test_ternop_32(0, SBorrowB, 0x00000008, 0x00000005, 1);
	test_ternop_64(0, SBorrowB, 0x00000000_00000008, 0x00000000_00000005, 0);
	test_ternop_64(0, SBorrowB, 0x00000000_00000008, 0x00000000_00000005, 1);

	test_ternop_8 (0, SBorrowB, 0xFE, 0x05, 0);
	test_ternop_8 (0, SBorrowB, 0xFE, 0x05, 1);
	test_ternop_16(0, SBorrowB, 0xFFFE, 0x0005, 0);
	test_ternop_16(0, SBorrowB, 0xFFFE, 0x0005, 1);
	test_ternop_32(0, SBorrowB, 0xFFFFFFFE, 0x00000005, 0);
	test_ternop_32(0, SBorrowB, 0xFFFFFFFE, 0x00000005, 1);
	test_ternop_64(0, SBorrowB, 0xFFFFFFFF_FFFFFFFE, 0x00000000_00000005, 0);
	test_ternop_64(0, SBorrowB, 0xFFFFFFFF_FFFFFFFE, 0x00000000_00000005, 1);

	test_ternop_8 (1, SBorrowB, 0x7F, 0xFF, 0);
	test_ternop_8 (1, SBorrowB, 0x80, 0x01, 0);
	test_ternop_8 (1, SBorrowB, 0x80, 0x00, 1);
	test_ternop_8 (0, SBorrowB, 0xFF, 0xFF, 0);
	test_ternop_16(0, SBorrowB, 0x00FF, 0x00FF, 0);
	test_ternop_32(0, SBorrowB, 0x000000FF, 0x000000FF, 0);
	test_ternop_64(0, SBorrowB, 0x00000000_000000FF, 0x00000000_000000FF, 0);

	test_ternop_16(1, SBorrowB, 0x7FFF, 0xFFFF, 0);
	test_ternop_16(1, SBorrowB, 0x8000, 0x0001, 0);
	test_ternop_16(1, SBorrowB, 0x8000, 0x0000, 1);
	test_ternop_16(0, SBorrowB, 0xFFFF, 0xFFFF, 0);
	test_ternop_32(0, SBorrowB, 0x0000FFFF, 0x0000FFFF, 0);
	test_ternop_64(0, SBorrowB, 0x00000000_0000FFFF, 0x00000000_0000FFFF, 0);

	test_ternop_32(1, SBorrowB, 0x7FFFFFFF, 0xFFFFFFFF, 0);
	test_ternop_32(1, SBorrowB, 0x80000000, 0x00000001, 0);
	test_ternop_32(1, SBorrowB, 0x80000000, 0x00000000, 1);
	test_ternop_32(0, SBorrowB, 0xFFFFFFFF, 0xFFFFFFFF, 0);
	test_ternop_64(0, SBorrowB, 0x00000000_FFFFFFFF, 0x00000000_FFFFFFFF, 0);

	test_ternop_64(1, SBorrowB, 0x7FFFFFFF_FFFFFFFF, 0xFFFFFFFF_FFFFFFFF, 0);
	test_ternop_64(1, SBorrowB, 0x80000000_00000000, 0x00000000_00000001, 0);
	test_ternop_64(1, SBorrowB, 0x80000000_00000000, 0x00000000_00000000, 1);
	test_ternop_64(0, SBorrowB, 0xFFFFFFFF_FFFFFFFF, 0xFFFFFFFF_FFFFFFFF, 0);
}

// TODO: tests for carriesc/borrowsb, I don't feel like it