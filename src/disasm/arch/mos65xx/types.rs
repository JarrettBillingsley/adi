
// ------------------------------------------------------------------------------------------------
// AddrMode
// ------------------------------------------------------------------------------------------------

/// All 65xx addressing modes.
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum AddrMode {
	/// Implied (no operand bytes), e.g. `rol`.
	IMP,
	/// Immediate (1 byte), e.g. `lda #$30`.
	IMM,

	/// Zero-page absolute (1 byte), e.g. `lda $10`.
	ZPG,
	/// Zero-page, X-indexed (1 byte), e.g. `lda $80,X`.
	ZPX,
	/// Zero-page, Y-indexed (1 byte), e.g. `lda $80,Y`.
	ZPY,

	/// Absolute (2 bytes), e.g. `lda $8040`.
	ABS,
	/// Absolute, X-indexed (2 bytes), e.g. `lda $8040,X`
	ABX,
	/// Absolute, Y-indexed (2 bytes), e.g. `lda $8040,Y`
	ABY,

	/// Indirect (2 bytes); used only for indirect jump i.e. `jmp ($2000)`.
	IND,
	/// "Indexed Indirect" - double-indirect zero-page X-indexed (1 byte), e.g. `lda ($10,X)`.
	/// Loads a 2-byte address from `X + offset`, then accesses the byte at that address.
	IZX, // ($11,X) where $11 is ZPGaddr
	/// "Indirect Indexed" - double-indirect zero-page Y-indexed (1 byte), e.g. `lda ($10),Y`.
	/// Loads a 2-byte address from `offset`, then accesses the byte at `Y + address`.
	IZY, // ($11),Y where $11 is ZPGaddr

	/// PC-relative (1 byte), e.g. `bcc whatever`.
	/// Signed offset added to PC (+2 for size of branch instruction).
	REL,
	/// Alias for `ABS` but for `jmp`/`jsr` instructions, to distinguish their operand types.
	LAB,
}

impl AddrMode {
	/// How many operand bytes are needed for this mode?
	pub fn op_bytes(&self) -> usize {
		use AddrMode::*;
		match self {
			IMP => 0,
			IZX | IZY | ZPG | ZPX | ZPY | IMM | REL => 1,
			ABS | ABX | ABY | IND | LAB => 2,
		}
	}
}

// ------------------------------------------------------------------------------------------------
// MetaOp
// ------------------------------------------------------------------------------------------------

/// The "fundamental operation" that an instruction performs, regardless of addressing mode.
/// Each meta-op can cover multiple real instructions with different addressing modes.
/// These correspond (mostly) with what a programmer would type.
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum MetaOp {
	UNK,
	ADC, AND,  ASLA, ASL,  BCC,  BCS,  BEQ,  BIT, BMI, BNE,
	BPL, BRK,  BVC,  BVS,  CLC,  CLD,  CLI,  CLV, CMP, CPX,
	CPY, DEC,  DEX,  DEY,  EOR,  INC,  INX,  INY, JMP, JSR,
	LDA, LDAI, LDX,  LDXI, LDY,  LDYI, LSRA, LSR, NOP, ORA,
	PHA, PHP,  PLA,  PLP,  ROLA, ROL,  RORA, ROR, RTI, RTS,
	SBC, SEC,  SED,  SEI,  STA,  STX,  STY,  TAX, TAY, TSX,
	TXA, TXS,  TYA,
}

impl MetaOp {
	/// The traditional mnemonics, but not so screamy.
	pub fn mnemonic_old(&self) -> &'static str {
		use MetaOp::*;
		match self {
			UNK  => "???",
			ADC  => "adc", AND  => "and", ASLA => "asl", ASL  => "asl",
			BCC  => "bcc", BCS  => "bcs", BEQ  => "beq", BIT  => "bit",
			BMI  => "bmi", BNE  => "bne", BPL  => "bpl", BRK  => "brk",
			BVC  => "bvc", BVS  => "bvs", CLC  => "clc", CLD  => "cld",
			CLI  => "cli", CLV  => "clv", CMP  => "cmp", CPX  => "cpx",
			CPY  => "cpy", DEC  => "dec", DEX  => "dex", DEY  => "dey",
			EOR  => "eor", INC  => "inc", INX  => "inx", INY  => "iny",
			JMP  => "jmp", JSR  => "jsr", LDA  => "lda", LDAI => "lda",
			LDX  => "ldx", LDXI => "ldx", LDY  => "ldy", LDYI => "ldy",
			LSRA => "lsr", LSR  => "lsr", NOP  => "nop", ORA  => "ora",
			PHA  => "pha", PHP  => "php", PLA  => "pla", PLP  => "plp",
			ROLA => "rol", ROL  => "rol", RORA => "ror", ROR  => "ror",
			RTI  => "rti", RTS  => "rts", SBC  => "sbc", SEC  => "sec",
			SED  => "sed", SEI  => "sei", STA  => "sta", STX  => "stx",
			STY  => "sty", TAX  => "tax", TAY  => "tay", TSX  => "tsx",
			TXA  => "txa", TXS  => "txs", TYA  => "tya",
		}
	}

	/// My more "modern" mnemonics. Get those damn registers out of the mnemonics!
	/// Destination on the left!!
	pub fn mnemonic_new(&self) -> &'static str {
		use MetaOp::*;
		match self {
			UNK  => "???",
			ADC  => "adc a,",   AND  => "and a,",   ASLA => "shl a",    ASL  => "shl",
			BCC  => "bcc",      BCS  => "bcs",      BEQ  => "beq",      BIT  => "bit",
			BMI  => "bmi",      BNE  => "bne",      BPL  => "bpl",      BRK  => "brk",
			BVC  => "bvc",      BVS  => "bvs",      CLC  => "clr c",    CLD  => "clr d",
			CLI  => "clr i",    CLV  => "clr v",    CMP  => "cmp a,",   CPX  => "cmp x,",
			CPY  => "cmp y,",   DEC  => "dec",      DEX  => "dec x",    DEY  => "dec y",
			EOR  => "xor a,",   INC  => "inc",      INX  => "inc x",    INY  => "inc y",
			JMP  => "jmp",      JSR  => "jsr",      LDA  => "ld  a,",   LDAI => "li  a,",
			LDX  => "ld  x,",   LDXI => "li  x,",   LDY  => "ld  y,",   LDYI => "li  y,",
			LSRA => "shr a",    LSR  => "shr",      NOP  => "nop",      ORA  => "or  a,",
			PHA  => "psh a",    PHP  => "psh p",    PLA  => "pul a",    PLP  => "pul p",
			ROLA => "rol a",    ROL  => "rol",      RORA => "ror a",    ROR  => "ror",
			RTI  => "rti",      RTS  => "rts",      SBC  => "sbc a,",   SEC  => "set c",
			SED  => "set d",    SEI  => "set i",    STA  => "st  a,",   STX  => "st  x,",
			STY  => "st  y,",   TAX  => "mov x, a", TAY  => "mov y, a", TSX  => "mov x, s",
			TXA  => "mov a, x", TXS  => "mov s, x", TYA  => "mov a, y",
		}
	}
}

// ------------------------------------------------------------------------------------------------
// Reg
// ------------------------------------------------------------------------------------------------

/// 65xx registers.
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum Reg {
	A, X, Y, S, P
}

impl Default for Reg {
	fn default() -> Reg { Reg::A }
}
