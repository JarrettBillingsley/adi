#![allow(dead_code)]
#![allow(unused_imports)]

use std::fmt::Write;

use better_panic::{ Settings as PanicSettings, Verbosity as PanicVerbosity };
use simplelog::*;
use log::*;
use colored::*;

use colored::Color;

use adi::*;

fn main() -> Result<(), Box<dyn std::error::Error>> {
	// setup_logging(LevelFilter::Trace)?;
	setup_panic();
	// test_gb()?;
	// test_nes()?;
	test_toy()?;
	Ok(())
}

fn setup_logging(max_level: LevelFilter) -> Result<(), SetLoggerError> {
	let log_config = ConfigBuilder::new()
		.set_level_color(Level::Info, simplelog::Color::Green)
		.set_level_color(Level::Debug, simplelog::Color::Blue)
		.set_level_color(Level::Trace, simplelog::Color::Cyan)
		.set_time_level(LevelFilter::Off)
		.set_thread_level(LevelFilter::Off)
		.set_target_level(LevelFilter::Off)
		.set_location_level(LevelFilter::Debug)
		.build();
	TermLogger::init(max_level, log_config, TerminalMode::Mixed)
}

fn setup_panic() {
	PanicSettings::new()
		.lineno_suffix(true)
		// .most_recent_first(false)
		.verbosity(PanicVerbosity::Full)
	.install();
}

// ------------------------------------------------------------------------------------------------

fn toy_test_all_instructions() -> Vec<u8> {
	use adi::arch::toy::{ Reg, ToyBuilder };
	use Reg::*;

	let mut b = ToyBuilder::new();
	b.movi(A, 0xBE);
	b.mov(D, A);
	b.add(A, D);
	b.addi(B, 0x30);
	b.adc(B, C);
	b.adci(C, 1);
	b.sub(A, D);
	b.subi(B, 0x30);
	b.sbc(B, C);
	b.sbci(C, 1);
	b.and(A, D);
	b.andi(B, 0x30);
	b.or(B, C);
	b.ori(C, 1);
	b.xor(A, D);
	b.xori(B, 0x30);
	b.not(B, C);
	b.noti(C, 1);
	let branch_dest = b.cmp(A, D);
	b.cmpi(B, 0x30);
	b.cmc(B, C);
	b.cmci(C, 1);
	b.blt_to(branch_dest);
	b.ble_to(branch_dest);
	b.beq_to(branch_dest);
	b.bne_to(branch_dest);
	let jmp = b.jmp();
	b.jump_here(jmp);
	b.cal_to(0x7FFE);
	b.ldi(C, 0x8000);
	b.ld(A, DC);
	b.sti(C, 0x8000);
	b.st(A, A);
	b.ret();

	b.org(0x7FFE);
	b.movi(Reg::A, 10);

	b.finish()
}

fn toy_test_ssa() -> Vec<u8> {
	use adi::arch::toy::{ Reg, ToyBuilder };
	use Reg::*;

	let mut b = ToyBuilder::new();

	// bb0
	b.movi(D, 0);

	// bb1
	let bb1 = b.ldi(D, 0x8000);
	b.cmpi(D, 0);
	let bb1_branch = b.beq();

	// bb2
	b.movi(B, 0);
	b.movi(A, 0);
	let bb2_jump = b.jmp();

	// bb3
	b.branch_here(bb1_branch);
	b.mov(C, A);
	b.mov(A, B);
	b.mov(B, C);
	b.cmpi(D, 1);
	let bb3_branch = b.beq();

	// bb4
	b.jump_here(bb2_jump);
	b.sti(A, 0x8000);
	b.sti(B, 0x8001);
	let bb4_cal = b.cal();
	b.ldi(A, 0x8002);
	b.cmpi(A, 1);
	b.beq_to(bb1);

	// bb5
	b.branch_here(bb3_branch);
	b.sti(A, 0x8000);
	b.ret();

	// f
	b.org(0x40);
	b.jump_here(bb4_cal);
	b.ret();

	b.finish()
}

fn toy_test_const_prop() -> Vec<u8> {
	use adi::arch::toy::{ Reg, ToyBuilder };
	use Reg::*;

	let mut b = ToyBuilder::new();
	b.movi(A, 0x80);
	b.movi(B, 0xE7);
	b.mov(C, B);
	b.mov(D, A);
	b.ld(A, DC);

	b.movi(A, 0x10);
	b.addi(A, 0x20);
	b.movi(B, 0x01);
	b.addi(B, 0x02);
	b.mov(C, A);
	b.or(C, B);
	b.mov(D, C);
	b.ld(A, DC);

	b.ldi(A, 0x8000);
	b.sti(A, 0x8001);

	b.ret();

	b.finish()
}

fn toy_test_calls() -> Vec<u8> {
	use adi::arch::toy::{ Reg, ToyBuilder };
	use Reg::*;

	let mut b = ToyBuilder::new();
	b.movi(A, 0x30);
	b.cal_to(0x10);
	b.sti(A, 0x8000);
	b.ret();

	b.org(0x10);
	b.addi(A, 3);
	b.ret();

	b.finish()
}

fn toy_test_loop() -> Vec<u8> {
	use adi::arch::toy::{ Reg, ToyBuilder };
	use Reg::*;

	let mut b = ToyBuilder::new();
	b.movi(B, 10);
	b.movi(C, 13);

	let loop_top = b.cmpi(B, 0);
	let loop_cond = b.beq();
		b.movi(C, 13);

		b.subi(B, 1);
		b.jmp_to(loop_top);
	b.branch_here(loop_cond);

	b.mov(A, C);
	b.ret();

	b.finish()
}

fn test_toy() -> Result<(), Box<dyn std::error::Error>> {
	let img_data = toy_test_all_instructions();
	// let img_data = toy_test_ssa();
	// let img_data = toy_test_const_prop();
	// let img_data = toy_test_calls();
	// let img_data = toy_test_loop();

	let img = Image::new("<toy test>", &img_data);
	let mut prog = program_from_image(img)?;
	println!("{}", prog);

	let state = prog.initial_mmu_state();
	let reset_ea = prog.ea_from_va(state, VA(0));

	prog.enqueue_function(state, reset_ea);
	prog.analyze_queue();

	println!("found {} functions.", prog.all_funcs().count());

	for segid in prog.all_image_segs() {
		show_segment(&prog, segid);
	}

	Ok(())
}

// ------------------------------------------------------------------------------------------------

fn test_gb() -> Result<(), Box<dyn std::error::Error>> {
	let img = Image::new_from_file("tests/data/tetris.gb")?;
	let mut prog = program_from_image(img)?;
	println!("{}", prog);

	let state = prog.initial_mmu_state();
	prog.enqueue_function(state, prog.ea_from_name("RESET"));
	prog.analyze_queue();

	println!("found {} functions.", prog.all_funcs().count());

	for segid in prog.all_image_segs() {
		show_segment(&prog, segid);
	}

	Ok(())
}

// ------------------------------------------------------------------------------------------------

fn test_nes() -> Result<(), Box<dyn std::error::Error>> {
	let img = Image::new_from_file("tests/data/megaman.nes")?;
	let mut prog = program_from_image(img)?;

	println!("{}", prog);

	// find_identical_image_pieces(&prog);

	let state = prog.initial_mmu_state();
	prog.enqueue_function(state, prog.ea_from_name("VEC_RESET"));
	prog.enqueue_function(state, prog.ea_from_name("VEC_NMI"));

	if let Some(ea) = prog.ea_for_name("VEC_IRQ") {
		prog.enqueue_function(state, ea);
	}

	// BATTLETOAAADS
	// prog.enqueue_function(state, prog.ea_from_va(state, VA(0x8003)));
	// prog.enqueue_function(state, prog.ea_from_va(state, VA(0x8006)));
	// prog.enqueue_function(state, prog.ea_from_va(state, VA(0x8009)));
	// prog.enqueue_function(state, prog.ea_from_va(state, VA(0x800C)));
	// prog.enqueue_function(state, prog.ea_from_va(state, VA(0x800F)));
	// prog.enqueue_function(state, prog.ea_from_va(state, VA(0x8012)));
	// prog.enqueue_function(state, prog.ea_from_va(state, VA(0x8015)));
	// prog.enqueue_function(state, prog.ea_from_va(state, VA(0x8018)));
	// prog.enqueue_function(state, prog.ea_from_va(state, VA(0x801B)));
	// prog.enqueue_function(state, prog.ea_from_va(state, VA(0x801E)));
	// prog.enqueue_function(state, prog.ea_from_va(state, VA(0x8021)));
	// prog.enqueue_function(state, prog.ea_from_va(state, VA(0x8024)));
	// prog.enqueue_function(state, prog.ea_from_va(state, VA(0x8027)));
	// prog.enqueue_function(state, prog.ea_from_va(state, VA(0x802A)));
	// prog.enqueue_function(state, prog.ea_from_va(state, VA(0x802D)));
	// prog.enqueue_function(state, prog.ea_from_va(state, VA(0x8030)));
	// prog.enqueue_function(state, prog.ea_from_va(state, VA(0x8085)));
	// prog.enqueue_function(state, prog.ea_from_va(state, VA(0x80F7)));
	// prog.enqueue_function(state, prog.ea_from_va(state, VA(0x831F)));
	// prog.enqueue_function(state, prog.ea_from_va(state, VA(0x84E8)));
	// prog.enqueue_function(state, prog.ea_from_va(state, VA(0x857A)));
	// prog.enqueue_function(state, prog.ea_from_va(state, VA(0x86DE)));
	// prog.enqueue_function(state, prog.ea_from_va(state, VA(0x87A3)));
	// prog.enqueue_function(state, prog.ea_from_va(state, VA(0x87C2)));
	// prog.enqueue_function(state, prog.ea_from_va(state, VA(0x884B)));
	// prog.enqueue_function(state, prog.ea_from_va(state, VA(0x88EF)));
	// prog.enqueue_function(state, prog.ea_from_va(state, VA(0x8DC1)));
	// prog.enqueue_function(state, prog.ea_from_va(state, VA(0x9150)));
	// prog.enqueue_function(state, prog.ea_from_va(state, VA(0x9200)));
	// prog.enqueue_function(state, prog.ea_from_va(state, VA(0x923E)));
	// prog.enqueue_function(state, prog.ea_from_va(state, VA(0x9252)));
	// prog.enqueue_function(state, prog.ea_from_va(state, VA(0x930A)));
	// prog.enqueue_function(state, prog.ea_from_va(state, VA(0x93C8)));
	// prog.enqueue_function(state, prog.ea_from_va(state, VA(0x944E)));
	// prog.enqueue_function(state, prog.ea_from_va(state, VA(0x95E8)));
	// prog.enqueue_function(state, prog.ea_from_va(state, VA(0x95EB)));
	// prog.enqueue_function(state, prog.ea_from_va(state, VA(0x9643)));
	// prog.enqueue_function(state, prog.ea_from_va(state, VA(0x9E4E)));
	// prog.enqueue_function(state, prog.ea_from_va(state, VA(0x9ED2)));
	// prog.enqueue_function(state, prog.ea_from_va(state, VA(0xA51E)));
	// prog.enqueue_function(state, prog.ea_from_va(state, VA(0xA521)));
	// prog.enqueue_function(state, prog.ea_from_va(state, VA(0xA560)));
	// prog.enqueue_function(state, prog.ea_from_va(state, VA(0xB06B)));
	// let ea   = prog.ea_from_va(state, VA(0xFFB3));
	// let ty   = Type::array(Type::U8, 24);
	// let size = ty.size().fixed();
	// prog.new_data(Some("BANK_CHANGE"), ea, ty, size);

	// MAYROOOOOO
	// prog.enqueue_function(state, prog.ea_from_va(state, VA(0x8231)));
	// prog.enqueue_function(state, prog.ea_from_va(state, VA(0x838B)));
	// prog.enqueue_function(state, prog.ea_from_va(state, VA(0x9218)));
	// prog.enqueue_function(state, prog.ea_from_va(state, VA(0xAEDC)));
	// let ea   = prog.ea_from_va(state, VA(0x821A));
	// let ty   = Type::array(Type::ptr(Type::Code, Type::U16), 3);
	// prog.new_data(Some("array"), ea, ty, ty.size().fixed());

	let ty = Type::ptr(Type::Code, Type::U16);
	prog.new_data(Some("VEC_NMI_PTR"),   prog.ea_from_va(state, VA(0xFFFA)), ty.clone(), 2);
	prog.new_data(Some("VEC_RESET_PTR"), prog.ea_from_va(state, VA(0xFFFC)), ty.clone(), 2);
	prog.new_data(Some("VEC_IRQ_PTR"),   prog.ea_from_va(state, VA(0xFFFE)), ty.clone(), 2);

	prog.analyze_queue();
	println!("found {} functions.", prog.all_funcs().count());

	for segid in prog.all_image_segs() {
		show_segment(&prog, segid);
	}

	// show_all_func_cfgs(&prog);

	// show_all_funcs(&prog);
	// show_prg0(&prog);

	Ok(())
}

fn find_identical_image_pieces(prog: &Program) {
	let seg_datas = prog.all_image_segs()
		.map(|id| prog.segment_from_id(id).image_slice_all().data())
		.collect::<Vec<_>>();

	let mut in_run = false;
	let mut run_start = 0;

	for i in 0 .. seg_datas[0].len() {
		let b0 = seg_datas[0][i];

		let all_eq = seg_datas[1..].iter().all(|sd| sd[i] == b0);

		if !all_eq {
			if in_run {
				in_run = false;
				println!("run of identical bytes from {:04X} to {:04X}",
					run_start + 0x8000, i + 0x8000);

				for d in seg_datas.iter() {
					print!("{:02X} ", d[i]);
				}
				println!();
			}
		} else if !in_run {
			in_run = true;
			run_start = i;
		}
	}

	if in_run {
		println!("run of identical bytes from {:04X} to {:04X}",
			run_start + 0x8000, seg_datas[0].len() + 0x8000);
	}
}

fn show_prg0(prog: &Program) {
	show_segment(prog, prog.segment_for_name("PRG0").unwrap().id());
}

fn show_segment(prog: &Program, segid: SegId) {
	let seg = prog.segment_from_id(segid);

	let mut cur_func = None;

	for span in seg.all_spans() {
		if let Some(func) = prog.func_that_contains(span.start()) {
			let func_id = func.id();

			if cur_func != Some(func_id) {
				cur_func = Some(func_id);

				if span.bb() == Some(func.head_id()) {
					show_func_header(prog, func);
				} else {
					show_func_piece_header(prog, func);
				}
			}
		} else {
			cur_func = None;
		}

		match span.kind() {
			SpanKind::Unk      => show_unk(prog, &span),
			SpanKind::Code(id) => show_bb(prog, prog.get_bb(id)),
			SpanKind::Data(id) => show_data(prog, prog.get_data(id)),
			_ => {}
		}
	}
}

const UNK_SIZE_CUTOFF: usize = 512;
const UNK_STRIDE: usize = 16;

fn show_unk(prog: &Program, span: &Span) {
	let divider =
		"; -------------------------------------------------------------------------".green();

	// TODO: this is kind of a mess
	let ea    = span.start();
	let seg   = prog.segment_from_ea(ea);
	let state = prog.mmu_state_at(ea).unwrap_or_else(|| prog.initial_mmu_state());
	let va    = prog.va_from_ea(state, ea);
	let addr  = prog.fmt_addr(va.0);
	let msg   = format!("[{} unexplored byte(s)]", span.len());

	println!("{}", &divider);
	println!("{:>4}:{} {}", seg.name().yellow(), addr, msg.truecolor(255, 127, 0));

	if seg.is_real() {
		let len = span.len().min(UNK_SIZE_CUTOFF);
		let slice = seg.image_slice(ea .. ea + len);
		let data = slice.data();
		let seg_name = seg.name().yellow();

		for (i, chunk) in data.chunks(UNK_STRIDE).enumerate() {
			let mut bytes = String::with_capacity(chunk.len() * 3);

			bytes.push_str(&format!("{:02X}", chunk[0]));

			for byte in &chunk[1 ..] {
				bytes.push_str(&format!(" {:02X}", byte));
			}

			let addr = prog.fmt_addr(va.0 + i * UNK_STRIDE);
			println!("{:>4}:{} {}", seg_name, addr, bytes.truecolor(255, 127, 0));
		}

		if span.len() > UNK_SIZE_CUTOFF {
			println!("          {}", "...".truecolor(255, 127, 0));
		}
	}

	println!("{}", &divider);
	println!();
}

fn show_data(prog: &Program, data: &DataItem) {
	let divider =
		"; -------------------------------------------------------------------------".green();

	let start = data.ea();
	let size = data.size();

	println!("{}", divider);
	let msg = format!("; {} byte(s), type {}", size, data.ty());
	println!("{}: {}", prog.name_of_ea(start).truecolor(127, 63, 0), msg.green());

	let seg = prog.segment_from_ea(start);

	if seg.is_real() {
		let slice = seg.image_slice(start .. start + size);
		println!("    {}", interpret_data(prog, data.radix(), data.ty(), &slice));
	}
}

fn interpret_data(prog: &Program, radix: Radix, ty: &Type, slice: &ImageSlice) -> String {
	use Type::*;

	let endian = prog.endianness();

	match ty {
		Bool => format!("{}", slice.read_u8(0) != 0),

		I8  => interpret_int(slice.read_u8(0) as i64, 8, radix),
		I16 => interpret_int(slice.read_u16(0, endian) as i64, 16, radix),
		I32 => interpret_int(slice.read_u32(0, endian) as i64, 32, radix),
		I64 => interpret_int(slice.read_u64(0, endian) as i64, 64, radix),

		U8  => interpret_uint(slice.read_u8(0) as u64, 8, radix),
		U16 => interpret_uint(slice.read_u16(0, endian) as u64, 16, radix),
		U32 => interpret_uint(slice.read_u32(0, endian) as u64, 32, radix),
		U64 => interpret_uint(slice.read_u64(0, endian) as u64, 64, radix),

		Char  => interpret_char(slice.read_u8(0) as char),
		WChar => {
			let v = slice.read_u16(0, endian);

			match std::char::from_u32(v as u32) {
				Some(c) => interpret_char(c),
				None    => interpret_uint(v as u64, 16, Radix::Hex),
			}
		}

		Array(at) => {
			let mut ret = String::with_capacity(at.len() * 4);
			let sub_ty = at.ty();
			let stride = sub_ty.size().fixed();

			for i in 0 .. at.len() {
				let offs = i * stride;
				let sub_slice = slice.image_slice(offs .. offs + stride);
				let sub_str = interpret_data(prog, radix, sub_ty, &sub_slice);

				if i != 0 {
					ret.push(' ');
				}

				ret.push_str(&sub_str);
			}

			ret
		}

		Ptr(pt) => {
			interpret_data(prog, Radix::Hex, &pt.kind(), slice)
		}

		StrZ(_len) => unimplemented!(),
		WStrZ(_len) => unimplemented!(),

		Enum(_) | Bitfield(_) | Struct(_) => unimplemented!(),
		Code => unreachable!(),
	}
}

fn interpret_char(val: char) -> String {
	format!("'{}'", val.escape_default())
}

fn interpret_int(val: i64, bits: usize, radix: Radix) -> String {
	match radix {
		Radix::Bin => format!("0b{:0width$b}", val, width = bits),
		Radix::Dec => format!("{}", val),
		Radix::Hex => format!("0x{:0width$X}", val, width = bits / 4),
	}
}

// really the only difference is the Dec case
fn interpret_uint(val: u64, bits: usize, radix: Radix) -> String {
	match radix {
		Radix::Bin => format!("0b{:0width$b}", val, width = bits),
		Radix::Dec => format!("{}", val),
		Radix::Hex => format!("0x{:0width$X}", val, width = bits / 4),
	}
}

fn show_all_funcs(prog: &Program) {
	let mut funcs = prog.all_funcs().collect::<Vec<_>>();
	funcs.sort_by(|a, b| a.ea().cmp(&b.ea()));

	for func in funcs {
		show_func(prog, func);
	}
}

fn show_all_func_cfgs(prog: &Program) {
	let mut funcs = prog.all_funcs().collect::<Vec<_>>();
	funcs.sort_by(|a, b| a.ea().cmp(&b.ea()));

	for func in funcs {
		show_func_cfg(prog, func);
	}
}

fn show_func(prog: &Program, func: &Function) {
	show_func_header(prog, func);

	let mut bbs = func.all_bbs().map(|bbid| prog.get_bb(bbid)).collect::<Vec<_>>();
	bbs.sort_by(|a, b| a.ea().cmp(&b.ea()));

	for bb in bbs {
		show_bb(prog, bb);
	}
}

fn show_func_cfg(prog: &Program, func: &Function) {
	show_func_header(prog, func);

	let mut bbs = func.all_bbs().map(|bbid| prog.get_bb(bbid)).collect::<Vec<_>>();
	bbs.sort_by(|a, b| a.ea().cmp(&b.ea()));

	for bb in bbs {
		let bb_ea = bb.ea();

		print!("{}", prog.name_of_ea(bb_ea).truecolor(127, 63, 0));

		// S U C C
		let succ = bb.successors().collect::<Vec<_>>();

		if !succ.is_empty() {
			print!(" ->");

			for s in succ {
				print!(" {}", prog.name_of_ea(*s));
			}
		}

		println!();
	}
}

fn show_func_piece_header(prog: &Program, func: &Function) {
	let divider =
		"; -------------------------------------------------------------------------".green();

	println!("{}", divider);
	let name = prog.name_of_ea(func.ea());
	println!("{}{}{}", "; (Piece of function ".green(), name.green(), ")".green());
}

fn show_func_header(prog: &Program, func: &Function) {
	let divider =
		"; -------------------------------------------------------------------------".green();

	println!("{}", divider);

	let name = prog.name_of_ea(func.ea());
	println!("{}{}", "; Function ".green(), name.green());

	if !func.attrs().is_empty() {
		let attrs = format!("{:?}", func.attrs());
		println!("{}{}", "; Attributes: ".green(), attrs.green());
	}

	if func.is_multi_entry() {
		let entrypoints = func.entrypoints().iter().map(|bbid| prog.get_bb(*bbid).ea());

		print!("{}", "; Entry points: ".green());

		for ea in entrypoints {
			print!("{} ", prog.name_of_ea(ea).green());
		}

		println!();
	}
}

struct IoWriteWrapper<'w>(pub &'w mut dyn std::io::Write);

impl std::fmt::Write for IoWriteWrapper<'_> {
	fn write_str(&mut self, s: &str) -> Result<(), std::fmt::Error> {
		self.0.write_all(s.as_bytes()).map_err(|_| std::fmt::Error)
	}
}

fn show_bb(prog: &Program, bb: &BasicBlock) {
	let bb_ea = bb.ea();
	let seg = prog.segment_from_ea(bb_ea);

	// Inrefs and label
	if let Some(ir) = prog.get_inrefs(bb_ea) {
		print!("{:20}{}", "", ";".green());

		for &r in ir {
			print!(" {}{}", "<-".green(), prog.name_of_ea(r).green());
		}

		println!();

		println!("{:20}{}:", "", prog.name_of_ea(bb_ea).truecolor(127, 63, 0));
	}

	// println!("{}", format!("; mmu state = {:?}", bb.mmu_state()).green());

	// Instructions

	let state = bb.mmu_state();

	for inst in bb.insts() {
		let mut bytes = String::new();
		let b = inst.bytes();

		match b.len() {
			1 => write!(bytes, "{:02X}",               b[0]).unwrap(),
			2 => write!(bytes, "{:02X} {:02X}",        b[0], b[1]).unwrap(),
			3 => write!(bytes, "{:02X} {:02X} {:02X}", b[0], b[1], b[2]).unwrap(),
			_ => unreachable!()
		}

		let addr = prog.fmt_addr(inst.va().0);
		// let mnem = prog.inst_get_mnemonic(inst);
		// let ops  = prog.inst_operands_to_string(inst, state);

		// println!("{:>4}:{}  {:8}      {:3} {:30}",
		// 	seg.name().yellow(), addr, bytes.truecolor(63, 63, 255), mnem.red(), ops);

		print!("{:>4}:{}  {:8}      ", seg.name().yellow(), addr, bytes.truecolor(63, 63, 255));
		let mut output = AnsiConsolePrintOutput;
		prog.inst_print(inst, state, &mut output).unwrap();
		println!();
	}

	// Terminator
	use BBTerm::*;
	match bb.term() {
		DeadEnd => println!("{}", "---------- DEAD END ----------".red().bold()),
		BankChange(..) => println!("{}", "---------- BANK CHANGE ----------".cyan().bold()),
		Halt | Return => {
		}
		FallThru(ea) => {
			thinger(prog, bb_ea, *ea, "Fall through", Color::Yellow);
		}
		Jump(ea) => {
			thinger(prog, bb_ea, *ea, "Tailcall", Color::Yellow);
		}
		Call { ret, .. } => {
			thinger(prog, bb_ea, *ret, "Fall through", Color::Yellow);
		}
		Cond { t, f } => {
			thinger(prog, bb_ea, *t, "Tailbranch", Color::Yellow);
			thinger(prog, bb_ea, *f, "Fall through", Color::Yellow);
		}
		JumpTbl(..) => println!("{}", "---------- JUMP TABLE ----------".yellow())
	}

	println!();
}

fn thinger(prog: &Program, from: EA, to: EA, msg: &str, color: Color){
	if diff_funcs(prog, from, to) {
		let dest = prog.name_of_ea(to);
		let msg = format!("---------- {} to {} ----------", msg, dest);
		println!("{}", msg.color(color));
	}
}

fn diff_funcs(prog: &Program, ea1: EA, ea2: EA) -> bool {
	let func1 = prog.func_that_contains(ea1).map(|f| f.id());
	let func2 = prog.func_that_contains(ea2).map(|f| f.id());

	func1 != func2
}
