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
	setup_logging(LevelFilter::Trace)?;
	setup_panic();
	// test_gb()?;
	test_nes()?;
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

fn test_gb() -> Result<(), Box<dyn std::error::Error>> {
	let img = Image::new_from_file("tests/data/tetris.gb")?;
	let mut prog = program_from_image(img)?;
	println!("{}", prog);

	let state = prog.initial_mmu_state();
	prog.enqueue_function(state, prog.ea_from_name("RESET"));
	prog.analyze_queue();

	println!("found {} functions.", prog.all_funcs().count());
	show_all_funcs(&prog);

	Ok(())
}

// ------------------------------------------------------------------------------------------------

fn test_nes() -> Result<(), Box<dyn std::error::Error>> {
	// let's set it up
	let img = Image::new_from_file("tests/data/battletoads.nes")?;
	let mut prog = program_from_image(img)?;

	println!("{}", prog);

	let state = prog.initial_mmu_state();
	prog.enqueue_function(state, prog.ea_from_name("VEC_RESET"));
	prog.enqueue_function(state, prog.ea_from_name("VEC_NMI"));
	prog.analyze_queue();

	println!("found {} functions.", prog.all_funcs().count());

	let ea   = prog.ea_from_va(state, VA(0xFFB3));
	let ty   = Type::array(Type::U8, 24);
	let size = ty.size().fixed();
	prog.new_data(Some("array_of_things"), ea, ty, size);

	for segid in prog.all_image_segs() {
		show_segment(&prog, segid);
	}

	// show_all_funcs(&prog);
	// show_prg0(&prog);


	// let ea   = prog.ea_from_va(state, VA(0x821A));
	// let ty   = Type::array(Type::ptr(Type::Code, Type::U16), 3);
	// let size = ty.size().fixed();
	// prog.new_data(Some("array"), ea, ty, size);

	Ok(())
}

fn show_prg0(prog: &Program) {
	show_segment(prog, prog.segment_for_name("PRG0").unwrap().id());
}

fn show_segment(prog: &Program, segid: SegId) {
	let seg = prog.segment_from_id(segid);
	let divider = "; -------------------------------------------------------------------------";
	let mut cur_func = None;

	for span in seg.all_spans() {
		if let Some(func) = prog.func_that_contains(span.start()) {
			let func_id = func.id();

			if cur_func.is_none() || cur_func.unwrap() != func_id {
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
			SpanKind::Unk => {
				// TODO: this is kind of a mess
				let ea    = span.start();
				let state = prog.mmu_state_at(ea).unwrap_or_else(|| prog.initial_mmu_state());
				let va    = prog.va_from_ea(state, ea);
				let addr  = prog.fmt_addr(va.0);
				let msg   = format!("[{} unexplored byte(s)]", span.len());

				println!("{}", divider.green());
				println!("{:>4}:{} {}", seg.name().yellow(), addr, msg.truecolor(255, 127, 0));
				println!("{}", divider.green());
				println!();
			}

			SpanKind::Code(id) => {
				show_bb(prog, prog.get_bb(id));
			}

			SpanKind::Data(id) => {
				show_data(prog, prog.get_data(id));
			}

			_ => {}
		}
	}
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

		StrZ(_len) => unimplemented!(),
		WStrZ(_len) => unimplemented!(),
		Ptr(_pt) => unimplemented!(),

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

fn show_func(prog: &Program, func: &Function) {
	show_func_header(prog, func);

	let mut bbs = func.all_bbs().map(|bbid| prog.get_bb(bbid)).collect::<Vec<_>>();
	bbs.sort_by(|a, b| a.ea().cmp(&b.ea()));

	for bb in bbs {
		show_bb(prog, bb);
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
		let mnem = prog.inst_fmt_mnemonic(inst);
		let ops  = prog.inst_fmt_operands(state, inst);

		println!("{:>4}:{}  {:8}      {:3} {:30}",
			seg.name().yellow(), addr, bytes.truecolor(63, 63, 255), mnem.red(), ops);
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
