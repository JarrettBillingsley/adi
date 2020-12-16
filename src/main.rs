#![allow(dead_code)]

use std::fmt::Write;

use better_panic::{ Settings as PanicSettings, Verbosity as PanicVerbosity };
use simplelog::*;
use colored::*;

use adi::*;
use mos65xx::{ Disassembler, Printer, SyntaxFlavor };

fn main() -> Result<(), Box<dyn std::error::Error>> {
	setup_logging()?;
	setup_panic();
	test_nes()?;
	Ok(())
}

fn setup_logging() -> Result<(), TermLogError> {
	let log_config = ConfigBuilder::new()
		.set_time_level(LevelFilter::Off)
		.set_thread_level(LevelFilter::Off)
		.set_target_level(LevelFilter::Off)
		.set_location_level(LevelFilter::Debug)
		.build();
	TermLogger::init(LevelFilter::Debug, log_config, TerminalMode::Mixed)
}

fn setup_panic() {
	PanicSettings::new()
		.lineno_suffix(true)
		// .most_recent_first(false)
		.verbosity(PanicVerbosity::Full)
	.install();
}

fn test_nes() -> Result<(), Box<dyn std::error::Error>> {
	// let's set it up
	let img = Image::new_from_file("tests/data/SuperMarioBros.nes")?;
	let mut prog = program_from_image(img)?;

	println!("{}", prog);

	let vec_reset_loc = prog.loc_from_name("VEC_RESET");
	let vec_nmi_loc   = prog.loc_from_name("VEC_NMI");
	let seg           = prog.segment_from_loc(vec_reset_loc);
	let reset_va      = VA(seg.read_le_u16(vec_reset_loc) as usize);
	let nmi_va        = VA(seg.read_le_u16(vec_nmi_loc) as usize);
	let reset_loc     = seg.loc_from_va(reset_va);
	let nmi_loc       = seg.loc_from_va(nmi_va);

	prog.add_ref(vec_reset_loc, reset_loc);
	prog.add_ref(vec_nmi_loc, nmi_loc);

	{
		// huh huh huh
		let mut anal = Analyzer::new(&mut prog, Disassembler);
		anal.enqueue_function(reset_loc);
		anal.enqueue_function(nmi_loc);
		anal.analyze_queue();
	}

	println!("found {} functions.", prog.all_funcs().count());

	// show_all_funcs(&prog);

	let seg = prog.segment_for_name("PRG0").unwrap();

	let divider = "; -------------------------------------------------------------------------";

	for span in seg.all_spans() {
		match span.kind() {
			SpanKind::Unk => {
				let addr = prog.fmt_addr(seg.va_from_loc(span.start()).0);
				let msg = format!("[{} unexplored byte(s)]", span.len());

				println!("{}", divider.green());
				println!("{:>4}:{} {}", seg.name().yellow(), addr, msg.truecolor(255, 127, 0));
				println!("{}", divider.green());
				println!();
			}

			SpanKind::Code(bbid) => {
				let func_id = bbid.func();
				let func = prog.get_func(func_id);
				let bb = func.get_bb(bbid);
				show_bb(&prog, bb);
			}

			_ => {}
		}
	}

	Ok(())
}

fn show_all_funcs(prog: &Program) {
	let mut funcs = prog.all_funcs().map(|(_, func)| func).collect::<Vec<_>>();
	funcs.sort_by(|a, b| a.start_loc().cmp(&b.start_loc()));

	for func in funcs {
		show_func(&prog, func);
	}
}

fn show_func(prog: &Program, func: &Function) {
	let divider =
		"; -------------------------------------------------------------------------".green();

	println!("{}", divider);

	let name = prog.name_of_loc(func.start_loc());
	println!("{}{}", "; Function ".green(), name.green());

	let mut bbs = func.all_bbs().collect::<Vec<_>>();
	bbs.sort_by(|a, b| a.loc().cmp(&b.loc()));

	for bb in bbs {
		show_bb(prog, &bb);
	}
}

fn show_bb(prog: &Program, bb: &BasicBlock) {
	let (seg, span) = prog.seg_and_span_at_loc(bb.loc());
	let slice       = seg.image_slice(span).into_data();
	let bb_va       = seg.va_from_loc(bb.loc());

	// Inrefs and label
	if let Some(ir) = prog.get_inrefs(bb.loc()) {
		print!("{:20}{}", "", ";".green());

		for &r in ir {
			print!(" {}{}", "<-".green(), prog.name_of_loc(r).green());
		}

		println!();

		println!("{:20}{}:", "", prog.name_of_loc(bb.loc()).truecolor(127, 63, 0));
	}

	// Instructions
	let print = Printer::new(SyntaxFlavor::New);

	for inst in Disassembler.disas_all(slice, bb_va, bb.loc()) {
		let mut bytes = String::new();
		let b = inst.bytes();

		match b.len() {
			1 => write!(bytes, "{:02X}",               b[0]).unwrap(),
			2 => write!(bytes, "{:02X} {:02X}",        b[0], b[1]).unwrap(),
			3 => write!(bytes, "{:02X} {:02X} {:02X}", b[0], b[1], b[2]).unwrap(),
			_ => unreachable!()
		}

		let addr = prog.fmt_addr(inst.va().0);
		let mnem = print.fmt_mnemonic(&inst);
		let ops  = print.fmt_operands(&inst, prog);

		println!("{:>4}:{}  {:8}      {:3} {:30}",
			seg.name().yellow(), addr, bytes.truecolor(63, 63, 255), mnem.red(), ops);
	}

	// Terminator
	use BBTerm::*;
	match bb.term() {
		DeadEnd => println!("{}", "---------- DEAD END ----------".red().bold()),
		Halt | Return => {
		}
		FallThru(loc) => {
			thinger(prog, bb.loc(), *loc, "Fall through", Color::Yellow);
		}
		Jump(loc) => {
			thinger(prog, bb.loc(), *loc, "Tailcall", Color::Yellow);
		}
		Cond { t, f } => {
			thinger(prog, bb.loc(), *t, "Tailbranch", Color::Yellow);
			thinger(prog, bb.loc(), *f, "Fall through", Color::Yellow);
		}
		JumpTbl(..) => println!("{}", "---------- JUMP TABLE ----------".yellow())
	}

	println!();
}

fn thinger(prog: &Program, from: Location, to: Location, msg: &str, color: Color){
	if diff_funcs(prog, from, to) {
		let dest = prog.name_of_loc(to);
		let msg = format!("---------- {} to {} ----------", msg, dest);
		println!("{}", msg.color(color));
	}
}

fn diff_funcs(prog: &Program, loc1: Location, loc2: Location) -> bool {
	let func1 = prog.func_that_contains(loc1);
	let func2 = prog.func_that_contains(loc2);

	func1 != func2
}