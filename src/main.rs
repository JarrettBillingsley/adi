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
	setup_logging(LevelFilter::Debug)?;
	setup_panic();
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

fn test_nes() -> Result<(), Box<dyn std::error::Error>> {
	// let's set it up
	let img = Image::new_from_file("tests/data/smb.nes")?;
	let mut prog = program_from_image(img)?;

	println!("{}", prog);

	let state = prog.initial_mmu_state();
	prog.enqueue_function(state, prog.loc_from_name("VEC_RESET"));
	prog.enqueue_function(state, prog.loc_from_name("VEC_NMI"));
	prog.analyze_queue();

	println!("found {} functions.", prog.all_funcs().count());

	show_all_funcs(&prog);
	// show_prg0(&prog);
	Ok(())
}

fn show_prg0(prog: &Program) {
	let seg = prog.segment_for_name("PRG0").unwrap();

	let divider = "; -------------------------------------------------------------------------";

	for span in seg.all_spans() {
		match span.kind() {
			SpanKind::Unk => {
				// TODO: this is kind of a mess
				let loc = span.start();
				let state = prog.mmu_state_at(loc).unwrap_or_else(|| prog.initial_mmu_state());
				let va = prog.va_from_loc(state, loc);
				let addr = prog.fmt_addr(va.0);

				let msg = format!("[{} unexplored byte(s)]", span.len());

				println!("{}", divider.green());
				println!("{:>4}:{} {}", seg.name().yellow(), addr, msg.truecolor(255, 127, 0));
				println!("{}", divider.green());
				println!();
			}

			SpanKind::Code(bbid) => {
				show_bb(&prog, bbid);
			}

			_ => {}
		}
	}
}

fn show_all_funcs(prog: &Program) {
	let mut funcs = prog.all_funcs().collect::<Vec<_>>();
	funcs.sort_by(|&a, &b| prog.func_start_loc(a).cmp(&prog.func_start_loc(b)));

	for func in funcs {
		show_func(prog, func);
	}
}

fn show_func(prog: &Program, func: FuncId) {
	let divider =
		"; -------------------------------------------------------------------------".green();

	println!("{}", divider);

	let name = prog.name_of_loc(prog.func_start_loc(func));
	println!("{}{}", "; Function ".green(), name.green());

	let mut bbs = prog.func_all_bbs(func).collect::<Vec<_>>();
	bbs.sort_by(|&a, &b| prog.bb_loc(a).cmp(&prog.bb_loc(b)));

	for bb in bbs {
		show_bb(prog, bb);
	}
}

fn show_bb(prog: &Program, bb: BBId) {
	let bb_loc = prog.bb_loc(bb);
	let seg = prog.segment_from_loc(bb_loc);

	// Inrefs and label
	if let Some(ir) = prog.get_inrefs(bb_loc) {
		print!("{:20}{}", "", ";".green());

		for &r in ir {
			print!(" {}{}", "<-".green(), prog.name_of_loc(r).green());
		}

		println!();

		println!("{:20}{}:", "", prog.name_of_loc(bb_loc).truecolor(127, 63, 0));
	}

	// Instructions

	for (i, inst) in prog.bb_insts(bb) {
		let mut bytes = String::new();
		let b = inst.bytes();

		match b.len() {
			1 => write!(bytes, "{:02X}",               b[0]).unwrap(),
			2 => write!(bytes, "{:02X} {:02X}",        b[0], b[1]).unwrap(),
			3 => write!(bytes, "{:02X} {:02X} {:02X}", b[0], b[1], b[2]).unwrap(),
			_ => unreachable!()
		}

		let addr = prog.fmt_addr(inst.va().0);
		let mnem = prog.inst_fmt_mnemonic(bb, i);
		let ops  = prog.inst_fmt_operands(bb, i);

		print!("{:>4}:{}  {:8}      {:3} {:30}",
			seg.name().yellow(), addr, bytes.truecolor(63, 63, 255), mnem.red(), ops);

		for i in 0 .. inst.num_ops() {
			let op = inst.get_op(i);

			match op.access() {
				// writing to memory...
				Some(MemAccess::Write) | Some(MemAccess::Rmw) => {
					let va = op.addr();
					if let Some(loc) = prog.loc_for_va(inst.mmu_state(), va) {
						// ...into a segment with an image mapping...
						if !prog.segment_from_loc(loc).is_fake() {
							// ooooh, that might be a bank switch!
							print!("{}", "<------------------------- BANK SWITCH??".yellow());
						}
					}
				}
				_ => {}
			}
		}

		println!();
	}

	// Terminator
	use BBTerm::*;
	match prog.bb_term(bb) {
		DeadEnd => println!("{}", "---------- DEAD END ----------".red().bold()),
		Halt | Return => {
		}
		FallThru(loc) => {
			thinger(prog, bb_loc, *loc, "Fall through", Color::Yellow);
		}
		Jump(loc) => {
			thinger(prog, bb_loc, *loc, "Tailcall", Color::Yellow);
		}
		Call { ret, .. } => {
			thinger(prog, bb_loc, *ret, "Fall through", Color::Yellow);
		}
		Cond { t, f } => {
			thinger(prog, bb_loc, *t, "Tailbranch", Color::Yellow);
			thinger(prog, bb_loc, *f, "Fall through", Color::Yellow);
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
