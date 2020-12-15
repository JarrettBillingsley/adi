#![allow(dead_code)]

use std::fmt::Write;

use better_panic::{ Settings as PanicSettings, Verbosity as PanicVerbosity };
use simplelog::*;
use colored::*;

use adi::*;
use mos65xx::{ Disassembler, Printer, SyntaxFlavor };
use MemoryRegionKind::*;

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

fn test_nes() -> std::io::Result<()> {
	// let's set it up
	let img = Image::new_from_file("tests/data/smb.prg")?;

	let mut segs = SegCollection::new();
	// default
	let ram_seg  = segs.add_segment("RAM",   VA(0x0000), VA(0x0800), None);
	let ppu_seg  = segs.add_segment("PPU",   VA(0x2000), VA(0x2008), None);
	let io_seg   = segs.add_segment("IOREG", VA(0x4000), VA(0x4020), None);
	// ROM-specific
	let prg0_seg = segs.add_segment("PRG0",  VA(0x8000), VA(0x10000), Some(img));


	let map = MemoryMap::new(16, &[
		// default
		MemoryRegion::new("RAM".into(),     VA(0x0000), VA(0x0800),  Ram,    Some(ram_seg)),
		MemoryRegion::new("RAMECHO".into(), VA(0x0800), VA(0x2000),  Mirror, None),
		MemoryRegion::new("PPU".into(),     VA(0x2000), VA(0x2008),  Mmio,   Some(ppu_seg)),
		MemoryRegion::new("PPUECHO".into(), VA(0x2008), VA(0x4000),  Mirror, None),
		MemoryRegion::new("IOREG".into(),   VA(0x4000), VA(0x4020),  Mmio,   Some(io_seg)),
		// ROM-specific
		MemoryRegion::new("PRGROM".into(),  VA(0x8000), VA(0x10000), Rom,    Some(prg0_seg)),
	]);

	let mem = Memory::new(Endian::Little, segs, map);
	let mut prog = Program::new(mem);
	setup_nes_labels(&mut prog);

	println!("{}", prog);

	let seg = prog.segment_from_id(prg0_seg);

	let vec_reset_loc = prog.loc_from_name("VEC_RESET");
	let vec_nmi_loc   = prog.loc_from_name("VEC_NMI");
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

fn setup_nes_labels(prog: &mut Program) {
	for StdName(name, addr) in NES_STD_NAMES {
		prog.add_name_va(name, VA(*addr));
	}
}

struct StdName(&'static str, usize);

const NES_STD_NAMES: &[StdName] = &[
	StdName("PPU_CTRL_REG1",         0x2000),
	StdName("PPU_CTRL_REG2",         0x2001),
	StdName("PPU_STATUS",            0x2002),
	StdName("PPU_SPR_ADDR",          0x2003),
	StdName("PPU_SPR_DATA",          0x2004),
	StdName("PPU_SCROLL_REG",        0x2005),
	StdName("PPU_ADDRESS",           0x2006),
	StdName("PPU_DATA",              0x2007),

	StdName("SND_PULSE1_CTRL1",      0x4000),
	StdName("SND_PULSE1_CTRL2",      0x4001),
	StdName("SND_PULSE1_TIMER",      0x4002),
	StdName("SND_PULSE1_LENGTH",     0x4003),
	StdName("SND_PULSE2_CTRL1",      0x4004),
	StdName("SND_PULSE2_CTRL2",      0x4005),
	StdName("SND_PULSE2_TIMER",      0x4006),
	StdName("SND_PULSE2_LENGTH",     0x4007),
	StdName("SND_TRI_CTRL",          0x4008),
	StdName("SND_TRI_TIMER",         0x400A),
	StdName("SND_TRI_LENGTH",        0x400B),
	StdName("SND_NOISE_CTRL",        0x400c),
	StdName("SND_NOISE_PERIOD",      0x400e),
	StdName("SND_NOISE_LENGTH",      0x400f),
	StdName("SND_DMC_CTRL",          0x4010),
	StdName("SND_DMC_COUNTER",       0x4011),
	StdName("SND_DMC_ADDR",          0x4012),
	StdName("SND_DMC_LEN",           0x4013),
	StdName("SPR_DMA",               0x4014),
	StdName("SND_MASTER_CTRL",       0x4015),
	StdName("JOYPAD_PORT1",          0x4016),
	StdName("JOYPAD_PORT2",          0x4017),
	StdName("TESTMODE_PULSE_DAC",    0x4018),
	StdName("TESTMODE_TRINOISE_DAC", 0x4019),
	StdName("TESTMODE_DPCM_DAC",     0x401A),
	StdName("UNUSED_TIMER_LO",       0x401C),
	StdName("UNUSED_TIMER_MID",      0x401D),
	StdName("UNUSED_TIMER_HI",       0x401E),
	StdName("UNUSED_TIMER_CTRL",     0x401F),

	StdName("VEC_NMI",               0xFFFA),
	StdName("VEC_RESET",             0xFFFC),
	StdName("VEC_IRQ",               0xFFFE),
];