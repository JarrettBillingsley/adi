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

	// test_gb()
	// test_nes()
	test_toy()
}

// ------------------------------------------------------------------------------------------------

fn setup_logging(max_level: LevelFilter) -> Result<(), SetLoggerError> {
	let log_config = ConfigBuilder::new()
		.set_level_color(Level::Info, Some(simplelog::Color::Green))
		.set_level_color(Level::Debug, Some(simplelog::Color::Cyan))
		.set_level_color(Level::Trace, Some(simplelog::Color::White))
		.set_time_level(LevelFilter::Off)
		.set_thread_level(LevelFilter::Off)
		.set_target_level(LevelFilter::Off)
		// .set_location_level(LevelFilter::Error)
		.set_location_level(LevelFilter::Off)
		.set_level_padding(LevelPadding::Right)
		.build();
	TermLogger::init(max_level, log_config, TerminalMode::Mixed, ColorChoice::Always)
}

fn setup_panic() {
	PanicSettings::new()
		.lineno_suffix(true)
		.most_recent_first(false)
		.verbosity(PanicVerbosity::Medium)
	.install();
}

// ------------------------------------------------------------------------------------------------

fn test_common(mut prog: Program) -> Result<(), Box<dyn std::error::Error>> {
	prog.enqueue_reg_usage();
	prog.analyze_queue();

	println!("found {} functions.", prog.all_funcs().count());

	for segid in prog.all_image_segs() {
		show_segment(&prog, segid);
	}

	// for (ea, name) in prog.all_names_by_ea() {
	// 	println!("{} {:25} {:?}", ea, name.name, name.kind);
	// }

	// let cg = prog.build_call_graph();
	// prog.dump_call_graph(&cg);

	// let sccs = cg.sccs();

	// println!("SCCs: ");

	// for scc in sccs.into_iter() {
	// 	println!("  {:?}", scc);
	// }

	Ok(())
}

// ------------------------------------------------------------------------------------------------

fn test_gb() -> Result<(), Box<dyn std::error::Error>> {
	let img_name =
		"tests/data/tetris.gb"; // no MBC
		// "tests/data/sml.gb";    // MBC1  (ROM only)
		// "tests/data/sml2.gb";   // MBC1B (ROM + RAM)
		// "tests/data/pkblue.gb"; // MBC3

	let img = Image::new_from_file(img_name)?;
	let (mut prog, start_ea) = program_from_image(img)?;
	println!("{}", prog);
	println!("Start EA: {:?} ({})", start_ea, prog.name_of_ea(start_ea));

	let state = prog.initial_mmu_state();
	prog.enqueue_new_func(state, prog.ea_from_name("RESET"));

	// TETRIS
	if img_name.contains("tetris") {
		// from rst 0x28 at ROM0:02FA
		for va in [
			0x1BCE, 0x1CE2, 0x1244, 0x127B, 0x1D06, 0x1D26, 0x03AE, 0x0479, 0x1444, 0x148C,
			0x1A07, 0x1DC0, 0x1F16, 0x1F1F, 0x1525, 0x14B0, 0x157B, 0x15BF, 0x1629, 0x167A,
			0x16EB, 0x1913, 0x0677, 0x072C, 0x0825, 0x08E4, 0x0B31, 0x0CEB, 0x0AD2, 0x0D32,
			0x0E23, 0x1112, 0x0D99, 0x0E8A, 0x1DCE, 0x1E41, 0x0369, 0x0393, 0x1167, 0x11E6,
			0x11FC, 0x121C, 0x05C7, 0x05F7, 0x12B3, 0x1305, 0x1324, 0x1351, 0x1367, 0x137E,
			0x13B5, 0x13E5, 0x131B, 0x03A0, 0x27EA
		] {
			prog.enqueue_new_func(state, prog.ea_from_va(state, VA(va)));
		}

		let ty = prog.type_array(&Type::U16, 41);
		prog.new_data(None, prog.ea_from_va(state, VA(0x6480)), &ty, prog.type_sizeof(&ty).fixed());

		// from jump table at ROM1:6480
		for va in [
			0x65AA, 0x65C6, 0x66FC, 0x6628, 0x6734, 0x66AF, 0x65F1, 0x6654,
			0x65B2, 0x65CE, 0x6714, 0x65CE, 0x65CE, 0x66C3, 0x65F7, 0x6660,
			0x67D4, 0x67DC, 0x679D, 0x67A5, 0x67E4, 0x67E4, 0x67E4, 0x67AD,
		] {
			prog.enqueue_new_func(state, prog.ea_from_va(state, VA(va)));
		}

	}

	test_common(prog)
}

// ------------------------------------------------------------------------------------------------

fn test_nes() -> Result<(), Box<dyn std::error::Error>> {
	let img_name =
		// listed in order of decreasing mapper popularity
		// "tests/data/dragonwarrior.nes"; // 1   (mmc1/sxrom)              *UNIMPLEMENTED*
		// "tests/data/gauntlet.nes";      // 4   (mmc3/txrom, mmc6/hkrom)  *UNIMPLEMENTED*
		// "tests/data/smb3.nes";          // 4   (mmc3/txrom, mmc6/hkrom)  *UNIMPLEMENTED*
		// "tests/data/megaman.nes";       // 2   (uxrom)
		"tests/data/10yf.nes";          // 0   (nrom)
		// "tests/data/duckhunt.nes";      // 0   (nrom)
		// "tests/data/smb.nes";           // 0   (nrom)
		// "tests/data/arkanoid.nes";      // 3   (cnrom)
		// "tests/data/battletoads.nes";   // 7   (axrom)
		// NO ROM: e.g. gauntlet // 206 (mimic-1, namcot 118)     *UNIMPLEMENTED*
		// "tests/data/exodus.nes"; // 11  (color dreams)
		// "tests/data/castlevania3.nes";  // 5   (mmc5/exrom)              *UNIMPLEMENTED*
		// NO ROM: e.g. star wars, rolling thunder // 19 (namco N129/N163)          *UNIMPLEMENTED*
	let img = Image::new_from_file(img_name)?;

	let (mut prog, start_ea) = program_from_image(img)?;

	println!("{}", prog);
	println!("Start EA: {:?} ({})", start_ea, prog.name_of_ea(start_ea));

	// find_identical_image_pieces(&prog);

	let state = prog.initial_mmu_state();
	prog.enqueue_new_func(state, prog.ea_from_name("VEC_RESET"));
	prog.enqueue_new_func(state, prog.ea_from_name("VEC_NMI"));

	if let Some(ea) = prog.ea_for_name("VEC_IRQ") {
		prog.enqueue_new_func(state, ea);
	}

	if img_name.contains("battletoads") {
		for va in [
			0x8003, 0x8006, 0x8009, 0x800C, 0x800F, 0x8012, 0x8015, 0x8018,
			0x801B, 0x801E, 0x8021, 0x8024, 0x8027, 0x802A, 0x802D, 0x8030,
			0x8085, 0x80F7, 0x831F, 0x84E8, 0x857A, 0x86DE, 0x87A3, 0x87C2,
			0x884B, 0x88EF, 0x8DC1, 0x9150, 0x9200, 0x923E, 0x9252, 0x930A,
			0x93C8, 0x944E, 0x95E8, 0x95EB, 0x9643, 0x9E4E, 0x9ED2, 0xA51E,
			0xA521, 0xA560, 0xB06B
		] {
			prog.enqueue_new_func(state, prog.ea_from_va(state, VA(va)));
		}
		let ea   = prog.ea_from_va(state, VA(0xFFB3));
		let ty   = prog.type_array(&Type::U8, 24);
		let size = prog.type_sizeof(&ty).fixed();
		prog.new_data(Some("BANK_CHANGE"), ea, &ty, size);
	} else if img_name.contains("smb.nes") {
		for va in [0x8231, 0x838B, 0x9218, 0xAEDC] {
			prog.enqueue_new_func(state, prog.ea_from_va(state, VA(va)));
		}
		let ea   = prog.ea_from_va(state, VA(0x821A));
		let ty   = prog.type_array(&prog.type_ptr(&Type::Code, &Type::U16), 3);
		let size = prog.type_sizeof(&ty).fixed();
		prog.new_data(Some("array"), ea, &ty, size);
	}

	test_common(prog)
}

// ------------------------------------------------------------------------------------------------

fn test_toy() -> Result<(), Box<dyn std::error::Error>> {
	// let test = toy_test_all_instructions();
	// let test = toy_test_ssa();
	// let test = toy_test_const_prop();
	let test = toy_test_calls();
	// let test = toy_test_loop()
	// let test = toy_test_state_change();
	// let test = toy_test_ccall_cret();
	// let test = toy_test_data();

	let (mut prog, start_ea) = program_from_image(Image::new(test.name, &test.image))?;
	prog.add_name("main", start_ea, false);
	let state = prog.initial_mmu_state();

	for (name, va) in test.labels {
		prog.add_name_va(&name, state, va, false);
	}

	for (name, va, ty, size) in test.data {
		let ea = prog.ea_from_va(state, va);
		let id = prog.new_data(Some(&name), ea, &ty, size);
		prog.get_data_mut(id).set_radix(Radix::Dec);
	}

	println!("{}", prog);
	println!("Start EA: {:?} ({})", start_ea, prog.name_of_ea(start_ea));

	prog.enqueue_new_func(state, start_ea);

	test_common(prog)
}

struct ToyTest {
	image:  Vec<u8>,
	name:   &'static str,
	labels: Vec<(String, VA)>,
	data:   Vec<(String, VA, Type, Size)>,
}

fn toy_test_all_instructions() -> ToyTest {
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
	b.call_to(0x7FFE);
	b.ldi(C, 0x8000);
	b.ld(A, DC);
	b.sti(C, 0x8000);
	b.st(A, A);

	b.movi(C, 0x34);
	b.movi(D, 0x12);
	b.cali();

	b.ret();

	b.org(0x1234);
	b.ret();

	b.org(0x7FFE);
	b.movi(Reg::A, 10);

	ToyTest {
		image: b.finish(),
		name:  "<toy_test_all_instructions>",
		labels: vec![
			("indir_func".to_string(), VA(0x1234)),
			("func".to_string(), VA(0x7FFE)),
		],
		data: vec![],
	}
}

fn toy_test_ssa() -> ToyTest {
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
	let bb4_call = b.call();
	b.ldi(A, 0x8002);
	b.cmpi(A, 1);
	b.beq_to(bb1);

	// bb5
	b.branch_here(bb3_branch);
	b.sti(A, 0x8000);
	b.ret();

	// f
	b.org(0x40);
	b.jump_here(bb4_call);
	b.ret();

	ToyTest {
		image: b.finish(),
		name:  "<toy_test_ssa>",
		labels: vec![
			("f".into(), VA(0x40)),
		],
		data: vec![],
	}
}

fn toy_test_const_prop() -> ToyTest {
	use adi::arch::toy::{ Reg, ToyBuilder };
	use Reg::*;

	let mut b = ToyBuilder::new();
	b.movi(A, 0x80);
	b.movi(B, 0xE7);
	b.mov(C, B);
	b.mov(D, A);
	b.ld(A, DC);
	b.movi(D, 0x90);
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

	b.movi(B, 0x69);
	b.sti(B, 0x8002);

	b.movi(A, 5);
	b.movi(D, 0x84);
	let loop_start =
	b.mov(C, A);
	b.ld(B, DC);
	b.addi(A, 0xFF);
	b.cmpi(A, 0);
	b.beq_to(loop_start);

	b.ret();

	ToyTest {
		image: b.finish(),
		name:  "<toy_test_const_prop>",
		labels: vec![("_loop_start".into(), VA(loop_start))],
		data: vec![],
	}
}

fn toy_test_calls() -> ToyTest {
	use adi::arch::toy::{ Reg, ToyBuilder };
	use Reg::*;

	const FUNC_FIRST_HALF: Offs = 0x30;
	const SELF_RECURSIVE: Offs = 0x50;
	const MUT_REC_1: Offs = 0x70;
	const MUT_REC_2: Offs = 0x90;

	let mut b = ToyBuilder::new();  // main:
	b.movi(A, 0x30);                //     mov  a, 0x30
	b.call_to(FUNC_FIRST_HALF);     //     call func_first_half
	b.sti(A, 0x9000);               //     st   a, [var_9000]
	b.ldi(A, 0x8000);               //     ld   a, [var_8000]
	let call_second = b.call();     //     call func_second_half
	b.sti(A, 0x9000);               //     st   a, [var_9000]
	b.sti(A, 0x8000);               //     st   a, [var_8000]
	b.movi(A, 5);                   //     mov  a, 5
	b.movi(B, 0);                   //     mov  b, 0
	b.call_to(SELF_RECURSIVE);      //     call self_recursive
	b.sti(B, 0x8002);               //     st   b, [var_8002]
	b.movi(A, 15);                  //     mov  a, 15
	b.movi(B, 0);                   //     mov  b, 0
	b.call_to(MUT_REC_1);           //     call mut_rec_1
	b.ldi(A, 0x8003);               //     ld   a, [var_8003]
	b.st(A, DC);                    //     st   a, [dc]
	b.ret();                        //     ret

	b.org(FUNC_FIRST_HALF);         // func_first_half:
	b.addi(A, 3);                   //     add  a, 3
	let func_second_half =
	b.jump_here(call_second);       // func_second_half:
	b.addi(A, 5);                   //     add  a, 5
	b.movi(C, 0xFF);                //     mov  c, -1
	b.ret();                        //     ret

	b.org(SELF_RECURSIVE);          // self_recursive:
	b.cmpi(A, 0);                   //     cmp  a, 0
	let rec_branch = b.beq();       //     beq  _base_case
	b.add(B, A);                    //     add  b, a
	b.subi(A, 1);                   //     sub  a, 1
	b.call_to(SELF_RECURSIVE);      //     call self_recursive
	let base_case =                 //
	b.branch_here(rec_branch);      // _base_case:
	b.ret();                        //     ret

	b.org(MUT_REC_1);               // mut_rec_1:
	b.cmpi(A, 0);                   //     cmp  a, 0
	let rec_branch_m1 = b.beq();    //     beq _base_case_m1
	b.add(B, A);                    //     add  b, a
	b.subi(A, 1);                   //     sub  a, 1
	b.call_to(MUT_REC_2);           //     call mut_rec_2
	let base_case_m1 =
	b.branch_here(rec_branch_m1);   // _base_case_m1:
	b.ldi(D, 0xA0);                 //     mov  d, 0xA0
	b.mov(C, B);                    //     mov  c, b
	b.ret();                        //     ret

	b.org(MUT_REC_2);               // mut_rec_2:
	b.cmpi(A, 0);                   //     cmp  a, 0
	let rec_branch_m2 = b.beq();    //     beq _base_case_m2
	b.add(B, A);                    //     add  b, a
	b.subi(A, 1);                   //     sub  a, 1
	b.call_to(MUT_REC_1);           //     call mut_rec_1
	let base_case_m2 =
	b.branch_here(rec_branch_m2);   // _base_case_m2:
	b.ret();                        //     ret

	ToyTest {
		image: b.finish(),
		name:  "<toy_test_calls>",
		labels: vec![
			("func_first_half".to_string(),  VA(FUNC_FIRST_HALF)),
			("func_second_half".to_string(), VA(func_second_half)),
			("self_recursive".to_string(),   VA(SELF_RECURSIVE)),
			("_base_case".to_string(),       VA(base_case)),
			("mut_rec_1".to_string(),        VA(MUT_REC_1)),
			("_base_case_m1".to_string(),    VA(base_case_m1)),
			("mut_rec_2".to_string(),        VA(MUT_REC_2)),
			("_base_case_m2".to_string(),    VA(base_case_m2)),
		],
		data: vec![],
	}
}

fn toy_test_loop() -> ToyTest {
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
	let loop_end = b.branch_here(loop_cond);

	b.mov(A, C);
	b.ret();

	ToyTest {
		image: b.finish(),
		name:  "<toy_test_loop>",
		labels: vec![
			("_loop_top".to_string(), VA(loop_top)),
			("_loop_end".to_string(), VA(loop_end)),
		],
		data: vec![],
	}
}

fn toy_test_state_change() -> ToyTest {
	use adi::arch::toy::{ Reg, ToyBuilder };
	use Reg::*;

	let mut b = ToyBuilder::new();
	const FUNC2: Offs = 0x50;
	const FUNC3: Offs = 0x90;
	const STATE_CHANGE_FUNC: Offs = 0x150;

	// ---------------------------------
	// main
	b.movi(D, 0xFF);
	b.movi(C, 0xFF);
	b.movi(A, 13);
	b.st(A, DC);
	b.ldi(A, 0x8000);
	b.st(A, DC);

	b.subi(C, 1);
	b.ld(B, DC);
	b.st(B, DC);

	b.movi(A, 4);
	b.call_to(STATE_CHANGE_FUNC);
	b.call_to(FUNC2);
	b.call_to(FUNC3);

	b.ret();

	// ---------------------------------
	// func2 - tests an if-else where both sides change state to the same value.
	// state change analysis should unify both predecessors and determine that
	// final block has that same value.
	b.org(FUNC2);

	// set state to 10
	b.movi(D, 0xFF);
	b.movi(C, 0xFF);
	b.movi(A, 10);
	b.st(A, DC);

	// branch on B
	b.ldi(B, 0x8000);
	b.cmpi(B, 10);
	let func2_branch = b.beq();
		// then side
		b.movi(A, 11);
		b.st(A, DC);
	let func2_jmp = b.jmp();
	b.branch_here(func2_branch);
		// else side
		b.movi(A, 12);
		b.st(A, DC);
		b.movi(A, 11);
		b.st(A, DC);
	b.jump_here(func2_jmp);

	// this block should see 11 as the state no matter what
	b.ldi(A, 0x8000);
	b.sti(A, 0x8001);

	// for good measure, let's call the state change function so it sees a new caller state
	b.call_to(STATE_CHANGE_FUNC);

	b.ret();

	// ---------------------------------
	// func3 - tests an if-else where both sides change state to *different* values.
	// final block will currently be incorrectly analyzed as a single state, but
	// in the future it will be represented as a multi-state block.
	b.org(FUNC3);

	// set state to 20
	b.movi(D, 0xFF);
	b.movi(C, 0xFF);
	b.movi(A, 20);
	b.st(A, DC);

	// branch on B
	b.ldi(B, 0x8000);
	b.cmpi(B, 10);
	let func3_branch = b.beq();
		// then side
		b.movi(A, 21);
		b.st(A, DC);
	let func3_jmp = b.jmp();
	b.branch_here(func3_branch);
		// else side
		b.movi(A, 22);
		b.st(A, DC);
	b.jump_here(func3_jmp);

	// this block *should* have Multi(21, 22) but currently it will pick one of them.
	b.ldi(A, 0x8000);
	b.sti(A, 0x8001);

	b.ret();

	// ---------------------------------
	// state change function
	b.org(STATE_CHANGE_FUNC);
	b.andi(A, 31);
	b.sti(A, 0xFFFF);
	b.ret();

	ToyTest {
		image: b.finish(),
		name:  "<toy_test_state_change>",
		labels: vec![
			("func2".to_string(), VA(FUNC2)),
			("func3".to_string(), VA(FUNC3)),
			("state_change_func".to_string(), VA(STATE_CHANGE_FUNC)),
		],
		data: vec![],
	}
}

fn toy_test_ccall_cret() -> ToyTest {
	use adi::arch::toy::{ Reg, ToyBuilder };
	use Reg::*;

	const FUNC1: Offs = 0x20;

	let mut b = ToyBuilder::new();
	b.ldi(A, 0x8000);
	b.cmpi(A, 0x35);
	b.calz_to(FUNC1);
	b.sti(A, 0x8001);
	b.ret();

	b.org(FUNC1);
	b.retz();
	b.subi(A, 1);
	b.retz();
	b.add(A, A);
	b.ret();

	ToyTest {
		image: b.finish(),
		name:  "<toy_test_ccall_cret>",
		labels: vec![
			("func1".to_string(), VA(FUNC1)),
		],
		data: vec![],
	}
}

fn toy_test_data() -> ToyTest {
	use adi::arch::toy::{ Reg, ToyBuilder };
	use Reg::*;

	let mut b = ToyBuilder::new();
	b.ret();
	let bfalse = b.append(&[0x00]);
	let btrue  = b.append(&[0x01]);
	let ubyte =  b.append(&[0x94]);
	let sbyte =  b.append(&[0x94]);
	let sshort = b.append(&[0x33, 0x94]);
	let ushort = b.append(&[0x33, 0x94]);
	let uint =   b.append(&[0x33, 0x00, 0x01, 0x94]);
	let sint =   b.append(&[0x33, 0x00, 0x01, 0x94]);
	let ulong =  b.append(&[0x33, 0x00, 0x01, 0x00, 0x02, 0x04, 0x06, 0x94]);
	let slong =  b.append(&[0x33, 0x00, 0x01, 0x00, 0x02, 0x04, 0x06, 0x94]);
	let char_ =  b.append(b"x");
	let wchar =  b.append(&[b'X', 0x00]);

	ToyTest {
		image: b.finish(),
		name:  "<toy_test_data>",
		labels: vec![],
		data: vec![
			("bfalse".to_string(), VA(bfalse), Type::Bool,  1),
			("btrue" .to_string(), VA(btrue),  Type::Bool,  1),
			("ubyte" .to_string(), VA(ubyte),  Type::U8,    1),
			("sbyte" .to_string(), VA(sbyte),  Type::S8,    1),
			("ushort".to_string(), VA(ushort), Type::U16,   2),
			("sshort".to_string(), VA(sshort), Type::S16,   2),
			("uint"  .to_string(), VA(uint),   Type::U32,   4),
			("sint"  .to_string(), VA(sint),   Type::S32,   4),
			("ulong" .to_string(), VA(ulong),  Type::U64,   8),
			("slong" .to_string(), VA(slong),  Type::S64,   8),
			("char"  .to_string(), VA(char_),  Type::Char,  1),
			("wchar" .to_string(), VA(wchar),  Type::WChar, 2),
		]
	}
}

// ------------------------------------------------------------------------------------------------

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

const UNK_SIZE_CUTOFF: usize = 128;
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
		let len = span.len().min(UNK_SIZE_CUTOFF as Offs);
		let slice = seg.image_slice(ea .. ea + len);
		let data = slice.data();
		let seg_name = seg.name().yellow();

		for (i, chunk) in data.chunks(UNK_STRIDE).enumerate() {
			let mut bytes = String::with_capacity(chunk.len() * 3);

			bytes.push_str(&format!("{:02X}", chunk[0]));

			for byte in &chunk[1 ..] {
				bytes.push_str(&format!(" {:02X}", byte));
			}

			let addr = prog.fmt_addr(va.0 + (i * UNK_STRIDE) as Offs);
			println!("{:>4}:{} {}", seg_name, addr, bytes.truecolor(255, 127, 0));
		}

		if span.len() > UNK_SIZE_CUTOFF as Offs {
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
	let msg = format!("; {} byte(s), type {:?}", size, data.ty());
	println!("{}: {}", prog.name_of_ea(start).name.truecolor(127, 63, 0), msg.green());

	let seg = prog.segment_from_ea(start);

	if seg.is_real() {
		let slice = seg.image_slice(start .. start + size);
		let vaddr = prog.fmt_addr(seg.va_from_ea(start).0);

		println!("{} {:>4}:{}  {}", start, seg.name().yellow(), vaddr,
			interpret_data(prog, data.radix(), data.ty(), &slice));
	}
}

fn interpret_data(prog: &Program, radix: Radix, ty: &Type, slice: &ImageSlice) -> String {
	use Type::*;

	let endian = prog.endianness();

	match ty {
		Bool => format!("{}", slice.read_u8(0) != 0),

		S8  => interpret_int(slice.read_u8(0) as i8 as i64, 8, radix),
		S16 => interpret_int(slice.read_u16(0, endian) as i16 as i64, 16, radix),
		S32 => interpret_int(slice.read_u32(0, endian) as i32 as i64, 32, radix),
		S64 => interpret_int(slice.read_u64(0, endian) as i64, 64, radix),

		U8  => interpret_uint(slice.read_u8(0) as u64, 8, radix),
		U16 => interpret_uint(slice.read_u16(0, endian) as u64, 16, radix),
		U32 => interpret_uint(slice.read_u32(0, endian) as u64, 32, radix),
		U64 => interpret_uint(slice.read_u64(0, endian)       , 64, radix),

		Char  => interpret_char(slice.read_u8(0) as char),
		WChar => {
			let v = slice.read_u16(0, endian);

			match std::char::from_u32(v as u32) {
				Some(c) => interpret_char(c),
				None    => interpret_uint(v as u64, 16, Radix::Hex),
			}
		}

		Array(arrty) => {
			let mut ret = String::with_capacity(to_usize(arrty.len() * 4));
			let sub_ty = arrty.ty();
			let stride = prog.type_sizeof(sub_ty).fixed();

			for i in 0 .. arrty.len() {
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
			interpret_data(prog, Radix::Hex, pt.kind(), slice)
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

fn mask_to(nbits: usize, v: u64) -> u64 {
	if nbits < 64 {
		v & ((1 << nbits) - 1)
	} else {
		v
	}
}

fn interpret_int(val: i64, bits: usize, radix: Radix) -> String {
	match radix {
		Radix::Bin => format!("0b{:0width$b}", mask_to(bits, val as u64), width = bits),
		Radix::Dec => format!("{}", val),
		Radix::Hex => format!("0x{:0width$X}", mask_to(bits, val as u64), width = bits / 4),
	}
}

// really the only difference is the Dec case
fn interpret_uint(val: u64, bits: usize, radix: Radix) -> String {
	match radix {
		Radix::Bin => format!("0b{:0width$b}", mask_to(bits, val), width = bits),
		Radix::Dec => format!("{}", val),
		Radix::Hex => format!("0x{:0width$X}", mask_to(bits, val), width = bits / 4),
	}
}

fn show_func_piece_header(prog: &Program, func: &Function) {
	let divider =
		"; -------------------------------------------------------------------------".green();

	println!("{}", divider);
	let name = prog.name_of_ea(func.ea());
	println!("{}{}{}", "; (Piece of function ".green(), name.name.green(), ")".green());
}

fn show_func_header(prog: &Program, func: &Function) {
	let divider =
		"; -------------------------------------------------------------------------".green();

	println!("{}", divider);

	let name = prog.name_of_ea(func.ea());
	println!("{}{}", "; Function ".green(), name.name.green());

	println!("{}{}", "; ".green(), format!("{:?}", func).green());

	// TODO: rewrite this using some new API
	// if let Some(usage) = func.reg_usage() {
	// 	println!("{}{}", "; Argument registers: ".green(), format!("{:?}", usage.args()).green());
	// 	println!("{}{}", "; Return registers: ".green(), format!("{:?}", usage.rets()).green());
	// 	println!("{}{}", "; Clobbered registers: ".green(), format!("{:?}", usage.clobbers()).green());
	// }

	if !func.attrs().is_empty() {
		let attrs = format!("{:?}", func.attrs());
		println!("{}{}", "; Attributes: ".green(), attrs.green());
	}

	if func.is_multi_entry() {
		let entrypoints = func.entrypoints().iter().map(|bbid| prog.get_bb(*bbid).ea());

		print!("{}", "; Entry points: ".green());

		for ea in entrypoints {
			print!("{} ", prog.name_of_ea(ea).name.green());
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
			print!(" {}{}", "<-".green(), prog.name_of_ea(r).name.green());
		}

		println!();

		println!("{:20}{}:", "", prog.name_of_ea(bb_ea).name.truecolor(127, 63, 0));
	}

	// MMU state
	println!("{}", format!("; mmu state = {:?}", bb.mmu_state()).green());

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

		let vaddr = prog.fmt_addr(inst.va().0);
		// let mnem = prog.inst_get_mnemonic(inst);
		// let ops  = prog.inst_operands_to_string(inst, state);

		// println!("{:>4}:{}  {:8}      {:3} {:30}",
		// 	seg.name().yellow(), vaddr, bytes.truecolor(63, 63, 255), mnem.red(), ops);

		print!("{} {:>4}:{}  {:8}      ", inst.ea(), seg.name().yellow(), vaddr,
			bytes.truecolor(63, 63, 255));
		let mut output = AnsiConsolePrintOutput;
		prog.inst_print(inst, state, &mut output).unwrap();

		// Outrefs
		if let Some(or) = prog.get_outrefs(inst.ea()) {
			print!(" {}", ";".green());

			for &r in or {
				print!(" {}{}", "->".green(), prog.name_of_ea(r).name.green());
			}
		}

		println!();
	}

	// Terminator
	use BBTerm::*;
	match bb.term() {
		DeadEnd => println!("{}", "---------- DEAD END ----------".red().bold()),
		StateChange { state_after, .. } => {
			println!("{}",
				format!("---------- STATE CHANGE {:?} ----------", state_after)
				.cyan().bold());
		}
		Halt | Return { .. } => {
		}
		FallThru { cont } => {
			print_divider_if_diff_funcs(prog, bb_ea, *cont, "Fall through", Color::Yellow);
		}
		Jump { dst } => {
			print_divider_if_diff_funcs(prog, bb_ea, *dst, "Tailcall", Color::Yellow);
		}
		Call { cont, cond, .. } => {
			if *cond {
				print_divider_if_diff_funcs(prog, bb_ea, *cont, "Fall through with cond call",
					Color::Yellow);
			} else {
				print_divider_if_diff_funcs(prog, bb_ea, *cont, "Fall through", Color::Yellow);
			}
		}
		IndirCall { cont, .. } => {
			print_divider_if_diff_funcs(prog, bb_ea, *cont, "Fall through", Color::Yellow);
		}
		Cond { dst, cont } => {
			print_divider_if_diff_funcs(prog, bb_ea, *dst, "Tailbranch", Color::Yellow);
			print_divider_if_diff_funcs(prog, bb_ea, *cont, "Fall through", Color::Yellow);
		}
		IndirJump { .. } => println!("{}", "---------- JUMP TABLE ----------".yellow())
	}

	println!();
}

fn print_divider_if_diff_funcs(prog: &Program, from: EA, to: EA, msg: &str, color: Color) {
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
