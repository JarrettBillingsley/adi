
# Yak stack

brainstorming how `const_addrs` should work/what info it should report

AND ALSO

make const prop build ASTs for constant provenance
...to finish writing IR analysis
...to replace what used to be done by the interpreter (dynamic bank change analysis)

AND THEN

write IR compilers for the real arches (oof)

**FUNCTION SPLITTING:** is the predecessor BB to the new entrypoint marked as fallthrough/jump?
**am I converting to EAs too early???** first pass of function analysis is already trying to find EAs for like control flow targets. seems premature. is it even necessary? intra-function control flow is almost certainly (but not necessarily) going to stay within the same segment...
**evaluate uses of `usize`** - I think I should be using `u64` instead in some places

# Tasks!

- Analysis priority
	- it's something that bounced around in my head and then I saw that IDA explicitly does this
	- diff analysis phases have priorities over others
		- so e.g. "finding new code" takes precedence over "argument/retval analysis"
	- this makes sense cause some of the later phases do better work with more information made available from earlier phases
	- IDA implements it by having a different queue for each phase, and (presumably) taking the next action item from the highest-priority queue first
- IR stuff
	- apply results of const prop to the IR? rewrite it?
		- would definitely simplify some things like the `const_addrs` iteration - rather than having to double-check that a register is constant, just... have a constant there.
		- should `IrConst` have a field for provenance AST reference?
		- **questions:**
			- could this trigger another round of const prop? I mean yes it could
			- how would this interact with the constant provenance AST?
	- IR DSE (dead store elim - eyyy there's some commented-out code at the bottom of `ssa.rs`!), which kinda also depends on:
	- bottom-up function argument/return value/clobber list determination to prune down the number of `use()` and `= <return>` IR instructions around `call` and `ret` IR instructions
		- use of SSA gen 0 vars indicate arguments
		- *caller's* use of `<return>` values indicate *callee's* return(s)
		- use of non-gen-0 before `ret` indicates clobbers
	- ooh! stack tracking! yeah that'll also help improve data flow analysis
- modifying functions
	- removing BBs (mis-analyzed code e.g. after a switch jump or a no-return call)
	- adding BBs
- data analysis
	- mapper RAM banking
- GB
	- more MBCs
- jump table analysis
- function attributes
	- e.g. "bankswitch", "jumptable"
- "points of interest" to let user know things to investigate
	- bank switches that can't be automatically determined
	- jumptables
	- functions that access particular hardware registers
- more NES mappers

# TODO:

- **features**
	- comments (line, repeatable)
		- on code, data items, enum values, struct members..
	- custom fields on `Instruction` and `Operand`
		- for e.g. remembering which instruction description it is so we don't have to keep looking it up, operands that don't fall into one of the provided categories, etc.

- **design issues**
	- `Instruction::next_ea()` is fundamentally dangerous.
		- it can produce invalid EAs.
		- it's not used in many places, so it shouldn't be hard to replace it...
		- really it's the program that knows what the "next" address is.
		- ...really the next address is a *VA*, so getting the "next" EA is a bit meaningless.
	- write some more FUCKING tests
	- `RefMap`: does this need to be `BTreeMap/Set`? (do we need ordering?)
	- Disassemblers and Printers can take ctor arguments
		- have to be able to account for that in IArchitecture.
	- Names should be more than just Strings...
		- `Name::Hardware` (for MMIO regs, vector locations etc)
		- `Name::AutoGen` (not actually in the name table, just used for display)
		- `Name::User` (user-given)
	- **Should `Function/DataItem` have a name field *in addition to* the name map??** maybe not
	- evaluate what really should be `pub`, `pub(crate)`, `pub(super)`, or private
	- BB MMU state could really be more than just one thing...
		- `Unknown` for that
		- `Single(state)` if the MMU is in just one state
		- `Multi(Vec<MmuState>)` if the MMU could be one of many states?
		- how to deal with functions for which the MMU state can be multiple possibilities?
	- what does `StateChange::Maybe` *mean?*
		- like under what circumstances would that happen?
		- and what would you do to handle it?
		- I guess I was anticipating this as being useful for indirect stores, but on SM83/Z80 that's like, 98% of all stores, so not terribly useful.
		- well.. **I guess it's for cases like "store into `0x1FF0 + x` where `0x2000` is an MMIO reg, so *maybe* it gets written and *maybe* it doesn't.** I think that's it.
	- GB arch handling of operands is... messy
		- you've got `GBOpKind` that says how to *decode* any explicit operands; indirectly based on that, you've got the actual operands in the instruction; and then you've got `SynOp` which says how to *display* the instruction, which mixes implied and explicit operands
		- it's kind of a lot
		- it seems to work for now so not the highest priority but...
		- if we add **custom operands,** it might be worth redoing this

- **analysis**
	- **implied operands**
		- some of that can come from running the code to see what addresses are actually accessed
	- **marking functions as "bankswitch functions"**
		- if it e.g. takes the bank to switch as an argument
		- ofc user can help with this, but we should be able to identify candidates
	- **dead-end/invalid control flow back-propagation**
		- if we hit a dead end, that's a sign that the control flow that got us here is mis-analyzed - maybe an "unconditional conditional branch" or sth
		- also common with *jump table functions* - right after the call is *not code*
	- **conditional calls/returns**
		- 8080/Z80 is fuckin weird and lets you do this??
		- in analysis, treat conditional return as a conditional branch
			- end BB, and push next instruction as potential BB
		- `InstructionKind` needs to represent these
			- as a field of `Call/Ret`? as different variants (`CondCall/CondRet`)?
	- **jumptable analysis**
		- should support multiple strategies (depending on the arch), e.g.
			- absolute
			- PC-relative
			- jumptable-base-relative
		- should support *jumptable functions* - call a function to perform the switch
	- **CFG merge points:** have to check if the MMU state is the same on all predecessors
		- if not, what do we do? whine? represent the state as some "union"?
		- right now the `BBStateChanger` returns an `Err`
	- **functions for which the MMU state can be multiple possibilities**
		- e.g. common functions called from multiple banks
		- and the MMU state on *exit* from a function can be different than on entry
	- **stack pointer tracking** can also be useful
		- if a function makes the stack pointer go *past its return address* then that's a pretty strong signal it's doing something funky, like implementing a jump table

- **arch/platform-specific**
	- 65xx
		- MMU doesn't yet handle external RAM
		- loader incorrectly sets `Image::orig_offs` due to Ines not supporting that
		- std labels need data item once data is implemented
		- UXRom linear performance issue
	- SM83/Z80
		- in these arches, most memory accesses are done indirectly through registers
			- e.g. `ld hl, 0xC004`, `ld a, [hl]`
		- this complicates reference analysis because you have to, like, back-propagate info about how a register is used
			- that might not be a bad idea... backwards dataflow analysis to say, when it sees `[hl]` as an operand, then `hl` is an address before, so if `hl` (or individually `h` and `l`) is loaded with a constant, that constant is a memory address
			- even a simple local analysis might yield good results, since that pattern of "load an address, then immediately access it" seems to be super common
		- oooooh, but then that opens up the possibility of having to do bank change analysis *after* figuring out memory addresses!
			- which could in turn open up more references... ahaha

---

## Big Assumptions

- **individual instructions (like, their bytes) will never cross segments.**
	- `https://wiki.nesdev.com/w/index.php/Tricky-to-emulate_games` lists only a *single* game that breaks this rule (The Magic of Scheherazade)
	- for practical reasons, I really doubt programmers would do this very much

- **a single function's code will be entirely contained within one segment.**
	- if it isn't, we can split it into two functions and have the first tailcall the other.
	- this doesn't account for functions that have a BB in the other segment and like, bounce back and forth but cmon really?

- **if the *currently-executing bank* is swapped out by its own code, then the code at that address will be the same in *all banks.***
	- this avoids the conceptual tarpit of not knowing what the *next instruction* will be because *any instruction* could possibly change the banks
		- and cmon, humans wouldn't do that. instead, they'd have the same code at the same place.
	- this seems to be true for battletoads (at least initial investigation shows)
	- **corollary:** the *same function* can appear in **multiple banks**
		- this is the case for the Battletoads bankswap function near the end of the ROM
		- so we have to be able to detect and de-duplicate them

- **a bank will only appear at ONE virtual address.**
	- that is, when it's mapped in, it's always mapped in at the same address (same window).
	- in some cases this is trivially true, e.g. mappers where only one window is bankable
	- but in MMUs where the same bank *could* be mapped to multiple windows, it's still not likely to be a problem, because these CPUs don't really support position-independent code/data
		- besides, we *can* track and detect this, so we can error out
	- **the only way we can get an EA is from a Segment whose base VA is known.**
		- either it's statically known (hardwired)...
		- or we determined it using some MMU state.
		- so, EA -> VA should always be possible/safe and not require any MMU state.
		- **however,** to do this may require that we set a segment's base VA when we discover what it should be when the MMU state changes.
			- that seems like it should happen during bank change analysis.

---

## IR Analysis thoughts

**AAAAAAA SOMETHING TO REMEMBER:** we also have to deal with the fact that **function calls** can modify all our "locals" (registers). Some kind of analysis/annotation may be able to tell us that a function call preserves certain regs, but pessimistically we have to assume that after each call, there is an assignment of some "unknown" value to every reg. (Since calls are already BB terminators, this could be done by inserting an extra BB between a call and its successor which just contains these assignments and falls through to the successor.)

This could be done by having a special IR Src that means "from a call." If any of these variables are actually used, it indicates that the callee has return values; otherwise, they will just be stripped by dead store elimination.

**CONVENIENT THING:** because of the way the algorithm works, after SSA renaming, any variable subscripted with 0 is an argument. So, we don't have to like, assign everything a special value at the beginning of the function.

**SOMETHING ELSE TO CONSIDER:** will we still have "ir branching"? If so, will that be part of the "real" CFG? If not, how will those kinds of instructions (e.g. 68K `movem`, Z80 `ldir`) be represented/handled? (Since they can't cause "real" control flow, maybe they can be represented by just recording their "end-state" effects, like "now BC = 0" etc.)

**IMPORTANT:** originally I was thinking paired registers would overlap like in Sleigh, but now I'm going against that. Registers may not overlap in any way in the IR, since overlapping complicates SSA.

---

**Dead store elimination:** the problem with doing any kind of dead store elim on this SSA is that we don't actually know which values are used **in the presence of function calls and returns.**

At any return point (and there can be more than one in the function!), any currently-live value *might* be a return value. We don't know, because there are no calling conventions. Similarly before a call, any value *might* be an argument.

Argument and return value analysis would have to be done in a particular order - on the **call graph,** from leaves to root. Basic idea:

- If a function uses a _0 value, **that is an argument to that function.**
- If the caller uses one of those auto-generated defs that comes after a `call` instruction, **that is a return value from the *callee*.**

Each arch's IR compiler needs to give more info to the IR analyzer:

- A list of regs which can be used as args (like, all of them)
- A list of regs which can be used as returns (again, all of them)

These can be used as the "worst case" starting points for the algorithm which can then prune them down from there (e.g. if a function never uses a _0 reg, then it cannot possibly be an argument........ unless it calls a function which does, and it just passes that reg through!)

Insight: *if a function uses a _0 reg at all return points, then it does not take/return that register at all.* Why:

- Each return is annotated with uses of all currently-live registers.
- If that register is changed at any point in the function, it will have a nonzero generation.
- If there are multiple returns, and it's been changed in any control path, then at least one of the return uses will use a nonzero generation of that register.
- Therefore, if all returns use the _0 version, then it can't have been assigned anywhere (even in a callee), and is unaffected by the function.

---

## Function analysis phase ordering

1. Find rough bounds and collect BBs.
2. MMU state change analysis.
	- Convert to IR/SSA
	- Do constant propagation
	- Recover addresses and attach to instructions
	- Determine when state changes happen
	- Propagate state change info
3. References.
	- Use address info from phase 2 to determine references to other entities
	- Can cause phases 4 and 5
4. Splitting.
	- Refs phase may have found a call into the middle of an existing function
	- After splitting, does anything need to be redone...?
5. Jump table analysis.
	- Refs phase finds jump table candidates
	- This hasn't really been done, yet
6. Argument/return value analysis??
	- This has to be done on the call graph, so maybe once all the other phases complete...

---

## NES Mappers

Most common in descending order: 1, 4, 2, 0, 3, 7, 206, 11, 5, 19

- 1 (mmc1/sxrom) **e.g. dragon warrior**
	- *PRG ROM*
		- each bank is **16KB**
		- PRG banking unused/disabled if prg0 size <= 32KB
		- mode 0: entire 32KB `8000-FFFF` swapped in pairs of banks
			- bank pair is given by even number (0, 2, 4..) - low bit ignored
			- so `{ 8000 bank n, C000 bank n + 1 }` for n in 0, 2, 4..
		- mode 2: `{ 8000 bank 0,    C000 swappable }`
		- mode 3: `{ 8000 swappable, C000 bank -1   }`
	- *PRG RAM*
		- optional, each bank is **8KB**, up to 4 banks, mapped to `6000-7FFF`
- 4 (mmc3/txrom, mmc6/hkrom) - **e.g. mario 3**
	- **mmc6** is almost same except it has built-in PRG RAM?
	- *PRG ROM*
		- each bank is **8KB**, up to 64 banks
		- frames at `8000, A000, C000, E000`
		- mode 0: `{ 8000 swappable, A000 swappable, C000 bank -2,   E000 bank -1}`
		- mode 1: `{ 8000 bank -2,   A000 swappable, C000 swappable, E000 bank -1}`
	- *PRG RAM*
		- unbanked, optional **8KB** at `6000-7FFF`
- 2 (uxrom) **e.g. mega man 1**
	- *PRG ROM*
		- each bank is **16KB**
		- frames at `8000, C000`
		- `{ 8000 swappable, C000 bank -1 }`
	- *PRG RAM*
		- none.
- 0 (nrom) **e.g. mario 1, duck hunt, 10-yard fight**
	- *PRG ROM*
		- no banking; either 16K or 32K; if 16K, starts at `C000`
	- *PRG RAM*
		- in Family Basic only, 2/4K at `6000`, mirrored until `7FFF`
- 3 (cnrom) **e.g. arkanoid**
	- *PRG ROM*
		- no banking, just 32K.
	- *PRG RAM*
		- none.
- 7 (axrom) **e.g. battletoads**
	- *PRG ROM*
		- just one **32KB** bank at `8000`, up to 8 banks.
	- *PRG RAM*
		- none.
- 206 (mimic-1, namcot 118) **e.g. gauntlet**
	- *PRG ROM*
		- predecessor of MMC3
		- each bank is **8KB**, up to 16 banks
		- frames at `8000` and `A000`
		- `{ 8000 swappable, A000 swappable, C000 bank -2, E000 bank -1 }`
	- *PRG RAM*
		- none.
- 11 (color dreams) **e.g. exodus (lmao)**
	- *PRG ROM*
		- just one **32KB** bank at `8000`, up to 4 banks.
	- *PRG RAM*
		- none.
- 5 (mmc5/exrom) **e.g. castlevania 3**
	- *PRG ROM*
		- ..... it's complicated.
	- *PRG RAM*
- 19 (namco N129/N163) **e.g. star wars (N129), rolling thunder (N163)**
	- *PRG ROM*
		- **8KB** banks, up to **512KB** (64 banks) total
		- frames at `8000, A000, C000, E000`
		- `{ 8000 swappable, A000 swappable, C000 swappable, E000 bank -1}`
	- *PRG RAM*
		- *optional* battery-backed **8KB** at `6000`
	- also may have 128B internal battery-backed RAM
		- that's used for the wavetable synth at runtime, but I guess they use it for savegames when it's powered off

---

# Data blathering

- need to be able to represent **types**
- would be nice to have *different types for read and write*
	- that comes up  *A  L O T*  in MMIO
	- e.g. reading 0x2000 gets a ROM byte; writing 0x2000 changes MMU state!

- **Primitive types**
	- `i/u8/16/32/64`
		- `i/u24?` (I *know* I've seen it in some games, but is it common enough to warrant?)
	- `bool` (1-byte)
	- `char` (ASCII)
	- `wchar` (it was that era, after all... I've seen it)
	- floating point?? how new do we wanna go
		- well **fixed point** might be good, even NES games use it for subpixels and stuff
- **Strings**
	- arrays of char/wchar, *but*
	- zero-terminated is common
	- other terminators can exist (I feel like I've seen that in e.g. text box scripts)
	- pascal-style...?
- **Enums**
	- mmmmmyep
	- not much to say?
- **Bitfields**
	- deeeefinitely need these, and more intuitively than IDA does em
	- something that comes up a lot that IDA doesn't handle well at all is like...
		- `| flag3 | flag2 | flag1 | flag0 |               data                 |`
		- so you might have `FLAG3 | FLAG1 | 2`
		- and IDA shits the bed about this and complains that 2 isn't a valid bitfield value
		- so you'd have to make `DATA_0`, `DATA_1`, ... `DATA_255` to represent it and ugh
	- some kind of **bitfield macro/shorthand** facility would be nice
		- e.g. `VDP_SET_ADDR(0xE000)` instead of some ugly-ass scrambled mess
		- or names for common combinations of fields like `X = A | B | C`
- **Structs**
	- IDA does em pretty good
	- BUT: **trailing/variadic arrays**
		- I've run into those a few times and IDA doesn't handle them at all
- **Arrays**
	- yeeeeeep
	- some facility to make working with *parallel arrays* easier would be nice
		- since these architectures often prefer this arrangement of memory
- **Pointers**
	- OUCH!!!!!!!
	- worthwhile to have both relative (short) and absolute (far) pointers?
		- probably, for "offsets" (e.g. PC-relative jumptables)
		- offsets may also need a custom base

---

- a single data item has a location, a type, and a size.
- its size is >= its types minimum size.