
# Yak stack

- I don't think OpInfo::VARef is needed
	- its job can be done with an unresolved EA

# Imminent tasks!

- detect "always taken" branches (IR `cbranch` instructions where condition is constant)
	- 10yf examples
		- PRG0:A9A6
		- PRG0:BFA9
		- PRG0:C17F
	- duck hunt
		- PRG0:F844
- IR instruction name/method name/outputs are needlessly inconsistent.
- refs pass needs to notify any existing referenced functions of the MMU state flowing into them...
	- would that trigger a re-state-analysis? maybe only if the new state differs from the old
- write GB IR compiler

# TODO:

- **Features**
	- **Function attributes**
		- e.g. "bankswitch", "jumptable"
		- for bankswitch functions, we could **let the user specify some formula for them**
			- like "when this function is called, the MMU state is set to `(A & 0x7) | 0x10`"
			- and there can be a little expression parser which turns it into an IR template that can be blobbed down in place of each time the function is called
	- **Comments (line, repeatable)**
		- on code, data items, enum values, struct members..
	- **Custom fields on `Instruction` and `Operand`**
		- for e.g. remembering which instruction description it is so we don't have to keep looking it up, operands that don't fall into one of the provided categories, etc.
	- **Alternate mnemonics for instructions**
		- e.g. on x86 there are `jz` and `je`, which are technically two names for the same instruction, but in some contexts it's being used to check for zero and in other for equality... would be a nice quality-of-life addition
			- same thing on Mos65xx, `bcs` and `bge` are aliases
	- **"Points of interest" to let user know things to investigate**
		- state changes that can't be automatically determined
		- jumptables
		- functions that access particular hardware registers
	- **Generating "name + delta" output is a little more subtle than my first attempt**
		- really these should be rare
	- **Modifying functions**
		- removing BBs (mis-analyzed code e.g. after a switch jump or a no-return call)
		- adding BBs...?

- **Design issues**
	- **Should IrFunction hold a ref to the owning function?**
		- would prevent issues like modifying a function and then using the outdated IR
	- **Does state change analysis needs to take multiple entry points into account?**
	- **License: GPL3?**
		- it's what Mesen uses and I'm referencing that heavily for Mos65xx so idk
	- **Refactor `Analysis` cause it really seems to be more like "a function's CFG"**
	- **Write some more FUCKING tests**
	- **Evaluate what really should be `pub`, `pub(crate)`, `pub(super)`, or private**
	- `Instruction::next_ea()` is fundamentally dangerous.
		- it can produce invalid EAs past the end of the segment it appears in.
		- it's not used in many places, so it shouldn't be hard to replace it...
		- really it's the program that knows what the "next" address is. it's either:
			- a resolved EA in the same segment
			- an unresolved EA
	- **Does `RefMap` need ordering? (Does this need to be `BTreeMap/Set`?)**
		- I feel like no...
	- **Disassemblers and Printers can take ctor arguments**
		- have to be able to account for that in IArchitecture.
	- **Names should be more than just Strings**
		- `Name::Hardware` (for MMIO regs, vector locations etc)
		- `Name::AutoGen` (not actually in the name table, just used for display)
		- `Name::User` (user-given)
	- **Should `Function/DataItem` have a name field *in addition to* the name map??** maybe not
	- **GB arch handling of operands is... messy**
		- you've got `GBOpKind` that says how to *decode* any explicit operands; indirectly based on that, you've got the actual operands in the instruction; and then you've got `SynOp` which says how to *display* the instruction, which mixes implied and explicit operands
		- it's kind of a lot
		- it seems to work for now so not the highest priority but...
		- if we add **custom operands,** it might be worth redoing this
	- **Evaluate uses of `usize/isize`**
		- I think I should be using `u64/i64` instead in some places
		- well indexing slices `s[i]` requires `i` to be `usize`, so I think anything related to accessing the underlying data should be `usize`.
		- also I think it might be good to have type aliases like IDA's `addr_t` for `u64` (and an equivalent for `i64`... `delta_t`? `offs_t`?)
			- this way it's clear when something is referring to "a big unsigned int" vs. "an abstract address/offset from an address"
		- offenders:
			- `VA`
			- `EA` (multiple methods/impls)
			- `Segment::size`
			- `SpanMap`
			- `Memory::len`
			- `Memory::fmt_addr`
			- `ImageSlice<usize>`
			- `ImageSliceable<usize>`
	- **Custom IR instructions?**
		- How are multi-step instruction (e.g. 68K `movem`, Z80 `ldir`) represented/handled in the IR? Since they can't cause "real" control flow, maybe they can be represented by just recording their "end-state" effects, like "now BC = 0" etc.
		- because of that, might be useful to have a custom IR instruction type for things like this. I think Ghidra Pcode does.

- **Analysis**
	- **Make const prop build ASTs for constant provenance**
	- **IR Dead Store Elimination**
		- ties into argument/return value/clobber analysis
	- **Data analysis**
		- mapper RAM banking
		- **much more is written below**
	- **Marking functions as "bankswitch functions"**
		- if it e.g. takes the bank to switch as an argument
		- ofc user can help with this, but we should be able to identify candidates
	- **Dead-end/invalid control flow back-propagation**
		- if we hit a dead end, that's a sign that the control flow that got us here is mis-analyzed - maybe an "always-taken conditional branch" or sth
		- also common with *jump table functions* - right after the call is *not code*
	- **Conditional calls/returns**
		- 8080/Z80 is fuckin weird and lets you do this??
		- in analysis, treat conditional return as a conditional branch
			- end BB, and push next instruction as potential BB
		- `InstructionKind` needs to represent these
			- as a field of `Call/Ret`? as different variants (`CondCall/CondRet`)?
	- **Jumptable analysis**
		- should support multiple strategies (depending on the arch), e.g.
			- absolute
			- PC-relative
			- jumptable-base-relative
		- should support *jumptable functions* - call a function to perform the switch
	- **BBs and functions for which the MMU state can be multiple possibilities**
		- BB MMU state could really be more than just one thing...
			- `Dynamic` if the MMU state cannot be *statically* determined
			- `Single(state)` if the MMU is in just one state
			- `Multi(Vec<MmuState>)` if the MMU could be one of many states
		- this means each *function* can also have multiple MMU states on entry
			- e.g. common functions called from multiple banks
		- and the MMU state on *exit* from a function can be different than on entry
		- this would have a pretty big knock-on effect... lots of things depend on a BB's state
			- VA => EA translation
				- with multiple states, one VA could refer to:
					- a single EA (where state doesn't matter)
					- a single EA (where state *does* matter but every state maps to same EA)
					- multiple EAs
				- and consequently function discovery during refs pass
					- it'd be wonderful for that! dynamic dispatch to multiple functions across multiple banks at the same VA...
			- name lookup
				- and consequently instruction printing...
			- state change analysis
				- which actually would handle this just fine already, it's written for it
	- **Stack pointer tracking**
		- IR makes this straightforward (ha... ha ha.....)
		- if a function makes the stack pointer go *past its return address* then that's a pretty strong signal it's doing something funky, like implementing a jump table
		- this can also improve dataflow analysis - constants that are pushed/popped can be tracked through the virtual stack!
			- TRICKY in the presence of calls tho
	- **Detect and de-duplicate identical functions in multiple banks**
		- e.g. the NES Battletoads bankswitch function
	- **Ensure each segment's base VA is the same every time it's mapped in**
		- this is a big fuckin assumption on my part, that each e.g. ROM block will be mapped into the same VA window every time it's accessed
		- during state change analysis, whenever state changes, check with the mapped-in segments to see if their VA is the same as it ever was
	- **Argument/return value/clobber analysis**
		- very low-priority pass, done on whole program call graph
		- can be used to prune `use` and `=<return>` in IR, which gives better info for const prop, which allows better MMU state determination
			- so it could trigger MMU state analysis again on just about every function in the program lol
		- **much more is written below**

- **arch/platform-specific**
	- **NES**
		- MMU doesn't yet handle external RAM
		- loader incorrectly sets `Image::orig_offs` due to Ines not supporting that
		- std labels need data item once data is implemented
		- UXRom linear performance issue
		- more mappers (remember to set segment base VA when state changes)
	- **GB**
		- more MBCs (remember to set segment base VA when state changes)
	- **Mos65xx**
		- there are more (unofficial) variations of `NOP`
		- correct `DOP` addressing modes
		- implement `TOP`
		- implement unofficial opcodes in some way?
			- maybe not actually implement them in IR but at least disassemble them
		- **CPU variants/revisions?**
			- Early (no ROR)
			- NMOS
			- CMOS (slightly different behavior
		- decimal mode (ew)
			- that would require dataflow analysis in and of itself... UGH...

---

## Dead store elimination and argument/return value/clobber analysis thoughts

The problem with doing any kind of dead store elim on this SSA is that we don't actually know which values are used **in the presence of function calls and returns.**

I will define **exit point** as anywhere where control flow leaves the function permanently. A return *is just one example;* there are also tailcalls, tailbranches, fallthroughs, etc.

At any exit point (and there can be more than one in the function!), any currently-live value *might* be a return value. We don't know, because there are no calling conventions. Similarly before a call, any value *might* be an argument.

Argument and return value analysis would have to be done in a particular order - on the **call graph,** from leaves to root. Basic idea:

- If a function uses a _0 value, **that is an argument to that function.**
- If the caller uses one of those auto-generated defs that comes after a `call` instruction, **that is a return value from the *callee*.**

Each arch's IR compiler needs to give more info to the IR analyzer:

- A list of regs which can be used as args (like, all of them)
- A list of regs which can be used as returns (again, all of them)

These can be used as the "worst case" starting points for the algorithm which can then prune them down from there (e.g. if a function never uses a _0 reg, then it cannot possibly be an argument........ unless it (tail)calls a function which does, and it just passes that reg through!)

A **return point** is special kind of exit point: where the function returns. A function can have multiple return points.

Insight: *if a function **only** has return points, and uses a _0 reg at **all** return point points, then it does not take/return that register at all.* Why:

- If there are *only* return points (and no other kinds of exit points), then the register cannot be needed by this function, the callee. The caller may need it, but *not* the callee.
	- (By contrast, **if there are any non-return exit points (e.g. a tailcall),** it could mean that the register is an argument passed through to the tailcalled function!)
- Each return point is annotated with uses of all currently-live registers.
- If that register is changed at any point in the function, any use of it thereafter will have a nonzero generation.
- If there are multiple return points, and it's been changed in **any** control path, then **at least one** of the return point uses will use a nonzero generation of that register.
- Therefore, if all return points use the zero generation, then it can't have been assigned anywhere and is unaffected by the function.
	- **This definition even extends to functions which call others!** If we know a callee doesn't touch register R, then the `R_n = <return>`s after the `call` will be pruned, leading to `R_0` still being the most recent generation.

The *meaning* of "is unaffected" is ambiguous, however. It could be:

- A caller-saved register that was never used
	- e.g. `t` registers in MIPS, most go unused in most functions
- A callee-saved register that was not needed
	- e.g. `s` registers in MIPS; not using one maintains the agreement that they will have the same value on exit as on entry
- Something that really *was* an argument, but the code was rewritten and it went unused, so even though the caller sets it before each call, it never gets read
- I'm sure there are other cases I'm not thinking of...

---

# Data blathering

- need to be able to represent **types**
- would be nice to have *different types for read and write*
	- that comes up  *A  L O T*  in MMIO
	- e.g. reading 0x2000 gets a ROM byte; writing 0x2000 changes MMU state!
- a single data item has a location, a type, and a size.
- its size is >= its types minimum size.

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
