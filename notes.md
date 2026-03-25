
# Yak stack

# Major tasks

- IR correctness testing
- Jump table analysis, indirect jumps and calls
- Const prop provenance ASTs
- Data analysis
- Multi-state BBs/functions
- Argument/return value/clobber analysis
- Type propagation
- Stack analysis
- Undo/Redo support
- Save/Load support

# Imminent tasks!

- **Data analysis**
	- mapper external RAM and RAM banking
	- see "data blathering" below, tho I think most of that has been implemented
	- rather than using `Rc/RefCell` in `Type::Bitfield/Enum/Struct`...
		- have virtual segments with silly segment numbers, like 0xFFFE is for structs, 0xFFFD for enums, 0xFFFC for bitfields
		- use `EA` in those types to refer to them
	- this also gives us inrefs to data types for free, because they're at `EA`s so they can participate in `RefMap`
		- didn't *really* seem like a good idea to instantly deallocate a type the instant nothing was using it anyway
	- and this will make `Program` `Send`!
	- move data printing into `Program`
		- it can call some of the `IPrinter` methods for printing numbers, addresses etc.
- IR
	- `ValSize::_1` for bools?
	- god it'd be REALLY nice if the IR printing used the platform's actual register names instead of r0, r1, etc.
		- maybe `IIrCompiler` could have a `name_map` method that returns a `Vec<&'static str>` of register names
		- maybe there could be a macro to declare all the `IrReg`s for an arch that generates this vec for you cause it's already getting annoying (and error-prone, since you have to come up with the indexes yourself based on the sizes of the regs)
			- and it could also generate the lists of arg/return regs
		- or maybe not a macro just a builder that's lazily initialized
- Mos65xx IR:
	- reimplement rotates and uses of `iand` which could be bit instructions
- **Cleanup/reorganize both Mos65xx and Toy to match GB IR compiler (methods on `IrBuilder`, free functions instead of methods on `InstDesc`, method chaining)**
- **Put some sanity checking to ensure that IR insts that refer to operands *actually refer to real operands on the source instruction***
	- ...and that all operands in the source instruction are referenced by the IR
- **GB IR stress test - test *all* possible opcodes**
	- I'm just not sure I'm hitting them all with the test ROMs
- **IR/const prop correctness tests**
	- make a test ROM that can be run in an emulator
	- validate against emulator output (any emulators output memory traces?)
	- use a dummy testing mapper/MBC which can be used to output the contents of stores to specific location to output results of const prop
	
- detect "always/never taken" branches (IR `cbranch` instructions where condition is constant)
	- examples:
		- 10yf
			- PRG0:A9A6
			- PRG0:BFA9
			- PRG0:C17F
		- duck hunt
			- PRG0:F844
	- finding them should be simple
		- IR, consts, find all `cbranch` whose `cond` is constant, boom.
	- **BUUUUUUUUUUUUUUUUUT**
		- I could see this being a little too eager
		- I already ran into an issue with state change analysis where it determined a state change was constant... until a revisit, when it realized it was dynamic
		- so I don't think applying any *permanent* change to the function's CFG would be good
		- or maybe limit it to `cbranch`es whose `cond` is constant *and that constant has some kind of provenance that proves it could never be dynamic*
			- since 6502 has no uncond branch, it's common to do:
				lda #$10   ; nonzero,
				bne _label ; so always taken
			- in IR that'd be like (with constant info):
				mov     A, const 0x10   ; A    = Some(0x10) from <const 0x10>
				slt     NF, A, const 0  ; NF   = Some(0)    from <A, const 0>
				seq     ZF, A, const 0  ; ZF   = Some(0)    from <A, const 0>
				bnot    tmp1, ZF        ; tmp1 = Some(1)    from <ZF>
				cbranch tmp1, _label    ;
			- the chain of provenance leads to an `IrSrc::const` and *it's in the same BB*
			- **BUUUUUUUUUUUUUUUUUUUUUUUUUUT** something could split the BB between the `lda` and the `bne` and ffffffffffffffuck it all up lol. aAHGHAHGlLlL
	- I think what makes the most sense then is:
		- un-make the BB which can never be run
		- put a POI there
	- **BUT THIS IS IMPORTANT:** since this is changing the function's CFG, it means it needs to rerun the static analysis pass. **but that means it's rescheduling itself.**
		- so something else needs to be put at that address/on the BB that owns the terminator to say "hey, we analyzed this before, don't do it again" or else it could loop infinitely
- refs pass needs to notify any existing referenced functions of the MMU state flowing into them...
	- would that trigger a re-state-analysis? maybe only if the new state differs from the old

# TODO:

- **Design issues**
	- **`BBTerm` and `InstructionKind` really encode *control flow strategies***
		- so maybe that's what they should be named
		- also opens up ideas for "custom" control flow set by user (does it continue to the next PC? does it have a target? multiple targets? does it change state? etc)
	- **MMU State display/encoding**
		- for showing to the user (esp for more complex MMU state like GB MBC1)
		- for allowing the user to set the state manually
	- **Should IrFunction hold a ref to the owning function?**
		- would prevent issues like modifying a function and then using the outdated IR
	- **Does state change analysis needs to take multiple entry points into account?**
	- **License: GPL3?**
		- it's what Mesen uses and I'm referencing that heavily for Mos65xx so idk
	- **Write some more FUCKING tests**
	- **Evaluate what really should be `pub`, `pub(crate)`, `pub(super)`, or private**
	- **Does `RefMap` need ordering? (Does this need to be `BTreeMap/Set`?)**
		- I feel like no...
	- **Is there duplication of info between `OpInfo::Ref` and `RefMap`?**
		- On the one hand, we kinda need `OpInfo::Ref` to know *which* operand is doing the reference; `RefMap` only operates on an `EA -> [EA]` basis so all references in the instruction are flattened into a single entry
		- On the other hand, we kinda need `RefMap` in order to support fast cross-reference lookups
			- Especially inrefs - might have to change all inrefs if their target changes, and discovering that by just looking at `OpInfo` would be way too slow
		- So maybe this is fine?
	- **Disassemblers and Printers can take ctor arguments**
		- have to be able to account for that in IArchitecture.
	- **GB arch handling of operands is... messy**
		- you've got `GBOpKind` that says how to *decode* any explicit operands; indirectly based on that, you've got the actual operands in the instruction; and then you've got `SynOp` which says how to *display* the instruction, which mixes implied and explicit operands
		- it's kind of a lot
		- it seems to work for now so not the highest priority but...
		- if we add **custom operands,** it might be worth redoing this
	- **Custom IR instructions?**
		- How are multi-step instruction (e.g. 68K `movem`, Z80 `ldir`) represented/handled in the IR? Since they can't cause "real" control flow, maybe they can be represented by just recording their "end-state" effects, like "now BC = 0" etc.
		- because of that, might be useful to have a custom IR instruction type for things like this. I think Ghidra Pcode does.
		- already running into a case where I wish I had this (GB `daa`, god the logic is gross to express in vanilla IR)
		- but **I think I'd wanna put this off until after const prop builds ASTs** - not sure what the implications are for custom IR instructions if that's the case

- **Analysis**
	- **Method to only analyze one queue item**
		- this way the queue can be analyzed in an executor-based environment instead of being forced onto a second thread
		- and/or some listener for analysis steps
	- **Make const prop build ASTs for constant provenance**
		- wait uhhhhhhhhh
		- that could be used for *way more* than just constant provenance right?
		- you could have it show little HLL-like snippets of what a sequence of instructions does
		- like a very limited decompiler
	- **IR Dead Store Elimination**
		- ties into argument/return value/clobber analysis
	- **Marking functions as "bankswitch functions"**
		- if it e.g. takes the bank to switch as an argument
		- ofc user can help with this, but we should be able to identify candidates
	- **Dead-end/invalid control flow back-propagation**
		- if we hit a dead end, that's a sign that the control flow that got us here is mis-analyzed - maybe an "always-taken conditional branch" or sth
		- also common with *jump table functions* - right after the call is *not code*
	- **Jumptable analysis**
		- should support multiple strategies (depending on the arch), e.g.
			- absolute
			- PC-relative
			- jumptable-base-relative
		- should support *jumptable functions* - call a function to perform the switch
	- **BBs and functions for which the MMU state can be multiple possibilities**
		- BB MMU state could really be more than just one thing...
			- `Single(state)` if the MMU is in just one state
			- `Multi(Vec<MmuState>)` if the MMU could be one of many states
			- `Dynamic` if the MMU state cannot be *statically* determined
				- and maybe it can hold a `Vec<MmuState>` for user-added MMU state possibilities
		- this means each *function* can also have multiple MMU states on entry
			- e.g. common functions called from multiple banks
		- and the MMU state on *exit* from a function can be different than on entry
			- e.g. "bank change" functions
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
		- more mappers (remember to set segment base VA when state changes)
	- **GB**
		- MMU doesn't yet handle external RAM
		- more MBCs (remember to set segment base VA when state changes)
		- syntax options - `[hl]` vs. `(hl)`; `add a, b` vs. `add b`
	- **Mos65xx**
		- there are more (unofficial) variations of `NOP`
		- correct `DOP` addressing modes
		- implement `TOP`
		- implement unofficial opcodes in some way?
			- maybe not actually implement them in IR but at least disassemble them
		- **CPU variants/revisions?**
			- Early (no ROR)
			- NMOS
			- CMOS (slightly different behavior)
		- decimal mode (ew)
			- that would require dataflow analysis in and of itself... UGH...
			- model it as part of the MMU state?? lmao

- **Features**
	- **Function-local labels**
		- if none of a code label's inrefs are outside its owning function, it's function-local and can be displayed differently
		- how to keep it globally-unique for the `NameMap` tho?
			- well, could leave that to the frontend to deal with it
			- e.g. frontend could generate a globally-unique prefix/suffix for each local name, and simply not *display* that part to the user
	- **"Points of interest" to let user know things to investigate**
		- state changes that can't be automatically determined
		- jumptables
		- functions that access particular hardware registers
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
	- **Generating "name + delta" output is a little more subtle than my first attempt**
		- really these should be rare
	- **Modifying functions**
		- removing BBs (mis-analyzed code e.g. after a switch jump or a no-return call)
		- adding BBs...?
	- **Control flow depth indentation**
		- those dumb arrows in IDA/Ghidra/r2/everything are so useless
		- INDENTATION is what shows control flow
		- ofc New and Creative Forms of Control Flow abound in hand-written asm so it might not be automatable...
		- but maybe this can fall under the same sorta thing as line comments

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
