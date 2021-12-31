
## On switching from interpretation to IR

What do I want this IR to be for?

Dynamic analysis, through interpretation?

Static analysis, through dataflow analyses (and type propagation)?

Both?

I feel like dynamic analysis can help, but only gets you so far. That's the problem with the current interpreter. Like, what if only one control path through a function gets run? How do you force the other paths to run, when there can be an unbounded number of dependencies on globals and things that are hard/impossible to model like IO/hardware?

Doing fruitful dynamic analysis would really require something more like an instrumented emulator. See what distributions of functions are called depending on what's on-screen. *That* would be *really* cool, but maybe out of scope for now.

It feels like static analysis is more likely to yield fruitful results sooner, and aligns more with my goals of having a tool that can automate a lot of the RE drudgery (like finding all xrefs to a variable and then propagating the knowledge/type forward/backward).

---

## Now that the IR is a Thing...

The point of the IR is to replace the interpreter, right?

So...

1. Get rid of the interpreter stuff altogether.
2. Get rid of the "instruction state change" stuff in the first pass.
3. The second phase of function analysis will construct IR and analyze that:
	- SSA construction, then constant propagation
	- Based on that, we can do proper state change analysis
	- Also, const prop can reconstruct addresses used in many indirect accesses, for use in refs
		- So even if the IR doesn't stick around past the end of this phase, those can
		- Idea: rewrite `Instruction` operands during this phase

---

## What is the `Instruction` type useful for, anyway?

Originally each architecture had an associated type for instructions, so there was an `IInstruction` trait that instructions had to implement and beyond that it was anything goes. But that led to a really awkward choice: either you parameterize the entire `Program` type for each platform (which seems really silly, since most of it doesn't care about the platform specifics, and would lead to huge compile times for all the monomorphization); or everything is a `&dyn` reference, and there were. issues with that? I think? I remember getting rid of them all in favor of `enum_dispatch`.

Well the `Instruction` type stuck around as a sort of "architecture-neutral" representation of instructions. It's used heavily by the analysis module and serves that purpose fairly well, but there are some issues:

- The operands are very abstract.
	- There's a bunch of weird dynamic checks in architectures' printer/interpreter code to convert from the abstract `Operand`s back to their native ones, which is frustrating because those checks (1) should never fail, if everything is written correctly, and (2) will only fail **at runtime** if there are bugs. I hate that!
- There's also nothing that requires that the operands be filled out properly.
	- Right now, the operands serve two purposes: one is for the analysis to look at **addresses;** the other is for the arch code to print/interpret/etc. and so nothing outside the arch really cares about e.g. register operands.
		- So, for example the SM83 arch *only fills out address operands* and leaves all others implicit, because the analysis doesn't care.
	- Once the IR is in place, there *really* won't be any use for non-address operands.
		- I guess the original idea was to use `Instruction` for interpretation but it's too abstract to be useful for that.
		- I guess also the idea was that you could print out an `Instruction` by iterating over the operands, but there's no way to know how they should be formatted, which is why the arch does it.

- It *is* a useful place to attach the VA/EA, kind, control target, and any memory accesses.
- But as far as printing/interpreting/conversion to IR goes, it feels like an impediment.
	- It almost seems easier (or at least the same difficulty) to re-disassemble the raw bytes, at which point it's a time/space tradeoff:
		- is it really worth coming up with a per-arch specialized operand type and figuring out how to store that in each `Instruction` upon first disassembling?
		- Or is it fine to just re-disassemble for printing and such, especially since we know it will succeed and might be able to fast-path it?

**SO:** maybe a plan would be:

- each `Instruction` has *addresses* instead of operands
	- since that's all the analysis cares about anyway
	- these addresses can be added to during analysis (e.g. after const prop)
	- the addresses are ordered in the same order they are accessed by the instruction
		- this way e.g. double-indirect addressing modes can list *both* addresses
- each `Instruction` also has a single "custom" field, maybe just a `u32` or something
	- which is usable per-arch as a way to keep track of what instruction it is
	- e.g. stores index of instruction template, so that subsequent printing etc. can fast-path
