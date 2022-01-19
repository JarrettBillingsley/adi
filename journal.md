
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

- **Immediates** are an additional complication...
	- if we think about IDA, the markup rules decide how an immediate is displayed
	- it might be displayed as a decimal integer, signed or unsigned; hex; binary; some enumeration value (or bitwise OR of multiple values)...
	- or it might be a memory address!
	- `IPrinter` deals with this by taking an `INameLookup` as an argument to `fmt_operands`
		- but that will quickly get out of hand once we have other kinds of markup like above
	- *does markup go on the instructions, then?*
		- and who's responsible for actually displaying it?
		- maybe the `IPrinter` interface is the wrong abstraction
		- maybe the program asks the arch for an instruction's format template string, and does the markup/formatting itself?
		- ew strings tho
		- and that also doesn't account for assembly syntaxes where the kind of operand dictates the syntax (like 6502 `#$05` for immediates vs `$05` for addresses) - meaning the arch is doing some of the formatting work itself
		- aaaaaaaaaaaaaaaa

- What does IDA (seem to) do?
	- seems like...... pretty much exactly what I'm doing now lol
	- instruction has array of operands
		- operands know if they're regs, immediates, etc.
		- just line mine, seems like not all arches fill in all operands ;P
		- it also has - you guessed it - some custom fields that arches can use for whatever
	- printer ("out") method is passed a useful context object
		- you can tell it to output strings, symbols, values (which it interprets in the right base/as an offset etc.)
		- you can even tell it to change colors
		- so like the `INameLookup` but way more
		- that moves a lot of the boilerplate out of the arch code
	- operand markup seems to be handled entirely by the operand types
		- I see `num(10)`, `num(16)`, `char`, `OFFSET` etc.

Well now that the interpreter is gone, really the only issue is the awkwardness around *printing* instructions - and that seems to mostly be because of the poorly defined interface there. `INameLookup` is only one tiny piece of what needs to be done.

SSsssssssoooooooooooooooo

I suppose it doesn't really matter if all operands are filled in or not - the analysis will skip the ones it doesn't care about and the arch can choose to implement printing any way it likes. If it's easier to do that by filling in all operands, it can do it. If not, it won't.

...how do we handle the results of const prop, though? Say we have code like:

```
	ld h, $C4     ; 1
	ld l, $08     ; 2
	ld [hl], 3    ; 3
```

Const prop will determine that `hl = 0xC408` on line 3. But we can't just rewrite the instruction's operand to say it's an immediate `0xC408` now. Instead, **we should display something like "`hl` is constant `0xC408` here".** Furthermore, the information that `h` and `l` are parts of an address needs to be back-propagated to lines 1 and 2 so they can be displayed as something like...

```
	ld h, hi(loc_C408)
	ld l, lo(loc_C408)
	ld [hl], 3 ; hl = loc_C408
```

(or whatever syntax is appropriate for the architecture's assembler)

That comment on line 3 is optional, but we **do still need to remember that info internally** even if we don't display it.

...

Wait so, what if we kept track of *both* operands *and* addresses? like, the arch fills in the operands, and then based on IR analysis, we can extract the addresses from that. That also opens up the option for the user to manually add dependencies/references in places where the automatic analysis can't figure it out.

---

## Printing framework

Right now, the printing takes references to two objects to do its work:

- a `std::fmt::Write` trait object
- an `IPrintStyler` trait object

the idea being, the actual text goes to the `Write` object and the styling/metadata info goes to the `IPrintStyler`. That way, you can mix and match and e.g. produce a string without styling; a string of HTML; have it go directly to the console (with or without ANSI color sequences); have it go directly to a file; create GUI widgets on the boundaries of styling etc.

I like the flexibility, but there are some practical issues with this:

- it's a bit awkward to have to make two objects to print things out, instead of one
- the text and the styling may need to go to the *same place* but...
	- you have to have two *separate* objects to do the work of those two things (you can't have a single object that implements both `Write` and `IPrintStyler` because you can't have two mutable borrows of the same object simultaneously)
- turns out `std::fmt::Write` isn't really implemented by a lot of things
	- not even standard output or files!
	- so if you want the stuff to go directly to the output you have to use a wrapper object
	- so much for making it flexible :\

What if the `Write` (or something like it) was composited *into* the `IPrintStyler` (which would now be renamed `IPrintOutput` or something)? Would that solve things?

Yes, it would, and it did ;o

Next is to rip out the old printing stuff (and update the arch tests!!).

And that is DONE.

