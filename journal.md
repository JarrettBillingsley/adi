
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

**And that is DONE.**

---

## Applying results of IR analysis back to `Instruction`

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

That comment on line 3 is optional, but we **do still need to remember that info internally** even if we don't display it. IDA seeeeeeems to keep track of this with its xrefs. Each data xref can be read/write/offset.

Wait so, what if we kept track of *both* operands *and* addresses? like, the arch fills in the operands, and then based on IR analysis, we can extract the addresses from that. That also opens up the option for the user to manually add dependencies/references in places where the automatic analysis can't figure it out.

What the fuck do we name this type? It's "an EA that an instruction refers to." `InstAddr`? `InstRefEa`? `InstEaRef`? `Whatever`? IDA calls this `refinfo_t` so let's say `RefInfo`.

- as the above example shows, sometimes an address is associated with a non-`Mem` operand.
- and sometimes, an address is associated with *no* operand, like if the user adds a manual ref, or possibly for some kind of implicit memory operand...? no examples now, but.
- but that makes for an awkward situation for `Mem` and `Indir` operands, since we're duplicating info...
	- well, the address is not - the operand holds the *VA* but the `RefInfo` holds the *EA*
	- but the access type is duplicated
	- and it does mean we kind of have to check both operands *and* `RefInfo`s to discover all the addresses an instruction accesses
	- also for `Indir`, it's possible we don't have a `RefInfo` that corresponds to it, since we may not have any idea statically what the address is
	- well wait. maybe the `access` on `Operand::Mem` is... useless then. the disassembler fills in the operand to say "here one is!" and then it's up to the deeper IR analysis to determine what it *actually* accesses, and *how* it accesses it.
		- so the operand *just* tells you the VA, and it's up to the `RefInfo` to tell you what/how it accesses it.
- so basically it's a nothing-to-nothing correspondence lol
	- you can have `Indir` operands without corresponding `RefInfo`s
	- you can have `RefInfo`s without a corresponding operand
	- you can have both
	- you can... technically have multiple `RefInfo`s for one operand? is that? correct?
	- like consider `ld a, [SomeAddr + x]` - *is SomeAddr being both used as an `Offset` and read from?* would that be two `RefInfo`s with different access types?
		- or `inc [SomeAddr + x]` - offset, read, and write? good god lol
		- ...maybe MemAccess should be bitflags then?
		- **now MemAccess is bitflags-esque.**

**SO..................**

It seems that I'm being a silliam william and maybe putting this info in the wrong place?

- In IDA, all the "memory accesses" are instead tracked with the **xrefs.**
- There are two kinds: code refs (crefs) and data refs (drefs).
- crefs cover what I represent with `Target`, though they distinguish between jumps and calls.
	- is that useful? probably? possibly? prossibly?
- drefs cover what I represent with `R/W/Offset`.
- Each ref is annotated with the kind of reference it is.

What is not clear, is *how operands are matched up with references.*

- When you create a reference in IDA, it's just from source EA to dest EA.
- There's `out_name_expr` which takes the operand (which knows its index) and the EA
	- And the EA seems to come from `map_ea` which seems like a super important function that has absolutely no documentation, of course.
- If that fails, examples fall back to calling `out_value` with `OOF_ADDR`...
	- But the documentation is such garbage that it doesn't describe what it does, how it decides to format the address, how it determines whether that operand is matched up with the reference etc.

If we put the reference info in the refmap, then we only need `RefInfo`s for the operands which correspond to a reference. But how do you do the mapping? If you have 7 outrefs, which one does the operand correspond to? I mean I guess you could just do it by target EA, and do a linear search of the outrefs, since n is *very* unlikely to be more than 1 or 2.

Yeah, I think that works.

**Still....** how do you map from the IR back to the operands? Will each `IrInst` need sort of... "debug info" to do that mapping, provided by the arch's IR compiler?

I'm overthinking this: **the IR compiler can associate an optional "real instruction operand" index with each address operand** so that the back-mapping is trivial. duh. Then that opens the question: **where** is that information attached?

If we put it in the `IrInstKind` members, we could either:

- add more field(s) to encode the operand indices
	- like for:
		`Assign  { dst: IrReg, src: IrSrc },`
		it could become
		`Assign  { dst: (IrReg, Option<u8>), src: (IrSrc, Option<u8>) },`
	which keeps the mappings right next to the fields, but makes it really awkward to use them...

	- or it could become
		`Assign  { dst: IrReg, src: IrSrc, dstn: Option<u8>, srcn: Option<u8> },`
	which makes it easier to use (just `..` the operand nums in patterns) and the mappings are easy to get (`field` vs `fieldn`), but disconnects the mappings from the fields
		- ***Done!***
- or add the mappings to the `IrInst`
	- add an `opn: [Option<u8>; 4]` field
		- least intrusive, but it's a very awkward mapping from the `IrInstKind` fields to items of this array
		- unless you
	- or add `srcn`, `src1n`, `src2n`, `src3n`, `addrn`, `targetn`, `condn`
		- makes the mapping more obvious but blows up the size of `IrInst` unnecessarily because each instruction can have at most 4 mappings

---

## Const propagation back-mapping info

With this example again:

```
	ld h, $C4     ; 1
	ld l, $08     ; 2
	ld [hl], 3    ; 3
```

which we'd like to display as something like:

```
	ld h, hi(loc_C408)
	ld l, lo(loc_C408)
	ld [hl], 3 ; hl = loc_C408
```

Assuming we associate the operand number with the accessed addr, I could see the final `ld` being easy to add a `RefInfo` to. But what about the first two?

In order to know that they really are parts of a full address, we'd have to know that the **later use of `C408` is a reference, and know that it was produced from those two earlier constants.**

I'm thinking during const prop, rather than *just* doing the computation, we also keep a tree of how that constant was actually created. That way, we know that "constant 3 was computed by concatenating constants 1 and 2" and can go back to the instructions which used constants 1 and 2 and mark them as references too. ***Done!***

**Secondary issue:** I'm imagining the const prop results will be discarded after we back-apply this info to the original instructions. But that would mean that in the above, the first two `ld`s would no longer be "connected" either to each other or to the third `ld`. So if the user then changed one of them to be a constant rather than a memory reference, the other ones would not be updated...

Is that a problem? Either they're doing it because they made a mistake (in which case an undo feature would save them) or they're doing it because the automatic analysis was wrong (in which case they'll probably change all three).

I guess they could also manually retrigger a const prop after doing something like this. But what would the const prop algorithm do? Would it take user annotations into account? Give an "I tried unifying these constants into one but you annotated this as "not a reference" so idk what to do here" message?

---

## When do we resolve references?

First, the instructions are decoded. Some instructions are known to access/reference memory statically. This is encoded in `Operand`; it has a `has_addr` method.

But that's not the whole story, because **some instructions which don't "have an address" refer to one anyway.** That's what constant propagation can discover - not only which addresses indirect references refer to, but also that earlier instructions which were just e.g. loading an immediate were actually **putting together pieces of an address.**

However, **this is still a VA,** and mapping from VA to EA can't be done until state change analysis has been done.

> Although, some EAs *can* be determined early. Right now, the first pass of function analysis is trying to find EAs for control flow targets. But is it even necessary? Intra-function control flow is almost certainly (but not necessarily) going to stay within the same segment...
>
> WELLLLLL when we are first analyzing a function we *do* actually have its EA. We have to. The only way we were able to discover it is by knowing its EA! So maybe it's not so premature after all.
>
> So I think that's harmless. About the only thing that bugs me is the magical invalid EA value... though for ergonomics' sake, I think this is better than an Option<VA>. Hm.

So `OpInfo::Ref` can't be created on instructions until *after* state change analysis...

But state change analysis can't be done until doing const prop and determining potential state change locations.....

AAAAAAA so it's like:

Initial instruction (finds statically-known VAs)
-> const prop (finds more VAs)
	- **we need to pass this info to the state change pass WITHOUT using `OpInfo::Ref`??**
	- add another `OpInfo` variant? `VARef`? sort of an "unresolved" reference? ***yes, Done***
-> state change pass (STILL VAs)
	- to distinguish between `StateChange::Dynamic` and `StateChange::Static(new_state)`, `inst_state_change` needs the const prop info!!! because if you're storing some register into an address, you have to know 1. if that address is constant and 2. if the reg value is constant.
	- wait, this seems like a duplication of effort. why ask the arch to figure out if its instruction writes to memory? *we already KNOW if it does from the IR.*
	- rather than looking at which *macro* instructions access memory, **we should really be looking at which *IR* instructions access memory,** and then ask the arch "could this trigger an MMU state change?"
		- and we can give it an actual target VA and optional value being stored
		- YEAHHHHHHHH.
		- also apparently it's common for MMUs to respond to *reads* not just writes, so we gotta check both
-> references pass (can finally map from VAs to EAs and convert `OpInfo::VARef`s to `OpInfo::Ref`s, as well as adding references from the statically-known VAs determined in the initial disassembly)

---

## Ah shit, const prop srcs prob does need to be some kind of AST...

Right now the srcs are just listed saying "this const was made from these other const(s)" without saying *how* but that information is needed to determine e.g. on earlier instructions, was that the "high half" or "low half" of an address, or if it was "base + offset."

The only way to know that is by representing the provenance of each constant as an AST.

Right now the way the const prop algo works is by computing the value (`val`) and remembering where it came from (`srcs`). But it could be instead changed to **having a parallel AST where each `Info::Some` just holds an AST node index;** the node would then contain the computed `val` and its children represent how that value was calculated.

---

## Is it a problem to have EAs as `BBTerm` fields?

Well, I was originally thinking:

1. "it's impossible to map from VA to EA until after doing state change analysis, so doing VA -> EA mapping during the new function pass seems premature"
2. "it's annoying having a special invalid value for EAs"

*But...*

1. The new func pass doesn't really try to do VA -> EA mapping *except within the current segment.* Since we're assuming most/all code doesn't swap out its own segment, that's safe.
2. The more I think about it, the more I realize "invalid" **isn't a good name for that kind of EA.** The way it's stringified is more accurate: it's **unresolved.** So my bias against this "weird invalid EA" is probably more rooted in the name than anything.
	- Unresolved EAs are also *totally* possible to have in the final, fully-analyzed code!
	- Not everything can be statically determined! An unresolved EA is *something for the user to look into, a point of interest, not a mistake.*

So.