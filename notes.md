
`x_from_y` = "infallibly get an x from a y. panic if it doesn't exist."
`x_for_y`  = "try to get an x corresponding to y, if one exists."

# TODO:

- write some FUCKING tests
- memory tests: test config, map, region, segment...
- use more traits to overload methods like `x_from_y` and `x_for_y`
- implied operands in `InstructionTrait`
- bankable region mem configs
- `Memory`: removing/redefining/iterating segments
- `Program`: `VarIndex`(?)
- `Platform`
- `MemoryMap`: iterators for bankable regions, ROM regions, etc?
- `SpanKind`: like, an array/variable owner type
- `RefMap`: does this need to be `BTreeMap/Set`? (do we need ordering?)
- Disassemblers and Printers can take ctor arguments
	- have to be able to account for that in IArchitecture.
- Names should be more than just Strings...
	- Name::Hardware (for MMIO regs, vector locations etc)
	- Name::AutoGen (not actually in the name table, just used for display)
	- Name::User (user-given)
- ditch derive_new, it's not helping a lot

**Idea:** have "special" segment for unresolved locations (know the VA but not the segment). That way we can point to them, but not actually resolve them until later. it'll also make it easier to resolve them since there will be inrefs to them.

## **ASSUMPTION:** instructions will never cross segments.

- `https://wiki.nesdev.com/w/index.php/Tricky-to-emulate_games` lists only a *single* game that breaks this rule (The Magic of Scheherazade)
- for practical reasons, I really doubt programmers would do this very much

## **ASSUMPTION:** a single function's code will be entirely contained within one segment.

- if it isn't, we can split it into two functions and have the first tailcall the other.
- this doesn't account for functions that have a BB in the other segment and like, bounce back and forth but cmon really?

## Memory Banking

- in theory, figuring out memory banking is not *too* difficult.
	- the system starts in a particular configuration.
	- certain "magical operations" (MMIO register writes) will change the bank configuration.
	- control flow into the banked areas will not be resolvable unless the configuration is known.
	- that configuration must be propagated across BB boundaries, function calls, etc.

### Tricky bits

- **knowing what the initial config is**
	- that's probably straightforward based on the platform/mapper chip.
- **knowing what the bank swap mechanism is**
	- again, defined by the platform/mapper.
- **being able to ignore conflicting configs for a single function**
	- e.g. common functions in unbanked ROM being called from many other banks.
	- those common functions might ignore most/all configurations, so it's OK for them.
	- each bankable area could have a set of possible banks; and only if we jump into that
	- area would it require human assistance.
- **sometimes the bank to swap to is *dynamically* determined**
	- e.g. object jump table, with parallel bank table
	- so it loads the bank from the table, swaps, and then jumps to the jump table.
	- so those jump table entries are associated with a particular bank too...
	- but that would be pretty tricky to automatically determine.
- **crossing bank boundaries.**
	- I haven't *seen it* but I can imagine a case where e.g. a game maps A000-DFFF to a pair of banks which function as one.
	- In that case you might have a BB, table whatever which crosses the boundary at C000.
	- that seems like a niche issue... detectable, but not likely to happen.
- **banking RAM.**
	- RAM can be banked on many systems.
- **mapping one bank to multiple virtual addresses?**
	- like, "bank 4 sometimes appears at A000 and other times at C000"
	- nothing in the hardware *prevents* this but not likely to happen, since these CPUs don't really support position-independent code.
	- so is it OK to tie a segment to a particular VA? probably?

---

An `Architecture` describes a CPU architecture, including:

- its endianness
- its address size
- its Disassembler, Printer, and Instruction types

A `Platform` describes a system, including:

- its CPU architecture
- its memory map
- devices?
- memory mappers, ROM parsers...

A `Platform` can take an image and give a `Program`. It can also give access to the architecture's disassembler and printer types.

hmmmmmmm

so the mappers *come from* the platform, but they *act upon* the memory.

...

what is a mapper, but a miserable--- er simple memory management unit?
and some platforms have REAL MMUs.
and some platforms have built-in "mappers".

so I don't think the concept of a "Mapper" should even be limited to a Platform.
instead, there should be `IMmu` which plugs into a `Memory`.
*this* is what would handle the memory configuration and do "real" VA->PA translation.
this also interacts with analysis; an `IMmu` can have a state (i.e. a `MemoryConfig`) which can change over time. analysis can track the state across instructions.

actually `MemoryMap` is... kinda the right place for this behavior, no?
some `MemoryRegion`s are static, and some can be swapped out.
the `IMmu` is responsible for handling this swapping/mapping through `MemoryConfig`s.
but a string name-name mapping is probably not the uh, best/most efficient way to do that.
especially if we want to create those things all over the place in analysis.

1. the file type is determined. this decides the `Platform` that will be used.
2. the `Platform` parses the file, and determines:
	- what kind of `IMmu` should be used
		- e.g. the base NES MMU but with a certain mapper installed
	- what `Segment`s exist
		- e.g. the base hardware segments for RAM, MMIO etc. and segments for the ROM
3. the `Platform` can then create the `Memory` object from that info
4. ...and the `Program` object
	- which can have some default names defined by the `Platform`


1. determine `Platform`
2. `Platform::program_from_image(Image) -> Program`


---

if we collapse Memory and MemoryMap...
and give MemoryRegions an Option<SegId> to which they correspond...
and MemoryRegions with None will instead by looked up by the plugin MMU...
I think that might work.

how are the initial segments created (for the built-in hardware regions)? who does that?
the segment owner... but then how does the MMU know about them?
