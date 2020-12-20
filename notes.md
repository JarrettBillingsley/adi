
1   (mmc1)  - dragon warrior
4   (mmc3)  - mario 3
2   (uxrom) - mega man 1
0   (nrom)  - mario 1, duck hunt, 10-yard fight
3   (cnrom) - arkanoid
7   (axrom) - battletoads
206 (mimic) - gauntlet
11  (color dreams) - exodus (lmao)
5   (mmc5)  - castlevania 3

`x_from_y` = "infallibly get an x from a y. panic if it doesn't exist."
`x_for_y`  = "try to get an x corresponding to y, if one exists."

# TODO:

- write some FUCKING tests
- implied operands in `InstructionTrait`
- bankable region mem configs
- `Program`: `VarIndex`(?)
- `SpanKind`: like, an array/variable owner type
- `RefMap`: does this need to be `BTreeMap/Set`? (do we need ordering?)
- Disassemblers and Printers can take ctor arguments
	- have to be able to account for that in IArchitecture.
- Names should be more than just Strings...
	- Name::Hardware (for MMIO regs, vector locations etc)
	- Name::AutoGen (not actually in the name table, just used for display)
	- Name::User (user-given)
- ditch derive_new, it's not helping a lot
- remove extra 65xx meta-ops (lsla etc) since they're done differently now

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

- analysis
	- ProtoBB
	- PBBIdx
	- ProtoFunc
	- AnalysisItem
	- Analyzer
- arch
	- IArchitecture
- disasm
	- MemAccess
	- IOperand
	- InstructionKind
	- IInstruction
	- IDisassembler
	- DisasAll
	- IPrinter
	- INameLookup
	- NullLookup
- memory
	- Endian
	- SegCollection
	- IMemory
	- Memory

	- image
		- ImageRead
		- ImageSlice
		- ImageSliceable
		- Image
	- mmu
		- IMmu
		- MmuState
	- segment
		- SegId
		- Location
		- Segment
	- spans
		- Span
		- SpanKind
		- SpanMap/SpanInternal
	- va
		- VA
- platform
	- IPlatform
	- `program_from_image`
	- nes
		- NesPlatform
- program
	- Program

	- func
		- FuncId
		- Function
		- BBId
		- BasicBlock
		- IntoBasicBlock
		- BBTerm
		- FuncIndex
	- namemap
		- NameMap
	- refmap
		- RefMap
		- RefSet