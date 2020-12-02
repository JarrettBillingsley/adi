
`x_from_y` = "infallibly get an x from a y. panic if it doesn't exist."
`x_for_y`  = "try to get an x corresponding to y, if one exists."

## Base types

- `VAddr`
	- a virtual address in the CPU's normal address space.
- `PAddr`
	- a physical address; really an offset into a `RomImage`'s data.
- `SegOffset`
	- a positive offset from the beginning of a `Segment`.
- `SegId`
	- a unique identifier given to each `Segment`.
- `Location`
	- a pair of (`SegId`, `SegOffset`); globally unique "address" to anything.
- `ImageRange`
	- like `Range<PAddr>`, but `Copy`
- `RomImage`
	- a read-only executable file, ROM, etc.
- `Endian`
	- endianness (big, little, or n/a)

## Memory types

- `MemoryRegion`
	- part of a CPU's memory map - which address ranges mean what.
- `MemoryMap`
	- an immutable collection of `MemoryRegion`s to describe a CPU's VA map.
- `Span`
	- a range of memory addresses in a `Segment`. can be code, data, unknown. can have owner.
- `SpanMap`
	- covers an address space in `Span`s. think of the colored image map in IDA. types:
- `Segment`
	- a unique address space at a specific VAddr, containing a `SpanMap`. types:
		- **Image segment:** a slice of an image (rom, executable, whatever).
		- **Fake segment:** everything else, so we can name and refer to things.
- `MemoryConfig`
	- a mapping from `MemoryRegion` names to `Segment` names.
- `Memory`
	- consists of `MemoryMap`, `MemoryConfig`, and `Segment`s.

## Program types

- `NameMap`
	- bidirectional mapping between names and `Location`s.
- `RefMap`
	- many-to-many mapping of directed edges between pairs of `Location`s.
- `Program`
	- consists of a `RomImage`, `Memory`, `NameMap`, and `RefMap`.

## Disasm types

- `Architecture`: a CPU ISA. consists of:
	- `Disassembler`, to turn raw binary data into instructions
	- `Printer`, to turn binary data into a textual representation
- `Platform`: the system a CPU exists within. consists of:
	- `Architecture`
	- `MemoryMap`
	- maybe `RomImage`s, for things like system ROMs?

## Analysis types

- `Function`: a set of `BasicBlock`s which comprise a single function. has a "head" BB.
- `BasicBlock`: a sequence of instructions; part of a `Function`, exists within a `Span`.

## Ideas

- represent as much implicitly as possible.
- duplicating state is evil.
- don't store a BB's start and end; just its start, and let the associated span define the end.
- the span map should be the *primary* data structure.
- querying it by address is fast.
- querying it by type is not... "Find all code spans which are heads of functions" is slow.
- which is why I wanted a map of functions elsewhere.
- but then that map and the span map can get out of sync.
- so maybe the span map needs some kind of facility for indexing it.
- it's still "dumb," just points to "owners" for each span...
- but setting the owner for a span can also update these indexes.
- e.g. if the owner is a function head BB, can update the function index to point to this.

I'm kind of working under the assumption that the image segments are easily determined and more or less fixed after loading. That isn't necessarily the case IRL... but for now, I'm gonna assume  we're not gonna be dumping "raw" images into this thing, and that we already know something about  what the image looks like.

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