
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

## Blathering

- BBs have successors they need to refer to.
	- but that feels like duplicating the info that's in the ref map.
	- is that okay? I mean, I guess the ref map is just there for "xrefs" lookups
	- the ref map also doesn't have any implied ordering of references, which the BBs need
- I think it's okay to duplicate references from the ref map...
	- cause it's really there to act as a fast index for all references.
	- it's more of a cache. if it gets out of sync, that's bad, but it's not likely to break stuff.
- but the span map info should *not* be duplicated.
	- e.g. BBs only know their starting location; their end is dictated by the span map.

- inrefs to a BB can be from the same function or from other functions
	- the head has inrefs from other functions
	- the rest of the BBs have inrefs from the same function
- outrefs from a BB terminator can be to the same function or to other functions
	- same function for control flow
	- other functions for tailcalls

- so what... what would need to happen if you deleted a function? undefined it as code?
	- for all of its basic blocks,
		- remove all same-func inrefs to BBs
		- remove *all* outrefs from terms
		- remove all outrefs *period* (for each BB span, remove range of outrefs for that span)
		- redefine each span to be unknown
			- *should adjacent unknown spans be coalesced? I think yes*
	- remove the function from the FuncIndex

- wait. wouldn't the owners be the ones *initiating the changes in the first place?*
	- like. why would we be changing the span map without them knowing?
- for example, if the user selects some stuff in the interface and chooses "undefine"...
	- we'd use the span map to *find the owners*
	- but then *tell the owners directly* to change/delete themselves
	- then *they* would tell the span map "change/undefine me plz"
- we could go a step further and say that *undef <-> code/data is OK*, but **code <-> data is not**
	- if you wanna go between them, have to undefine them and then redefine anew
	- the interface might let you "go directly" but underneath this is what it does

## Span map rules

1. can only go between unk and non-unk.
	- adjacent unk spans are coalesced.
2. span map is not directly modified.
	- exists in service of code and data indexes.
3. spans can be deleted or shortened...
	- but can't have their *starts* changed.
	- have to delete existing span and make a new one for that.
4. spans cannot be bisected.
	- that leaves two non-contiguous spans with the same owner, which makes no sense
	- have to shorten existing span, then create new span

## Analysis algorithm

(reconstructing my thoughts from the Python code)

**The analyzer has two work queues,** one for functions and one for jump tables. `analyzeQueue` picks jobs off the front to do until they're both empty.

**Function analysis** starts at an entry point. It starts decoding instructions there, extending a BB until it hits control flow. That ends the BB and possibly starts a new one (or stops analysis).

It also logs jump/branch targets as it sees them, and...... Okay it's not really as elegant or straightforward as I was thinking

Maybe this can be done differently. Two phases?

First "roughing" phase feels out the boundaries of the function. It's not *guaranteed* that a function's code is contiguous, but it's pretty common. So, we can kinda follow control flow to see where it goes, until we get to a return/halt instruction. Jumps to unexplored/other functions and calls can be ignored at this point.

We can build up "proto-BBs" by having ranges of instructions which we know "belong" to this function. But we won't actually modify the span map yet. This will avoid all the "nudging the end of the BB up by 1 byte over and over" that the old algorithm did.

## **ASSUMPTION:** instructions will never cross segments.

- `https://wiki.nesdev.com/w/index.php/Tricky-to-emulate_games` lists only a *single* game that breaks this rule (The Magic of Scheherazade)
- for practical reasons, I really doubt programmers would do this very much

## **ASSUMPTION:** a single function's code will be entirely contained within one segment.

- if it isn't, we can split it into two functions and have the first tailcall the other.

