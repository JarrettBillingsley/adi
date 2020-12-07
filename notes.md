
`x_from_y` = "infallibly get an x from a y. panic if it doesn't exist."
`x_for_y`  = "try to get an x corresponding to y, if one exists."

# TODO:

- write some FUCKING tests
- disas is working with a raw &[u8] but should be an ImageSlice probably
- use more traits to overload methods like `x_from_y` and `x_for_y`
- move analysis::func to program
- implied operands in InstructionTrait
- bankable region mem configs
- Memory: removing/redefining/iterating segments
- Program: FuncIndex, VarIndex(?)
- Platform
- MemoryMap: iterators for bankable regions, ROM regions, etc?
- SpanKind: like, an array/variable owner type
- memory tests: test config, map, region, segment...
- RefMap: does this need to be BTreeMap/Set? (do we need ordering?)

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

## Basic Block Blathering

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
	- remove the function from the FuncIndex

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
- this doesn't account for functions that have a BB in the other segment and like, bounce back and forth but cmon really?
