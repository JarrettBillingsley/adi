# Design

This is an overview of the major concepts/model this framework uses. (It's partly notes to myself, since I have a habit of going for months or years without working on something.)

---

## Memory Model

The basis of the analysis framework is the **memory model,** which models how the CPU accesses things like RAM, ROM, hardware devices, etc.

Here is a diagrammatic overview, and all the terms/pieces are defined after it:

```
                                        Segments
                    _______________________/\________________________
                   /                                                 \
+------------+     +--------+ +--------+ +------------+ +------------+
|   Memory   |---->|  RAM   |,|  MMIO  |,|    PRG0    |,|    PRG1    |,...
|            | has | (fake) | | (fake) | |   (real)   | |   (real)   |
+------------+     +--------+ +--------+ +------------+ +------------+
       ^                                  | is a         | is a 
       |                                  v slice of     v slice of
       |                                 +---------------------------+ ...
       |                           Image |  Code that  |             | ...
   EAs |                                 |  uses VAs   |             | ...
       |                                 +---------------------------+ ...
    +-------+                                 |
    |  MMU  |        VAs + MMU state          |
    |       |<--------------------------------+
    +-------+
```

### Images

All analysis is done on an **image,** which is a single file that contains all the code and data for a program. ROM dumps, disk images, and executable files are all kinds of images.

The part of the image we're most interested in is the **machine code,** which is disassembled into a higher-level data structure on which program analysis is performed.

### Memory and Segments

The **memory** is a model of the memory architecture of the target platform. It models anything the CPU can access: RAM, ROM, MMIO, etc.

The memory is split into multiple **segments,** regions of memory with specific behaviors and contents. Some segments are **real,** meaning they are mapped to a slice of the image. Others are **fake,** meaning they refer to something built into the platform but not present in the image.

In the above diagram, the `PRG0` and `PRG1` segments are real and map onto pieces of the image; the `RAM` and `MMIO` segments are fake and have no backing image. "Fake" segments aren't useless! We still need them to model things like variables and IO behavior. We just don't know what their contents are until runtime.

The issue with modeling memory on these old systems is that it was common for the size of the image to exceed the CPU's addressing capability. So, the solution was to use some kind of **memory management unit.**

### Virtual Addresses (VAs)

A **virtual address (VA)** is an address written in the code. For example, if the code attempts to load a variable from address `0x8004`, that is a virtual address. 

It's called "virtual" because **the *actual* piece of memory or device accessed at a virtual address can *change over time.*** This means that **VAs can be ambiguous!** The **same VA** accessed from different places or at different times can **refer to different things.**

Hence the address is "virtualized" or abstracted away from the real, underlying addresses. The thing that virtualizes these addresses is the...

### MMUs and MMU state

The **MMU (memory management unit)** models how the platform maps the addresses the CPU *uses* onto the hardware, RAM, and ROM that is actually *accessed.* These systems didn't usually have "a thing" called the MMU, but they usually had circuitry to **decode addresses,** so that e.g. one part of the address space accessed RAM and another part accessed ROM. 

Although the memory object has a bunch of segments for things like pieces of the ROM, RAM, and MMIO, **it's the MMU that defines the CPU's memory map.**

Beyond simple address decoding, many of these systems made heavy use of **memory banking:** treating parts of the address space as "windows" which could "view" different pieces of the image. To do this, **some special register(s)** would be used to select which pieces would be visible in the address space. The contents of that register/those registers is the **MMU state.**

And this is what makes analyzing these programs much more complicated than the "flat" binaries that are common today: **different parts of the program come into and out of view over time, and *knowing what a VA refers to requires knowing what state the MMU is in at that point in the program!***

So the MMU is basically a function which maps from a tuple `(VA, MmuState)` to **effective addresses (EAs).**

### Effective Addresses (EAs)

An **effective address (EA)** is a pair of values `(segment, offset)`. Every segment in the memory has a unique ID number, and the offset is how many bytes into the segment the EA is. 

While a VA can be ambiguous, **an EA uniquely identifies some location in a way that doesn't rely on the current MMU state.** A variable is "in RAM bank 1, offset 12" and it's *always* there, so its EA is `(RAM1, 12)`.

Despite that, EAs may be *displayed* as `SEG_NAME:0000` where `0000` is not the offset, but *the VA that was used to access it.* It's just easier for our Human Brains. (This is exactly how IDA does it too.)

---

## So what's new about this?

The whole point of this framework is **to accurately track MMU state and map VAs to EAs automatically.** 

IDA is really not up to this task. It *does* have Something Like This, but it's tightly tied to x86 as it tracks the values of "code segment" and "data segment" registers. This forces every non-x86 memory mapping system into an x86-shaped hole, and if it doesn't fit, you're screwed.

For example, many NES memory mappers let you control *four* ROM windows at a time and many Gameboy memory mappers control *three.* IDA just falls the fuck over here, forcing a human analyzer (like me) to go through and manually determine the MMU state and fix up references, over and over and over again.

I think Ghidra *can* do this better than IDA can but I'll be honest I just don't like Ghidra that much. I could never get into a "flow" with it the way I could with IDA. Also looking at how to modify/extend it just made my eyes glaze over.

---

## Things in Memory

The whole point of binary analysis is to figure out *what's inside* the image.

### Spans

Each segment is covered completely by a set of one or more **spans.** These spans *do not overlap* and there are *no gaps* between them.

There are (currently) three kinds of spans: **unknown, code,** and **data.** "Unknown" spans are un-analyzed. All segments start off with a single unknown span covering the whole segment:

```
+-------------------------------------------------------------+
| Unk                                                         |
+-------------------------------------------------------------+
```

Then, as analysis progresses, parts of the unknown span are "sliced off" and converted into code and data spans:

```
+-------------------------------------------------------------+
| Code | Unk   | Code | Code      | Data | Unk         | Code |
+-------------------------------------------------------------+
```

If you have an EA, you can efficiently query which span contains it.

### Basic Blocks and Functions

Each code span has a corresponding **basic block (BB).** This is a straight-line sequence of CPU instructions *without* control flow, ending in a "terminator" instruction which performs control flow.

A **function** is mostly just a collection of BBs. One BB is designated the "head" and is the entry point to the function. It and the rest of the BBs form the **control flow graph (CFG).** The CFG is not stored explicitly, but is created on-demand during certain function analyses.

> Note: it's also possible for functions to have multiple entry points because hand-written assembly code is lawless. In that case there is still a "head," and the other entry points are into the middle of the function somewhere.

### Instructions, Operands, and Terminators

Each BB's code span refers to a sequence of bytes in the original image, which are the encoded instructions. During analysis, each instruction is decoded into a platform-agnostic **instruction** object, which can have **operands;** these instructions are stored in the BB. So you can imagine there are the "real" instructions (a sequence of bytes), and overlaid on them are the "abstract" instructions (actual objects) that represent them.

The last instruction of a BB is special; it's a **terminator** which performs some kind of control flow. For example (and this is not an exhaustive list):

- Return instructions
- Unconditional jumps/branches
- Conditional jumps/branches
- Function calls
- MMU state changes

The last one is key for MMU state tracking: **MMU state changes can only occur on the boundaries of basic blocks,** meaning each BB only has to remember one state.

> Well. Haha. Maybe each BB has to remember one **set** of states that it could possibly use...

### Data Items

**Data items** are to data spans as BBs are to code spans. It hasn't been developed much so far, but there's a whole **type system** prototyped out for representing all kinds of things like:

- primitive types
- strings (including those represented in non-ASCII formats)
- arrays
- pointers (absolute and relative)
- structs (including those which end in VLAs)
- bitfields (EXTREMELY common in these old memory-constrained systems)
- enums

---

## Function Analysis

Function analysis is sort of the beating heart of the analysis framework; it's what largely discovers what's code and what's data, builds BBs and puts them together into functions, and splits up previously-analyzed BBs and functions if later analysis reveals that it was mistaken.

### Pass 1: discovery (the "new function" pass)

First, the framework is given the EA of a potential function to analyze. This might be an entry point into the program (e.g. a reset/interrupt vector or the entry point in an executable), or it might have come from a function call instruction in another function.

Regardless, the first pass just tries to figure out the boundaries of the function. It decodes instructions, building BBs as it goes, and follows intra-function control flow to ultimately get to the end (or ends) of the function.

Importantly, **at this point no MMU state tracking is done, so any control flow out of the current segment is assumed to be out of the function.** That control flow will, in later passes, become references to new functions which will have their own pass 1 run!

If this process completes successfully, the collection of BBs is turned into a real function. Then, if the function contains any "self-calls" (call instructions which call into any BB inside itself), it is scheduled for function splitting; otherwise it's scheduled for static analysis.

### Pass 1.5: Function splitting

It is very common for hand-written assembly to do things like:

```ruby
func1:
_block1:
	blah blah
	here is code
	fall into next block
_block2:
	wow continuing on with func1
	yep still definitely the same function
	return
	
# ---------------------------------------------

func2:
	call _block2 # oh shit
	return
```

In pass 1, func1 would be analyzed as containing 2 BBs, `_block1` and `_block2`. But later, `func2` is analyzed and it turns out `_block2` is *treated as a function,* meaning that `func1` is really two functions in a row! The first function `_block1` simply **falls through** into the second `_block2` function.

The function splitting pass takes situations like this and either splits that one function into two functions, or marks the function as **multi-entry** if its control flow graph is shaped in a way that splitting would be impossible. In either case, it then schedules the function for static analysis, even if it's been analyzed before, because changing the function's CFG can affect the outcome of that pass.

This can happen *before the static analysis pass runs,* but it may happen *any* time after the initial first pass.

### Pass 2: static function analysis

This tries to figure out exactly what the MMU state is for each BB in the function. This is a pretty complicated algorithm which progresses through 5 stages:

1. Raising to IR and conversion to SSA
2. Global constant copy propagation (GCCP)
3. Locating state changes using the results of GCCP
4. BB splitting and terminator rewriting
5. MMU state dataflow analysis on the updated function CFG

This is described in more detail elsewhere, but it suffices to say: after doing this pass, we have a **pretty good idea** of what the MMU state is in each BB, and therefore can **perform VA -> EA mapping** in the next pass.

### Pass 3: references

Now that the MMU state has been (hopefully) determined for the whole function, the "references" pass goes through every instruction in the function to look for references to things. This pass does a few things:

- It **turns previously-unresolved VAs into EAs** using the known MMU state.
- It **attaches some information to instruction operands** that says "operand X refers to EA Y", so that the instructions can be printed out in a human-readable form.
- It adds entries to the **references map** (or "refmap"), a global bidirectional mapping that records what things refer to what other things. 
	- This is how "cross-references" ("xrefs") work. 
	- From the refmap, you can query what things refer to an address, or what things an address refers to.
- It finds *new things to analyze.* 
	- For example if it sees a call instruction, that's a good indication that the thing it refers to is the beginning of a function, so that referent can be scheduled for pass 1 analysis.

After pass 3, the function is officially Done Being Automatically Analyzed!

But there are other kinds of analysis operations which can be triggered, such as function splitting (again!) or...

### Jumptable analysis

The references pass may discover indirect jumps and calls which are commonly used to implement jump tables. Since the MMU state is now known, it can schedule these instructions to be automatically analyzed.

**THIS IS NOT WRITTEN YET**

But the idea will be to like, heuristically estimate size of the jump table and enqueue those as functions to be analyzed...

---

## Architectures, Platforms, and Programs

The function analysis can't just read arbitrary blobs of bytes and understand them. To do that translation, the analysis uses an **architecture** to disassemble binaries into the platform-agnostic **instruction** objects.

Each architecture has (currently) three important parts:

1. **Disassembler**
	- This is given raw byte slices and is expected to produce `Instruction` objects from them. It's a very simple interface, but the implementation can be pretty complex - see some of the implemented architectures in the `arch/` directory.
2. **Printer**
	- This is used to pretty-print disassembled instructions in a human-readable way.
3. **IR Compiler**
	- This is given a disassembled instruction, and produces IR code which implements that instruction's behavior.

But an architecture only describes the behavior of the *CPU.* Software executes within a larger system involving ROM, RAM, MMIO, and so on. And the same CPU architecture can be used in multiple systems with totally different configurations!

A **platform** describes the system as a whole. There are again three parts, but they're not all bundled together in a single object like architectures:

1. **Platform**
	- This is extremely simple right now - it just gives the architecture for this platform.
2. **Loader**
	- Given a binary image, decides whether or not this is a binary for this platform.
	- When an image is loaded, the loaders for all platforms are asked if they can load the image until one is found.
3. **MMU**
	- Each platform has its own MMU which defines its memory map.
	- Images will often extend this basic memory map in their own way (corresponding to memory mapper controllers on cartridges/in the system).

Finally, a **program** is the big object that wraps everything up. It's the public interface to the analysis framework. 

---

# Analyzing 8-bit programs is hard

Disassembling machine code is not hard. What is hard is **knowing which addresses to look at.** You can try disassembling every byte of a file from top to bottom but you're unlikely to find much useful - many of those bytes aren't code, and since these architectures almost all use variable-length instructions, you may be "out of sync" with the real instruction stream and just produce a bunch of garbage and invalid instructions.

To do the analysis properly you have to **start from a known address and MMU state** - typically the power-on reset vector and state - and proceed from there. Typically MMU state is accessed as MMIO, so every load or store *could* affect MMU state, but you don't know which ones do until you know which addresses they access. And on 8-bit architectures, the addresses are frequently *not written in the instruction.*

```ruby
	ld [hl], a   # MIGHT change MMU state
	ld b, [de]   # MIGHT depend on MMU state set by first instruction,
	# or even worse, first instruction may have *swapped out the CURRENT memory bank*
	# meaning that the second instruction is not executed - some instruction at the
	# same VA in some other bank is executed instead!!!
```

This means basically any memory-accessing instruction becomes a non-local control flow whose target is unknown. This is an absolute conceptual *tarpit.* Fortunately, we have something on our side: **most programs written for 8-bit computers were hand-written by human beings.**

Humans don't like complexity. It's why all our computers have nice flat memory address spaces and orthogonal registers and instructions now. So, humans writing programs for 8-bit computers **tended to make things easier on themselves by simplifying things.**

Here are the major simplifying assumptions I am operating under for this analysis framework:

1. **The bytes of a single instruction will be *entirely* contained within one memory segment.**
	- Typically the way these games were written was by having one source code file per memory segment. You run that file through the assembler, you get the ROM image for that segment. So, it would be *very* unlikely for a human program to split a single instruction across two files that represent different segments.
	- In the entire NES game library, there is a *single* game that splits the bytes of an instruction across two non-contiguous segments a *single* time. Is it even intentional?? Who knows!!
		- [on this page](https://wiki.nesdev.com/w/index.php/Tricky-to-emulate_games) it's The Magic of Scheherazade
2. **A function's code will be *entirely* contained within one memory segment.**
	- Again, following from how these programs were written, all the basic blocks that comprise a function would *typically* be in one file, and therefore one segment.
	- In the probably-rare-but-definitely-possible case that this assumption is wrong, it might be the case that the two parts of the function can be modeled as two functions where the first tailcalls the second. 
	- In the vanishingly-unlikely-but-it'll-probably-bite-me-in-the-ass case that there *is* a function written where some basic blocks are in one segment and some are in another and it ping-pongs between them, I will personally seek out the programmer (if they're still alive) and ask them who hurt them.
3. **If the *currently-executing bank* is swapped out by its own code (like in the above example), then the code at that address will be the same in *all banks.***
	- That is, *the exact same sequence of bytes* will be at *the exact same offset* in all banks, meaning that even though the VA could map to multiple EAs, *the code at every EA will be identical.*
	- **This is a real thing that happens!** The NES Battletoads has a function to switch memory banks; it is replicated across all memory banks at the same offset, so that no matter what the MMU state is, the same function will always be accessible at the same address.
4. **Each mappable memory bank will only appear at ONE virtual address.**
	- To explain this by example: 
		- Several NES memory mappers allow you to swap out 8KiB "windows" at virtual addresses `0x8000`, `0xA000`, `0xC000`, and `0xE000`. 
		- Each window can "view" one of N banks, for N > 4.
		- For some bank X, *technically* you could map it into any of those 4 windows... but *in practice* each bank would be mapped to the **same window - and therefore the same VA - every time.**
	- Mostly the architectures themselves prevent this from being a problem - they can't really do "position-independent code" the way modern architectures do. But I also imagine that humans wouldn't map the same thing at different addresses at different times... that would be nightmare world

---

# Finding memory references (or: The Tarpit Never Ends!)

*Even with* the above simplifying assumptions in place, you still have to *find references to memory addresses* which sounds like it should be simple but it really isn't.

Modern 32- and 64-bit architectures commonly use either absolute addressing (full addresses) or relative addressing (some pointer register plus an offset, like `sp + n` or `pc + n`) where it's easy to know/track the register value. This makes it pretty easy to find memory references. 

These old 8-bit CPUs usually had 16-bit address buses, meaning that **an address would not fit in a single register.** So they had to resort to more complicated ways of specifying addresses.

The MOS 6502 series *did* use quite a few absolute addresses embedded in instructions, but it also had some complex multiply-indirect addressing modes which can obscure the final accessed address if you just do a simple instruction-by-instruction scan for addresses.

The Intel 8080 and its derivatives - the SM83 (Gameboy) and Z80 (SO many things) - are even worse about this. *Occasionally* you will find an absolute address, but *most* addresses are either relative or **two parts, concatenated together.** For example in SM83/Z80,

```ruby
	ld h, $80    # h = 0x80
	ld l, $04    # l = 0x04
	ld a, [hl]   # a = load from memory address 0x8004 (!!!!!!)
```

On the surface *none* of these instructions specifies a memory address. Putting an immediate into a register is used in many situations, not just loads and stores. The `h` and `l` registers are *frequently* used to hold memory addresses, but that's not their only purpose. It's only the final instruction which tells us the `hl` register - a concatenation of the bits of `h` and `l` - is used as a memory address, which means **the first two instructions are actually loading the two halves of a memory address into `h` and `l`!**

This pattern is used *absolutely everywhere* in SM83 code. Or, they set `h` to something and "move around" to different variables in the same 256-byte block by only changing `l`. Or, they put an address in `hl`, then copy it to `bc` to back it up, then back into `hl` later and reuse it. And so on and so forth. Just juggling pieces of addresses around in registers, forever. The straightforward "does this instruction have a memory address in it?" style of analysis is basically useless here.

There are really two problems to be solved in this example:

1. How do you determine the actual VA accessed by the final `ld` instruction?
2. Even if you answer #1, how do you mark the first two `ld` instructions as "referencing memory"?

## First try: Interpretation

My first attempt at solving this was to *interpret* the machine code, like an emulator does. This... kind of works. It *does* manage to recover some addresses, especially in short straight-line code segments like the example above. But it has some serious downsides:

- You have to write an interpreter for every CPU architecture. 
	- That is... tedious.
- In order to get enough information to recover memory addresses, you can't just run one basic block - you kind of have to run an entire function.
	- But you have no idea what arguments the function takes, so if it calculates memory addresses based on its arguments, and you didn't pass good arguments, it might give you garbage.
- You also can't solve the halting problem, dummy.
	- So, you put instruction count limits on the interpreter so that you don't get stuck in infinite loops.
- ...except that it's hard to tell whether a loop is infinite or just taking a long time.
	- ...and in many games, the **very first thing they do on start** is enter some very long loops to zero out memory or copy a bunch of data into RAM or whatever.
- So you increase the instruction count limit until it gets past those initialization loops, only to run into a new problem: **it's stuck in an infinite loop waiting for some memory-mapped IO location to change value.**
	- That means in order to get accurate behavior, you also have to emulate the *other* hardware in the system. 
	- Now it's sounding like a multi-system emulator project, and that's not what I signed up for.

And even if you DO recover a memory address through interpretation...

- Where did it come from? As in, how was it computed?
	- Interpreting the code above *will* tell you that the third load accesses `0x8004` and that `h` and `l` held the two halves of it. It does *not* tell you how `h` and `l` *got* those values.
- Will that instruction ALWAYS access that same address?
	- In the above code, yes. In other code, not necessarily!
	
	```ruby
		ld h, [bc]
		ld l, [de]
		ld a, [hl]
	```
	
	Same final instruction, but now the first two loads get the two halves of the address from *two other memory locations* which could be anything at runtime.
	
Ultimately, **the interpretation approach *kind of* solved problem 1 and didn't solve problem 2.**

Then I taught a compilers course for a few semesters and learned some shit

## Second try: Intermediate Representation

This is meant to be a *static* reverse engineering and analysis framework. Interpretation is a *dynamic* analysis. It just doesn't fit. What *does* fit is **dataflow analysis.** This is a class of algorithms that treats the CFG of a function as a sort of network of "pipes" through which data flows; eventually the flow reaches a stable state, and you can read out the state of each "pipe" to learn things about the code.

Doing dataflow analysis directly on the underlying instructions is hard, so instead the instructions are **"raised" to a higher-level intermediate representation (IR).**

This IR is based on both Ghidra's Pcode and rustc's MIR. The IR:

- **abstracts away all the platform differences** so the analysis algorithms don't have to be hand-tailored to each ISA
- **makes all operations and side effects explicit,** allowing much more fine-grained analysis
	- for example, "condition flags" are usually set implicitly by many instructions; in the IR, every condition flag is set explicitly in its own IR instruction

Furthermore, the IR is converted to **single static assignment (SSA)** form before analysis. SSA rewrites the code so that every "register" (well, abstract representation of a register) is only assigned **once,** and decisions of which value to use on control flow paths are made explicit through "phi functions." It's really confusing stuff *but* you get some *really nice benefits* from using SSA:

- Many interesting facts about the code kind of "fall out" of the SSA with minimal or zero further analysis. For example you get all this stuff basically "for free:"
	- Which registers are used as arguments to this function?
	- Which are used as return values?
	- Which are unaffected by this function?
	- Which registers contain constant values?
- Some analyses which are difficult to do on code with mutable registers become trivial in SSA
	- Such as *constant copy propagation* (if a register is assigned a constant e.g. `A = 10`, then anywhere else where `A` is used, it can be replaced with that constant `10`)
	- And *constant folding* (if you have an operation with two constant operands e.g. `10 + 3`, then it can be replaced with a constant that is the result of that operation: `13`)
	- And those two analyses feed off of each other to come up with **constant values in many places throughout the IR, such as *constant memory addresses used in loads and stores***, gasp

It's also possible to **build up an AST** representing the "provenance" of a constant as you do constant folding and copy propagation, meaning that you can **discover every instruction which contributed to calculating a value.** And with that, this example becomes trivial:

```ruby
	ld h, $80    # h = 0x80 (a constant)
	ld l, $04    # l = 0x04 (a constant)
	ld a, [hl]   # here, hl = 0x8004 (a constant), and it was created by concatenating h and l,
	# and h and l were set to constant values by the first two loads
```

Problems solved!

1. How do you determine the actual VA accessed by the final `ld` instruction?
	- **By doing constant folding and copy propagation.**
2. How do you mark the first two `ld` instructions as "referencing memory"?
	- By knowing that **the constant is used as a memory address...**
	- and by knowing **how it was built up, and by which instructions.**

---

## How MMU state change analysis works

Finally we can talk about how the MMU state change analysis algorithm works.

### 1. Raising to IR and conversion to SSA

The MMU state change pass builds up a sort of parallel "IR Function" to the real function, with a similar CFG and one "IR BB" for each real BB. Each architecture has an IR compiler which is capable of turning any low-level instruction into one or more IR instructions; each IR BB is filled in by this compiler.

Then that IR code is converted to SSA, which is its whole own big damn algorithm. I'm not gonna explain it here.

### 2. Global constant copy propagation (GCCP)

A single dataflow algorithm is run on the IR function which performs constant copy propagation and constant folding simultaneously, typically called GCCP. The results of this algorithm are a map with one entry for each register that was determined to have a constant value, and what that value is.

### 3. Locating state changes using the results of GCCP

Now that we have a bunch of brand-new constant values, we can iterate every instruction that uses one of those constant values as a memory address. 

For each of those, we ask the platform's MMU, "does this cause an MMU state change?" We can also frequently know exactly what the state is changing to because the constant propagation discovered e.g. *what value* is being stored into the state change MMIO register. 

**Not every state change can be determined!** If, for example, the new state is passed as an argument to a function, we might be able to determine *that a state change happened* but not necessarily *what the new state is.* This is both a limitation and a strength of static analysis - it gives the reverse engineer information that *something more complex is going on here,* and it's something they should look into more deeply.

### 4. BB splitting and terminator rewriting

As a result of the previous step, some instructions which were in the middle of BBs now need to be at the ends of them (since MMU state is only tracked on a per-BB basis). So we split any needed BBs, and for all the locations where the state is changed, the BB terminator is rewritten to a "state changed" terminator. 

At this point, the IR is no longer in sync with the function so it is discarded.

### 5. MMU state dataflow analysis on the updated function CFG

A second dataflow algorithm! This time, it's run on **the real function's CFG.** Each BB has one or more **predecessors** (BBs that transfer control to it), and therefore one or more MMU states flowing into it. The result of running this algorithm is that for each BB, we come up with one of two possibilities for the MMU state for it:

- The state can only be a single possibility
- The state could be one of a set of possible states

The second possibility is not fully handled yet but yeahhhhhhhhh that's it 