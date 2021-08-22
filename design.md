# Design

This is an overview of the major concepts/model this framework uses. (It's partly notes to myself, since I have a habit of going for months or years without working on something.)

---

## Memory Model

The basis of the analysis framework is the **memory model,** which models how the CPU accesses things like RAM, ROM, hardware devices, etc.

### Images and Physical Addresses (PAs)

All analysis is done on an **image,** which is a single file that contains all the code and data for a program. ROM dumps, disk images, and executable files are all kinds of images.

A **physical address (PA)** is an offset into an image's data. PAs don't really come up in the public interface, but that's what they are.

### Memory and Segments

The **memory** is a model of the memory architecture of the target platform. It models anything the CPU can access with loads and stores: RAM, ROM, MMIO, etc.

The memory is split into multiple **segments,** regions of memory with specific behaviors and contents. Some segments are **real,** meaning they are mapped to a slice of the image. Others are **fake,** meaning they refer to something built into the platform (like RAM). (Fake is kind of a weird name for them, I guess.)

The issue with modeling memory on these old systems is that it was common for the size of the image to exceed the CPU's addressing capability. So, the solution was to use some kind of **memory management unit.**

### MMUs and MMU state

The **MMU (memory management unit)** models how the platform maps the addresses the CPU *uses* onto the hardware, RAM, and ROM that is actually *accessed.* These systems might not have had "a thing" called the MMU, but they usually had circuitry to **decode addresses,** so that e.g. one part of the address space accessed RAM and another part accessed ROM. Although the memory object has a bunch of segments for things like ROM and RAM, it's really the MMU that defines the map of the address space.

Beyond simple address decoding, many of these systems made heavy use of **memory banking:** treating parts of the address space as "windows" which could "view" different pieces of the image. To do this, **some special register(s)** would be used to select which pieces would be visible in the address space. The contents of that register/those registers is the **MMU state.**

*This* is kind of the whole point of this framework: **to track MMU state.** RE tools like IDA and Ghidra just don't seem to be up to the task, and it makes analyzing these kinds of programs extremely tedious.

### Virtual Addresses (VAs) and Effective Addresses (EAs)

Every memory access the CPU does uses **virtual addresses (VAs).** These VAs are sent to the MMU to be translated into **effective addresses (EAs).** An EA is really a pair of (segment, offset): the segment it refers to, and the offset within that segment.

Despite that, EAs may be displayed displayed as `SEG_NAME:0000` where `0000` is not the offset, but *the VA that was used to access it.* It's just easier for our Human Brains. (This is exactly how IDA does it too.)

Here's the problem: in the presence of memory banking, the VA -> EA relationship **may be one-to-many.** That is, some VA (e.g. `0x8000`) may refer to one of several different EAs (e.g. `PRG0:8000`, `PRG1:8000`, `PRG2:8000` etc.) *depending on the MMU state.* This is what makes analysis of these programs so Interesting.

Some mappings are one-to-one though. E.g. on the NES, VAs `0x0000 ..= 0x07FF` *always* refer to the RAM.

### A Diagrammatic Overview

So here's how all those pieces fit together, rendered in ASCII:

```
                                    Segments
                    ___________________/\____________________
                   /                                         \
+------------+     +--------+ +--------+ +--------+ +--------+
|   Memory   |---->|  RAM   |,|  MMIO  |,|  PRG0  |,|  PRG1  |,...
|            | has | (fake) | | (fake) | | (real) | | (real) |
+------------+     +--------+ +--------+ +--------+ +--------+
                      ^                       | PA       | PA
                      |                       v          v
                      |                  +------------------------...
                      |            Image |Code that|         |    ...
                  EAs |                  | uses VAs|         |    ...
                      |                  +------------------------...
                   +-------+                  |
                   |  MMU  |  VAs + MMU state |
                   |       |<-----------------+
                   +-------+
```

The secret sauce is the "VAs + MMU state" arrow. This is the Novel Thing that this framework does. Woo.

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

Each BB's code span refers to a sequence of bytes in the original image, which are the encoded instructions. During analysis, each instruction is decoded into a platform-agnostic **instruction** object, which can have **operands;** these are stored in the BB. So you can imagine there are the "real" instructions (a sequence of bytes), and overlaid on them are the "abstract" instructions (actual objects) that represent them.

The last instruction of a BB is special; it's a **terminator** which performs some kind of control flow. For example (and this is not an exhaustive list):

- Return instructions
- Unconditional jumps/branches
- Conditional jumps/branches
- Function calls
- MMU state changes

The last one is key for MMU state tracking: MMU state changes can only occur on the boundaries of basic blocks, meaning each BB only has to remember one state.

### Data Items

**Data items** are to data spans as BBs are to code spans. It hasn't been developed much so far, but there's a whole **type system** prototyped out for representing all kinds of things like:

- primitive types
- strings
- arrays
- pointers (absolute and relative)
- structs
- bitfields
- enums

---

## Function Analysis

Function analysis is sort of the beating heart of the analysis framework; it's what largely discovers what's code and what's data, builds BBs and puts them together into functions, and splits up previously-analyzed BBs and functions if later analysis reveals that it was mistaken.

### Pass 1: discovery

First, the framework is given the EA of a potential function to analyze. This might be an entry point into the program (e.g. a reset/interrupt vector or the entry point in an executable), or it might have come from a function call instruction in another function.

Regardless, the first pass just tries to figure out the boundaries of the function. It decodes instructions, building BBs as it goes, and follows intra-function control flow to ultimately get to the end (or ends) of the function.

If this process completes successfully, the collection of BBs is turned into a real function, and that function is then scheduled for either pass 2 or pass 3.

### Pass 2: MMU state change analysis

If, during pass 1, it was discovered that some instruction in the function changes the MMU state, this is the next analysis pass. (If not, it skips to pass 3.)

This tries to figure out exactly what the new MMU state will be for each state change terminator in the function. Some of this is done by interpreting the code with an augmented interpreter that tracks the source of values. It's not super advanced right now, but it's clever enough that it can e.g. see that the MMU state being set was loaded as a constant into a register a few instructions earlier.

If the new MMU state can be determined, it is propagated through the rest of the function's CFG; this can cause some of the code to be re-analyzed as addresses that may have been unresolvable during the first pass may make sense now.

If the new MMU state *can't* be determined, well, uh, it doesn't do anything right now, but it *should* record this somewhere to notify the user that it needs help.

Regardless, after this pass finishes, the function is scheduled for pass 3.

### Pass 3: references

Now that the MMU state has been (hopefully) determined for the whole function, the "references" pass goes through every instruction in the function to look for references to things. This pass has kind of two responsibilities:

One, it adds entries to the **references map** (or "refmap"), a global bidirectional mapping that records what things refer to what other things. This is how "cross-references" ("xrefs") work. From the refmap, you can query what things refer to an address, or what things an address refers to.

Two, it finds *new things to analyze.* For example if it sees a call instruction, that's a good indication that the thing it refers to is the beginning of a function, so that referent can be scheduled for pass 1 analysis.

After pass 3, the function is officially Done Being Analyzed!

---

## Architectures and Platforms

The function analysis can't just read arbitrary blobs of bytes and understand them. To do that translation, the analysis uses an **architecture** to disassemble binaries into the platform-agnostic **instruction** objects.

Each architecture has (currently) three important parts:

1. **Disassembler**
    - This is given raw byte slices and is expected to produce `Instruction` objects from them. It's a very simple interface, but the implementation can be pretty complex - see some of the implemented architectures in the `arch/` directory.
2. **Printer**
    - This is used to pretty-print disassembled instructions in a human-readable way.
3. **Interpreter**
    - This is given a basic block, and is expected to interpret the instructions within in order to extract information that cannot be determined statically. However I think it's going away and being replaced by the IR compiler soon.

But an architecture only describes the behavior of the *CPU.* Software executes within a larger system involving ROM, RAM, MMIO, and so on. And the same CPU architecture can be used in multiple systems with totally different configurations!

A **platform** describes the system as a whole. There are again three parts, but they're not all bundled together in a single object like architectures:

1. **Platform**
    - This is extremely simple right now - it just gives the architecture for this platform.
2. **Loader**
    - Given a binary image, decides whether or not this is a binary for this platform.
    - When an image is loaded, the loaders for all platforms are asked if they can load the image until one is found.
3. **MMU**
    - Each platform has its own MMU.