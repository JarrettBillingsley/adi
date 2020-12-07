# Adi

I have fun reverse engineering (RE) games for 8- and 16-bit consoles like the NES, Game Boy, and Genesis/Mega Drive. I wanted to make it easier to do that.

Adi (name not official, you can figure out where I got it, ohoho so original) is a work-in-progress reverse engineering library, written in Rust, and aimed at REing retro games.

Eventually I want to have an interface that runs in the browser, probably, for cross-platform-ness. Maybe with WASM the whole thing can be in the browser. Idk, I'm not married to the idea.

## Another RE thing?? Well let me explain

There are already several modern pieces of RE software, like:

- **IDA Pro**
- **ghidra**
- **radare2** (r2)
- **Binary Ninja**
- **Hopper**

Most modern RE software is aimed at information security experts for things like malware analysis, or at hardware engineers attempting to reverse firmware for microcontrollers. They focus on current architectures (x86/x64, ARM) and they largely expect you to be analyzing code compiled from a high-level language.

Retro games present some unique reverse engineering challenges that they struggle with. This software:

- is written for **old architectures**
	- most of which are all but extinct
- is mostly or entirely **hand-written assembly** which:
	- uses nonstandard calling conventions
	- makes extensive use of jump tables and value lookup tables
	- sometimes does really weird things
- runs on **bare hardware**
	- so no operating system, no system calls, no standard shared libraries...
- almost always uses **memory banking** ("memory mappers")
	- i.e. changing what data is visible in certain parts of the CPU address space
- can access **multiple address spaces** with different memory maps
	- e.g. main RAM and VRAM
- uses a lot of **memory-mapped IO,** sometimes in strange locations
	- e.g. on the Game Boy, writing into a "ROM" segment changes banks
- can contain code written for **two or more CPU architectures**
	- e.g. the Genesis/Mega Drive has a Motorola 68000 *and* a Zilog Z80
- might contain **little or no text**
	- or even text in *non-standard character encodings!*
- but on the other hand, contains **lots of graphics and sound**
	- which, if properly decoded and viewable, can be *extremely* useful for reversing

## So what's the idea with this library?

It's being designed from the ground up with these kinds of programs in mind.

- It's **architecture-agnostic**
	- And the same image can be analyzed with multiple architectures
	- e.g. on a Genesis game, the Z80 code can be analyzed as a "sub-image" from the main game
- Furthermore the same disassembly/analysis engine can be used with **multiple *platforms***
	- That is, the same CPU architecture, but in different machines
	- e.g. 6502 in an NES vs. a C64
- It has **first-class support** for memory banking
	- The memory map can be specified as having "bankable" regions
	- **Bank configurations** can be specified
- It can support **multiple memory spaces**
	- So you can e.g. map out what goes in VRAM at what locations

The plan is to have analysis passes which can deal with these and other issues - like robust jump table analysis, bank configuration tracking, emulation of memory mapper chips...

But it's mostly in the "I'm working on data structures" phase at the moment so

who knows