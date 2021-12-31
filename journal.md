
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