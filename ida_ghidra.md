
- during initial analysis...
	- each operand has a VA associated with it
		- **according to docs:** The target virtual address is stored in `op_t::addr` and the full address is calculated as `to_ea(insn_t::cs, op_t::addr)`. For the processors with complex memory organization the final address can be calculated *using other segment registers.* For flat memories, `op_t::addr` is the final address and `insn_t::cs` is usually equal to zero. In any case, the address **within the segment** should be stored in `op_t::addr`.
	- each instruction has 1 or more outrefs associated with it
- then during output...
	- each operand calls `out_name_expr` which takes the operand and its associated VA
	- and **presumably, internally, it looks in the outrefs to determine how to output it (as a label or raw address or whatever)** - this is what I didn't understand before
	- and if `out_name_expr` fails, it falls back on the implementor to output a value with `out_value` which just prints it out as a raw address or something

	`out_name_expr` calls...
		`get_name_expr`, which calls...
			`get_name_base_ea` which accounts for fixups
			then it calls `get_name_nodisp`, which calls...
				`get_colored_demangled_name`, which calls...
					`get_demangled_name`, which calls...
						`get_true_name`, which...
							1. tries to find a function-local name for it first
							2. then looks in the database using `netnode_name`
							3. then if the debugger is enabled I'm *guessing* it looks up some kind of temporary name that might be set in the debugger?
							4. finally it falls back on `generate_default_name?`
				or if that failed, calls `generate_default_name?` as a fallback
			and finally calls `print_disp` which prints the `+ 0x14` or whatever if applicable


	`ea get_name_base_ea(ea from, ea to)` - Get address of the name used in the expression for the address. Returns address of the name used to represent the operand.
		- ?????? what the fuck does any of this mean?
		- a **name expression** is a "name with a displacement."
			- ahhh. that's like, `loc_0840 + 0x14` or whatever.

	`ea_t ida_export map_code_ea(const insn_t &insn, ea_t addr, int opnum)` calls...
		`get_refinfo` !!!!!
		how does `set_refinfo` work?
			you're not supposed to call it - it's called by `set_opinfo`, which itself is a low-level function called by the kernel
			so the kernel is determining this...?
			a few processor modules do call it but only really in special situations

	so yeah it seems like the operand has an addr (VA), BUT THEN each operand has a refinfo
	that can be associated with it which holds the ACTUAL reference. or something.

Okay. So here's how IDA handles instruction operands. `op_t` has:

- n (index of operand)
- type (type of operand)
- offb (number of bytes from start of instr)
- offo (something else)
- flags (8b, miscellaneous stuff)
- dtype (type of the value that the operand gives within the instruction)
- reg/phrase (which reg(s) it represents)
- value (the actual value, like for immediates)
- addr (VA)
- specval (for user-defined value)
- specflag1~4 (for user-defined flags)

BUT THEN THERE'S MORE SHIT

each ea can have flags, and the flags associated with the ea of an instruction has multiple nybbles representing the operands.. these flags seem to control the representation, how it shows up in the disassembly view and has options like hex, dec, char, segment, offset, enum, struct offset, stack var, custom repr etc.

AND THEN there's `opinfo_t` which is a union which can be one of

- `refinfo_t` for offset members
- `tid_t` for struct, etc. members
- `strpath_t` for stroff
- `int32` for strings (\ref STRTYPE_)
- `enum_const_t` for enums
- `custom_data_type_ids_t` for custom data

and that `refinfo_t` contains:

- target EA (an explicitly specified expression target)
- base EA (the offset base, a "linear address" - basically, the beginning of the address space where the target is pointing? I think?)
- tdelta (a displacement from the target which will be displayed in the expression)
- flags (MORE FUCKIN FLAGS which includes things like):
	- what kind of reference it is ("offset" (which may be absolute, but "relative" to 0), or just the low/high 8/16 bits of an addr)

`target = operandvalue - tdelta + base`
and once we have `target`, the actual `EA = target + tdelta - base`

so summing it up, the way memory references are handled is:

- each operand can hold a VA
- each operand has an associated nybble in flags64_t to encode how it's represented in the listing
- each operand can have associated refinfo, which holds the target EA (operand - delta + base)

and the kernel is what fills that refinfo in.