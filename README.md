# Hack


## Assembly / Assembler


### Hack Hardware

The Hach machine is a Harvard architecture, turing-complete, 16-bit machine that provides basic input and output.

#### Registers

There are two programmer-accessible registers in the Hack architecture; the Address register (A) and the Data register (D).  The A register is used to hold the address of a memory location and can also be used for general purposes.  The D register is a general purpose register that is typically used to accumulate the result of calculations to be stored in memory.

#### Memory + I/O

The Hack architecture has a ROM program memory size of 32K (2^15), and 16K (2^14) data memory locations, 0-16383.  ROM is inaccessible to the programmer.  The Hack architecture will always start with instruction 0 in the ROM and all data memory values set to 0.

The instruction width used in the architecture makes it possible to address up to location 32767 in memory.  I/O devices take advantage of this and are mapped to locations 16384 onward.  The basic Hack architecture has two I/O devices; a keyboard and a black-and-white display.

The keyboard is mapped to address 24576.  It is impossible to write to this address.  Reading from this address will yield the ASCII code of the currently pressed key, or 0 if no key is currently down.  In addition to the standard ASCII codes, the keyboard recognizes:

* newline --> 128
* backspace --> 129
* left arrow --> 130
* up arrow --> 131
* right arrow --> 132
* down arrow --> 133
* home --> 134
* end --> 135
* page up --> 136
* page down --> 137
* insert --> 138
* delete --> 139
* esc --> 140
* f1-f12 --> 141 - 152

The screen is a strictly black-and-white screen with 512 columns and 256 rows.  The screen is mapped to addresses 16384 - 24575.  Each bit in this range refers to one pixel on the screen, starting with bit 0 of memory location 16384 referring to the upper left corner of the screen and bit 16 of memory location 24575 referring to the bottom right.  A high bit (1) means the pixel is black and low (0) means it is white.  Each location is readable AND writable.


### Assembly Specification

Assembly files are plaintext files consisting of instructions and comments.  Whitespace (apart from line seperator(s)) is completely ignored.  Comments span from the characters "//" to the end of the line.

Hack assembly instructions are divided into three categories: Labels, Addressing, and Calculations.

#### Labels

Labels and Addressing work together to allow for branching in a program.  Labels associate arbitrary lines of assembly code with a symbol that can then be used by an Addressing command to reference that point in the program.  A symbol in Hack assembly is any sequence of letters, digits, '_', '.', '$', and/or ':' that does not begin with a digit.  Symbols are CaSe SeNsItIvE; `THIS` is not the same as `this`, for instance.  The syntax for declaring a label is `(LABEL_NAME_HERE)`.  Note that labels are not actually translated into machine language, but are used by the programmer and assembler to allow for easier program location referencing.

#### Addressing (Load)

Addressing commands allow data to be loaded into the A register.  The syntax for addressing is simply `@ADDRESS_HERE`.  An address can either be a symbol or a literal decimal integer (between `0` and `32767` inclusive).  If a symbol is used that matches a label declaration anywhere in the program, the location of the instruction after that label command will be loaded into the A register.  If a symbol matches one of the predefined Hack assembly built-in symbols, it will address that memory location.  Otherwise, the symbol will be assigned an arbitrary memory address that is not already predefined.  Note that due to hardware limitations, there can only be up to 16368 of these final address symbol types (non-predefined, non-label).

The pre-defined Hack assembly symbols are:

* `SP` --> 0
* `LCL` --> 1
* `ARG` --> 2
* `THIS` --> 3
* `THAT` --> 4
* `R0`-`R15` --> 0 - 15
* `SCREEN` --> 16384
* `KBD` --> 24576

A reminder that reading memory beyond address 24576 (and writing beyond 24575) produces unknown results as defined by the base Hack architecture.  It is perhaps possible for additional tooling to be added to the machine that takes advantage of these ports; consult the relevant documentation for your hardware implementation.

#### Calculations

The calculation commands are what allow the machine to perform operations on data.  All calculation instruction are of the following form:

`DEST=CALC;JUMP`

Each possible CALC produces some output (optionally) using the state of the A and D registers ('M' references the memory location at the address in 'A').  The output is then stored in the location(s) specified by DEST, and JUMP is evaluated to see if execution should continue linearly.

All CALCs are predefined, and cover a large range of operations.  They are:

<dl>
<dt>0 / 1 / -1 / D / A / M</dt>
<dd>Pass along the provided value</dd>
<dt>!D / !A / !M</dt>
<dd>Bitwise invert (NOT) the value</dt>
<dt>-D / -A / -M</dt>
<dd>Negate the numeric value</dt>
<dt>D+1 / A+1 / M+1 / D-1 / A-1 / M-1</dt>
<dd>Add/subtract one to/from the value</dd>
<dt>D+A / D+M / D-A / D-M / A-D / M-D</dt>
<dd>Add/subtract between two values</dd>
<dt>D&A / D&M / D|A / D&M</dt>
<dd>Perform bitwise operations between two values</dd>
</dl>

The DEST location is any mixture of the three characters 'A' (address), 'M', (memory), and 'D' (data).  A and D store the result of CALC in their respective registers while, M stores the result in the memory location loaded into the A register *in the current machine state* (That is, the value of A after the previous instruction fully completed, NOT the value of A that will be set by this instruction).  'M' must be mentioned before 'D', and 'A' before 'M' (and 'D').  Each location may only be referred to once.  If no DEST is specified, the '=' symbol should be removed from the command, and the registers and memory will not have their data altered.

The JUMP check determines if the program will execute the command at the next program address or if it will jump to the location stored in the A register.  If JUMP 'evaluates' to true, execution will branch.  JUMP makes a comparison between the result of CALC and the value `0`.  If no JUMP is specified, the ';' symbol should be removed from the command, and the program will continue with the next linear instruction.  The JUMP comparisons that can be made are: `JGT` (>0), `JLT` (<0), `JGE` (>=0), `JLE` (<=0), `JEQ` (==0), `JNE` (!=0), `JMP` (unconditional).

Some example calculation instructions are `D=A+1`, `0;JMP`, `MD=D&A;JNE`, and `M+1` (remember, just because it's valid does not mean it's useful).


### Assembler Usage

`$ HAssemble [file]...`

The assembler takes a number of input files as its arguments.  Each file is assembled individually, and the output is placed in the same location as the input, under the filename `[input].hack` (as opposed to [input].asm).  If no arguments are specified, the assembler reads from STDIN and outputs to STDOUT.

## VM / Translator

Virtual machine files are plaintext files that contain VM instructions and comments.  Each VM instruction appears on a single line whose tokens are seperated by some amount of whitespace. Comments span frmo the characters "//" to the end of the line.

The virtual machine is stack-based; all commands perform some operation on the stack or otherwise affect memory based on the state of the stack.  Virtual machine commands are divided into four major types: arithmetic, memory, flow, and function.

### Arithmetic Commands

These commands perform arithmetic and logical operations on the top of the stack.  `and`, `sub`, `eq`, `gt`, `lt`, `and`, and `or` pop a value during computation, while `neg` and `not` simply modify the top of the stack.  `eq`, `gt`, and `lt` replace the top of the stack with either `0` or `-1`.

### Memory Access Commands

These commands transfer data between segments of memory and the stack.  Each segment (except for `constant`) can be pushed from and popped to.


### Program Flow Commands

These commands provide basic branching in the form of `label`, `goto`, and `if-goto`.  The goto commands are only able to jump to a label declared in the same function the goto is located in.

### Function Commands

These commands allow for the defining, calling, and returning of functions.  Functions are declared with a number indicating the number of local varaibles they have, and are called with a number indicating the number of arguments passed.  `return` simply returns to the calling point in the program.

### Translator Usage

`$ HTranslate [file]...`

The translator takes a number of input VM files as its arguments.  These files are all assembled into one .asm program which is sent to STDOUT.
