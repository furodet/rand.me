rand.me virtual machine architecture overview
=============================================

General structure
-----------------
The virtual machine architecture is defined by an execution
context, maintained by `VmContext` objects including:
  * Heap: a set of global variables
  * Stack: local variables and function parameters
  * Instruction memory: the program
  * Exit code: set when the program terminates with an exit value

**Virtual machine profile**

An execution context is created on the basis of a profile (`VmProfile`), initially
represented by a description string:

```
    VmContext.usingProfileString(...)
```

The profile string has the following form:

```
    bl:BYTELEN:heap:HEAPSIZE
```

Where:
  * `BYTELEN` is the machine word length, as desribed in *Virtual machine types*
    * It must be a power of two
  * `HEAPSIZE` is the number of variables in the heap
    * It must be strictly greater than 0

**Virtual machine types**

rand.me virtual machine is not designed for a specific word length, but supports any
scalar type whose size is less than a supplied "target machine byte length".

For example, to emulate a 32-bits machine, the VM will define 8 scalar types:
  * unsigned and signed bytes: `u8` and `s8`
  * unsigned and signed shorts: `u16` and `s16`
  * unsigned and signed integers: `u32` and `s32`

As a consequence, the virtual machine types are not something hard-coded,
but a list of allowed types, constructed by `VmTypes.forMachineWordByteLength`.

The resulting `VmTypes` allow to:
  * get a type definition from its name (`valueOf`)
  * select types according to the length and signedness (`select`)

**Variables and VarSet**

The virtual machine manages different types of variables, based on the common trait `Variable`:
  * `Variable.Scalar`: a scalar value, with a given virtual machine type
  * `Variable.Pointer`: a reference to another variable:
    * `ToHeapVariable`: in the heap
    * `ToStackVariable`: in the stack
    * `ToInstruction`: in the instruction memory

Every variable belongs to a specific `VarSet`, intentionally designed as a mutable structure
indexed by variable numbers.

One can:
  * `getVariable(id:Int)`: get the value of variable number `id`, which
    can be `None` if the value is not set yet
  * `putVariable(id:Int, value:Variable)`: set the value of variable number `id`

Note: `VarSet` must be created with at least ONE element. No protection is put in the constructor,
since this is naturally checked at an upper layer.

**Heap**

`VmContext.heap` is a `VarSet` providing all the program's global variables.

**Stack**

The stack is a collection of frames consisting of individual `VarSet`. Only variables of the "top"
frame are accessible:
  * `getVariable(id:Int)`: get the value of variable number `id` in the top frame
  * `putVariable(id:Int, value:Variable)`: set the value of variable number `id` in the top frame

The top frame is managed via two functions:
  * `createFrameOfSize(nrVariables:Int)`: to push a new frame
  * `popFrame()`: to delete the current top frame

**Instruction memory and program counter**

The instruction memory is represented as a collection of *basic blocks* (`VmProgram.BasicBlock`,
along with the companion builder object `VmProgram.BasicBlockBuilder`).

Each block consists of:
  * A name (which must be unique amongst the program)
  * A sequence of instructions (represented by the common trait `Instruction`)

The program counter (`VmProgram.Counter`) maintains a "pointer" to the next instruction to execute:
  * The current basic block
  * The instruction index within this block

Notice that if the program is not executing, the counter's basic block is set to `None`.

From the instruction memory, one can invoke `nextInstruction` to fetch the next instruction to execute.
This function may raise errors:
  * If the program is stopped (i.e. no current basic block)
  * Or if all the instructions of the basic block were read

A `VmContext` provides functions to move the PC through the program:
  * `incrementPc`: to go to next instruction
  * `setPcToBlockCalled` to jump to another block (which resets the instruction index)

Construction of a program consists of:
  * Creating basic block builders to which instructions can be appended with `+`
  * Build the basic blocks with builders' `build` function
  * Register the basic blocks to a program, with the `++` method
  * Registering the program to the virtual machine context, thanks to `setProgram`

**Instructions and operands**

Every instruction is an object extending `Instruction` to provide an `execute` function that:
  * Fetches the supplied operands within the execution context and translates them to variable. For example:
    * Operand `%1` is fetched as *heap variable number 1*
    * If no such type of variable is expected by the instruction, the fetching stage returns an error
    * If no such variable is found, the fetching stage returns an error
  * Executes some operations that update the execution context

A list of operands, along with the instruction to execute is an `InstructionInstance`. Such an
object is built by an assembler to create a program. The virtual machine's core execution engine
simply fetches each instruction and operands from an `InstructionInstance` and calls `execute`
accordingly.

Operands can be:
  * `Instruction.Operand.Source`: a source operand, read by the instruction:
    * `Immediate`: a scalar value
    * `Variable.InTheHeap`: the value of a heap variable
    * `Variable.InTheStack`: the value of a stack variable
    * `Reference.InTheHeap`: a `Variable.Pointer.InTheHeap` referencing a variable
    * `Reference.InTheStack`: a `Variable.Pointer.InTheStack` referencing a variable
    * `Indirect`: the variable pointed by a series of indirections (e.g. `**pointer`)
  * `Instruction.Operand.Destination`: a destination operand, modified by the instruction:
    * `NoDestination`: for fruitless operations
    * `TargetVariable.InTheHeap`: a pointer to a heap variable
    * `TargetVariable.InTheStack`: a pointer to a stack variable
    * `Redirect`: a series of pointers redirecting to a variable (e.g. `**pointer`)

