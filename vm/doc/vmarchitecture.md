rand.me virtual machine architecture overview
=============================================

General structure
-----------------
The virtual machine architecture is defined by an execution
context, maintained by `VmContext` including:
  * Heap: a set of global variables
  * Stack: local variables and function parameters
  * Instruction memory

**Vitual machine profile**

An execution context is created on the basis of a profile (`VmProfile`), initially
represented by a description string:

```
    VmContext.usingProfileString(...)
```

**Virtual machine types**

rand.me virtual machine is not designed for a specific word length, but support any
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
  * `setVariable(id:Int, value:Variable)`: set the value of variable number `id`

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

**TODO: instruction memory, PC**
