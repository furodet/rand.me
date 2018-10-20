rand.me virtual machine architecture overview
=============================================

General structure
-----------------
The virtual machine architecture is defined by an execution
context, maintained by `VmContext` including:
  * A set of variables (aka the *VarSet*)
  * An instruction memory
  * A stack

**Vitual machine profile**

An execution context is created on the basis of a profile (`VmProfile`), initially
represented by a description string:

```
    VmContext.usingProfileString(...)
```

**Virtual machine types**

TODO: format:

```
  *
  * Given that the machine words have fixed length, the VM supports any
  * type whose number of bytes is less than this length. Moreover, each
  * type can be signed or unsigned.
  *
  * Each type has a given name, equal to "u" (for unsigned) or "s" (for
  * signed), followed by the type length, in bits.
  *
  * For example, for a 32-bits machine (4 bytes word), the allowed types
  * are: {{{s8 u8 s16 u16 s32 u32}}}.
  *
  * As a consequence, the virtual machine types are not something hard-coded,
  * but a list of allowed types, constructed by
  * {{{VmTypes.forMachineWordByteLength}}}. The resulting {{{VmTypes}}} allow
  * to:
  *   - get a type definition from its name {{{valueOf}}}
  *   - select types according to the length and signedness {{{select}}}
```

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

**Heap**

TODO

**TODO: instruction memory, PC, stack**
