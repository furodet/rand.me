rand.me virtual machine embedded assembler
==========================================

The embedded assembler is part of the virtual machine's domain specific layer
that aims to provide programmatic assembler. One may import all the elements
of this module to quickly forge instructions, operands...:

```
    import me.rand.vm.AbstractAsmInstructionBuilder._
    import me.rand.vm.AbstractAsmOperandBuilder._
```

Notice that the main purpose of the embedded assembler is **NOT** to perform a strict
syntax validation but provide a simple way to construct programs. Most of the potential
mistakes that coders may do when writing assembly code are detected during the execution
of instructions, not at assembly time.

Instructions
------------
The assembler allows to create instruction instances, specifying the invoked command
and the operands.

The general structure of such a constructor is:

```
    INSTRUCTION < OPERANDS_IN > OPERAND_OUT
```

Where:
 * `INSTRUCTION` is the instruction object that needs to be invoked (e.g. `Copy`, `Exit`...)
 * `OPERANDS_IN` is the list of operands to the built instruction instance
 * `OPERAND_OUT` is the operation's destination operand

`OPERANDS_IN` is:
 * either `()` for instructions that have no input operands
 * or `(X)` (or simply `X`) for instructions that have only one operand `X`
 * or `(X,Y,...)` for instructions with multiple operands `X`, `Y`...

`OPERAND_OUT` is:
  * either `()` for instructions that produce no output or fruitless operations
  * or `(X)` (or simply `X`) for instructions that write back a result into `X`

Operands
--------

The embedded assembler defines the following types of source operands:
  * `<number> / <t>`: immediate value `<number>` of type `<t>`, where:
    * `<number>` can be `Char`, `Byte`, `Short`, `Int`, `Long` or `BigInt`
    * `<t>` is a scala `Symbol` representing the type (e.g. `'u8` for an unsigned byte)
  * `%(<index>)`: the heap variable at the specified index
  * `$(<index>)`: the stack variable at the specified index
  * `&(<variable>)`: the address of the specified variable:
    * `&(%(<index>))`: for a heap variable
    * `&($(index))`: for a stack variable
  * `*.*...(<pointer>)`: the specified pointer with *N* indirections. For example:
    * `*(%(0))`: the value pointed by heap variable `%(0)`
    * `*.*($(9))`: the value pointed by the value pointed by stack variable `$(9)` 

The embedded assembler defines the following types of destination operands:
  * `%(<index>)`: the heap variable at the specified index
  * `$(<index>)`: the stack variable at the specified index
  * `*.*...(<pointer>)`: the specified pointer redirected *N* times. For example:
    * `*(%(0))`: the value pointed by heap variable `%-(0)`
    * `*.*($(9))`: the value pointed by the value pointed by stack variable `:-(9)`

Examples
--------

 * `Exit < 123 / 'u8 > ()`
   * Generates a VM trap with exit code 123, expressed as an unsigned byte
 * `Copy < %(0) > $(1)`
   * Copies the contents of heap variable 0 into stack variable 1
 * `Add < (&%(0), 1 / 's32) > *$(0)`
   * Adds one (expressed as a signed integer) to the address of heap variable 0 and
     stores the result into the variables pointed by stack variable 0
 * `Copy < *.*($0) > %(1)`
   * Copies the contents of a variable pointed by a variable pointed by stack variable 0
     into heap variable 1
