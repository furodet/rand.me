/*
 * Copyright (c) 2018-2019 rand.me project
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */
package me.rand.simulator.test.instructions

import me.rand.commons.idioms.Status._
import me.rand.simulator.main.SimulatorError
import me.rand.simulator.test.BaseSpec
import me.rand.vm.engine.{VarSet, Variable, VmContext}
import me.rand.vm.main.VmError.VmContextError.{EmptyStackAccess, VariableIndexOutOfBounds}
import me.rand.vm.main.VmError.VmExecutionError.IllegalEncodingError.UnspecifiedDestinationOperand
import me.rand.vm.main.VmError.VmExecutionError.VmFetchOperandError.InvalidPointerValue._
import me.rand.vm.main.VmError.VmExecutionError.VmFetchOperandError.UndefinedVariable

// This is the best opportunity to validate operand translation, since copy accepts any kind
// of input variable and writes to any kind of output.
class CopySpec extends BaseSpec {
  private def main(body: String): String =
    s"""
       | $aStandardMachineConfiguration
       | .bb main
     """.stripMargin +
      body +
      s"""
         |  exit (00:u8)
         | .boot main
     """.stripMargin

  "copy" should "fail x > _" in {
    failToAssembleOrExecute(
      main(body =
        "copy (deadbeef:u32) > _"
      )
    ).thenVerify {
      case SimulatorError.FromVmError(UnspecifiedDestinationOperand) => true
    }
  }

  "copy" should "pass imm > %x (truncate store)" in {
    successfullyAssembleAndExecute(
      main(body =
        s"""
           | .var x %0 u8
           | copy (deadbeef:u32) > %0
         """.stripMargin
      )
    ).thenVerify {
      case vmContext =>
        hasHeapVariableSetToImmediate(("x", 0xef), 0, vmContext)
    }
  }

  "copy" should "pass imm > %x (unsigned extend)" in {
    successfullyAssembleAndExecute(
      main(body =
        s"""
           | .var x %0 u32
           | copy (f1:s8) > %0
         """.stripMargin
      )
    ).thenVerify {
      case vmContext =>
        hasHeapVariableSetToImmediate(("x", 0xf1), 0, vmContext)
    }
  }

  "copy" should "pass imm > %x (signed extend)" in {
    successfullyAssembleAndExecute(
      main(body =
        s"""
           | .var x %0 s32
           | copy (f1:u8) > %0
        """.stripMargin
      )
    ).thenVerify {
      case vmContext =>
        hasHeapVariableSetToImmediate(("x", 0xfffffff1), 0, vmContext)
    }
  }

  "copy" should "fail imm > %undef" in {
    failToAssembleOrExecute(
      main(body =
        s"""
           | copy (deadbeef:u32) > %0
        """.stripMargin
      )
    ).thenVerify {
      case SimulatorError.FromVmError(InvalidTargetReference(None, 0, None)) => true
    }
  }

  "copy" should "pass imm > $x" in {
    successfullyAssembleAndExecute(
      main(body =
        s"""
           | .push 1
           | .var x $$0 u32
           | copy (deadbeef:u32) > $$0
        """.stripMargin
      )
    ).thenVerify {
      case vmContext =>
        hasStackVariableSetToImmediate(("x", 0xdeadbeef), 0, vmContext)
    }
  }

  "copy" should "fail imm > $undef (empty stack)" in {
    failToAssembleOrExecute(
      main(body =
        s"""
           | copy (deadbeef:u32) > $$0
        """.stripMargin
      )
    ).thenVerify {
      case SimulatorError.FromVmError(InvalidTargetReference(None, 0, Some(EmptyStackAccess(_)))) => true
    }
  }

  "copy" should "fail imm > $undef" in {
    failToAssembleOrExecute(
      main(body =
        s"""
           | .push 1
           | copy (deadbeef:u32) > $$0
        """.stripMargin
      )
    ).thenVerify {
      case SimulatorError.FromVmError(InvalidTargetReference(None, 0, None)) => true
    }
  }

  "copy" should "pass imm > **%x " in {
    successfullyAssembleAndExecute(
      main(body =
        s"""
           | .var x   %0 u32
           | .var px  %1 ptr
           | .var ppx %2 ptr
           | copy &%0 > %1
           | copy &%1 > %2
           | copy (deadbeef:u32) > **%2
       """.stripMargin
      )
    ).thenVerify {
      case vmContext: VmContext =>
        hasHeapVariableSetToHeapPointer(("px", 0), 1, vmContext) &&
          hasHeapVariableSetToHeapPointer(("ppx", 1), 2, vmContext) &&
          hasHeapVariableSetToImmediate(("x", 0xdeadbeef), 0, vmContext)
    }
  }

  "copy" should "fail imm > *%undef" in {
    failToAssembleOrExecute(
      main(body =
        s"""
           | copy (deadbeef:u32) > *%0
       """.stripMargin
      )
    ).thenVerify {
      case SimulatorError.FromVmError(InvalidTargetReference(None, 0, None)) => true
    }
  }

  "copy" should "fail imm > *%undef (pointer type)" in {
    failToAssembleOrExecute(
      main(body =
        s"""
           | .var ptr %0 ptr
           | copy &@main > %0
           | copy (deadbeef:u32) > *%0
        """.stripMargin
      )
    ).thenVerify {
      case SimulatorError.FromVmError(InvalidRedirect) => true
    }
  }

  "copy" should "pass imm > **$x" in {
    successfullyAssembleAndExecute(
      main(body =
        s"""
           | .push 3
           | .var x   $$0 u32
           | .var px  $$1 ptr
           | .var ppx $$2 ptr
           | copy &$$0 > $$1
           | copy &$$1 > $$2
           | copy (deadbeef:u32) > **$$2
        """.stripMargin
      )
    ).thenVerify {
      case vmContext =>
        hasStackVariableSetToStackPointer(("px", 0), 1, vmContext) &&
          hasStackVariableSetToStackPointer(("ppx", 1), 2, vmContext) &&
          hasStackVariableSetToImmediate(("x", 0xdeadbeef), 0, vmContext)
    }
  }

  "copy" should "fail imm > *$undef (empty stack)" in {
    failToAssembleOrExecute(
      main(body =
        s"""
           | copy (deadbeef:u32) > *$$0
        """.stripMargin
      )
    ).thenVerify {
      case SimulatorError.FromVmError(InvalidTargetReference(None, 0, Some(EmptyStackAccess(_)))) => true
    }
  }

  "copy" should "fail imm > *$undef" in {
    failToAssembleOrExecute(
      main(body =
        s"""
           | .push 3
           | copy (deadbeef:u32) > *$$0
        """.stripMargin
      )
    ).thenVerify {
      case SimulatorError.FromVmError(InvalidTargetReference(None, 0, None)) => true
    }
  }

  "copy" should "fail imm > *$undef (pointer type)" in {
    failToAssembleOrExecute(
      main(body =
        s"""
           | .push 3
           | .var ptr $$0 ptr
           | copy &@main > $$0
           | copy (deadbeef:u32) > *$$0
        """.stripMargin
      )
    ).thenVerify {
      case SimulatorError.FromVmError(InvalidRedirect) => true
    }
  }

  "copy" should "pass imm > %x[y]" in {
    successfullyAssembleAndExecute(
      main(body =
        s"""
           | .var x0 %0 u32
           | .var x1 %1 u32
           | .var x2 %2 u32
           | copy (deadbeef:u32) > %0[2]
        """.stripMargin
      )
    ).thenVerify {
      case vmContext =>
        hasHeapVariableSetToImmediate(("x2", 0xdeadbeef), 2, vmContext)
    }
  }

  "copy" should "fail imm > %x[y] (undef)" in {
    failToAssembleOrExecute(
      main(body =
        s"""
           | .var x0 %1 u32
           | copy (deadbeef:u32) > %1[2]
        """.stripMargin
      )
    ).thenVerify {
      case SimulatorError.FromVmError(InvalidTargetReference(Some("&x0[2]"), 3, None)) => true
    }
  }

  "copy" should "fail imm > %undef[y]" in {
    failToAssembleOrExecute(
      main(body =
        s"""
           | copy (deadbeef:u32) > %0[2]
        """.stripMargin
      )
    ).thenVerify {
      case SimulatorError.FromVmError(InvalidTargetReference(None, 0, None)) => true
    }
  }

  "copy" should "fail imm > %x[y] (oob)" in {
    failToAssembleOrExecute(
      main(body =
        s"""
           | .var x0 %0 u32
           | copy (deadbeef:u32) > %0[-1]
        """.stripMargin
      )
    ).thenVerify {
      case SimulatorError.FromVmError(InvalidTargetReference(Some("&x0[-1]"), -1, Some(VariableIndexOutOfBounds(-1)))) => true
    }
  }

  "copy" should "pass imm > $x[y]" in {
    successfullyAssembleAndExecute(
      main(body =
        s"""
           | .push 3
           | .var x0 $$0 u32
           | .var x1 $$1 u32
           | .var x2 $$2 u32
           | copy (deadbeef:u32) > $$0[2]
        """.stripMargin
      )
    ).thenVerify {
      case vmContext =>
        hasStackVariableSetToImmediate(("x2", 0xdeadbeef), 2, vmContext)
    }
  }

  "copy" should "fail imm > $x[y] (undef)" in {
    failToAssembleOrExecute(
      main(body =
        s"""
           | .push 4
           | .var x0 $$1 u32
           | copy (deadbeef:u32) > $$1[2]
        """.stripMargin
      )
    ).thenVerify {
      case SimulatorError.FromVmError(InvalidTargetReference(Some("&x0[2]"), 3, None)) => true
    }
  }

  "copy" should "fail imm > $undef[y] (empty stack)" in {
    failToAssembleOrExecute(
      main(body =
        s"""
           | copy (deadbeef:u32) > $$0[2]
        """.stripMargin
      )
    ).thenVerify {
      case SimulatorError.FromVmError(InvalidTargetReference(None, 0, Some(EmptyStackAccess(_)))) => true
    }
  }

  "copy" should "fail imm > $undef[y] (oob)" in {
    failToAssembleOrExecute(
      main(body =
        s"""
           | .push 3
           | .var x0 $$1 u32
           | copy (deadbeef:u32) > $$1[2]
        """.stripMargin
      )
    ).thenVerify {
      case SimulatorError.FromVmError(InvalidTargetReference(Some("&x0[2]"), 3, Some(VariableIndexOutOfBounds(3)))) => true
    }
  }

  "copy" should "fail imm > $undef[y]" in {
    failToAssembleOrExecute(
      main(body =
        s"""
           | .push 3
           | copy (deadbeef:u32) > $$0[2]
        """.stripMargin
      )
    ).thenVerify {
      case SimulatorError.FromVmError(InvalidTargetReference(None, 0, None)) => true
    }
  }

  "copy" should "pass &%x > %y" in {
    successfullyAssembleAndExecute(
      main(body =
        s"""
           | .var x %0 ptr
           | .var y %1 u32
           | copy &%1 > %0
        """.stripMargin
      )
    ).thenVerify {
      case vmContext =>
        hasHeapVariableSetToHeapPointer(("x", 1), 0, vmContext)
    }
  }

  "copy" should "fail &%undef > %y" in {
    failToAssembleOrExecute(
      main(body =
        s"""
           | .var x %0 ptr
           | copy &%1 > %0
        """.stripMargin
      )
    ).thenVerify {
      case SimulatorError.FromVmError(InvalidSourceReference("heap", 1, UndefinedVariable)) => true
    }
  }

  "copy" should "fail &%undef > %y (oob)" in {
    failToAssembleOrExecute(
      main(body =
        s"""
           | .var x %0 ptr
           | copy &%${VmContext.maximumNumberOfVariablesInHeap} > %0
        """.stripMargin
      )
    ).thenVerify {
      case SimulatorError.FromVmError(InvalidSourceReference("heap", _, VariableIndexOutOfBounds(VmContext.maximumNumberOfVariablesInHeap))) => true
    }
  }

  "copy" should "pass &$x > %y" in {
    successfullyAssembleAndExecute(
      main(body =
        s"""
           | .push 1
           | .var x $$0 u32
           | .var y %0 ptr
           | copy &$$0 > %0
        """.stripMargin
      )
    ).thenVerify {
      case vmContext =>
        hasHeapVariableSetToStackPointer(("y", 0), 0, vmContext)
    }
  }

  "copy" should "fail &$undef > %y" in {
    failToAssembleOrExecute(
      main(body =
        s"""
           | .push 1
           | .var y %0 ptr
           | copy &$$0 > %0
        """.stripMargin
      )
    ).thenVerify {
      case SimulatorError.FromVmError(InvalidSourceReference("stack", 0, UndefinedVariable)) => true
    }
  }

  "copy" should "fail &$undef > %y (oob)" in {
    failToAssembleOrExecute(
      main(body =
        s"""
           | .push 1
           | .var y %0 ptr
           | copy &$$1 > %0
        """.stripMargin
      )
    ).thenVerify {
      case SimulatorError.FromVmError(InvalidSourceReference("stack", _, VariableIndexOutOfBounds(1))) => true
    }
  }

  "copy" should "pass &@x > %y" in {
    successfullyAssembleAndExecute(
      main(body =
        s"""
           |   .var ptr %0 ptr
           |   copy &@main > %0
        """.stripMargin
      )
    ).thenVerify {
      case vmContext =>
        hasHeapVariableSetToInstructionPointer("main", 0, vmContext)
    }
  }

  "copy" should "fail &@undef > %y" in {
    failToAssembleOrExecute(
      main(body =
        s"""
           |   .var ptr %0 ptr
           |   copy &@none > %0
        """.stripMargin
      )
    ).thenVerify {
      case SimulatorError.FromVmError(InvalidBasicBlockReference("none")) => true
    }
  }

  "copy" should "pass **%x > %y" in {
    successfullyAssembleAndExecute(
      main(body =
        s"""
           | .var x   %0 u32
           | .var px  %1 ptr
           | .var ppx %2 ptr
           | .var y   %3 u32
           | copy (feedcafe:u32) > %0
           | copy &%0 > %1
           | copy &%1 > %2
           | copy **%2 > %3
        """.stripMargin
      )
    ).thenVerify {
      case vmContext =>
        hasHeapVariableSetToImmediate(("y", 0xfeedcafe), 3, vmContext)
    }
  }

  "copy" should "fail *%undef > %y" in {
    failToAssembleOrExecute(
      main(body =
        s"""
           | .var x   %0 u32
           | .var y   %3 u32
           | copy *%2 > %3
        """.stripMargin
      )
    ).thenVerify {
      case SimulatorError.FromVmError(InvalidSourceReference("heap", 2, UndefinedVariable)) => true
    }
  }

  "copy" should "fail *%undef > %y (oob)" in {
    failToAssembleOrExecute(
      main(body =
        s"""
           | .var x   %0 u32
           | .var y   %3 u32
           | copy *%${VmContext.maximumNumberOfVariablesInHeap} > %3
        """.stripMargin
      )
    ).thenVerify {
      case SimulatorError.FromVmError(InvalidSourceReference("heap", VmContext.maximumNumberOfVariablesInHeap, VariableIndexOutOfBounds(VmContext.maximumNumberOfVariablesInHeap))) => true
    }
  }

  "copy" should "fail *%undef > %y (pointer type)" in {
    failToAssembleOrExecute(
      main(body =
        s"""
           | .var x %0 u32
           | .var ptr %1 ptr
           | copy &@main > %1
           | copy *%1 > %0
        """.stripMargin
      )
    ).thenVerify {
      case SimulatorError.FromVmError(InvalidIndirect("ptr")) => true
    }
  }

  "copy" should "pass **$x > %y" in {
    successfullyAssembleAndExecute(
      main(body =
        s"""
           | .var x   %0 u32
           | .var y   %3 u32
           | .push 2
           | .var px  $$0 ptr
           | .var ppx $$1 ptr
           | copy (feedcafe:u32) > %0
           | copy &%0  > $$0
           | copy &$$0 > $$1
           | copy **$$1 > %3
        """.stripMargin
      )
    ).thenVerify {
      case vmContext =>
        hasHeapVariableSetToImmediate(("y", 0xfeedcafe), 3, vmContext)
    }
  }

  "copy" should "fail *$undef > %y" in {
    failToAssembleOrExecute(
      main(body =
        s"""
           | .var x %0 u32
           | .push 1
           | copy *$$0 > %0
        """.stripMargin
      )
    ).thenVerify {
      case SimulatorError.FromVmError(InvalidSourceReference("stack", 0, UndefinedVariable)) => true
    }
  }

  "copy" should "fail *$undef > %y (oob)" in {
    failToAssembleOrExecute(
      main(body =
        s"""
           | .var x %0 u32
           | .push 1
           | copy *$$1 > %0
        """.stripMargin
      )
    ).thenVerify {
      case SimulatorError.FromVmError(InvalidSourceReference("stack", 1, VariableIndexOutOfBounds(1))) => true
    }
  }

  "copy" should "fail *$undef > %y (empty stack)" in {
    failToAssembleOrExecute(
      main(body =
        s"""
           |   .var x %0 u32
           |   copy *$$0 > %0
        """.stripMargin
      )
    ).thenVerify {
      case SimulatorError.FromVmError(InvalidSourceReference("stack", 0, EmptyStackAccess(_))) => true
    }
  }

  "copy" should "fail *$undef > %y (pointer type)" in {
    failToAssembleOrExecute(
      main(body =
        s"""
           | .push 1
           | .var x %0 u32
           | .var ptr $$0 ptr
           | copy &@main > $$0
           | copy *$$0 > %0
        """.stripMargin
      )
    ).thenVerify {
      case SimulatorError.FromVmError(InvalidIndirect("ptr")) => true
    }
  }

  "copy" should "pass %x[y] > %z" in {
    successfullyAssembleAndExecute(
      main(body =
        s"""
           | .var y  %0 u32
           | .var x0 %1 u32
           | .var x1 %2 u32
           | copy (cafefade:u32) > %2
           | copy %1[1] > %0
        """.stripMargin
      )
    ).thenVerify {
      case vmContext =>
        hasHeapVariableSetToImmediate(("y", 0xcafefade), 0, vmContext)
    }
  }

  "copy" should "fail %undef[y] > %z" in {
    failToAssembleOrExecute(
      main(body =
        s"""
           | .var y  %0 u32
           | .var x1 %2 u32
           | copy (cafefade:u32) > %2
           | copy %1[1] > %0
        """.stripMargin
      )
    ).thenVerify {
      case SimulatorError.FromVmError(InvalidSourceReference("heap", 1, UndefinedVariable)) => true
    }
  }

  "copy" should "fail %x[y] > %z (undef)" in {
    failToAssembleOrExecute(
      main(body =
        s"""
           | .var y  %0 u32
           | .var x0 %1 u32
           | copy (cafefade:u32) > %1
           | copy %1[1] > %0
        """.stripMargin
      )
    ).thenVerify {
      case SimulatorError.FromVmError(InvalidSourceReference("heap", 2, UndefinedVariable)) => true
    }
  }

  "copy" should "fail %undef[y] > %z (oob)" in {
    val base = VmContext.maximumNumberOfVariablesInHeap - 3
    failToAssembleOrExecute(
      main(body =
        s"""
           | .var y  %0 u32
           | .var x1 %$base u32
           | copy (cafefade:u32) > %$base
           | copy %$base[3] > %0
        """.stripMargin
      )
    ).thenVerify {
      case SimulatorError.FromVmError(InvalidSourceReference("heap", VmContext.maximumNumberOfVariablesInHeap, VariableIndexOutOfBounds(VmContext.maximumNumberOfVariablesInHeap))) => true
    }
  }

  "copy" should "pass $x[y] > %z" in {
    successfullyAssembleAndExecute(
      main(body =
        s"""
           | .var y %0 u32
           | .push 2
           | .var x0 $$0 u32
           | .var x1 $$1 u32
           | copy (cafefade:u32) > $$1
           | copy $$0[1] > %0
        """.stripMargin
      )
    ).thenVerify {
      case vmContext =>
        hasHeapVariableSetToImmediate(("y", 0xcafefade), 0, vmContext)
    }
  }

  "copy" should "fail $undef[y] > %z" in {
    failToAssembleOrExecute(
      main(body =
        s"""
           | .var y %0 u32
           | .push 2
           | .var x1 $$1 u32
           | copy (cafefade:u32) > $$1
           | copy $$0[1] > %0
        """.stripMargin
      )
    ).thenVerify {
      case SimulatorError.FromVmError(InvalidSourceReference("stack", 0, UndefinedVariable)) => true
    }
  }

  "copy" should "fail $x[y] > %z (undef)" in {
    failToAssembleOrExecute(
      main(body =
        s"""
           | .var y %0 u32
           | .push 2
           | .var x0 $$0 u32
           | copy $$0[1] > %0
        """.stripMargin
      )
    ).thenVerify {
      case SimulatorError.FromVmError(InvalidSourceReference("stack", 1, UndefinedVariable)) => true
    }
  }

  "copy" should "fail $x[y] > %z (empty stack)" in {
    failToAssembleOrExecute(
      main(body =
        s"""
           | .var y %0 u32
           | copy $$0[1] > %0
        """.stripMargin
      )
    ).thenVerify {
      case SimulatorError.FromVmError(InvalidSourceReference("stack", 0, EmptyStackAccess(_))) => true
    }
  }

  "copy" should "fail $x[y] > %z (oob)" in {
    failToAssembleOrExecute(
      main(body =
        s"""
           | .var y %0 u32
           | .push 1
           | .var x $$0 u32
           | copy $$0[1] > %0
        """.stripMargin
      )
    ).thenVerify {
      case SimulatorError.FromVmError(InvalidSourceReference("stack", 1, VariableIndexOutOfBounds(1))) => true
    }
  }

  private def hasHeapVariableSetToImmediate(nameValue: (String, Int), variableIndex: Int, vmContext: VmContext): Boolean =
    testVariable(vmContext.heap, variableIndex)(nameValue._1, nameValue._2) {
      case Variable.Scalar(name, value) => (name, value.toInt)
    }

  private def hasStackVariableSetToImmediate(nameValue: (String, Int), variableIndex: Int, vmContext: VmContext): Boolean =
    testVariable(vmContext.stack, variableIndex)(nameValue._1, nameValue._2) {
      case Variable.Scalar(name, value) => (name, value.toInt)
    }

  private def hasHeapVariableSetToHeapPointer(nameValue: (String, Int), variableIndex: Int, vmContext: VmContext): Boolean =
    testVariable(vmContext.heap, variableIndex)(nameValue._1, nameValue._2) {
      case Variable.Pointer.ToVariable.InTheHeap(name, value) => (name, value)
    }

  private def hasHeapVariableSetToStackPointer(nameValue: (String, Int), variableIndex: Int, vmContext: VmContext): Boolean =
    testVariable(vmContext.heap, variableIndex)(nameValue._1, nameValue._2) {
      case Variable.Pointer.ToVariable.InTheStack(name, value) => (name, value)
    }

  private def hasStackVariableSetToStackPointer(nameValue: (String, Int), variableIndex: Int, vmContext: VmContext): Boolean =
    testVariable(vmContext.stack, variableIndex)(nameValue._1, nameValue._2) {
      case Variable.Pointer.ToVariable.InTheStack(name, value) => (name, value)
    }

  private def hasHeapVariableSetToInstructionPointer(basicBlockName: String, variableIndex: Int, vmContext: VmContext): Boolean =
    vmContext.heap.getVariable(variableIndex) match {
      case Ok(Some(Variable.Pointer.ToInstruction(_, counter))) =>
        counter.basicBlock.isDefined && (counter.basicBlock.get.name == basicBlockName)

      case _ =>
        false
    }

  private def testVariable(fromVarSet: VarSet, atIndex: Int)
                          (expectedVariableName: String, expectedVariableIntValue: Int)
                          (getVarSpecIfOk: PartialFunction[Variable, (String, Int)]): Boolean =
    fromVarSet.getVariable(atIndex) match {
      case Ok(Some(variable)) =>
        getVarSpecIfOk
          .andThen(
            nameAndValue =>
              (expectedVariableName == nameAndValue._1) &&
                (expectedVariableIntValue == nameAndValue._2)
          )(variable)

      case _ =>
        false
    }
}
