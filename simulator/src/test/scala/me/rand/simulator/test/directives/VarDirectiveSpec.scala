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
package me.rand.simulator.test.directives

import me.rand.asm.main.AsmError
import me.rand.commons.idioms.Status._
import me.rand.simulator.main.SimulatorError
import me.rand.simulator.test.BaseSpec
import me.rand.vm.engine.{Variable, VmContext}
import me.rand.vm.main.VmError

class VarDirectiveSpec extends BaseSpec {
  ".var" should "fail with less than 3 arguments" in {
    failToAssembleOrExecute(
      s"""
         | $aStandardMachineConfiguration
         | .var hello %0
      """.stripMargin
    ).thenVerify {
      case SimulatorError.FromAsmError(AsmError.AsmParserError.InvalidVariableSpecification("hello %0", 3)) => true
    }
  }

  ".var" should "fail with an invalid variable identifier" in {
    failToAssembleOrExecute(
      s"""
         | $aStandardMachineConfiguration
         | .var hello 3 (00:u8)
       """.stripMargin
    ).thenVerify {
      case SimulatorError.FromAsmError(AsmError.AsmParserError.InvalidVariableSpecification("hello 3 (00:u8)", 3)) => true
    }
  }

  ".var" should "fail with an invalid value specification" in {
    failToAssembleOrExecute(
      s"""
         | $aStandardMachineConfiguration
         | .var hello %3 (00)
       """.stripMargin
    ).thenVerify {
      case SimulatorError.FromAsmError(AsmError.AsmParserError.InvalidVariableSpecification("hello %3 (00)", 3)) => true
    }
  }

  ".var" should "fail if not within a basic block" in {
    failToAssembleOrExecute(
      s"""
         | $aStandardMachineConfiguration
         | .var hello %3 u8
       """.stripMargin
    ).thenVerify {
      case SimulatorError.FromAsmError(AsmError.AsmProgramBuilderError.NoBasicBlockDeclared(3)) => true
    }
  }

  ".var" should "fail if creating a variable outside heap limits" in {
    failToAssembleOrExecute(
      s"""
         | $aStandardMachineConfiguration
         | .bb main
         |   .var hello %${VmContext.maximumNumberOfVariablesInHeap} u8
         | .boot main
       """.stripMargin
    ).thenVerify {
      case SimulatorError.FromVmError(VmError.VmContextError.VariableIndexOutOfBounds(VmContext.maximumNumberOfVariablesInHeap)) => true
    }
  }

  ".var" should "allow to declare heap variables initialized to 0" in {
    successfullyAssembleAndExecute(
      s"""
         | $aStandardMachineConfiguration
         | .bb main
         |   .var top %${VmContext.maximumNumberOfVariablesInHeap - 1} s8
         |   .var bottom %0 u8
         |   exit (00:u8)
         | .boot main
     """.stripMargin
    ).thenVerify {
      case vmContext =>
        heapVariableMatches(VmContext.maximumNumberOfVariablesInHeap - 1, "top", isSigned = true, byteLen = 1, intValue = 0, vmContext) &&
          heapVariableMatches(0, "bottom", isSigned = false, byteLen = 1, intValue = 0, vmContext) &&
          everyHeapVariableIsUnsetInRange(1, VmContext.maximumNumberOfVariablesInHeap - 1, vmContext)
    }
  }

  ".var" should "allow to declare heap variables initialized to nullptr" in {
    successfullyAssembleAndExecute(
      s"""
         | $aStandardMachineConfiguration
         | .bb main
         |   .var top %${VmContext.maximumNumberOfVariablesInHeap - 1} ptr
         |   .var bottom %0 ptr
         |   exit (00:u8)
         | .boot main
     """.stripMargin
    ).thenVerify {
      case vmContext =>
        heapVariableMatchesPointer(VmContext.maximumNumberOfVariablesInHeap - 1, "top", vmContext) &&
          heapVariableMatchesPointer(0, "bottom", vmContext) &&
          everyHeapVariableIsUnsetInRange(1, VmContext.maximumNumberOfVariablesInHeap - 1, vmContext)
    }
  }

  ".var" should "allow to declare stack variables initialized to 0" in {
    successfullyAssembleAndExecute(
      s"""
         | $aStandardMachineConfiguration
         | .bb main
         |   .push 1
         |   .var x $$0 s8
         |   exit (00:u8)
         | .boot main
     """.stripMargin
    ).thenVerify {
      case vmContext =>
        stackVariableMatches(0, "x", isSigned = true, byteLen = 1, intValue = 0, vmContext)
    }
  }

  ".var" should "allow to declare stack variables initialized to nullptr" in {
    successfullyAssembleAndExecute(
      s"""
         | $aStandardMachineConfiguration
         | .bb main
         |   .push 1
         |   .var x $$0 ptr
         |   exit (00:u8)
         | .boot main
     """.stripMargin
    ).thenVerify {
      case vmContext =>
        stackVariableMatchesPointer(0, "x", vmContext)
    }
  }

  private def heapVariableMatches(heapIndex: Int, name: String, isSigned: Boolean, byteLen: Int, intValue: Int, vmContext: VmContext): Boolean = {
    vmContext.heap.getVariable(heapIndex) match {
      case Ok(Some(x: Variable.Scalar)) =>
        (name == x.name) &&
          (isSigned == x.value.vmType.isSigned) &&
          (byteLen == x.value.vmType.byteLen) &&
          (intValue == x.value.toInt)

      case _ =>
        false
    }
  }

  private def stackVariableMatches(stackIndex: Int, name: String, isSigned: Boolean, byteLen: Int, intValue: Int, vmContext: VmContext): Boolean = {
    vmContext.stack.getVariable(stackIndex) match {
      case Ok(Some(x: Variable.Scalar)) =>
        (name == x.name) &&
          (isSigned == x.value.vmType.isSigned) &&
          (byteLen == x.value.vmType.byteLen) &&
          (intValue == x.value.toInt)

      case _ =>
        false
    }
  }

  private def heapVariableMatchesPointer(heapIndex: Int, name: String, vmContext: VmContext): Boolean = {
    vmContext.heap.getVariable(heapIndex) match {
      case Ok(Some(x: Variable.Pointer)) =>
        name == x.name

      case _ =>
        false
    }
  }

  private def stackVariableMatchesPointer(stackIndex: Int, name: String, vmContext: VmContext): Boolean = {
    vmContext.stack.getVariable(stackIndex) match {
      case Ok(Some(x: Variable.Pointer)) =>
        name == x.name

      case _ =>
        false
    }
  }

  private def everyHeapVariableIsUnsetInRange(from: Int, to: Int, vmContext: VmContext): Boolean = {
    for (eachHeapIndex <- from until to) {
      vmContext.heap.getVariable(eachHeapIndex) match {
        case Ok(None) =>
        // ok
        case _ =>
          return false
      }
    }
    true
  }
}
