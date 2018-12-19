/*-
 * Copyright (c) 2018 rand.me project
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
    failureOfAssemblyOrExecutionOf(
      s"""
         | $aStandardMachineConfiguration
         | .var hello %0
      """.stripMargin
    ) {
      case SimulatorError.FromAsmError(AsmError.AsmParserError.InvalidVariableSpecification("hello %0", 3)) => true
    }
  }

  ".var" should "fail with an invalid variable identifier" in {
    failureOfAssemblyOrExecutionOf(
      s"""
         | $aStandardMachineConfiguration
         | .var hello 3 (00:u8)
       """.stripMargin
    ) {
      case SimulatorError.FromAsmError(AsmError.AsmParserError.InvalidVariableSpecification("hello 3 (00:u8)", 3)) => true
    }
  }

  ".var" should "fail with an invalid value specification" in {
    failureOfAssemblyOrExecutionOf(
      s"""
         | $aStandardMachineConfiguration
         | .var hello %3 (00)
       """.stripMargin
    ) {
      case SimulatorError.FromAsmError(AsmError.AsmParserError.InvalidVariableSpecification("hello %3 (00)", 3)) => true
    }
  }

  ".var" should "fail if not within a basic block" in {
    failureOfAssemblyOrExecutionOf(
      s"""
         | $aStandardMachineConfiguration
         | .var hello %3 (00:u8)
       """.stripMargin
    ) {
      case SimulatorError.FromAsmError(AsmError.AsmProgramBuilderError.NoBasicBlockDeclared(3)) => true
    }
  }

  ".var" should "fail if creating a variable outside heap limits" in {
    failureOfAssemblyOrExecutionOf(
      s"""
         | $aStandardMachineConfiguration
         | .bb main
         |   .var hello %${VmContext.maximumNumberOfVariablesInHeap} (00:u8)
         | .boot main
       """.stripMargin
    ) {
      case SimulatorError.FromVmError(VmError.VmContextError.VariableIndexOutOfBounds(VmContext.maximumNumberOfVariablesInHeap)) => true
    }
  }

  ".var" should "allow to create variables in the heap" in {
    successfullyAssembleAndExecute(
      s"""
         | $aStandardMachineConfiguration
         | .bb main
         |   .var top %${VmContext.maximumNumberOfVariablesInHeap - 1} (ff:s8)
         |   .var bottom %0 (00:u8)
         |   exit (00:u8)
         | .boot main
     """.stripMargin) {
      case vmContext =>
        heapVariableMatches(VmContext.maximumNumberOfVariablesInHeap - 1, "top", isSigned = true, byteLen = 1, intValue = -1, vmContext) &&
          heapVariableMatches(0, "bottom", isSigned = false, byteLen = 1, intValue = 0, vmContext) &&
          everyHeapVariableIsUnsetInRange(1, VmContext.maximumNumberOfVariablesInHeap - 1, vmContext)
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
