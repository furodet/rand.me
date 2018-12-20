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
import me.rand.vm.engine.VmContext
import me.rand.vm.main.VmError
import me.rand.vm.main.VmError.VmContextError

class StackDirectiveSpec extends BaseSpec {
  ".push" should "fail if no size is specified" in {
    failToAssembleOrExecute(
      s"""
         | $aStandardMachineConfiguration
         | .bb main
         | .push
      """.stripMargin
    ).thenVerify {
      case SimulatorError.FromAsmError(AsmError.AsmParserError.InvalidDirectiveSpecification(".push", 4)) => true
    }
  }

  ".push" should "fail with an invalid size specification" in {
    failToAssembleOrExecute(
      s"""
         | $aStandardMachineConfiguration
         | .bb main
         | .push -1
      """.stripMargin
    ).thenVerify {
      case SimulatorError.FromAsmError(AsmError.AsmParserError.InvalidDirectiveSpecification(".push", 4)) => true
    }
  }

  ".push" should "fail if not within a basic block" in {
    failToAssembleOrExecute(
      s"""
         | $aStandardMachineConfiguration
         | .push 10
       """.stripMargin
    ).thenVerify {
      case SimulatorError.FromAsmError(AsmError.AsmProgramBuilderError.NoBasicBlockDeclared(3)) => true
    }
  }

  ".pop" should "fail if not within a basic block" in {
    failToAssembleOrExecute(
      s"""
         | $aStandardMachineConfiguration
         | .pop
       """.stripMargin
    ).thenVerify {
      case SimulatorError.FromAsmError(AsmError.AsmProgramBuilderError.NoBasicBlockDeclared(3)) => true
    }
  }

  ".pop" should "fail if no frame was previously pushed" in {
    failToAssembleOrExecute(
      s"""
         | $aStandardMachineConfiguration
         | .bb main
         | .pop
         | .boot main
       """.stripMargin
    ).thenVerify {
      case SimulatorError.FromVmError(VmError.VmContextError.EmptyStackAccess("pop")) => true
    }
  }

  ".push" should "properly create one frame" in {
    successfullyAssembleAndExecute(
      s"""
         | $aStandardMachineConfiguration
         | .bb main
         | .push 10
         | exit (00:u8)
         | .boot main
      """.stripMargin
    ).thenVerify {
      case vmContext =>
        stackSizeIs(1, vmContext) && topFrameSizeIs(10, vmContext)
    }
  }

  ".push" should "allow to push an empty frame" in {
    successfullyAssembleAndExecute(
      s"""
         | $aStandardMachineConfiguration
         | .bb main
         | .push 0
         | exit (00:u8)
         | .boot main
       """.stripMargin
    ).thenVerify {
      case vmContext =>
        stackSizeIs(1, vmContext) && topFrameSizeIs(0, vmContext)
    }
  }

  "a program" should "be able to push push then pop" in {
    successfullyAssembleAndExecute(
      s"""
         | $aStandardMachineConfiguration
         | .bb main
         | .push 1
         | .push 2
         | .pop
         | exit (00:u8)
         | .boot main
      """.stripMargin
    ).thenVerify {
      case vmContext =>
        stackSizeIs(1, vmContext) && topFrameSizeIs(1, vmContext)
    }
  }

  private def stackSizeIs(nrFrames: Int, vmContext: VmContext): Boolean =
    vmContext.stack.frames.size == nrFrames

  private def topFrameSizeIs(nrVariables: Int, context: VmContext): Boolean = {
    val top = context.stack.frames.head.vars
    top.getVariable(nrVariables) match {
      case Err(VmContextError.VariableIndexOutOfBounds(n)) if n == nrVariables =>
        for (eachVarIndex <- 0 until nrVariables) {
          top.getVariable(eachVarIndex) match {
            case Ok(None) =>
            // ok
            case _ =>
              return false
          }
        }
        true
      case _ =>
        false
    }
  }
}
