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
package me.rand.simulator.test.instructions

import me.rand.commons.idioms.Status._
import me.rand.simulator.main.SimulatorError
import me.rand.simulator.test.BaseSpec
import me.rand.vm.engine.{Variable, VmContext}
import me.rand.vm.main.VmError

class ExitSpec extends BaseSpec {
  "a program" should "fail if not ending with an exit" in {
    failToAssembleOrExecute(
      s"""
         | $aStandardMachineConfiguration
         | .bb main
         |   .var dummy %0 (abcd:u16)
         | .boot main
      """.stripMargin
    ).thenVerify {
      case SimulatorError.FromVmError(VmError.VmContextError.ProgramCounterOutOfBounds(1)) => true
    }
  }

  "exit" should "stop the machine with an immediate exit code" in {
    successfullyAssembleAndExecute(
      s"""
         | $aStandardMachineConfiguration
         | .bb main
         |   exit (ffff:s32)
         | .boot main
       """.stripMargin
    ).thenVerify {
      case vmContext: VmContext =>
        vmExitedWithCode(-1, vmContext)
    }
  }

  "exit" should "stop the machine with a variable value exit code" in {
    successfullyAssembleAndExecute(
      s"""
         | $aStandardMachineConfiguration
         | .bb main
         |   .var v %0 (abcd:u16)
         |   exit %0
         | .boot main
       """.stripMargin
    ).thenVerify {
      case vmContext =>
        vmExitedWithCode(0xabcd, vmContext)
    }
  }

  "exit" should "fail with pointer variable" in {
    failToAssembleOrExecute(
      s"""
         | $aStandardMachineConfiguration
         | .bb main
         |   .var v %0 (da5f:u16)
         |   exit &%0
         | .boot main
       """.stripMargin
    ).thenVerify {
      case SimulatorError.FromVmError(VmError.SyntaxError.NoMatchingProfile("exit", _)) => true
    }
  }

  "exit" should "not write into output operand if defined" in {
    successfullyAssembleAndExecute(
      s"""
         | $aStandardMachineConfiguration
         | .bb main
         |   .var dummy %0 (abcd:u16)
         |   exit (00:u8) > %0
         | .boot main
       """.stripMargin
    ).thenVerify {
      case vmContext =>
        vmExitedWithCode(0, vmContext) && (vmContext.heap.getVariable(0) match {
          case Ok(Some(Variable.Scalar(_, value))) if value.toInt == 0xabcd =>
            true
          case _ =>
            false
        })
    }
  }

  private def vmExitedWithCode(code: Int, vmContext: VmContext): Boolean =
    vmContext.exitCode match {
      case Some(value) if value == code =>
        true

      case _ =>
        false
    }

}
