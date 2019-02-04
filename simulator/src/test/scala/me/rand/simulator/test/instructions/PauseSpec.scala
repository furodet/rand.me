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
import me.rand.vm.engine.{Variable, VmContext, VmRunState}
import me.rand.vm.main.VmError

class PauseSpec extends BaseSpec {
  "pause" should "pass and leave the VM in paused state" in {
    successfullyAssembleAndExecute(
      s"""
         | $aStandardMachineConfiguration
         | .bb main
         |   pause
         | .boot main
       """.stripMargin
    ).thenVerify {
      case vmContext: VmContext =>
        vmIsPaused("main", 0, vmContext)
    }
  }

  "exit" should "fail %x" in {
    failToAssembleOrExecute(
      s"""
         | $aStandardMachineConfiguration
         | .bb main
         |   .var v %0 u16
         |   copy (abcd:u16) > %0
         |   pause %0
         | .boot main
       """.stripMargin
    ).thenVerify {
      case SimulatorError.FromVmError(VmError.SyntaxError.NoMatchingProfile("pause", _)) => true
    }
  }

  "pause" should "fail &%x" in {
    failToAssembleOrExecute(
      s"""
         | $aStandardMachineConfiguration
         | .bb main
         |   .var v %0 u16
         |   pause &%0
         | .boot main
       """.stripMargin
    ).thenVerify {
      case SimulatorError.FromVmError(VmError.SyntaxError.NoMatchingProfile("pause", _)) => true
    }
  }

  "pause" should "pass > %x (%x not updated)" in {
    successfullyAssembleAndExecute(
      s"""
         | $aStandardMachineConfiguration
         | .bb main
         |   .var dummy %0 u8
         |   copy (ab:u8) > %0
         |   pause > %0
         | .boot main
       """.stripMargin
    ).thenVerify {
      case vmContext =>
        vmIsPaused("main", 2, vmContext) && (vmContext.heap.getVariable(0) match {
          case Ok(Some(Variable.Scalar(_, value))) if value.toInt == 0xab =>
            true
          case _ =>
            false
        })
    }
  }

  private def vmIsPaused(expectedBlockName: String, expectedPcIndex: Int, vmContext: VmContext): Boolean =
    vmContext.state match {
      case VmRunState.Paused(pc) =>
        pc.basicBlock.isDefined && (pc.basicBlock.get.name == expectedBlockName) && (pc.index == expectedPcIndex)

      case _ => false
    }
}
