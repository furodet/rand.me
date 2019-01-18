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

import me.rand.simulator.main.SimulatorError
import me.rand.simulator.test.BaseSpec
import me.rand.vm.engine.Variable
import me.rand.vm.main.VmError.SyntaxError.NoMatchingProfile
import me.rand.vm.main.VmError.VmExecutionError.VmFetchOperandError.InvalidPointerValue.InvalidTargetReference

class DecrementSpec extends BaseSpec {
  "--" should "pass %x > _" in {
    successfullyAssembleAndExecute(
      main(body =
        """
          | .var x %0 u8
          | copy (ff:u8) > %0
          | -- %0 > _
        """.stripMargin
      )
    ).thenVerify {
      case vmContext =>
        // x should be unchanged
        hasHeapVariable(0, 255, vmContext)
    }
  }

  "--" should "pass %x > %y" in {
    successfullyAssembleAndExecute(
      main(body =
        """
          | .var xu8 %0 u32
          | .var xs8 %1 s32
          | copy (ffffffff:u32) > %0
          | copy (00000000:s32) > %1
          | -- %0 > %0
          | -- %1 > %1
        """.stripMargin
      )
    ).thenVerify {
      case vmContext =>
        hasHeapVariable(0, -2, vmContext) &&
          hasHeapVariable(1, -1, vmContext)
    }
  }

  "--" should "pass %x > %y (heap pointer)" in {
    successfullyAssembleAndExecute(
      main(body =
        """
          | .var x %0 s32
          | .var px %1 ptr
          | copy &%1 > %1
          | -- %1 > %1
        """.stripMargin
      )
    ).thenVerify {
      case vmContext =>
        hasHeapVariable(1, Variable.Pointer.ToVariable.InTheHeap("x", 0), vmContext)
    }
  }

  "--" should "fail %x > %y (invalid heap pointer)" in {
    failToAssembleOrExecute(
      main(body =
        """
          | .var x0 %1 s32
          | .var px %10 ptr
          | copy &%1 > %10
          | -- %10 > %10
        """.stripMargin
      )
    ).thenVerify {
      case SimulatorError.FromVmError(InvalidTargetReference(_, 0, None)) => true
    }
  }

  "--" should "pass %x > %y (stack pointer)" in {
    successfullyAssembleAndExecute(
      main(body =
        """
          | .push 2
          | .var x0 $0 s32
          | .var x1 $1 s32
          | .var px %0 ptr
          | copy &$1 > %0
          | -- %0 > %0
        """.stripMargin
      )
    ).thenVerify {
      case vmContext =>
        hasHeapVariable(0, Variable.Pointer.ToVariable.InTheStack("x0", 0), vmContext)
    }
  }

  "--" should "fail %x > %y (invalid stack pointer)" in {
    failToAssembleOrExecute(
      main(body =
        """
          | .push 2
          | .var x0 $1 s32
          | .var px %0 ptr
          | copy &$1 > %0
          | -- %0 > %0
        """.stripMargin
      )
    ).thenVerify {
      case SimulatorError.FromVmError(InvalidTargetReference(_, 0, None)) => true
    }
  }

  "--" should "fail %x > _ (instruction pointer)" in {
    failToAssembleOrExecute(
      main(body =
        """
          | .var p %0 ptr
          | copy &@main > %0
          | -- %0 > %0
        """.stripMargin
      )
    ).thenVerify {
      case SimulatorError.FromVmError(NoMatchingProfile("--", _)) => true
    }
  }
}
