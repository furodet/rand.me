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

class AddSpec extends BaseSpec {
  "+" should "pass %x %y > _" in {
    successfullyAssembleAndExecute(
      main(body =
        """
          | .var x %0 s32
          | .var y %1 s32
          | copy (00000000:s32) > %0
          | copy (00000001:s32) > %1
          | + %0 %1
        """.stripMargin
      )
    )
  }.thenVerify {
    case vmContext =>
      // Nothing changed, fruitless operation
      hasHeapVariable(0, 0, vmContext) && hasHeapVariable(1, 1, vmContext)
  }

  "+" should "pass %x %y > %z" in {
    successfullyAssembleAndExecute(
      main(body =
        """
          | .var x %0 u8
          | .var y %1 s8
          | .var z %2 u8
          | copy (01:u8) > %0
          | copy (ff:s8) > %1
          | + %0 %1 > %2
        """.stripMargin
      )
    ).thenVerify {
      case vmContext =>
        hasHeapVariable(2, 0, vmContext)
    }
  }

  "+" should "pass %x %y > %z (heap pointer)" in {
    successfullyAssembleAndExecute(
      main(body =
        """
          | .var x %0 ptr
          | .var y %1 u16
          | .var z %2 u16
          | copy (01:u8) > %1
          | copy &%1 > %0
          | + %0 %1 > %0
        """.stripMargin
      )
    ).thenVerify {
      case vmContex =>
        hasHeapVariable(0, Variable.Pointer.ToVariable.InTheHeap("z", 2), vmContex)
    }
  }

  "+" should "fail %x imm > %z (invalid heap pointer)" in {
    failToAssembleOrExecute(
      main(body =
        """
          | .var x %0 ptr
          | .var y %1 ptr
          | copy &%1 > %0
          | + %0 (01:u8) > %0
        """.stripMargin
      )
    ).thenVerify {
      case SimulatorError.FromVmError(InvalidTargetReference(_, 2, None)) => true
    }
  }

  "+" should "pass %x %y > %z (stack pointer)" in {
    successfullyAssembleAndExecute(
      main(
        // We will decrement the value, so need to precisely define the stack
        // pointer type, so that normalization works.
        optionalMachSpec =
          Some(
            """
              | .machptr stack u16
            """.stripMargin),
        body =
          """
            | .push 2
            | .var y $0 s16
            | .var z $1 s16
            | .var x %0 ptr
            | copy (ffff:s16) > $0
            | copy &$1 > %0
            | + %0 $0 > %0
          """.stripMargin
      )
    ).thenVerify {
      case vmContext =>
        hasHeapVariable(0, Variable.Pointer.ToVariable.InTheStack("y", 0), vmContext)
    }
  }

  "+" should "fail %x %y" in {
    failToAssembleOrExecute(
      main(body =
        """
          | .push 2
          | .var y $0 s16
          | .var x %0 ptr
          | copy &$0 > %0
          | + %0 (01:u8) > %0
        """.stripMargin
      )
    ).thenVerify {
      case SimulatorError.FromVmError(InvalidTargetReference(_, 1, None)) => true
    }
  }

  "+" should "fail %x %y > _ (instruction pointer)" in {
    failToAssembleOrExecute(
      main(body =
        """
          | .var x %0 ptr
          | .var y %1 s32
          | copy &@main > %0
          | copy (00000001:s32) > %1
          | + %0 %1
        """.stripMargin)
    ).thenVerify {
      case SimulatorError.FromVmError(NoMatchingProfile("+", _)) => true
    }
  }
}
