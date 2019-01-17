/*-
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
import me.rand.vm.main.VmError.SyntaxError.NoMatchingProfile

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

  // TODO: heap, stack pointers, valid and with invalid result.

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
