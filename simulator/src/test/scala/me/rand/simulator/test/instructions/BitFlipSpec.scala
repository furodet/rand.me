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

import me.rand.simulator.main.SimulatorError
import me.rand.simulator.test.BaseSpec
import me.rand.vm.main.VmError.SyntaxError.NoMatchingProfile

class BitFlipSpec extends BaseSpec {
  "bitflip" should "pass ~%imm > _" in {
    successfullyAssembleAndExecute(
      main(body = "~ (aaaaaaaa:u32)")
    ).thenVerify {
      case _ => true
    }
  }

  "bitflip" should "pass ~%imm > %x" in {
    successfullyAssembleAndExecute(
      main(body =
        """
          | .var x %0 u32
          | ~ (aaaaaaaa:u32) > %0
        """.stripMargin
      )
    ).thenVerify {
      case vmContext =>
        hasHeapVariable(0, ~0xaaaaaaaa, vmContext)
    }
  }

  "bitflip" should "pass %x > %y" in {
    successfullyAssembleAndExecute(
      main(body =
        """
          | .var x %0 u32
          | .var y %1 u32
          | copy (87654321:u32) > %0
          | ~ %0 > %1
        """.stripMargin
      )
    ).thenVerify {
      case vmContext =>
        hasHeapVariable(1, ~0x87654321, vmContext)
    }
  }

  "bitflip" should "pass $x > %y" in {
    successfullyAssembleAndExecute(
      main(body =
        """
          | .push 1
          | .var x $0 u32
          | .var y %1 u32
          | copy (87654321:u32) > $0
          | ~ $0 > %1
        """.stripMargin
      )
    ).thenVerify {
      case vmContext =>
        hasHeapVariable(1, ~0x87654321, vmContext)
    }
  }

  "bitflip" should "fail %x > _ (heap pointer)" in {
    failToAssembleOrExecute(
      main(body =
        """
          | .var x %0 ptr
          | copy &%0 > %0
          | ~ %0 > _
        """.stripMargin
      )
    ).thenVerify {
      case SimulatorError.FromVmError(NoMatchingProfile("~", _)) => true
    }
  }

  "bitflip" should "fail %x > _ (stack pointer)" in {
    failToAssembleOrExecute(
      main(body =
        """
          | .push 1
          | .var x $0 ptr
          | copy &$0 > $0
          | ~ $0 > _
        """.stripMargin
      )
    ).thenVerify {
      case SimulatorError.FromVmError(NoMatchingProfile("~", _)) => true
    }
  }

  "bitflip" should "fail %x > _ (instruction pointer)" in {
    failToAssembleOrExecute(
      main(body =
        """
          | .var x %0 ptr
          | copy &@main > %0
          | ~ %0 > _
        """.stripMargin
      )
    ).thenVerify {
      case SimulatorError.FromVmError(NoMatchingProfile("~", _)) => true
    }
  }
}
