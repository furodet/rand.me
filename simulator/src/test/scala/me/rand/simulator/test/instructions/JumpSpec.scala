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
import me.rand.vm.main.VmError.SyntaxError.NoMatchingProfile

class JumpSpec extends BaseSpec {
  "jump" should "pass &@..." in {
    successfullyAssembleAndExecute(
      main(body =
        """
          | .var x %0 u32
          | copy (ffffffff:u32) > %0
          | jump &@foo
          | exit (01:u8)
          | .bb foo
          |   copy (9abcdef0:u32) > %0
          |   exit (02:u8)
        """.stripMargin
      )
    ).thenVerify {
      case vmContext =>
        hasHeapVariable(0, 0x9abcdef0, vmContext) &&
          (vmContext.exitCode.getOrElse(-1) == 2)
    }
  }

  "jump" should "fail %x (scalar)" in {
    failToAssembleOrExecute(
      main(body =
        """
          | .var x %0 u32
          | copy (9abcdef0:u32) > %0
          | jump %0
        """.stripMargin
      )
    ).thenVerify {
      case SimulatorError.FromVmError(NoMatchingProfile("jump", _)) => true
    }
  }

  "jump" should "fail %x (heap pointer)" in {
    failToAssembleOrExecute(
      main(body =
        """
          | .var x %0 ptr
          | copy &%0 > %0
          | jump %0
        """.stripMargin
      )
    ).thenVerify {
      case SimulatorError.FromVmError(NoMatchingProfile("jump", _)) => true
    }
  }

  "jump" should "fail %x (stack pointer)" in {
    failToAssembleOrExecute(
      main(body =
        """
          | .push 1
          | .var x $0 ptr
          | copy &$0 > $0
          | jump $0
        """.stripMargin
      )
    ).thenVerify {
      case SimulatorError.FromVmError(NoMatchingProfile("jump", _)) => true
    }
  }
}
