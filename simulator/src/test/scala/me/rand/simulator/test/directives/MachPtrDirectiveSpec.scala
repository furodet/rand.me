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

import me.rand.asm.main.AsmError.AsmParserError
import me.rand.simulator.main.SimulatorError
import me.rand.simulator.test.BaseSpec

class MachPtrDirectiveSpec extends BaseSpec {
  "default context" should "return max type for each pointer type" in {
    successfullyAssembleAndExecute(
      s"""
         | ${aMachDirectiveWithMachineWordLengthSetTo(8)}
         | .bb main
         |   exit (00:u8)
         | .boot main
      """.stripMargin
    ).thenVerify {
      case vmContext =>
        vmContext.profile.pointerTypes.toInstruction.isUnsigned &&
          vmContext.profile.pointerTypes.toInstruction.byteLen == 8 &&
          vmContext.profile.pointerTypes.toHeap.isUnsigned &&
          vmContext.profile.pointerTypes.toHeap.byteLen == 8 &&
          vmContext.profile.pointerTypes.toHeap.isUnsigned &&
          vmContext.profile.pointerTypes.toHeap.byteLen == 8
    }
  }

  ".machptr" should "fail if not provided a pointer type and a native type" in {
    failToAssembleOrExecute(
      s"""
         | .machptr foo
       """.stripMargin
    ).thenVerify {
      case SimulatorError.FromAsmError(AsmParserError.InvalidDirectiveSpecification(".machptr", 2)) => true
    }
  }

  ".machptr" should "fail if pointer category is not recognized" in {
    failToAssembleOrExecute(
      s"""
         | ${aMachDirectiveWithMachineWordLengthSetTo(8)}
         | .machptr something u16
       """.stripMargin
    ).thenVerify {
      case SimulatorError.FromAsmError(AsmParserError.InvalidPointerType("something", 3)) => true
    }
  }

  ".machptr" should "fail if native type is invalid" in {
    failToAssembleOrExecute(
      s"""
         | ${aMachDirectiveWithMachineWordLengthSetTo(8)}
         | .machptr instruction k2000
       """.stripMargin
    ).thenVerify {
      case SimulatorError.FromAsmError(AsmParserError.InvalidDirectiveSpecification(".machptr", 3)) => true
    }
  }

  ".machptr" should "pass instruction t" in {
    successfullyAssembleAndExecute(
      s"""
         | ${aMachDirectiveWithMachineWordLengthSetTo(8)}
         | .machptr instruction u8
         | .bb main
         |   exit (00:u8)
         | .boot main
       """.stripMargin
    ).thenVerify {
      case vmContext =>
        vmContext.profile.pointerTypes.toInstruction.isUnsigned &&
          vmContext.profile.pointerTypes.toInstruction.byteLen == 1
    }
  }

  ".machptr" should "pass heap t" in {
    successfullyAssembleAndExecute(
      s"""
         | ${aMachDirectiveWithMachineWordLengthSetTo(8)}
         | .machptr heap s8
         | .bb main
         |   exit (00:s8)
         | .boot main
       """.stripMargin
    ).thenVerify {
      case vmContext =>
        vmContext.profile.pointerTypes.toHeap.isSigned &&
          vmContext.profile.pointerTypes.toHeap.byteLen == 1
    }
  }

  ".machptr" should "pass stack t" in {
    successfullyAssembleAndExecute(
      s"""
         | ${aMachDirectiveWithMachineWordLengthSetTo(8)}
         | .machptr stack u32
         | .bb main
         |   exit (00:s8)
         | .boot main
       """.stripMargin
    ).thenVerify {
      case vmContext =>
        vmContext.profile.pointerTypes.toStack.isUnsigned &&
          vmContext.profile.pointerTypes.toStack.byteLen == 4
    }
  }
}
