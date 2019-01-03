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
import me.rand.simulator.main.SimulatorError
import me.rand.simulator.test.BaseSpec

class BasicBlockDirectiveSpec extends BaseSpec {
  ".bb" should "fail if no basic block name is provided" in {
    failToAssembleOrExecute(
      s"""
         | $aStandardMachineConfiguration
         | .bb
      """.stripMargin
    ).thenVerify {
      case SimulatorError.FromAsmError(AsmError.AsmParserError.InvalidDirectiveSpecification(".bb", 3)) => true
    }
  }

  ".bb" should "fail with an invalid basic block name" in {
    failToAssembleOrExecute(
      s"""
         | $aStandardMachineConfiguration
         | .bb 0
       """.stripMargin
    ).thenVerify {
      case SimulatorError.FromAsmError(AsmError.AsmParserError.InvalidDirectiveSpecification(".bb", 3)) => true
    }
  }

  "a program" should "expect a basic block to be defined before any instruction" in {
    failToAssembleOrExecute(
      s"""
         | $aStandardMachineConfiguration
         | exit (00:u8)
      """.stripMargin
    ).thenVerify {
      case SimulatorError.FromAsmError(AsmError.AsmProgramBuilderError.NoBasicBlockDeclared(3)) => true
    }
  }

  "a program" should "not allow duplicate basic block names" in {
    failToAssembleOrExecute(
      s"""
         | $aStandardMachineConfiguration
         | .bb bb0
         |   exit (42:u8)
         | .bb bb0
         |   exit (00:s8)
         | .boot bb0
      """.stripMargin
    ).thenVerify {
      case SimulatorError.FromAsmError(AsmError.AsmProgramBuilderError.DuplicateBasicBlockDefinition("bb0", 5)) => true
    }
  }
}
