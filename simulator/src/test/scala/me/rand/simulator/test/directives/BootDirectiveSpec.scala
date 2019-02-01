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
package me.rand.simulator.test.directives

import me.rand.asm.main.AsmError
import me.rand.simulator.main.SimulatorError
import me.rand.simulator.test.BaseSpec
import me.rand.vm.engine.VmRunState

class BootDirectiveSpec extends BaseSpec {
  "a program" should "fail if no bootstrap is defined" in {
    failToAssembleOrExecute(
      s"""
         | $aStandardMachineConfiguration
         | .bb block0
         |   exit (00:u8)
      """.stripMargin
    ).thenVerify {
      case SimulatorError.FromAsmError(AsmError.AsmProgramBuilderError.UndefinedBootstrapBlock) => true
    }
  }

  ".boot" should "fail if no basic block is provided" in {
    failToAssembleOrExecute(
      s"""
         | $aStandardMachineConfiguration
         | .boot
      """.stripMargin
    ).thenVerify {
      case SimulatorError.FromAsmError(AsmError.AsmParserError.InvalidDirectiveSpecification(".boot", 3)) => true
    }
  }

  ".boot" should "fail if multiple bootstrap blocks are defined" in {
    failToAssembleOrExecute(
      s"""
         | $aStandardMachineConfiguration
         | .bb block0
         |   exit (00:u8)
         | .bb block1
         |   exit (01:u8)
         | .boot block0
         | .boot block1
      """.stripMargin
    ).thenVerify {
      case SimulatorError.FromAsmError(AsmError.AsmProgramBuilderError.DuplicateBootstrapDefinition(8)) => true
    }
  }

  ".boot" should "be specified at any point of the program" in {
    successfullyAssembleAndExecute(
      s"""
         | $aStandardMachineConfiguration
         | .boot block0
         | .bb block0
         |   exit (42:u8)
       """.stripMargin
    ).thenVerify {
      case vmContext =>
        vmContext.state match {
          case VmRunState.Stopped(0x42) =>
            true

          case _ => false
        }
    }
  }

  ".boot" should "fail if specified block does not exist" in {
    failToAssembleOrExecute(
      s"""
         | $aStandardMachineConfiguration
         | .boot iDontExist
         | .bb iDoExist
         |   exit (00:u8)
      """.stripMargin
    ).thenVerify {
      case SimulatorError.FromAsmError(AsmError.AsmProgramBuilderError.NoSuchBootstrapBlock("iDontExist", 3)) => true
    }
  }
}
