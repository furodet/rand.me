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
package me.rand.simulator.test.directives

import me.rand.asm.main.AsmError.AsmParserError.{IncompatibleInstructionSet, InvalidDirectiveSpecification, InvalidMachineSpecification, UnspecifiedMachineProfile}
import me.rand.commons.idioms.Status._
import me.rand.simulator.main.SimulatorError
import me.rand.simulator.test.BaseSpec
import me.rand.vm.engine.VmTypes.VmType
import me.rand.vm.engine.{VmContext, VmTypes}
import me.rand.vm.main.VmError.{IncompatibleInstructionSetVersion, VmProfileStringError}

class MachDirectiveSpec extends BaseSpec {
  "a program" should "fail to fetch types if .mach is not set" in {
    failToAssembleOrExecute(
      """
        | .var hello %0 u1
      """.stripMargin
    ).thenVerify {
      case SimulatorError.FromAsmError(UnspecifiedMachineProfile(2)) => true
    }
  }

  ".mach" should "fail with less than two arguments" in {
    failToAssembleOrExecute(
      """
        | .mach xxx
      """.stripMargin
    ).thenVerify {
      case SimulatorError.FromAsmError(InvalidDirectiveSpecification(".mach", 2)) => true
    }
  }

  ".mach" should "fail if IS type is incompatible" in {
    failToAssembleOrExecute(
      """
        | .mach 9999.9999 bl:1:heap:1024
      """.stripMargin
    ).thenVerify {
      case SimulatorError.FromAsmError(IncompatibleInstructionSet(IncompatibleInstructionSetVersion(_), 2)) => true
    }
  }

  ".mach" should "fail if machine specification is invalid" in {
    failToAssembleOrExecute(
      s"""
         | ${aMachDirectiveWithSpecification("karakal")}
      """.stripMargin
    ).thenVerify {
      case SimulatorError.FromAsmError(InvalidMachineSpecification(_, VmProfileStringError.InvalidFormat(_, _), 2)) => true
    }
  }

  ".mach" should "fail if machine word length is not an integer" in {
    failToAssembleOrExecute(
      s"""
         | ${aMachDirectiveWithSpecification("bl:what?:heap:10")}
     """.stripMargin
    ).thenVerify {
      case SimulatorError.FromAsmError(InvalidMachineSpecification(_, VmProfileStringError.NotAPositiveNumber(_, "bl"), 2)) => true
    }
  }

  ".mach" should "fail if machine word length is null" in {
    failToAssembleOrExecute(
      s"""
         | ${aMachDirectiveWithSpecification("bl:0:heap:10")}
     """.stripMargin
    ).thenVerify {
      case SimulatorError.FromAsmError(InvalidMachineSpecification(_, VmProfileStringError.NotAPositiveNumber(_, "bl"), 2)) => true
    }
  }

  ".mach" should "fail if machine word length is too large" in {
    failToAssembleOrExecute(
      s"""
         | ${aMachDirectiveWithSpecification(s"bl:${VmContext.maximumByteSizeAllowed + 1}:heap:10")}
     """.stripMargin
    ).thenVerify {
      case SimulatorError.FromAsmError(InvalidMachineSpecification(_, VmProfileStringError.ValueExceedsMaximumAllowed(_, "bl", VmContext.maximumByteSizeAllowed), 2)) => true
    }
  }

  ".mach" should "fail if heap size is negative" in {
    failToAssembleOrExecute(
      s"""
         | ${aMachDirectiveWithSpecification(s"bl:1:heap:-1")}
     """.stripMargin
    ).thenVerify {
      case SimulatorError.FromAsmError(InvalidMachineSpecification(_, VmProfileStringError.NotAPositiveNumber(_, "heap"), 2)) => true
    }
  }

  ".mach" should "fail if heap size is too large" in {
    failToAssembleOrExecute(
      s"""
         | ${aMachDirectiveWithSpecification(s"bl:1:heap:${VmContext.maximumNumberOfVariablesInHeap + 1}")}
       """.stripMargin
    ).thenVerify {
      case SimulatorError.FromAsmError(InvalidMachineSpecification(_, VmProfileStringError.ValueExceedsMaximumAllowed(_, "heap", VmContext.maximumNumberOfVariablesInHeap), 2)) => true
    }
  }

  ".mach" should "properly initialize 8-bits machine" in {
    successfullyAssembleAndExecute(
      s"""
         | ${aMachDirectiveWithMachineWordLengthSetTo(1)}
         | .bb _boot
         | exit (00:u8)
         | .boot _boot
       """.stripMargin
    ).thenVerify {
      case vmContext =>
        vmTypesMapExactly(vmContext.profile.vmTypes, 1)
    }
  }

  ".mach" should "properly initialize 64-bits machine" in {
    successfullyAssembleAndExecute(
      s"""
         | ${aMachDirectiveWithMachineWordLengthSetTo(8)}
         | .bb _boot
         | exit (00:u8)
         | .boot _boot
       """.stripMargin
    ).thenVerify {
      case vmContext =>
        vmTypesMapExactly(vmContext.profile.vmTypes, 1, 2, 4, 8)
    }
  }

  "VM types" should "understand legal type strings" in {
    successfullyAssembleAndExecute(
      s"""
         | ${aMachDirectiveWithMachineWordLengthSetTo(1)}
         | .bb _boot
         | exit (00:u8)
         | .boot _boot
       """.stripMargin
    ).thenVerify {
      case vmContext =>
        vmTypesRecognize(vmContext.profile.vmTypes, "u8", 1, expectedIsSigned = false) &&
          vmTypesRecognize(vmContext.profile.vmTypes, "s8", 1, expectedIsSigned = true)
    }
  }

  "VM types" should "reject illegal type strings" in {
    successfullyAssembleAndExecute(
      s"""
         | ${aMachDirectiveWithMachineWordLengthSetTo(1)}
         | .bb _boot
         | exit (00:u8)
         | .boot _boot
       """.stripMargin
    ).thenVerify {
      case vmContext =>
        vmContext.profile.vmTypes.valueOf("u16").isLeft
    }
  }

  private def vmTypesRecognize(types: VmTypes, typeName: String, expectedByteLen: Int, expectedIsSigned: Boolean): Boolean =
    types.valueOf(typeName) match {
      case Ok(vmType) =>
        (vmType.byteLen == expectedByteLen) && (vmType.isSigned == expectedIsSigned)

      case _ =>
        false
    }

  private def vmTypesMapExactly(types: VmTypes, byteLens: Int*): Boolean = {
    // Has every expected type
    byteLens.foreach {
      eachLen =>
        if (!vmTypesSelectAllTypesOfByteLen(eachLen, types)) return false;
    }
    // Has no other type
    if (2 * byteLens.length != types.typeMap.size) return false
    // Just test with one stupid type at least once
    types.select(128, isSigned = true) match {
      case Some(_) =>
        false

      case None =>
        true
    }
  }

  private def vmTypesSelectAllTypesOfByteLen(byteLen: Int, types: VmTypes): Boolean = {
    if (types.select(byteLen, isSigned = true).isEmpty) return false
    if (types.select(byteLen, isSigned = false).isEmpty) return false
    types.select(byteLen) match {
      case seq if vmTypeSequenceContainsSignedAndUnsignedTypes(seq, byteLen) =>
        true
      case _ =>
        false
    }
  }

  private def vmTypeSequenceContainsSignedAndUnsignedTypes(seq: Seq[VmType], byteLen: Int): Boolean =
    (seq.length == 2) &&
      vmTypeSeqContains(seq, byteLen, isSigned = true) &&
      vmTypeSeqContains(seq, byteLen, isSigned = false) &&
      vmTypeSeqContainsTypeNameForLength(seq, byteLen)

  private def vmTypeSeqContains(seq: Seq[VmType], byteLen: Int, isSigned: Boolean): Boolean =
    seq.exists(t => (t.isSigned == isSigned) && (t.byteLen == byteLen))

  private def vmTypeSeqContainsTypeNameForLength(seq: Seq[VmType], byteLen: Int): Boolean =
    (seq.length == 2) &&
      seq.exists(t => t.toString == s"s${8 * byteLen}") &&
      seq.exists(t => t.toString == s"u${8 * byteLen}")
}
