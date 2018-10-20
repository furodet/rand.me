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
package me.rand.vm.engine

import me.rand.commons.idioms.Status._
import me.rand.vm.engine.VmContext.VmProfile
import me.rand.vm.engine.VmTypes.VmType
import me.rand.vm.main.VmError._
import org.scalatest.FlatSpec

class VmContextTest extends FlatSpec {
  "VM profile" should "not be created from invalid specification (illegal string)" in {
    VmProfile.fromString("babeubi") match {
      case Err(VmProfileStringError.InvalidFormat(_, _)) =>
        succeed

      case whatever =>
        fail(s"unexpected result from profile with illegal string: $whatever")
    }
  }

  "VM profile" should "not be created from invalid specification (illegal string integer)" in {
    VmProfile.fromString("bl:what?:heap:10") match {
      case Err(VmProfileStringError.NotAPositiveNumber(_, "bl")) =>
        succeed

      case whatever =>
        fail(s"unexpected result from profile with illegal string: $whatever")
    }
  }

  "VM profile" should "not be created from invalid specification (invalid byte len)" in {
    VmProfile.fromString("bl:0:heap:10") match {
      case Err(VmProfileStringError.NotAPositiveNumber(_, "bl")) =>
        succeed

      case whatever =>
        fail(s"unexpected result from profile with illegal string: $whatever")
    }
  }

  "VM profile" should "not be created from invalid specification (excessive byte len)" in {
    VmProfile.fromString("bl:257:heap:10") match {
      case Err(VmProfileStringError.ValueExceedsMaximumAllowed(_, "bl", VmContext.maximumByteSizeAllowed)) =>
        succeed

      case whatever =>
        fail(s"unexpected result from profile with illegal string: $whatever")
    }
  }

  "VM profile" should "not be created from invalid specification (byte len is not a power of two)" in {
    VmProfile.fromString("bl:3:heap:10") match {
      case Err(VmProfileStringError.NotAPowerOfTwo(3, "bl")) =>
        succeed

      case whatever =>
        fail(s"unexpected result from profile with illegal string: $whatever")
    }
  }

  "VM profile" should "not be created from invalid specification (invalid varsetsize)" in {
    VmProfile.fromString("bl:1:heap:-1") match {
      case Err(VmProfileStringError.NotAPositiveNumber(_, "heap")) =>
        succeed

      case whatever =>
        fail(s"unexpected result from profile with illegal string: $whatever")
    }
  }

  "VM profile" should "not be created from invalid specification (excessive varsetsize)" in {
    VmProfile.fromString(s"bl:1:heap:${VmContext.maximumNumberOfVariablesInHeap + 1}") match {
      case Err(VmProfileStringError.ValueExceedsMaximumAllowed(_, "heap", VmContext.maximumNumberOfVariablesInHeap)) =>
        succeed

      case whatever =>
        fail(s"unexpected result from profile with illegal string: $whatever")
    }
  }

  "VM context" should "properly initialize for 8-bits machine" in {
    VmContext.usingProfileString(s"bl:1:heap:${VmContext.maximumNumberOfVariablesInHeap}") match {
      case Ok(vmContext) =>
        assertThatVmTypesMapExactly(vmContext.vmTypes, 1)

      case whatever =>
        fail(s"unexpected result from profile/8-bits: $whatever")
    }
  }

  "VM context" should "properly initialize for 64-bits machine" in {
    VmContext.usingProfileString(s"bl:8:heap:${VmContext.maximumNumberOfVariablesInHeap}") match {
      case Ok(vmContext) =>
        assertThatVmTypesMapExactly(vmContext.vmTypes, 1, 2, 4, 8)

      case whatever =>
        fail(s"unexpected result from profile/64-bits: $whatever")
    }
  }

  private def assertThatVmTypesMapExactly(types: VmTypes, byteLens: Int*): Unit = {
    // Has every expected type
    byteLens.foreach {
      eachLen =>
        assertThatVmTypesSelectAllTypesOfByteLen(eachLen, types)
    }
    // Has no other type
    assert(2 * byteLens.length == types.typeMap.size)
  }

  private def assertThatVmTypesSelectAllTypesOfByteLen(byteLen: Int, types: VmTypes): Unit = {
    assert(types.select(byteLen, isSigned = true).isDefined)
    assert(types.select(byteLen, isSigned = false).isDefined)
    val signed = new VmType(byteLen, isSigned = false)
    val unsigned = new VmType(byteLen, isSigned = true)
    types.select(byteLen) match {
      case seq if vmTypeSequenceContainsExactly(seq, signed, unsigned) =>
        ()

      case whatever =>
        fail(s"failed to select signed or unsigned type with byte length $byteLen: $whatever")
    }
  }

  private def vmTypeSequenceContainsExactly(seq: Seq[VmType], signed: VmType, unsigned: VmType): Boolean =
    (seq.length == 2) && seq.contains(unsigned) && seq.contains(signed)
}
