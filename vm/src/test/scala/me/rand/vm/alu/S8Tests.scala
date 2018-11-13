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
package me.rand.vm.alu

import me.rand.vm.engine.VmTypes
import org.scalatest.FlatSpec

class S8Tests extends FlatSpec {
  private val s8 = VmTypes.forMachineWordByteLength(1).select(1, isSigned = true).get

  private def s8(value: Int): VmRegister = VmRegister.normalize(s8, value)

  "s8" should "return valid toInt" in {
    for (x <- -128 to 127) {
      assertValueIs(s8(x), x)
    }
  }

  "ALU" should "successfully bitflip s8" in {
    def assertBitFlip(value: Int, expectedBitFlipValue: Int) = {
      val x = s8(value)
      verify(Alu.bitFlip(x), expectedBitFlipValue)
    }

    for (x <- -128 to 127) {
      assertBitFlip(x, ~x)
    }
  }

  "ALU" should "successfully increment s8" in {
    def assertIncrement(value: Int, expectedIncrementedValue: Int) = {
      val x = s8(value)
      verify(Alu.increment(x), expectedIncrementedValue)
    }

    for (x <- -128 to 126) {
      assertIncrement(x, x + 1)
    }
    assertIncrement(127, -128)
  }

  "ALU" should "successfully negate s8" in {
    def assertNegate(value: Int, expectedNegateValue: Int) = {
      val x = s8(value)
      verify(Alu.neg(x), expectedNegateValue)
    }

    assertNegate(0, 0)
    for (x <- -127 to 127) {
      assertNegate(x, -x)
    }
    // Try it in C...
    assertNegate(-128, -128)
  }

  "ALU" should "successfully and s8" in {
    def assertAnd(value1: Int, value2: Int, expectedAndValue: Int) = {
      val x = s8(value1)
      val y = s8(value2)
      verify(Alu.and(x, y), expectedAndValue)
    }

    for (x <- -128 to 127)
      for (y <- -128 to 127)
        assertAnd(x, y, x & y)
  }

  "ALU" should "successfully or s8" in {
    def assertOr(value1: Int, value2: Int, expectedOrValue: Int) = {
      val x = s8(value1)
      val y = s8(value2)
      verify(Alu.or(x, y), expectedOrValue)
    }

    for (x <- -128 to 127)
      for (y <- -128 to 127)
        assertOr(x, y, x | y)
  }

  "ALU" should "successfully xor s8" in {
    def assertAnd(value1: Int, value2: Int, expectedXorValue: Int) = {
      val x = s8(value1)
      val y = s8(value2)
      verify(Alu.xor(x, y), expectedXorValue)
    }

    for (x <- -128 to 127)
      for (y <- -128 to 127)
        assertAnd(x, y, x ^ y)
  }

  "ALU" should "successfully add s8" in {
    def assertAdd(value1: Int, value2: Int, expectedAddValue: Int) = {
      val x = s8(value1)
      val y = s8(value2)
      verify(Alu.add(x, y), expectedAddValue)
    }

    for (x <- -128 to 127)
      for (y <- -128 to 127)
        assertAdd(x, y, x + y)
  }

  "ALU" should "successfully sub s8" in {
    def assertSub(value1: Int, value2: Int, expectedSubValue: Int) = {
      val x = s8(value1)
      val y = s8(value2)
      verify(Alu.sub(x, y), expectedSubValue)
    }

    for (x <- -128 to 127)
      for (y <- -128 to 127)
        assertSub(x, y, x - y)
  }

  private def verify(x: VmRegister, expected: Int) = {
    assertIsS8(x)
    assertValueIs(x, expected)
  }

  private def assertIsS8(x: VmRegister) = {
    assert(x.vmType.byteLen == 1)
    assert(x.vmType.isSigned)
  }

  private def assertValueIs(x: VmRegister, expected: Int) =
    assert(x.toInt == expected.toByte)
}
