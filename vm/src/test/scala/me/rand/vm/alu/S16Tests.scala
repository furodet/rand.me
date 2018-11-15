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

class S16Tests extends FlatSpec {
  private val s16 = VmTypes.forMachineWordByteLength(2).select(2, isSigned = true).get

  private def s16(value: Int): VmRegister = VmRegister.normalize(s16, value)

  "s16" should "return valid toInt" in {
    for (x <- -32767 to 32768) {
      verify(s16(x), x)
    }
  }

  "ALU" should "successfully bitflip s16" in {
    def assertBitFlip(value: Int, expectedBitFlipValue: Int) = {
      val x = s16(value)
      verify(Alu.bitFlip(x), expectedBitFlipValue)
    }

    for (x <- -32767 to 32768) {
      assertBitFlip(x, ~x)
    }
  }

  "ALU" should "successfully increment s16" in {
    def assertIncrement(value: Int, expectedIncrementedValue: Int) = {
      val x = s16(value)
      verify(Alu.increment(x), expectedIncrementedValue)
    }

    for (x <- -32767 to 32768) {
      assertIncrement(x, x + 1)
    }
    assertIncrement(65535, 0)
  }

  "ALU" should "successfully negate s16" in {
    def assertNegate(value: Int, expectedNegateValue: Int) = {
      val x = s16(value)
      verify(Alu.neg(x), expectedNegateValue)
    }

    assertNegate(0, 0)
    for (x <- -32767 to 32768) {
      assertNegate(x, -x)
    }
  }

  private lazy val interestingValues = Array[Range](
    -32767 to (-32767 + 512),
    -512 to 512,
    (32768 - 512) to 32768
  ).flatMap(_.toList)

  "ALU" should "successfully and s16" in {
    def assertAnd(value1: Int, value2: Int, expectedAndValue: Int) = {
      val x = s16(value1)
      val y = s16(value2)
      verify(Alu.and(x, y), expectedAndValue)
    }

    for (x <- interestingValues)
      for (y <- interestingValues)
        assertAnd(x, y, x & y)
  }

  "ALU" should "successfully or s16" in {
    def assertOr(value1: Int, value2: Int, expectedOrValue: Int) = {
      val x = s16(value1)
      val y = s16(value2)
      verify(Alu.or(x, y), expectedOrValue)
    }

    for (x <- interestingValues)
      for (y <- interestingValues)
        assertOr(x, y, x | y)
  }

  "ALU" should "successfully xor s16" in {
    def assertXor(value1: Int, value2: Int, expectedXorValue: Int) = {
      val x = s16(value1)
      val y = s16(value2)
      verify(Alu.xor(x, y), expectedXorValue)
    }

    for (x <- interestingValues)
      for (y <- interestingValues)
        assertXor(x, y, x ^ y)
  }

  "ALU" should "successfully add s16" in {
    def assertAdd(value1: Int, value2: Int, expectedAddValue: Int) = {
      val x = s16(value1)
      val y = s16(value2)
      verify(Alu.add(x, y), expectedAddValue)
    }

    for (x <- interestingValues)
      for (y <- interestingValues)
        assertAdd(x, y, x + y)
  }

  "ALU" should "successfully sub s16" in {
    def assertSub(value1: Int, value2: Int, expectedSubValue: Int) = {
      val x = s16(value1)
      val y = s16(value2)
      verify(Alu.sub(x, y), expectedSubValue)
    }

    for (x <- interestingValues)
      for (y <- interestingValues)
        assertSub(x, y, x - y)
  }

  "ALU" should "successfully test equal s16" in {
    def assertIsEqual(value1: Int, value2: Int, expectedEqualValue: Boolean) = {
      val x = s16(value1)
      val y = s16(value2)
      assert(Alu.isEqual(x, y) == expectedEqualValue)
    }

    for (x <- interestingValues)
      for (y <- interestingValues)
        assertIsEqual(x, y, x == y)
  }

  "ALU" should "successfully test greater than s16" in {
    def assertIsGreaterThan(value1: Int, value2: Int, expectedGreaterThanValue: Boolean) = {
      val x = s16(value1)
      val y = s16(value2)
      assert(Alu.isGreater(x, y) == expectedGreaterThanValue)
    }

    for (x <- interestingValues)
      for (y <- interestingValues)
        assertIsGreaterThan(x, y, x.toShort > y.toShort)
  }

  "ALU" should "successfully test greater or equal s16" in {
    def assertIsGreaterOrEqual(value1: Int, value2: Int, expectedGreaterOrEqualValue: Boolean) = {
      val x = s16(value1)
      val y = s16(value2)
      assert(Alu.isGreaterOrEqual(x, y) == expectedGreaterOrEqualValue)
    }

    for (x <- interestingValues)
      for (y <- interestingValues)
        assertIsGreaterOrEqual(x, y, x.toShort >= y.toShort)
  }

  private def verify(x: VmRegister, expected: Int) = {
    assertIsS16(x)
    assertValueIs(x, expected)
  }

  private def assertIsS16(x: VmRegister) = {
    assert(Alu.sizeof(x) == 2)
    assert(x.vmType.isSigned)
  }

  private def assertValueIs(x: VmRegister, expected: Int) =
    assert(x.toInt == (expected.toShort))
}
