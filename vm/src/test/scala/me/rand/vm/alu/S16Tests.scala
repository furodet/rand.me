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

  private lazy val interestingValues = Array(
    0x0000,
    0x0001,
    0x0002,
    0x007f,
    0x0080,
    0x0081,
    0x00fd,
    0x00fe,
    0x00ff,
    0x0100,
    0x0101,
    0x0102,
    0x017f,
    0x0180,
    0x0181,
    0x01fd,
    0x01fe,
    0x01ff,
    0x0200,
    0x0201,
    0x0202,
    0x027f,
    0x0280,
    0x0281,
    0x02fd,
    0x02fe,
    0x02ff,
    0x7f00,
    0x7f01,
    0x7f02,
    0x7f7f,
    0x7f80,
    0x7f81,
    0x7ffd,
    0x7ffe,
    0x7fff,
    0x8000,
    0x8001,
    0x8002,
    0x807f,
    0x8080,
    0x8081,
    0x80fd,
    0x80fe,
    0x80ff,
    0x8100,
    0x8101,
    0x8102,
    0x817f,
    0x8180,
    0x8181,
    0x81fd,
    0x81fe,
    0x81ff,
    0xfd00,
    0xfd01,
    0xfd02,
    0xfd7f,
    0xfd80,
    0xfd81,
    0xfdfd,
    0xfdfe,
    0xfdff,
    0xfe00,
    0xfe01,
    0xfe02,
    0xfe7f,
    0xfe80,
    0xfe81,
    0xfefd,
    0xfefe,
    0xfeff,
    0xff00,
    0xff01,
    0xff02,
    0xff7f,
    0xff80,
    0xff81,
    0xfffd,
    0xfffe,
    0xffff
  )

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
    assert(x.toInt == expected.toShort)
}
