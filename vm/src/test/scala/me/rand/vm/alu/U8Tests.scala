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

class U8Tests extends FlatSpec {
  private val u8 = VmTypes.forMachineWordByteLength(1).select(1, isSigned = false).get

  private def u8(value: Int): VmRegister = VmRegister.normalize(u8, value)

  "u8" should "return valid toInt" in {
    for (x <- 0 to 255) {
      assert(u8(x).toInt == x)
    }
  }

  "ALU" should "successfully bitflip u8" in {
    def assertBitFlip(value: Int, expectedBitFlipValue: Int) = {
      val x = u8(value)
      val tildaX = Alu.bitFlip(x)
      assert(tildaX.vmType.byteLen == 1)
      assert(tildaX.vmType.isUnsigned)
      assert(tildaX.toInt == expectedBitFlipValue)
    }

    for (x <- 0 to 255) {
      assertBitFlip(x, 255 - x)
    }
  }

  "ALU" should "successfully increment u8" in {
    def assertIncrement(value: Int, expectedIncrementedValue: Int) = {
      val x = u8(value)
      val minusX = Alu.increment(x)
      assert(minusX.vmType.byteLen == 1)
      assert(minusX.vmType.isUnsigned)
      assert(minusX.toInt == expectedIncrementedValue)
    }

    for (x <- 0 to 254) {
      assertIncrement(x, x + 1)
    }
    assertIncrement(255, 0)
  }

  "ALU" should "successfully negate u8" in {
    def assertNegate(value: Int, expectedNegateValue: Int) = {
      val x = u8(value)
      val minusX = Alu.neg(x)
      assert(minusX.vmType.byteLen == 1)
      assert(minusX.vmType.isUnsigned)
      assert(minusX.toInt == expectedNegateValue)
    }

    assertNegate(0, 0)
    for (x <- 1 to 255) {
      assertNegate(x, 256 - x)
    }
  }

  "ALU" should "successfully and u8" in {
    def assertAnd(value1: Int, value2: Int, expectedAndValue: Int) = {
      val x = u8(value1)
      val y = u8(value2)
      val andXY = Alu.and(x, y)
      assert(andXY.vmType.byteLen == 1)
      assert(andXY.vmType.isUnsigned)
      assert(andXY.toInt == expectedAndValue)
    }

    for (x <- 0 to 255)
      for (y <- 0 to 255)
        assertAnd(x, y, x & y)
  }

  "ALU" should "successfully or u8" in {
    def assertOr(value1: Int, value2: Int, expectedOrValue: Int) = {
      val x = u8(value1)
      val y = u8(value2)
      val orXY = Alu.or(x, y)
      assert(orXY.vmType.byteLen == 1)
      assert(orXY.vmType.isUnsigned)
      assert(orXY.toInt == expectedOrValue)
    }

    for (x <- 0 to 255)
      for (y <- 0 to 255)
        assertOr(x, y, x | y)
  }

  "ALU" should "successfully xor u8" in {
    def assertXor(value1: Int, value2: Int, expectedXorValue: Int) = {
      val x = u8(value1)
      val y = u8(value2)
      val xorXY = Alu.xor(x, y)
      assert(xorXY.vmType.byteLen == 1)
      assert(xorXY.vmType.isUnsigned)
      assert(xorXY.toInt == expectedXorValue)
    }

    for (x <- 0 to 255)
      for (y <- 0 to 255)
        assertXor(x, y, x ^ y)
  }

}
