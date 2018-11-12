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

import me.rand.vm.engine.VmTypes.VmType

import scala.collection.mutable.ListBuffer

case class LargeNumber(vmType: VmType, value: Array[Byte]) extends VmRegister {
  override def operations: VmRegisterOperations[VmRegister] =
    LargeNumberOperations.asInstanceOf[VmRegisterOperations[VmRegister]]

  override def toInt: Int = {
    val i0 = getByteNumberOrZero(0)
    val i1 = getByteNumberOrZero(1)
    val i2 = getByteNumberOrZero(2)
    val i3 = getByteNumberOrZero(3)
    (i3 << 24) | (i2 << 16) | (i1 << 8) | i0
  }

  private def getByteNumberOrZero(byteNr: Int): Int =
    if (byteNr < value.length) value(byteNr) & 0xff else 0

  private[alu] def mostSignificantByte: Byte = value(value.length - 1)

  private[alu] def mostSignificantBitIsSet: Boolean =
    (mostSignificantByte & 0x80.toByte) == 0x80

  override def toString: String =
    value.reverse.map {
      eachByte =>
        "%02x".format(eachByte)
    }.mkString("0x", ".", "")
}

case object LargeNumberOperations extends VmRegisterOperations[LargeNumber] {
  private def build(vmType: VmType, value: Array[Byte]): LargeNumber = {
    def copyBigEndian(source: Array[Byte], destination: Array[Byte]): Array[Byte] = {
      // Example:
      // 0  1  2  3       0  1  2  3  4  5  6  7    0  1  2  3  4  5  6  7
      // S3 S2 S1 S0 INTO D7 D6 D5 D4 D3 D2 D1 D0 = S3 S2 S1 S0 D3 D2 D1 D0
      Array.copy(source, 0, destination, destination.length - source.length, source.length)
      destination
    }

    val byteLen = vmType.byteLen
    val signExtend = !vmType.isUnsigned

    // Truncate
    val truncated = value.takeRight(byteLen)
    // Extend
    val data = if (signExtend) {
      val valueIsNegative = (truncated(0) & 0x80) == 0x80
      val padValue = if (valueIsNegative) 0xff.toByte else 0.toByte
      val extendByteArray = Array.fill[Byte](byteLen + 1)(padValue)
      copyBigEndian(truncated, extendByteArray)
    } else {
      val extendByteArray = Array.fill[Byte](byteLen + 1)(0.toByte)
      copyBigEndian(truncated, extendByteArray)
    }
    // Switch to little endian, to simplify specific operations.
    LargeNumber(vmType, data.reverse)
  }

  override def build(vmType: VmType, value: BigInt): LargeNumber =
    build(vmType, value.toByteArray)

  def equalize(x: LargeNumber, y: LargeNumber): (LargeNumber, LargeNumber, VmRegisterOperations[LargeNumber]) =
    (x.vmType, x.vmType) match {
      case (t0, t1) if t0 <=> t1 =>
        (x, y, this)

      case (t0, t1) if t0 <~> t1 && t0.isUnsigned =>
        (x, build(t0, y.value), this)

      case (t0, t1) if t0 <~> t1 && t1.isUnsigned =>
        (build(t1, x.value), y, this)

      case (t0, t1) if t0 < t1 =>
        (build(t1, x.value), y, this)

      case (t0, t1) if t0 > t1 =>
        (x, build(t0, y.value), this)
    }

  override def bitFlip(x: LargeNumber): LargeNumber = {
    val result = x.value.map(b => (b ^ 0xff).toByte)
    LargeNumber(x.vmType, result)
  }

  override def addImmediate(x: LargeNumber, value: Int): LargeNumber = {
    val result = x.value.foldLeft((ListBuffer.empty[Byte], 0)) {
      case ((list, carry), eachByte) =>
        val sum = eachByte.toInt + carry
        list += (sum & 0xff).toByte
        (list, (sum & 0x100) >>> 8)
    }._1.toArray
    LargeNumber(x.vmType, result)
  }

  override def and(x: LargeNumber, y: LargeNumber): LargeNumber =
    __(x, y)((b0, b1) => (b0 & b1).toByte)

  override def or(x: LargeNumber, y: LargeNumber): LargeNumber =
    __(x, y)((b0, b1) => (b0 | b1).toByte)

  override def xor(x: LargeNumber, y: LargeNumber): LargeNumber =
    __(x, y)((b0, b1) => (b0 ^ b1).toByte)

  override def add(x: LargeNumber, y: LargeNumber): LargeNumber = {
    val z = x.value.zip(y.value).foldLeft((ListBuffer.empty[Byte], 0)) {
      case ((list, carry), (xx, yy)) =>
        val sum = xx + yy + carry
        list += (sum & 0xff).toByte
        (list, (sum >>> 8) & 1)
    }._1.toArray
    LargeNumber(x.vmType, z)
  }

  override def isEqualToZero(x: LargeNumber): Boolean =
    x.value.collectFirst {
      case xx =>
        xx != 0
    }.isEmpty

  override def isGreaterOrEqualToZero(x: LargeNumber): Boolean =
    if (x.vmType.isUnsigned) true
    else !x.mostSignificantBitIsSet

  override def isGreaterThanZero(x: LargeNumber): Boolean =
    if (x.vmType.isUnsigned) !isEqualToZero(x)
    else !x.mostSignificantBitIsSet && !isEqualToZero(x)

  private def __(x: LargeNumber, y: LargeNumber)(f: (Byte, Byte) => Byte): LargeNumber = {
    val z = x.value.zip(y.value).map {
      case (dis, dat) =>
        f(dis, dat)
    }
    LargeNumber(x.vmType, z)
  }
}
