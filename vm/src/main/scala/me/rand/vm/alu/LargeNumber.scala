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
package me.rand.vm.alu

import me.rand.vm.engine.VmTypes.VmType

import scala.collection.mutable.ListBuffer

case class LargeNumber(vmType: VmType, value: Array[Byte]) extends VmRegister {
  override def operations: VmRegisterOperations[VmRegister] =
    LargeNumberOperations.asInstanceOf[VmRegisterOperations[VmRegister]]

  override def toInt: Int = {
    val pad = if (vmType.isUnsigned || !mostSignificantBitIsSet) 0.toByte else 0xff.toByte
    val i0 = getByteNumberOrElse(0, pad)
    val i1 = getByteNumberOrElse(1, pad)
    val i2 = getByteNumberOrElse(2, pad)
    val i3 = getByteNumberOrElse(3, pad)
    (i3 << 24) | (i2 << 16) | (i1 << 8) | i0
  }

  private def getByteNumberOrElse(byteNr: Int, otherwise: Byte): Int =
    if (byteNr < value.length) value(byteNr) & 0xff else otherwise.toInt

  private[alu] def mostSignificantByte: Byte =
    value(vmType.byteLen - 1)

  private[alu] def mostSignificantBitIsSet: Boolean =
    (mostSignificantByte & 0x80.toByte) == 0x80.toByte

  override def toString: String = {
    val valueString = value.reverse.map {
      eachByte =>
        "%02x".format(eachByte)
    }.mkString("")
    s"($valueString:$vmType)"
  }
}

case object LargeNumberOperations extends VmRegisterOperations[LargeNumber] {
  override def buildFromBigEndian(vmType: VmType, value: Array[Byte]): LargeNumber = {
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
      val extendByteArray = Array.fill[Byte](byteLen)(padValue)
      copyBigEndian(truncated, extendByteArray)
    } else {
      val extendByteArray = Array.fill[Byte](byteLen)(0.toByte)
      copyBigEndian(truncated, extendByteArray)
    }
    // Switch to little endian, to simplify specific operations.
    LargeNumber(vmType, data.reverse)
  }

  def equalize(x: LargeNumber, y: LargeNumber): (LargeNumber, LargeNumber, VmRegisterOperations[LargeNumber]) = {
    def cast_y2x = (x, buildFromBigEndian(x.vmType, y.value.reverse), this)

    def cast_x2y = (buildFromBigEndian(y.vmType, x.value.reverse), y, this)
    // Applying rules of STDC 6.3.1.8.
    (x.vmType, y.vmType) match {
      case (t0, t1) if t0 <=> t1 =>
        // ... If both operands have the same type, then no further conversion is needed.
        (x, y, this)

      case (tx, ty) if tx.hasSameSignAs(ty) =>
        // ... if both operands have signed integer types or both have unsigned integer types,
        //     the operand with the type of lesser integer conversion rank
        //       is converted to the type of the operand with greater rank.
        if (x.vmType.byteLen > y.vmType.byteLen)
          cast_y2x
        else
          cast_x2y

      case (tx, ty) =>
        // ... if the operand that has unsigned integer type has rank greater or equal
        //       to the rank of the type of the other operand,
        //     then the operand with signed integer type is converted to the type
        //       of the operand with unsigned integer type.
        if (tx.isUnsigned && tx.byteLen >= ty.byteLen)
          cast_y2x
        else if (ty.isUnsigned && ty.byteLen >= tx.byteLen)
          cast_x2y
        // ... if the type of the operand with signed integer type can represent
        //       all of the values of the type of the operand with unsigned integer type,
        //     then the operand with unsigned integer type is converted
        //       to the type of the operand with signed integer type.
        else if (tx.isSigned && tx.byteLen > ty.byteLen)
          cast_y2x
        else if (ty.isSigned && ty.byteLen > ty.byteLen)
          cast_x2y
        // ... both operands are converted to the unsigned integer type corresponding to the type of the operand
        //       with signed integer type.
        else if (tx.isSigned)
          (buildFromBigEndian(x.vmType.asUnsigned, x.value.reverse), buildFromBigEndian(x.vmType.asUnsigned, y.value.reverse), this)
        else
          (buildFromBigEndian(y.vmType.asUnsigned, x.value.reverse), buildFromBigEndian(y.vmType.asUnsigned, y.value.reverse), this)
    }
  }

  override def bitFlip(x: LargeNumber): LargeNumber = {
    val result = x.value.map(b => (b ^ 0xff).toByte)
    LargeNumber(x.vmType, result)
  }

  override def increment(x: LargeNumber): LargeNumber = {
    val result = x.value.foldLeft((ListBuffer.empty[Byte], 1)) {
      case ((list, carry), eachByte) =>
        val sum = (eachByte.toInt & 0xff) + carry
        list += (sum & 0xff).toByte
        (list, (sum >>> 8) & 1)
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
        val sum = (xx & 0xff) + (yy & 0xff) + carry
        list += (sum & 0xff).toByte
        (list, (sum >>> 8) & 1)
    }._1.toArray
    LargeNumber(x.vmType, z)
  }

  override def isEqual(x: LargeNumber, y: LargeNumber): Boolean =
    !x.value.zip(y.value).exists { case (xx, yy) => xx != yy }

  override def isGreater(x: LargeNumber, y: LargeNumber): Boolean =
    if (x.vmType.isUnsigned) unsignedIsGreaterOrElse(x, y, ifEqual = false)
    else signedIsGreaterOrElse(x, y, ifEqual = false)

  override def isGreaterOrEqual(x: LargeNumber, y: LargeNumber): Boolean =
    if (x.vmType.isUnsigned) unsignedIsGreaterOrElse(x, y, ifEqual = true)
    else signedIsGreaterOrElse(x, y, ifEqual = true)

  private def unsignedIsGreaterOrElse(x: LargeNumber, y: LargeNumber, ifEqual: Boolean): Boolean = {
    (x.mostSignificantByte.toInt & 0xff, y.mostSignificantByte.toInt & 0xff) match {
      case (greater, lower) if greater > lower =>
        return true

      case (lower, greater) if lower < greater =>
        return false

      case _ =>
      // Need to check next bytes
    }
    isGreaterOrElseForLeastSignificantBytes(x, y, ifEqual)
  }

  private def signedIsGreaterOrElse(x: LargeNumber, y: LargeNumber, ifEqual: Boolean): Boolean = {
    (x.mostSignificantByte, y.mostSignificantByte) match {
      case (greater, lower) if greater > lower =>
        return true

      case (lower, greater) if lower < greater =>
        return false

      case _ =>
      // Need to check next bytes
    }
    isGreaterOrElseForLeastSignificantBytes(x, y, ifEqual)
  }

  private def isGreaterOrElseForLeastSignificantBytes(x: LargeNumber, y: LargeNumber, ifEqual: Boolean): Boolean = {
    x.value.reverse.tail.zip(y.value.reverse.tail).foreach {
      case (eachByteX, eachByteY) =>
        (eachByteX.toInt & 0xff, eachByteY.toInt & 0xff) match {
          case (greater, lower) if greater > lower =>
            return true
          case (lower, greater) if lower < greater =>
            return false
          case _ =>
          // The two current bytes are equal, check next, if any
        }
    }
    ifEqual
  }

  private def __(x: LargeNumber, y: LargeNumber)(f: (Byte, Byte) => Byte): LargeNumber = {
    val z = x.value.zip(y.value).map {
      case (dis, dat) =>
        f(dis, dat)
    }
    LargeNumber(x.vmType, z)
  }

  override def cast(x: LargeNumber, t: VmType): LargeNumber = buildFromBigEndian(t, x.value.reverse)
}
