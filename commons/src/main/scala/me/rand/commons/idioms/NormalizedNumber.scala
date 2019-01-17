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
package me.rand.commons.idioms

import java.nio.ByteBuffer

import scala.language.implicitConversions

/**
  * What the system (and especially the virtual machine) does care of when dealing
  * with number is to have an accurate representation that can be easily translated
  * to a sequence of bytes.
  *
  * As a consequence, every number should be translated to this normalized form,
  * with help of implicit converters defined here-after.
  *
  * TODO: extend to broader types (from arrays of ints, for example)
  */
sealed trait NormalizedNumber {
  val asBigEndianByteArray: Array[Byte]
}

object NormalizedNumber {
  implicit def ByteToNormalizedNumber(value: Byte): NormalizedNumber = new NormalizedNumber {
    override val asBigEndianByteArray: Array[Byte] = ByteBuffer.allocate(1).put(value).array()
  }

  implicit def ShortToNormalizedNumber(value: Short): NormalizedNumber = new NormalizedNumber {
    override val asBigEndianByteArray: Array[Byte] = ByteBuffer.allocate(2).putShort(value).array()
  }

  implicit def IntToNormalizedNumber(value: Int): NormalizedNumber = new NormalizedNumber {
    override val asBigEndianByteArray: Array[Byte] = ByteBuffer.allocate(4).putInt(value).array()
  }

  implicit def LongToNormalizedNumber(value: Long): NormalizedNumber = new NormalizedNumber {
    override val asBigEndianByteArray: Array[Byte] = ByteBuffer.allocate(8).putLong(value).array()
  }

  implicit def BigEndianIntListOfBytesToNormalizedNumber(value: Iterator[Int]): NormalizedNumber = new NormalizedNumber {
    override val asBigEndianByteArray: Array[Byte] = value.map(_.toByte).toArray
  }
}
