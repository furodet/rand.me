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

import me.rand.commons.idioms.NormalizedNumber
import me.rand.vm.engine.VmTypes.VmType

trait VmRegister {
  def vmType: VmType

  private[alu] def operations: VmRegisterOperations[VmRegister]

  def toInt: Int
}

trait VmRegisterOperations[T <: VmRegister] {
  def build(vmType: VmType, value: Array[Byte]): T

  def bitFlip(x: T): T

  def increment(x: T): T

  def and(x: T, y: T): T

  def or(x: T, y: T): T

  def xor(x: T, y: T): T

  def neg(x: T): T = increment(bitFlip(x))

  def add(x: T, y: T): T

  def sub(x: T, y: T): T = add(x, neg(y))

  def isEqual(x: T, y: T): Boolean

  def isGreater(x: T, y: T): Boolean

  def isGreaterOrEqual(x: T, y: T): Boolean
}

object VmRegister {
  def normalize(vmType: VmType, value: NormalizedNumber): VmRegister =
  // TODO: to speed up processing, use specific types of VmRegister for given lengths (e.g. longs for x32...)
    LargeNumberOperations.build(vmType, value.asBigEndianByteArray)
}
