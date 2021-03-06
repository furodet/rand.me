/*
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

object Alu {
  def sizeof(x: VmRegister): Int =
    x.vmType.byteLen

  def bitFlip(x: VmRegister): VmRegister =
    x.operations.bitFlip(x)

  def increment(x: VmRegister): VmRegister =
    x.operations.increment(x)

  def and(x: VmRegister, y: VmRegister): VmRegister = {
    val (ex, ey, ops) = equalize(x, y)
    ops.and(ex, ey)
  }

  def or(x: VmRegister, y: VmRegister): VmRegister = {
    val (ex, ey, ops) = equalize(x, y)
    ops.or(ex, ey)
  }

  def xor(x: VmRegister, y: VmRegister): VmRegister = {
    val (ex, ey, ops) = equalize(x, y)
    ops.xor(ex, ey)
  }

  def neg(x: VmRegister): VmRegister =
    x.operations.neg(x)

  def add(x: VmRegister, y: VmRegister): VmRegister = {
    val (ex, ey, ops) = equalize(x, y)
    ops.add(ex, ey)
  }

  def sub(x: VmRegister, y: VmRegister): VmRegister = {
    val (ex, ey, ops) = equalize(x, y)
    ops.sub(ex, ey)
  }

  def isEqual(x: VmRegister, y: VmRegister): Boolean = {
    val (ex, ey, ops) = equalize(x, y)
    ops.isEqual(ex, ey)
  }

  def isNotEqual(x: VmRegister, y: VmRegister): Boolean =
    !isEqual(x, y)

  def isGreater(x: VmRegister, y: VmRegister): Boolean = {
    val (ex, ey, ops) = equalize(x, y)
    ops.isGreater(ex, ey)
  }

  def isLower(x: VmRegister, y: VmRegister): Boolean =
    !isGreaterOrEqual(x, y)

  def isGreaterOrEqual(x: VmRegister, y: VmRegister): Boolean = {
    val (ex, ey, ops) = equalize(x, y)
    ops.isGreaterOrEqual(ex, ey)
  }

  def isLowerOrEqual(x: VmRegister, y: VmRegister): Boolean =
    !isGreater(x, y)

  private def equalize(x: VmRegister, y: VmRegister): (VmRegister, VmRegister, VmRegisterOperations[VmRegister]) =
    (x, y) match {
      case (xx: LargeNumber, yy: LargeNumber) =>
        LargeNumberOperations.equalize(xx, yy).asInstanceOf[(VmRegister, VmRegister, VmRegisterOperations[VmRegister])]

      case _ =>
        throw new RuntimeException("TODO!")
    }
}
