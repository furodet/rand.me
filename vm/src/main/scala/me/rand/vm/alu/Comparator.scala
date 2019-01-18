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

sealed trait Comparator {
  def name: String

  def aluComparator: (VmRegister, VmRegister) => Boolean

  def intComparator: (Int, Int) => Boolean

  override def toString: String = name
}

object Comparator {

  object ?= extends Comparator {
    override def name: String = "?=="

    override def aluComparator: (VmRegister, VmRegister) => Boolean = Alu.isEqual

    override def intComparator: (Int, Int) => Boolean = _ == _
  }

  object ?≠ extends Comparator {
    override def name: String = "?!="

    override def aluComparator: (VmRegister, VmRegister) => Boolean = Alu.isNotEqual

    override def intComparator: (Int, Int) => Boolean = _ != _
  }

  object ?< extends Comparator {
    override def name: String = "?<"

    override def aluComparator: (VmRegister, VmRegister) => Boolean = Alu.isLower

    override def intComparator: (Int, Int) => Boolean = _ < _
  }

  object ?> extends Comparator {
    override def name: String = "?>"

    override def aluComparator: (VmRegister, VmRegister) => Boolean = Alu.isGreater

    override def intComparator: (Int, Int) => Boolean = _ > _
  }

  object ?≤ extends Comparator {
    override def name: String = "?<="

    override def aluComparator: (VmRegister, VmRegister) => Boolean = Alu.isLowerOrEqual

    override def intComparator: (Int, Int) => Boolean = _ <= _
  }

  object ?≥ extends Comparator {
    override def name: String = "?>="

    override def aluComparator: (VmRegister, VmRegister) => Boolean = Alu.isGreaterOrEqual

    override def intComparator: (Int, Int) => Boolean = _ >= _
  }

}
