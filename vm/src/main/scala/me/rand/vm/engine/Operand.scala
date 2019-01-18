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
package me.rand.vm.engine

import me.rand.vm.alu.VmRegister

sealed trait Operand

object Operand {

  sealed trait Source extends Operand

  object Source {

    sealed trait Variable extends Source

    object Variable {

      case class InTheHeap(index: Int) extends Source.Variable {
        override def toString: String = s"%$index"
      }

      case class InTheStack(index: Int) extends Source.Variable {
        override def toString: String = s"$$$index"
      }

    }

    case class Indirect(pointer: Source.Variable, depth: Int) extends Source {
      override def toString: String = {
        val prefix = (0 until depth).foldLeft("") { (string, _) => string + "*" }
        s"$prefix$pointer"
      }
    }

    case class Indexed(pointer: Source.Variable, offset: Int) extends Source {
      override def toString: String = s"$pointer[$offset]"
    }

    sealed trait Reference extends Source

    object Reference {

      case class InTheHeap(index: Int) extends Source.Reference {
        override def toString: String = s"&%$index"
      }

      case class InTheStack(index: Int) extends Source.Reference {
        override def toString: String = s"&$$$index"
      }

      case class InInstructionMemory(basicBlockName: String) extends Source.Reference {
        override def toString: String = s"&@index"
      }
    }

    case class Immediate(value: VmRegister) extends Source {
      override def toString: String = s"$value"
    }

  }

  sealed trait Destination

  object Destination {

    object NoDestination extends Destination {
      override def toString: String = "_"
    }

    sealed trait Variable extends Destination

    object Variable {

      case class InTheHeap(variableIndex: Int) extends Destination.Variable {
        override def toString: String = s"%$variableIndex"
      }

      case class InTheStack(variableIndex: Int) extends Destination.Variable {
        override def toString: String = s"$$$variableIndex"
      }

    }

    case class Redirect(pointer: Destination.Variable, depth: Int) extends Destination {
      override def toString: String = {
        val prefix = (0 until depth).foldLeft("") { (string, _) => string + "*" }
        s"$prefix$pointer"
      }
    }

    case class Indexed(pointer: Destination.Variable, offset: Int) extends Destination {
      override def toString: String = s"$pointer[$offset]"
    }

  }

}
