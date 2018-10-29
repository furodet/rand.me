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
package me.rand.vm.engine

import me.rand.vm.engine.Variable.ScalarBuilder.Partial
import me.rand.vm.engine.VmTypes.VmType

// Documentation: doc/vmarchitecture.md
sealed trait Variable {
  def name: String

  def rename(newName: String): Variable

  def getValueString: String
}

object Variable {

  case class Scalar(name: String, value: VmWord) extends Variable {
    override def rename(newName: String): Variable = copy(name = newName)

    override def getValueString: String = value.toString
  }

  sealed trait Pointer extends Variable

  object Pointer {

    sealed trait ToVariable extends Pointer {
      def index: Int
    }

    object ToVariable {

      case class InTheHeap(name: String, index: Int) extends ToVariable {
        override def rename(newName: String): Variable = copy(name = newName)

        override def getValueString: String = s"HEAP[$index]"
      }

      case class InTheStack(name: String, index: Int) extends ToVariable {
        override def rename(newName: String): Variable = copy(name = newName)

        override def getValueString: String = s"STACK[$index]"
      }

    }

    case class ToInstruction(name: String, value: VmProgram.Counter) extends Pointer {
      override def rename(newName: String): Variable = copy(name = newName)

      override def getValueString: String = s"PC[${value.toString}]"
    }

  }

  class ScalarBuilder(name: String) {
    def ofType(varType: VmType): Partial = new Partial(name, varType)
  }

  object ScalarBuilder {
    def aScalarCalled(name: String) = new ScalarBuilder(name)

    class Partial(name: String, varType: VmType) {
      def setTo(value: BigInt): Scalar = Scalar(name, new VmWord(varType, value))

      def setTo(longValue: Long): Scalar = setTo(BigInt(longValue))
    }

  }

}
