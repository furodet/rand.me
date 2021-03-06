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
import me.rand.vm.engine.VmTypes.VmType

// Documentation: doc/vmarchitecture.md
sealed trait Variable {
  def name: String

  def rename(newName: String): Variable

  def getValueString: String
}

object Variable {
  private val anonymousVariableName = "<undef>"

  sealed trait BasicType

  object BasicType {

    case class _Scalar(vmType: VmType) extends BasicType {
      override def toString: String = vmType.toString
    }

    case object _Pointer extends BasicType {
      override def toString: String = "ptr"
    }

  }

  case class Scalar(name: String, value: VmRegister) extends Variable {
    override def rename(newName: String): Variable = copy(name = newName)

    override def getValueString: String = value.toString
  }

  object Scalar {
    def anonymous(value: VmRegister) = Scalar(anonymousVariableName, value)

    def anonymous(value: Int, vmContext: VmContext) = Scalar(anonymousVariableName, intToScalar(value, vmContext))

    private def intToScalar(value: Int, vmContext: VmContext): VmRegister = {
      val nrEncodingBits = 32 - Integer.numberOfLeadingZeros(value)
      val nrEncodingBytes = (nrEncodingBits + 7) / 8
      // It is safe to promote to the proper byte length: no machine would support N bytes and
      // log2(N) is not encodable on a machine word.
      VmRegister.normalize(vmContext.profile.vmTypes.select(nrEncodingBytes, isSigned = false).get, value)
    }
  }

  sealed trait Pointer extends Variable

  object Pointer {

    sealed trait ToVariable extends Pointer {
      def index: Int

      def getContainingVarSet(vmContext: VmContext): VarSet
    }

    object ToVariable {

      case class InTheHeap(name: String, index: Int) extends ToVariable {
        override def rename(newName: String): Variable = copy(name = newName)

        override def getValueString: String = s"HEAP[$index]"

        override def getContainingVarSet(vmContext: VmContext): VarSet = vmContext.heap
      }

      object InTheHeap {
        def anonymous(index: Int) = new InTheHeap(anonymousVariableName, index)
      }

      case class InTheStack(name: String, index: Int) extends ToVariable {
        override def rename(newName: String): Variable = copy(name = newName)

        override def getValueString: String = s"STACK[$index]"

        override def getContainingVarSet(vmContext: VmContext): VarSet = vmContext.stack
      }

      object InTheStack {
        def anonymous(index: Int) = new InTheStack(anonymousVariableName, index)
      }

    }

    case class ToInstruction(name: String, value: VmProgram.Counter) extends Pointer {
      override def rename(newName: String): Variable = copy(name = newName)

      override def getValueString: String = s"PC[${value.toString}]"
    }

  }

}
