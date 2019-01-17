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
package me.rand.vm.engine

import me.rand.commons.idioms.NormalizedNumber._
import me.rand.commons.idioms.Status.{OrElse, _}
import me.rand.vm.alu.VmRegister
import me.rand.vm.engine.Variable.Scalar
import me.rand.vm.engine.VmTypes.VmType
import me.rand.vm.main.{ExecutionContext, VmError}

sealed trait VmControl {
  def execute(vmContext: VmContext)(implicit executionContext: ExecutionContext): VmContext OrElse VmError
}

// Documentation: doc/vmarchitecture.md
object VmControl {

  case class TagVariable(variableName: String, variableId: Operand.Source.Variable, vmt: Variable.BasicType) extends VmControl {
    // Some sort of universal null pointer
    private lazy val nullptr = Variable.Pointer.ToVariable.InTheHeap(variableName, 0)

    override def execute(vmContext: VmContext)(implicit executionContext: ExecutionContext): VmContext OrElse VmError =
      vmt match {
        case Variable.BasicType._Scalar(vmType) =>
          initializeVariable(vmContext, zero(vmType))

        case Variable.BasicType._Pointer =>
          initializeVariable(vmContext, nullptr)
      }

    private def zero(vmType: VmType): Scalar = Scalar(variableName, VmRegister.ofType(vmType).withValue(0))

    private def initializeVariable(vmContext: VmContext, initialValue: Variable)(implicit executionContext: ExecutionContext): VmContext OrElse VmError =
      variableId match {
        case Operand.Source.Variable.InTheHeap(index) =>
          executionContext.logger ~> s"SET HEAP[$index] $initialValue"
          vmContext.putHeapVariable(index, initialValue)

        case Operand.Source.Variable.InTheStack(index) =>
          executionContext.logger ~> s"SET STACK[$index] $initialValue"
          vmContext.putStackVariable(index, initialValue)
      }

    override def toString: String =
      s"${TagVariable.name} $variableName $variableId $vmt"
  }

  object TagVariable {
    lazy val name: String = ".var"
  }

  sealed trait FrameOperation extends VmControl

  object FrameOperation {

    case class Push(nrVariables: Int) extends FrameOperation {
      override def execute(vmContext: VmContext)(implicit executionContext: ExecutionContext): VmContext OrElse VmError = {
        executionContext.logger >> s"PUSH FRAME $nrVariables"
        Ok(vmContext.createFrameOfSize(nrVariables))
      }

      override def toString: String = s"${Push.name} $nrVariables"
    }

    object Push {
      lazy val name: String = ".push"
    }

    case object Pop extends FrameOperation {
      lazy val name: String = ".pop"

      override def execute(vmContext: VmContext)(implicit executionContext: ExecutionContext): VmContext OrElse VmError = {
        executionContext.logger >> "POP FRAME"
        vmContext.popFrame()
      }

      override def toString: String = name
    }

  }

}