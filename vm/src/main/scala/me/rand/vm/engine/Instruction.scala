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

import me.rand.commons.idioms.Status._
import me.rand.vm.engine.Instruction.Operand.{DestinationOperand, SourceOperand}
import me.rand.vm.engine.Instruction.Operands
import me.rand.vm.main.VmError.VmExecutionError.IllegalEncodingError
import me.rand.vm.main.{ExecutionContext, VmError}

trait Instruction {
  def execute(vmContext: VmContext, operands: Operands)(implicit executionContext: ExecutionContext): VmContext OrElse VmError
}

object Instruction {

  sealed trait Operand

  object Operand {

    sealed trait SourceOperand extends Operand

    object SourceOperand {

      sealed trait SourceVariable extends SourceOperand

      object SourceVariable {

        case class InTheHeap(index: Int) extends SourceOperand.SourceVariable

        case class InTheStack(index: Int) extends SourceOperand.SourceVariable

      }

      case class Indirect(pointer: SourceOperand.SourceVariable, depth: Int) extends SourceOperand

      case class Immediate(value: VmWord) extends SourceOperand

    }

    sealed trait DestinationOperand

    object DestinationOperand {

      object NoDestination extends DestinationOperand

      sealed trait TargetVariable extends DestinationOperand

      object TargetVariable {

        case class InTheHeap(variableIndex: Int) extends DestinationOperand.TargetVariable

        case class InTheStack(variableIndex: Int) extends DestinationOperand.TargetVariable

      }

      case class Redirect(pointer: DestinationOperand.TargetVariable, depth: Int) extends DestinationOperand

    }

  }

  class Operands(val destination: DestinationOperand, val sources: Map[Int, SourceOperand]) {
    def setDestination(destinationOperand: DestinationOperand): Operands =
      new Operands(destination = destinationOperand, sources)

    def addSource(operandIndexAndValue: (Int, SourceOperand)): Operands =
      new Operands(destination, sources = sources + operandIndexAndValue)

    def fetchSource(operandId: Int): SourceOperand OrElse IllegalEncodingError =
      sources.get(operandId) match {
        case None =>
          Err(IllegalEncodingError.UnspecifiedSourceOperand(operandId))

        case Some(operand) =>
          Ok(operand)
      }
  }

  object Operands {
    def none = new Operands(DestinationOperand.NoDestination, Map.empty)
  }

  class OperandsBuilder(operands: Operands) {
    def +(destination: DestinationOperand): OperandsBuilder =
      new OperandsBuilder(new Operands(destination, operands.sources))

    def +(source: (Int, SourceOperand)): OperandsBuilder =
      new OperandsBuilder(new Operands(operands.destination, operands.sources + source))

    def build: Operands = operands
  }

  object OperandsBuilder {
    def none = new OperandsBuilder(Operands.none)
  }

}
