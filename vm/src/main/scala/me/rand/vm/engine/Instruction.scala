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
import me.rand.vm.engine.Variable.Pointer
import me.rand.vm.main.ExecutionContext
import me.rand.vm.main.VmError.VmExecutionError
import me.rand.vm.main.VmError.VmExecutionError.IllegalEncoding

trait Instruction {
  def execute(vmContext: VmContext, operands: Operands)(implicit executionContext: ExecutionContext): VmContext OrElse VmExecutionError
}

object Instruction {

  sealed trait Operand

  object Operand {

    case class SourceOperand(variable: Variable) extends Operand

    case class DestinationOperand(to: Option[Pointer.ToVariable]) extends Operand

    object DestinationOperand {
      def none = new DestinationOperand(None)
    }

  }

  class Operands(destination: DestinationOperand, sources: Map[Int, SourceOperand]) {
    def setDestination(destinationOperand: DestinationOperand): Operands =
      new Operands(destination = destinationOperand, sources)

    def addSource(operandIndexAndValue: (Int, SourceOperand)): Operands =
      new Operands(destination, sources = sources + operandIndexAndValue)

    def fetchDestination: Pointer.ToVariable OrElse IllegalEncoding =
      destination.to match {
        case None =>
          Err(IllegalEncoding.UnspecifiedDestinationOperand)

        case Some(target) =>
          Ok(target)
      }

    def fetchSource(operandId: Int): SourceOperand OrElse IllegalEncoding =
      sources.get(operandId) match {
        case None =>
          Err(IllegalEncoding.UnspecifiedSourceOperand(operandId))

        case Some(operand) =>
          Ok(operand)
      }
  }

  object Operands {
    def none = new Operands(DestinationOperand.none, Map.empty)
  }

}
