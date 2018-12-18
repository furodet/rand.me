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
package me.rand.vm.dsl

import me.rand.commons.idioms.Status._
import me.rand.vm.engine.VmProgram.InstructionInstance
import me.rand.vm.engine.{Instruction, Operands}
import me.rand.vm.main.VmError.SyntaxError

import scala.language.implicitConversions

// Documentation: doc/vmeasm.md
class AbstractAsmInstructionBuilder(instruction: Instruction) {

  class AbstractAsmInstructionBuilderWithInputs(instruction: Instruction, sourceOperands: Seq[AbstractAsmOperandBuilder]) {
    def >(targetOperand: AbstractAsmOperandBuilder): InstructionInstance OrElse SyntaxError =
      buildSourceAndDestinationOperands(sourceOperands, targetOperand) && (operands => InstructionInstance(instruction, operands))

    def >(): InstructionInstance OrElse SyntaxError =
      buildSourceOperands(sourceOperands) && (operands => InstructionInstance(instruction, operands))

    private def buildSourceAndDestinationOperands(sources: Seq[AbstractAsmOperandBuilder], target: AbstractAsmOperandBuilder) =
      for {
        operands <- buildSourceOperands(sources)
        withDestination <- addDestinationOperand(operands, target)
      } yield withDestination

    private def buildSourceOperands(sources: Seq[AbstractAsmOperandBuilder]): Operands OrElse SyntaxError =
      sources.tryFoldLeft(Operands.none) {
        case (operands, eachSource) =>
          eachSource.toSourceOperand && {
            builtOperand =>
              operands.addSource(builtOperand)
          }
      }

    private def addDestinationOperand(operands: Operands, destination: AbstractAsmOperandBuilder): Operands OrElse SyntaxError =
      destination.toDestinationOperand && (t => operands.setDestination(t))
  }

  def <(sourceOperands: AbstractAsmOperandBuilder*): AbstractAsmInstructionBuilderWithInputs =
    new AbstractAsmInstructionBuilderWithInputs(instruction, sourceOperands)
}

object AbstractAsmInstructionBuilder {
  implicit def InstructionToAbstractAsmInstructionBuilder(instruction: Instruction): AbstractAsmInstructionBuilder =
    new AbstractAsmInstructionBuilder(instruction)
}
