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
package me.rand.vm.is

import me.rand.commons.idioms.Status._
import me.rand.vm.engine.Instruction.Operand.SourceOperand
import me.rand.vm.engine.Instruction.Operands
import me.rand.vm.engine.Variable._
import me.rand.vm.engine._
import me.rand.vm.main.VmError.VmExecutionError
import me.rand.vm.main.VmError.VmExecutionError.IllegalEncoding.InvalidWordOperand
import me.rand.vm.main.VmError.VmExecutionError.InvalidPointerValue.{InvalidHeapReference, InvalidStackReference}
import me.rand.vm.main.VmError.VmExecutionError.UndefinedVariableValue.{UndefinedHeapVariableValue, UndefinedStackVariableValue}

import scala.annotation.tailrec

object InstructionHelpers {
  private[is] def fetchOperandValue(operandId: Int, operands: Operands)(implicit vmContext: VmContext): VmWord OrElse VmExecutionError =
    operands.fetchSource(operandId) match {
      case Err(error) =>
        Err(error)

      case Ok(SourceOperand(operand)) =>
        traverseOperandsToFetchAWord(operand, operandId)
    }

  @tailrec
  private def traverseOperandsToFetchAWord(operand: Variable, operandId: Int)(implicit vmContext: VmContext): VmWord OrElse VmExecutionError =
    operand match {
      case Scalar(_, w) =>
        Ok(w)

      case pointer@Pointer.ToVariable.InTheHeap(_, index) =>
        vmContext.heap.getVariable(index) match {
          case Err(contextError) =>
            Err(InvalidHeapReference(pointer, contextError))

          case Ok(None) =>
            Err(UndefinedHeapVariableValue(pointer))

          case Ok(Some(variable)) =>
            traverseOperandsToFetchAWord(variable, operandId)
        }

      case pointer@Pointer.ToVariable.InTheStack(_, index) =>
        vmContext.stack.getVariable(index) match {
          case Err(contextError) =>
            Err(InvalidStackReference(pointer, contextError))

          case Ok(None) =>
            Err(UndefinedStackVariableValue(pointer))

          case Ok(Some(variable)) =>
            traverseOperandsToFetchAWord(variable, operandId)
        }

      case _: Pointer.ToInstruction =>
        Err(InvalidWordOperand(operandId))
    }
}
