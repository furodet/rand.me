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
import me.rand.vm.engine.Instruction.Operand.DestinationOperand.{NoDestination, Redirections, ToStackVariable}
import me.rand.vm.engine.Instruction.Operand.{DestinationOperand, SourceOperand}
import me.rand.vm.engine.Instruction.Operands
import me.rand.vm.engine.Variable._
import me.rand.vm.engine._
import me.rand.vm.main.VmError.VmExecutionError.IllegalEncodingError
import me.rand.vm.main.VmError.VmExecutionError.VmFetchOperandError.InvalidPointerValue._
import me.rand.vm.main.VmError.VmExecutionError.VmFetchOperandError.NotAnImmediateOperand

import scala.annotation.tailrec

object InstructionHelpers {
  private[is] def fetchImmediateOperandValue(operandId: Int, operands: Operands): VmWord OrElse IllegalEncodingError =
    operands.fetchSource(operandId) & {
      case SourceOperand.Immediate(v) =>
        Ok(v)

      case _ =>
        Err(NotAnImmediateOperand(operandId))
    }

  private[is] def fetchImmediateOrVariable(operandId: Int, operands: Operands)(implicit vmContext: VmContext): Variable OrElse IllegalEncodingError =
    operands.fetchSource(operandId) & {
      case SourceOperand.Immediate(value) =>
        Ok(Scalar("imm", value))

      case SourceOperand.ToVariable(variable) =>
        Ok(variable)

      case SourceOperand.Indirections(pointer, nrIndirections) =>
        fetchIndirections(pointer, nrIndirections, operandId)
    }

  @tailrec
  private def fetchIndirections(variable: Variable, nrIndirections: Int, operandId: Int)(implicit vmContext: VmContext): Variable OrElse IllegalEncodingError =
    if (nrIndirections == 0) Ok(variable)
    else variable match {
      case pointer: Pointer.ToVariable =>
        fetchTargetVariable(pointer.getContainingVarSet(vmContext), pointer.name, pointer.index) match {
          case err@Err(_) =>
            err

          case Ok(target) =>
            fetchIndirections(target, nrIndirections - 1, operandId)
        }

      case _: Scalar =>
        Err(InvalidIndirection(operandId))

      case _: Pointer.ToInstruction =>
        Err(InvalidIndirection(operandId))
    }

  private[is] def updateDestination(pointer: Pointer, variable: Variable)(implicit vmContext: VmContext): Variable OrElse IllegalEncodingError =
    pointer match {
      case ptr: Pointer.ToVariable =>
        updateDestination(ptr.getContainingVarSet(vmContext), ptr.name, ptr.index, variable)

      case _: Pointer.ToInstruction =>
        Err(IllegalDestinationPointer)
    }

  private def updateDestination(varSet: VarSet,
                                pointerName: String, variableIndex: Int,
                                newValue: Variable): Variable OrElse IllegalEncodingError =
    for {
      targetVariable <- fetchTargetVariable(varSet, pointerName, variableIndex)
      mergedVariable = newValue.rename(targetVariable.name)
      // putVariable could not fail here: we just fetched the name at the same index.
      _ = varSet.putVariable(variableIndex, mergedVariable)
    } yield mergedVariable

  private def fetchTargetVariable(varSet: VarSet, pointerName: String, variableIndex: Int): Variable OrElse IllegalEncodingError =
    varSet.getVariable(variableIndex) match {
      case Err(error) =>
        Err(InvalidTargetReference(pointerName, variableIndex, Some(error)))

      case Ok(None) =>
        Err(InvalidTargetReference(pointerName, variableIndex, None))

      case Ok(Some(variable)) =>
        Ok(variable)
    }

  private[is] def fetchDestination(operands: Operands)(implicit vmContext: VmContext): Pointer.ToVariable OrElse IllegalEncodingError =
    operands.destination match {
      case DestinationOperand.ToHeapVariable(index) =>
        Ok(Variable.Pointer.ToVariable.InTheHeap("<undef>", index))

      case ToStackVariable(index) =>
        Ok(Variable.Pointer.ToVariable.InTheStack("<undef>", index))

      case Redirections(pointer, nrRedirections) =>
        fetchRedirections(pointer, nrRedirections)

      case NoDestination =>
        Err(IllegalEncodingError.UnspecifiedDestinationOperand)
    }

  @tailrec
  private def fetchRedirections(pointer: Variable, nrRedirections: Int)(implicit vmContext: VmContext): Pointer.ToVariable OrElse IllegalEncodingError =
    pointer match {
      case ptr: Pointer.ToVariable =>
        if (nrRedirections == 1)
          Ok(ptr)
        else fetchTargetVariable(ptr.getContainingVarSet(vmContext), ptr.name, ptr.index) match {
          case Err(error) =>
            Err(error)

          case Ok(nextPointer) =>
            fetchRedirections(nextPointer, nrRedirections - 1)
        }

      case _ =>
        Err(InvalidRedirection)
    }
}
