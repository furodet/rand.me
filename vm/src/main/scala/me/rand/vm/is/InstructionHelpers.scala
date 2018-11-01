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
import me.rand.vm.engine.Instruction.Operand.{DestinationOperand, Source}
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
      case Source.Immediate(v) =>
        Ok(v)

      case _ =>
        Err(NotAnImmediateOperand(operandId))
    }

  private[is] def fetchImmediateOrVariable(operandId: Int, operands: Operands)(implicit vmContext: VmContext): Variable OrElse IllegalEncodingError =
    operands.fetchSource(operandId) & {
      case Source.Immediate(value) =>
        Ok(Scalar("imm", value))

      case variableOperand: Source.Variable =>
        fetchSourceVariableOperand(variableOperand)

      case Source.Indirect(pointer, depth) =>
        for {
          pointerVariable <- fetchSourceVariableOperand(pointer)
          afterIndirect <- fetchIndirect(pointerVariable, depth, operandId)
        } yield afterIndirect

      case referenceOperand: Source.Reference =>
        fetchReferenceVariableOperand(referenceOperand)
    }

  private def fetchSourceVariableOperand(operand: Source.Variable)(implicit vmContext: VmContext): Variable OrElse IllegalEncodingError =
    operand match {
      case Source.Variable.InTheHeap(heapIndex) =>
        fetchSourceVariable("heap", vmContext.heap, heapIndex)

      case Source.Variable.InTheStack(stackIndex) =>
        fetchSourceVariable("stack", vmContext.stack, stackIndex)
    }

  private def fetchReferenceVariableOperand(reference: Source.Reference)(implicit vmContext: VmContext): Variable.Pointer OrElse IllegalEncodingError =
    reference match {
      case Source.Reference.InTheHeap(heapIndex) =>
        fetchSourceVariable("heap", vmContext.heap, heapIndex) && (
          referenced => Variable.Pointer.ToVariable.InTheHeap(referenced.name, heapIndex)
          )

      case Source.Reference.InTheStack(stackIndex) =>
        fetchSourceVariable("stack", vmContext.heap, stackIndex) && (
          referenced => Variable.Pointer.ToVariable.InTheStack(referenced.name, stackIndex)
          )
    }

  @tailrec
  private def fetchIndirect(variable: Variable, depth: Int, operandId: Int)(implicit vmContext: VmContext): Variable OrElse IllegalEncodingError =
    if (depth == 0) Ok(variable)
    else variable match {
      case pointer: Pointer.ToVariable =>
        fetchTargetVariable(pointer.getContainingVarSet(vmContext), Some(pointer.name), pointer.index) match {
          case err@Err(_) =>
            err

          case Ok(target) =>
            fetchIndirect(target, depth - 1, operandId)
        }

      case _: Scalar =>
        Err(InvalidIndirect(operandId))

      case _: Pointer.ToInstruction =>
        Err(InvalidIndirect(operandId))
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
      targetVariable <- fetchTargetVariable(varSet, Some(pointerName), variableIndex)
      mergedVariable = newValue.rename(targetVariable.name)
      // putVariable could not fail here: we just fetched the name at the same index.
      _ = varSet.putVariable(variableIndex, mergedVariable)
    } yield mergedVariable

  private def fetchTargetVariable(varSet: VarSet, pointerName: Option[String], variableIndex: Int): Variable OrElse IllegalEncodingError =
    varSet.getVariable(variableIndex) match {
      case Err(error) =>
        Err(InvalidTargetReference(pointerName, variableIndex, Some(error)))

      case Ok(None) =>
        Err(InvalidTargetReference(pointerName, variableIndex, None))

      case Ok(Some(variable)) =>
        Ok(variable)
    }

  private def fetchSourceVariable(sourceName: String, varSet: VarSet, variableIndex: Int): Variable OrElse IllegalEncodingError =
    varSet.getVariable(variableIndex) match {
      case Err(error) =>
        Err(InvalidSourceReference(sourceName, variableIndex, Some(error)))

      case Ok(None) =>
        Err(InvalidSourceReference(sourceName, variableIndex, None))

      case Ok(Some(variable)) =>
        Ok(variable)
    }

  private[is] def fetchDestination(operands: Operands)(implicit vmContext: VmContext): Pointer.ToVariable OrElse IllegalEncodingError =
    operands.destination match {
      case target: DestinationOperand.TargetVariable =>
        fetchDestinationVariableOperand(target)

      case DestinationOperand.Redirect(pointer, depth) =>
        for {
          firstPointer <- fetchDestinationVariableOperand(pointer)
          redirected <- fetchRedirect(firstPointer, depth)
        } yield redirected

      case DestinationOperand.NoDestination =>
        Err(IllegalEncodingError.UnspecifiedDestinationOperand)
    }

  private def fetchDestinationVariableOperand(operand: DestinationOperand.TargetVariable)(implicit vmContext: VmContext): Pointer.ToVariable OrElse IllegalEncodingError =
    operand match {
      case DestinationOperand.TargetVariable.InTheHeap(heapIndex) =>
        fetchTargetVariable(vmContext.heap, None, heapIndex) && (
          v => Variable.Pointer.ToVariable.InTheHeap(s"&${v.name}", heapIndex)
          )

      case DestinationOperand.TargetVariable.InTheStack(stackIndex) =>
        fetchTargetVariable(vmContext.stack, None, stackIndex) && (
          v => Variable.Pointer.ToVariable.InTheStack(s"&${v.name}", stackIndex)
          )
    }

  @tailrec
  private def fetchRedirect(pointer: Variable, depth: Int)(implicit vmContext: VmContext): Pointer.ToVariable OrElse IllegalEncodingError =
    pointer match {
      case ptr: Pointer.ToVariable =>
        if (depth == 0)
          Ok(ptr)
        else fetchTargetVariable(ptr.getContainingVarSet(vmContext), Some(ptr.name), ptr.index) match {
          case Err(error) =>
            Err(error)

          case Ok(nextPointer) =>
            fetchRedirect(nextPointer, depth - 1)
        }

      case _ =>
        Err(InvalidRedirect)
    }
}
