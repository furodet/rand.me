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
import me.rand.vm.engine.OperandReducer.Result
import me.rand.vm.engine.Variable.{Pointer, Scalar}
import me.rand.vm.main.VmError.VmExecutionError.IllegalEncodingError
import me.rand.vm.main.VmError.VmExecutionError.VmFetchOperandError.InvalidPointerValue
import me.rand.vm.main.VmError.VmExecutionError.VmFetchOperandError.InvalidPointerValue.{InvalidIndirect, InvalidRedirect, InvalidSourceReference, InvalidTargetReference}

import scala.annotation.tailrec

class OperandReducer(operands: Operands) {
  def execute(implicit vmContext: VmContext): Result OrElse IllegalEncodingError =
    for {
      sources <- reduceSourceOperands(operands)
      destination <- reduceDestinationOrNone(operands)
    } yield new Result(sources, destination)

  private def reduceSourceOperands(operands: Operands)(implicit vmContext: VmContext): List[Variable] OrElse IllegalEncodingError = {
    operands.sources.indices.tryFoldLeft(List.empty[Variable]) {
      case (list, eachOperandIndex) =>
        reduceImmediateOrVariable(eachOperandIndex, operands.sources(eachOperandIndex)) && (v => list :+ v)
    }
  }

  private def reduceImmediateOrVariable(operandId: Int, operand: Operand.Source)(implicit vmContext: VmContext): Variable OrElse IllegalEncodingError =
    operand match {
      case Operand.Source.Immediate(value) =>
        Ok(Scalar("imm", value))

      case variableOperand: Operand.Source.Variable =>
        reduceSourceVariableOperand(variableOperand)

      case Operand.Source.Indirect(pointer, depth) =>
        for {
          pointerVariable <- reduceSourceVariableOperand(pointer)
          afterIndirect <- reduceIndirect(pointerVariable, depth, operandId)
        } yield afterIndirect

      case Operand.Source.Indexed(pointer, index) =>
        for {
          pointerVariable <- reduceSourceVariableOperand(pointer)
          afterIndexing <- reduceIndexedVariableReference(pointerVariable, index)
        } yield afterIndexing

      case referenceOperand: Operand.Source.Reference =>
        reduceReferenceVariableOperand(referenceOperand)
    }

  private def reduceDestinationOrNone(operands: Operands)(implicit vmContext: VmContext): Option[Pointer.ToVariable] OrElse IllegalEncodingError =
    operands.destination match {
      case target: Operand.Destination.Variable =>
        reduceDestinationVariableOperand(target) && (Some(_))

      case Operand.Destination.Redirect(pointer, depth) =>
        for {
          firstPointer <- reduceDestinationVariableOperand(pointer)
          redirected <- reduceRedirect(firstPointer, depth)
        } yield Some(redirected)

      case Operand.Destination.Indexed(pointer, index) =>
        for {
          baseVariable <- reduceDestinationVariableOperand(pointer)
          variable <- reduceIndexedVariableReferenceToPointer(baseVariable, index)
        } yield Some(variable)

      case Operand.Destination.NoDestination =>
        Ok(None)
    }

  private def reduceSourceVariableOperand(operand: Operand.Source.Variable)(implicit vmContext: VmContext): Variable OrElse IllegalEncodingError =
    operand match {
      case Operand.Source.Variable.InTheHeap(heapIndex) =>
        reduceSourceVariable("heap", vmContext.heap, heapIndex)

      case Operand.Source.Variable.InTheStack(stackIndex) =>
        reduceSourceVariable("stack", vmContext.stack, stackIndex)
    }

  private def reduceReferenceVariableOperand(reference: Operand.Source.Reference)(implicit vmContext: VmContext): Variable.Pointer OrElse IllegalEncodingError =
    reference match {
      case Operand.Source.Reference.InTheHeap(heapIndex) =>
        reduceSourceVariable("heap", vmContext.heap, heapIndex) && (
          referenced => Variable.Pointer.ToVariable.InTheHeap(referenced.name, heapIndex)
          )

      case Operand.Source.Reference.InTheStack(stackIndex) =>
        reduceSourceVariable("stack", vmContext.stack, stackIndex) && (
          referenced => Variable.Pointer.ToVariable.InTheStack(referenced.name, stackIndex)
          )
    }

  @tailrec
  private def reduceIndirect(variable: Variable, depth: Int, operandId: Int)(implicit vmContext: VmContext): Variable OrElse IllegalEncodingError =
    if (depth == 0) Ok(variable)
    else variable match {
      case pointer: Pointer.ToVariable =>
        reduceTargetVariable(pointer.getContainingVarSet(vmContext), Some(pointer.name), pointer.index) match {
          case err@Err(_) =>
            err

          case Ok(target) =>
            reduceIndirect(target, depth - 1, operandId)
        }

      case _: Scalar =>
        Err(InvalidIndirect(operandId))

      case _: Pointer.ToInstruction =>
        Err(InvalidIndirect(operandId))
    }

  private def reduceIndexedVariableReference(root: Variable, offset: Int)(implicit vmContext: VmContext): Variable OrElse IllegalEncodingError =
    root match {
      case Variable.Pointer.ToVariable.InTheHeap(_, baseIndex) =>
        reduceSourceVariable("heap", vmContext.heap, baseIndex + offset)

      case Variable.Pointer.ToVariable.InTheStack(_, baseIndex) =>
        reduceSourceVariable("stack", vmContext.stack, baseIndex + offset)

      case _ =>
        Err(InvalidPointerValue.InvalidArrayBase(root.name))
    }

  private def reduceIndexedVariableReferenceToPointer(root: Variable, offset: Int)(implicit vmContext: VmContext): Variable.Pointer.ToVariable OrElse IllegalEncodingError =
    root match {
      case Variable.Pointer.ToVariable.InTheHeap(_, baseIndex) =>
        reduceSourceVariable("heap", vmContext.heap, baseIndex + offset) && (v => Variable.Pointer.ToVariable.InTheHeap(v.name, baseIndex + offset))

      case Variable.Pointer.ToVariable.InTheStack(_, baseIndex) =>
        reduceSourceVariable("stack", vmContext.stack, baseIndex + offset) && (v => Variable.Pointer.ToVariable.InTheStack(v.name, baseIndex + offset))

      case _ =>
        Err(InvalidPointerValue.InvalidArrayBase(root.name))
    }

  private def reduceTargetVariable(varSet: VarSet, pointerName: Option[String], variableIndex: Int): Variable OrElse IllegalEncodingError =
    varSet.getVariable(variableIndex) match {
      case Err(error) =>
        Err(InvalidTargetReference(pointerName, variableIndex, Some(error)))

      case Ok(None) =>
        Err(InvalidTargetReference(pointerName, variableIndex, None))

      case Ok(Some(variable)) =>
        Ok(variable)
    }

  private def reduceSourceVariable(sourceName: String, varSet: VarSet, variableIndex: Int): Variable OrElse IllegalEncodingError =
    varSet.getVariable(variableIndex) match {
      case Err(error) =>
        Err(InvalidSourceReference(sourceName, variableIndex, Some(error)))

      case Ok(None) =>
        Err(InvalidSourceReference(sourceName, variableIndex, None))

      case Ok(Some(variable)) =>
        Ok(variable)
    }

  private def reduceDestinationVariableOperand(operand: Operand.Destination.Variable)(implicit vmContext: VmContext): Pointer.ToVariable OrElse IllegalEncodingError =
    operand match {
      case Operand.Destination.Variable.InTheHeap(heapIndex) =>
        reduceTargetVariable(vmContext.heap, None, heapIndex) && (
          v => Variable.Pointer.ToVariable.InTheHeap(s"&${v.name}", heapIndex)
          )

      case Operand.Destination.Variable.InTheStack(stackIndex) =>
        reduceTargetVariable(vmContext.stack, None, stackIndex) && (
          v => Variable.Pointer.ToVariable.InTheStack(s"&${v.name}", stackIndex)
          )
    }

  @tailrec
  private def reduceRedirect(pointer: Variable, depth: Int)(implicit vmContext: VmContext): Pointer.ToVariable OrElse IllegalEncodingError =
    pointer match {
      case ptr: Pointer.ToVariable =>
        if (depth == 0)
          Ok(ptr)
        else reduceTargetVariable(ptr.getContainingVarSet(vmContext), Some(ptr.name), ptr.index) match {
          case Err(error) =>
            Err(error)

          case Ok(nextPointer) =>
            reduceRedirect(nextPointer, depth - 1)
        }

      case _ =>
        Err(InvalidRedirect)
    }
}

object OperandReducer {
  def appliedTo(operands: Operands) = new OperandReducer(operands)

  class Result(val sources: List[Variable], val destination: Option[Variable.Pointer])

}