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
import me.rand.vm.main.VmError.VmExecutionError.VmFetchOperandError.InvalidPointerValue._
import me.rand.vm.main.VmError.VmExecutionError.VmFetchOperandError.{InvalidPointerValue, UndefinedVariable}

import scala.annotation.tailrec

class OperandReducer(operands: Operands) {
  def execute(implicit vmContext: VmContext): Result OrElse IllegalEncodingError =
    for {
      sources <- reduceSourceOperands(operands)
      destination <- DestinationOperandReducer.forOperand(operands.destination).toVariableOrNone
    } yield new Result(sources, destination)

  private def reduceSourceOperands(operands: Operands)(implicit vmContext: VmContext): List[Variable] OrElse IllegalEncodingError = {
    operands.sources.indices.tryFoldLeft(List.empty[Variable]) {
      case (list, eachOperandIndex) =>
        SourceOperandReducer.forOperand(operands.sources(eachOperandIndex)).toVariable && (v => list :+ v)
    }
  }

  private class SourceOperandReducer(operand: Operand.Source)(implicit vmContext: VmContext) {
    def toVariable: Variable OrElse IllegalEncodingError =
      operand match {
        case Operand.Source.Immediate(value) =>
          Ok(Scalar("imm", value))

        case variableOperand: Operand.Source.Variable =>
          matchVariable(variableOperand)

        case Operand.Source.Indirect(pointer, depth) =>
          matchVariable(pointer) & (reduceIndirect(_, depth))

        case Operand.Source.Indexed(pointer, index) =>
          matchArrayBase(pointer) & (reduceIndexed(_, index))

        case referenceOperand: Operand.Source.Reference =>
          matchReference(referenceOperand)
      }

    private def matchVariable(operand: Operand.Source.Variable): Variable OrElse IllegalEncodingError =
      operand match {
        case Operand.Source.Variable.InTheHeap(heapIndex) =>
          getVariable(vmContext.heap, heapIndex)

        case Operand.Source.Variable.InTheStack(stackIndex) =>
          getVariable(vmContext.stack, stackIndex)
      }

    private def matchReference(reference: Operand.Source.Reference): Variable.Pointer OrElse IllegalEncodingError =
      reference match {
        case Operand.Source.Reference.InTheHeap(heapIndex) =>
          getVariable(vmContext.heap, heapIndex) && (
            referenced => Variable.Pointer.ToVariable.InTheHeap(referenced.name, heapIndex)
            )

        case Operand.Source.Reference.InTheStack(stackIndex) =>
          getVariable(vmContext.stack, stackIndex) && (
            referenced => Variable.Pointer.ToVariable.InTheStack(referenced.name, stackIndex)
            )

        case Operand.Source.Reference.InInstructionMemory(basicBlockName) =>
          getBasicBlock(basicBlockName)
      }

    private def matchArrayBase(variable: Operand.Source.Variable): Pointer.ToVariable OrElse IllegalEncodingError =
      variable match {
        case Operand.Source.Variable.InTheHeap(index) =>
          getVariable(vmContext.heap, index) && (x => Pointer.ToVariable.InTheHeap(x.name, index))

        case Operand.Source.Variable.InTheStack(index) =>
          getVariable(vmContext.stack, index) && (x => Pointer.ToVariable.InTheStack(x.name, index))

        case _ =>
          Err(InvalidPointerValue.InvalidArrayBase("<undef>"))
      }

    @tailrec
    private def reduceIndirect(variable: Variable, depth: Int): Variable OrElse IllegalEncodingError =
      if (depth == 0) Ok(variable)
      else variable match {
        case pointer: Pointer.ToVariable =>
          val varSet = pointer.getContainingVarSet(vmContext)
          getVariable(varSet, pointer.index) match {
            case err@Err(_) =>
              err

            case Ok(target) =>
              reduceIndirect(target, depth - 1)
          }

        case _: Scalar =>
          Err(InvalidIndirect(variable.name))

        case _: Pointer.ToInstruction =>
          Err(InvalidIndirect(variable.name))
      }

    private def reduceIndexed(root: Variable, offset: Int)(implicit vmContext: VmContext): Variable OrElse IllegalEncodingError =
      root match {
        case Variable.Pointer.ToVariable.InTheHeap(_, baseIndex) =>
          getVariable(vmContext.heap, baseIndex + offset)

        case Variable.Pointer.ToVariable.InTheStack(_, baseIndex) =>
          getVariable(vmContext.stack, baseIndex + offset)

        case _ =>
          Err(InvalidPointerValue.InvalidArrayBase(root.name))
      }


    private def getVariable(varSet: VarSet, variableIndex: Int): Variable OrElse IllegalEncodingError =
      varSet.getVariable(variableIndex) match {
        case Err(error) =>
          Err(InvalidSourceReference(varSet.name, variableIndex, error))

        case Ok(None) =>
          Err(InvalidSourceReference(varSet.name, variableIndex, UndefinedVariable))

        case Ok(Some(variable)) =>
          Ok(variable)
      }

    private def getBasicBlock(basicBlockName: String): Pointer.ToInstruction OrElse InvalidBasicBlockReference =
      vmContext.program.basicBlocks.get(basicBlockName) match {
        case None =>
          Err(InvalidBasicBlockReference(basicBlockName))

        case Some(basicBlock) =>
          Ok(Variable.Pointer.ToInstruction(basicBlockName, VmProgram.Counter.atTheBeginningOf(basicBlock)))
      }
  }

  private object SourceOperandReducer {
    def forOperand(operand: Operand.Source)(implicit vmContext: VmContext) = new SourceOperandReducer(operand)
  }

  private class DestinationOperandReducer(operand: Operand.Destination)(implicit vmContext: VmContext) {
    def toVariableOrNone: Option[Pointer.ToVariable] OrElse IllegalEncodingError =
      operand match {
        case target: Operand.Destination.Variable =>
          matchVariable(target) && (Some(_))

        case Operand.Destination.Redirect(pointer, depth) =>
          matchVariable(pointer) & (reduceRedirect(_, depth)) && (Some(_))

        case Operand.Destination.Indexed(pointer, index) =>
          matchVariable(pointer) & (reduceIndexed(_, index)) && (Some(_))

        case Operand.Destination.NoDestination =>
          Ok(None)
      }

    private def matchVariable(operand: Operand.Destination.Variable): Pointer.ToVariable OrElse IllegalEncodingError =
      operand match {
        case Operand.Destination.Variable.InTheHeap(heapIndex) =>
          getVariable(vmContext.heap, None, heapIndex) && (
            v => Variable.Pointer.ToVariable.InTheHeap(s"&${v.name}", heapIndex)
            )

        case Operand.Destination.Variable.InTheStack(stackIndex) =>
          getVariable(vmContext.stack, None, stackIndex) && (
            v => Variable.Pointer.ToVariable.InTheStack(s"&${v.name}", stackIndex)
            )
      }

    private def reduceIndexed(root: Variable, offset: Int): Variable.Pointer.ToVariable OrElse IllegalEncodingError =
      root match {
        case Variable.Pointer.ToVariable.InTheHeap(_, baseIndex) =>
          getVariable(vmContext.heap, Some(s"${root.name}[$offset]"), baseIndex + offset) && (v => Variable.Pointer.ToVariable.InTheHeap(v.name, baseIndex + offset))

        case Variable.Pointer.ToVariable.InTheStack(_, baseIndex) =>
          getVariable(vmContext.stack, Some(s"${root.name}[$offset]"), baseIndex + offset) && (v => Variable.Pointer.ToVariable.InTheStack(v.name, baseIndex + offset))

        case _ =>
          Err(InvalidPointerValue.InvalidArrayBase(root.name))
      }

    @tailrec
    private def reduceRedirect(pointer: Variable, depth: Int)(implicit vmContext: VmContext): Pointer.ToVariable OrElse IllegalEncodingError =
      pointer match {
        case ptr: Pointer.ToVariable =>
          if (depth == 0)
            Ok(ptr)
          else getVariable(ptr.getContainingVarSet(vmContext), Some(ptr.name), ptr.index) match {
            case Err(error) =>
              Err(error)

            case Ok(nextPointer) =>
              reduceRedirect(nextPointer, depth - 1)
          }

        case _ =>
          Err(InvalidRedirect)
      }

    private def getVariable(varSet: VarSet, pointerName: Option[String], variableIndex: Int): Variable OrElse IllegalEncodingError =
      varSet.getVariable(variableIndex) match {
        case Err(error) =>
          Err(InvalidTargetReference(pointerName, variableIndex, Some(error)))

        case Ok(None) =>
          Err(InvalidTargetReference(pointerName, variableIndex, None))

        case Ok(Some(variable)) =>
          Ok(variable)
      }
  }

  private object DestinationOperandReducer {
    def forOperand(operand: Operand.Destination)(implicit vmContext: VmContext) = new DestinationOperandReducer(operand)
  }

}

object OperandReducer {
  def appliedTo(operands: Operands) = new OperandReducer(operands)

  class Result(val sources: List[Variable], val destination: Option[Variable.Pointer])

}