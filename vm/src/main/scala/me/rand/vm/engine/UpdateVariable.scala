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
import me.rand.vm.engine.Variable.Pointer
import me.rand.vm.main.ExecutionContext
import me.rand.vm.main.VmError.VmExecutionError.IllegalEncodingError
import me.rand.vm.main.VmError.VmExecutionError.VmFetchOperandError.InvalidPointerValue.{IllegalDestinationPointer, InvalidRedirect, InvalidTargetReference}

import scala.annotation.tailrec

class UpdateVariable(maybePointer: Option[Pointer]) {
  // Instruction Helpers: update a destination variable
  def withValueOf(variable: Variable)(implicit vmContext: VmContext, executionContext: ExecutionContext): VmContext OrElse IllegalEncodingError =
    maybePointer match {
      case None =>
        Ok(vmContext)

      case Some(pointer) =>
        updateDestination(pointer, variable) && {
          updated =>
            executionContext.logger ~> s"UPDATE ${updated.name} ${updated.getValueString}"
            // Due to internal mutability of varsets, updated VM context is equal to the initial VM context object.
            vmContext
        }
    }

  private def updateDestination(pointer: Pointer, variable: Variable)(implicit vmContext: VmContext): Variable OrElse IllegalEncodingError =
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

object UpdateVariable {
  def pointedBy(maybePointer: Option[Pointer]) = new UpdateVariable(maybePointer)

  def pointedBy(pointer: Pointer) = new UpdateVariable(Some(pointer))
}
