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

import me.rand.commons.idioms.Status.OrElse
import me.rand.vm.engine.Instruction.Operands
import me.rand.vm.engine.{Instruction, Variable, VmContext}
import me.rand.vm.main.{ExecutionContext, VmError}

private[is] trait StandardMonadicInstruction extends Instruction {
  def executeOperation(x: Variable): Variable OrElse VmError

  def operationName: String

  def execute(vmContext: VmContext, operands: Operands)(implicit executionContext: ExecutionContext): VmContext OrElse VmError = {
    implicit val context: VmContext = vmContext
    for {
      op <- InstructionHelpers.fetchImmediateOrVariable(0, operands)
      destination <- InstructionHelpers.fetchDestination(operands)
      result <- executeOperation(op)
      update <- InstructionHelpers.updateDestination(destination, result)
      _ = executionContext.logger ~> s"${destination.name} := $operationName ${op.getValueString} = ${update.getValueString}"
    } yield vmContext
  }
}
