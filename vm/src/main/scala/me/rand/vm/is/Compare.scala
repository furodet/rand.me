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

import me.rand.commons.idioms.NormalizedNumber._
import me.rand.commons.idioms.Status._
import me.rand.vm.alu.{Comparator, VmRegister}
import me.rand.vm.engine.Variable.{Pointer, Scalar}
import me.rand.vm.engine.{Instruction, UpdateVariable, Variable, VmContext}
import me.rand.vm.main.{ExecutionContext, VmError}

object Compare {
  private[is] def apply(comparator: Comparator): Instruction =
    Instruction.called(comparator.name)
      .|(
        Instruction.Dyadic(classOf[Scalar], classOf[Scalar])
          .computeIfMatch {
            (x, y, vmx, ecx) =>
              val result = comparator.aluComparator(x.value, y.value)
              ecx.logger ~> s"${comparator.name} $x $y => $result"
              Ok(booleanToScalar(result)(vmx))
          }
          .withUpdateFunction(standardUpdateFunction)
      )
      .|(
        Instruction.Dyadic(classOf[Pointer.ToInstruction], classOf[Pointer.ToInstruction])
          .computeIfMatch {
            (x, y, vmx, ecx) =>
              val result = (x.value.basicBlock == y.value.basicBlock) && comparator.intComparator(x.value.index, y.value.index)
              ecx.logger ~> s"${comparator.name} $x $y => $result"
              Ok(booleanToScalar(result)(vmx))
          }
          .withUpdateFunction(standardUpdateFunction)
      )
      .|(
        Instruction.Dyadic(classOf[Pointer.ToVariable.InTheHeap], classOf[Pointer.ToVariable.InTheHeap])
          .computeIfMatch {
            (x, y, vmx, ecx) =>
              val result = comparator.intComparator(x.index, y.index)
              ecx.logger ~> s"${comparator.name} $x $y => $result"
              Ok(booleanToScalar(result)(vmx))
          }.withUpdateFunction(standardUpdateFunction))
      .|(
        Instruction.Dyadic(classOf[Pointer.ToVariable.InTheStack], classOf[Pointer.ToVariable.InTheStack])
          .computeIfMatch {
            (x, y, vmx, ecx) =>
              val result = comparator.intComparator(x.index, y.index)
              ecx.logger ~> s"${comparator.name} $x $y => $result"
              Ok(booleanToScalar(result)(vmx))
          }.withUpdateFunction(standardUpdateFunction)

      )


  private def booleanToScalar(b: Boolean)(vmContext: VmContext): Scalar = {
    val asRegister = VmRegister.ofType(vmContext.vmTypes.minUnsignedType)
      .withValue(if (b) 1 else 0)
    Scalar.anonymous(asRegister)
  }

  private def standardUpdateFunction(result: Variable, out: Option[Variable.Pointer], vmContext: VmContext, executionContext: ExecutionContext): VmContext OrElse VmError =
    UpdateVariable.pointedBy(out).withValueOf(result)(vmContext, executionContext)

}
