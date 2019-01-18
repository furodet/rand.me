/*
 * Copyright (c) 2018-2019 rand.me project
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
import me.rand.vm.engine.{Instruction, VmContext}

object Compare {
  private[is] def apply(comparator: Comparator): Instruction =
    Instruction.called(comparator.name)
      .|(
        Instruction.Dyadic(classOf[Scalar], classOf[Scalar])
          .withComputeFunction {
            (x, y, vmContext, _) =>
              val result = comparator.aluComparator(x.value, y.value)
              Ok(booleanToScalar(result)(vmContext))
          }.withDefaultUpdateFunction
      )
      .|(
        Instruction.Dyadic(classOf[Pointer.ToInstruction], classOf[Pointer.ToInstruction])
          .withComputeFunction {
            (x, y, vmContext, _) =>
              val result = (x.value.basicBlock == y.value.basicBlock) && comparator.intComparator(x.value.index, y.value.index)
              Ok(booleanToScalar(result)(vmContext))
          }.withDefaultUpdateFunction
      )
      .|(
        Instruction.Dyadic(classOf[Pointer.ToVariable.InTheHeap], classOf[Pointer.ToVariable.InTheHeap])
          .withComputeFunction {
            (x, y, vmContext, _) =>
              val result = comparator.intComparator(x.index, y.index)
              Ok(booleanToScalar(result)(vmContext))
          }.withDefaultUpdateFunction
      )
      .|(
        Instruction.Dyadic(classOf[Pointer.ToVariable.InTheStack], classOf[Pointer.ToVariable.InTheStack])
          .withComputeFunction {
            (x, y, vmContext, _) =>
              val result = comparator.intComparator(x.index, y.index)
              Ok(booleanToScalar(result)(vmContext))
          }.withDefaultUpdateFunction
      )


  private def booleanToScalar(b: Boolean)(vmContext: VmContext): Scalar = {
    val asRegister = VmRegister.ofType(vmContext.profile.vmTypes.minUnsignedType)
      .withValue(if (b) 1 else 0)
    Scalar.anonymous(asRegister)
  }
}
