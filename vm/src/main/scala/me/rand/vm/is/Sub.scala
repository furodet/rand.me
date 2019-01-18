/*-
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

import me.rand.commons.idioms.NormalizedNumber
import me.rand.commons.idioms.Status.Ok
import me.rand.vm.alu.{Alu, VmRegister}
import me.rand.vm.engine.Variable.Scalar
import me.rand.vm.engine.{Instruction, Variable}

object Sub {
  lazy val shortName = "-"

  // Note: in theory one may be able to sub two instruction pointers, but that's absolute non-sense in
  // this application context.
  // Also notice that pointer difference is designed to match the case of arrays and nothing else.
  private[is] def apply(): Instruction =
    Instruction.called(shortName)
      .|(
        Instruction.Dyadic(classOf[Scalar], classOf[Scalar])
          .withComputeFunction {
            (x, y, _, _) =>
              Ok(Scalar.anonymous(Alu.add(x.value, y.value)))
          }.withDefaultUpdateFunction
      ).|(
      Instruction.Dyadic(classOf[Variable.Pointer.ToVariable.InTheHeap], classOf[Variable.Pointer.ToVariable.InTheHeap])
        .withComputeFunction {
          (x, y, vmContext, _) =>
            val diff = x.index - y.index
            val result = VmRegister.ofType(vmContext.profile.vmTypes.maxUnsignedType).withValue(NormalizedNumber.IntToNormalizedNumber(diff))
            Ok(Scalar.anonymous(result))
        }.withDefaultUpdateFunction
    ).|(
      Instruction.Dyadic(classOf[Variable.Pointer.ToVariable.InTheStack], classOf[Variable.Pointer.ToVariable.InTheStack])
        .withComputeFunction {
          (x, y, vmContext, _) =>
            val diff = x.index - y.index
            val result = VmRegister.ofType(vmContext.profile.vmTypes.maxUnsignedType).withValue(NormalizedNumber.IntToNormalizedNumber(diff))
            Ok(Scalar.anonymous(result))
        }.withDefaultUpdateFunction
    )
}
