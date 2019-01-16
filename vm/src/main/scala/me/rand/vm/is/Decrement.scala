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

import me.rand.commons.idioms.NormalizedNumber
import me.rand.commons.idioms.Status._
import me.rand.vm.alu.{Alu, VmRegister}
import me.rand.vm.engine.Variable.{Pointer, Scalar}
import me.rand.vm.engine._

object Decrement {
  lazy val shortName = "--"
  private lazy val normalized1 = NormalizedNumber.ByteToNormalizedNumber(1)

  // Note: in theory one may be able to decrement an instruction pointer, but that's absolute non-sense in
  // this application context.
  def apply(): Instruction =
    Instruction.called(shortName)
      .|(
        Instruction.Monadic(classOf[Scalar]).withComputeFunction {
          (x, _, _) =>
            val one = VmRegister.ofType(x.value.vmType).withValue(normalized1)
            Ok(Scalar.anonymous(Alu.sub(x.value, one)))
        }.withDefaultUpdateFunction
      )
      .|(
        Instruction.Monadic(classOf[Pointer.ToVariable.InTheHeap]).withComputeFunction {
          (x, _, _) =>
            Ok(Pointer.ToVariable.InTheHeap.anonymous(x.index - 1))
        }.withDefaultUpdateFunction
      )
      .|(
        Instruction.Monadic(classOf[Pointer.ToVariable.InTheStack]).withComputeFunction {
          (x, _, _) =>
            Ok(Pointer.ToVariable.InTheStack.anonymous(x.index - 1))
        }.withDefaultUpdateFunction
      )
}
