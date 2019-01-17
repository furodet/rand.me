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

import me.rand.commons.idioms.Status._
import me.rand.vm.alu.Alu
import me.rand.vm.engine.Instruction
import me.rand.vm.engine.Variable.{Pointer, Scalar}

object SizeOf {
  lazy val shortName = "sizeof"

  private[is] def apply(): Instruction =
    Instruction.called("sizeof")
      .|(
        Instruction.Monadic(classOf[Scalar]).withComputeFunction {
          (x, vmContext, _) =>
            val result = Alu.sizeof(x.value)
            Ok(Scalar.anonymous(result, vmContext))
        }.withDefaultUpdateFunction
      )
      .|(
        Instruction.Monadic(classOf[Pointer.ToInstruction]).withComputeFunction {
          (_, vmContext, _) =>
            Ok(Scalar.anonymous(vmContext.profile.pointerTypes.toInstruction.byteLen, vmContext))
        }.withDefaultUpdateFunction
      )
      .|(
        Instruction.Monadic(classOf[Pointer.ToVariable.InTheHeap]).withComputeFunction {
          (_, vmContext, _) =>
            Ok(Scalar.anonymous(vmContext.profile.pointerTypes.toHeap.byteLen, vmContext))
        }.withDefaultUpdateFunction
      )
      .|(
        Instruction.Monadic(classOf[Pointer.ToVariable.InTheStack]).withComputeFunction {
          (_, vmContext, _) =>
            Ok(Scalar.anonymous(vmContext.profile.pointerTypes.toStack.byteLen, vmContext))
        }.withDefaultUpdateFunction
      )
}
