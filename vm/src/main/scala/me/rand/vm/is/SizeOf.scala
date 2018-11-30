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
import me.rand.vm.alu.{Alu, VmRegister}
import me.rand.vm.engine.Variable.{Pointer, Scalar}
import me.rand.vm.engine.{Instruction, UpdateVariable, Variable, VmContext}
import me.rand.vm.main.{ExecutionContext, VmError}

object SizeOf {
  lazy val shortName = "sizeof"

  private[is] def apply(): Instruction =
    Instruction.called("sizeof")
      .|(
        Instruction.Monadic(classOf[Scalar]).withComputeFunction {
          (x, vmContext, _) =>
            val result = Alu.sizeof(x.value)
            Ok(intToScalar(result, vmContext))
        }.withUpdateFunction(standardUpdateFunction)
      )
      .|(
        Instruction.Monadic(classOf[Pointer]).withComputeFunction {
          (_, vmContext, _) =>
            Ok(arbitrarySizeOfPointer(vmContext))
        }.withUpdateFunction(standardUpdateFunction)
      )

  private def arbitrarySizeOfPointer(vmContext: VmContext): Scalar =
  // TODO: size of pointers should be an optional configuration value, for any kind of pointer
    intToScalar(vmContext.vmTypes.maxUnsignedType.byteLen, vmContext)

  private def intToScalar(value: Int, vmContext: VmContext): Scalar = {
    val nrEncodingBits = 32 - Integer.numberOfLeadingZeros(value)
    val nrEncodingBytes = (nrEncodingBits + 7) / 8
    // It is safe to promote to the proper byte length: no machine would support N bytes and
    // log2(N) is not encodable on a machine word.
    val asRegister = VmRegister.normalize(vmContext.vmTypes.select(nrEncodingBytes, isSigned = false).get, value)
    Scalar.anonymous(asRegister)
  }

  private def standardUpdateFunction(result: Variable, out: Option[Variable.Pointer], vmContext: VmContext, executionContext: ExecutionContext): VmContext OrElse VmError =
    UpdateVariable.pointedBy(out).withValueOf(result)(vmContext, executionContext)
}
