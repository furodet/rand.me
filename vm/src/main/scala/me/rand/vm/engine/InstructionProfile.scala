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
import me.rand.vm.engine.InstructionProfile.InvocationProfiles
import me.rand.vm.main.VmError.SyntaxError.NoMatchingProfile
import me.rand.vm.main.{ExecutionContext, VmError}

import scala.language.implicitConversions

class InstructionProfile(instructionName: String, invocationProfiles: InvocationProfiles) {
  def applyToVariables(variables: Iterable[Variable])(implicit vmContext: VmContext, executionContext: ExecutionContext): VmContext OrElse VmError =
    for {
      profile <- invocationProfiles.findProfileMatching(variables, instructionName)
      result <- profile.compute(variables.toList, executionContext)
      newContext <- profile.update(result, vmContext, executionContext)
    } yield newContext
}

object InstructionProfile {
  type VariableType = Class[_ <: Variable]
  type ComputeFunction = (List[Variable], ExecutionContext) => Variable OrElse VmError
  type UpdateFunction = (Variable, VmContext, ExecutionContext) => VmContext OrElse VmError

  class InvocationProfiles(val profiles: List[InvocationProfile]) {
    def |(profile: InvocationProfile): InvocationProfiles =
      new InvocationProfiles(profiles = profiles :+ profile)

    def findProfileMatching(variables: Iterable[Variable], instructionName: String): InvocationProfile OrElse NoMatchingProfile =
      profiles.collectFirst {
        case profile if profile.matchesArguments(variables) =>
          profile
      } match {
        case None =>
          Err(NoMatchingProfile(instructionName, variables))

        case Some(profile) =>
          Ok(profile)
      }
  }

  object InvocationProfiles {
    def apply() = new InvocationProfiles(List.empty)
  }

  class InvocationProfile(signature: List[VariableType], val compute: ComputeFunction, val update: UpdateFunction) {
    def matchesArguments(variables: Iterable[Variable]): Boolean =
      (variables.size == signature.length) && variablesMatchTypes(variables, signature)

    private def variablesMatchTypes(variables: Iterable[Variable], types: List[InstructionProfile.VariableType]): Boolean =
      !variables.zip(types).exists {
        case (eachVariable, eachType) =>
          !eachVariable.getClass.isAssignableFrom(eachType)
      }
  }

  class PartialInvocationProfileBuilder(signature: List[VariableType], compute: ComputeFunction) {
    def &&(update: UpdateFunction): InvocationProfile =
      new InvocationProfile(signature, compute, update)
  }

  class InvocationProfileBuilder(signature: List[VariableType]) {
    def +(t: VariableType): InvocationProfileBuilder =
      new InvocationProfileBuilder(signature = signature :+ t)

    def ~>(compute: ComputeFunction): PartialInvocationProfileBuilder =
      new PartialInvocationProfileBuilder(signature, compute)
  }

  object InvocationProfile {
    def empty = new InvocationProfileBuilder(List.empty)
  }

  implicit def VariableTypeToInvocationProfileBuilder(t: VariableType): InvocationProfileBuilder =
    new InvocationProfileBuilder(List(t))
}
