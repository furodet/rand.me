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
package me.rand.vm.engine

import me.rand.commons.idioms.Status._
import me.rand.vm.main.VmError.SyntaxError.NoMatchingProfile
import me.rand.vm.main.{ExecutionContext, VmError}

// Documentation: doc/vmarchitecture.md
class Instruction(val name: String, signatures: Instruction.Signatures) {
  def execute(operands: Operands)(implicit vmContext: VmContext, executionContext: ExecutionContext): VmContext OrElse VmError = {
    executionContext.logger ~> s"RUN $name"
    traceOperands(operands, executionContext)
    for {
      reduced <- OperandReducer.appliedTo(operands).execute
      functions <- signatures.searchSignatureMatching(name, reduced.sources)
      computeResult <- functions.compute(vmContext, executionContext)
      updateResult <- functions.update(computeResult, reduced.destination, vmContext, executionContext)
    } yield updateResult
  }

  private def traceOperands(operands: Operands, executionContext: ExecutionContext): Unit = {
    operands.sources.foreach(operand => executionContext.logger ~> s"<- $operand")
    executionContext.logger ~> s"-> ${operands.destination}"
  }

  // Encapsulate signatures constructor for simplicity
  def |(newSignature: Instruction.Signature): Instruction =
    new Instruction(name, signatures | newSignature)

  override def toString: String = s"$name $signatures"
}

object Instruction {
  type UpdateFunction = (Variable, Option[Variable.Pointer], VmContext, ExecutionContext) => VmContext OrElse VmError
  type ComputeFunction = (VmContext, ExecutionContext) => Variable OrElse VmError

  def called(name: String): Instruction = new Instruction(name, Signatures.empty)

  // Nifty decorator, not super useful, but prefer to phrase it that wai
  protected class ComputeUpdateFunctions(val compute: ComputeFunction, val update: UpdateFunction)

  class Signatures(list: List[Instruction.Signature]) {
    private lazy val signatureString =
      list.map(_.getVariableTypes.map(_.getSimpleName).mkString("(", ",", ")")).mkString("[", ",", "]")

    def |(newSignature: Signature): Signatures =
      new Signatures(list :+ newSignature)

    def searchSignatureMatching(instructionName: String, variables: List[Variable]): ComputeUpdateFunctions OrElse NoMatchingProfile = {
      list.foreach {
        eachSignature =>
          eachSignature.getComputeFunctionIfVariablesMatch(variables) match {
            case None =>
            // Try next signature

            case Some(computeFunction) =>
              return Ok(new ComputeUpdateFunctions(computeFunction, eachSignature.getUpdateFunction))
          }
      }
      Err(NoMatchingProfile(instructionName, variables))
    }

    override def toString: String = signatureString
  }

  object Signatures {
    def empty: Signatures = new Signatures(List.empty)
  }

  sealed trait Signature {
    def getComputeFunctionIfVariablesMatch(variables: Iterable[Variable]): Option[ComputeFunction]

    def getUpdateFunction: UpdateFunction

    def getVariableTypes: List[Class[_]]
  }

  class PartialSignature(fetch: Iterable[Variable] => Option[ComputeFunction], vts: List[Class[_]]) {
    def withUpdateFunction(updateFunction: UpdateFunction): Signature =
      new Signature {
        override def getComputeFunctionIfVariablesMatch(variables: Iterable[Variable]): Option[ComputeFunction] =
          fetch(variables)

        override def getUpdateFunction: UpdateFunction = updateFunction

        override def getVariableTypes: List[Class[_]] = vts
      }

    def withDefaultUpdateFunction: Signature =
      withUpdateFunction(verifyThatResultIsValidThenUpdate)
  }

  private def verifyThatResultIsValidThenUpdate(result: Variable, out: Option[Variable.Pointer], vmContext: VmContext, executionContext: ExecutionContext): VmContext OrElse VmError =
    verifyThatResultIsValid(result, vmContext) & (_ => UpdateVariable.pointedBy(out).withValueOf(result)(vmContext, executionContext))

  private def verifyThatResultIsValid(variable: Variable, vmContext: VmContext): Variable OrElse VmError =
    variable match {
      case scalar: Variable.Scalar =>
        Ok(scalar)

      case Variable.Pointer.ToVariable.InTheHeap(name, index) =>
        UpdateVariable.fetchTargetVariable(vmContext.heap, Some(name), index)

      case Variable.Pointer.ToVariable.InTheStack(name, index) =>
        UpdateVariable.fetchTargetVariable(vmContext.stack, Some(name), index)

      case address: Variable.Pointer.ToInstruction =>
        Ok(address)
    }

  case class Zeroadic() {
    def withComputeFunction(f: (VmContext, ExecutionContext) => Variable OrElse VmError) =
      new PartialSignature(
        variables =>
          if (variables.isEmpty)
            Some((vmContext: VmContext, executionContext: ExecutionContext) => {
              f(vmContext, executionContext) && {
                result =>
                  executionContext.logger ~> s"=> ${result.getValueString}"
                  result
              }
            })
          else None,
        List.empty
      )
  }

  case class Monadic[T1 <: Variable](vt1: Class[T1]) {
    def withComputeFunction(f: (T1, VmContext, ExecutionContext) => Variable OrElse VmError) =
      new PartialSignature(
        variables =>
          if ((variables.size == 1) && variableTypeCompliesWith(variables.head, vt1))
            Some((vmContext: VmContext, executionContext: ExecutionContext) => {
              val x = variables.head.asInstanceOf[T1]
              f(x, vmContext, executionContext) && {
                result =>
                  executionContext.logger ~> s"IN ${x.name} ${x.getValueString}"
                  executionContext.logger ~> s"=> ${result.getValueString}"
                  result
              }
            })
          else None,
        List(vt1)
      )
  }

  case class Dyadic[T1 <: Variable, T2 <: Variable](vt1: Class[T1], vt2: Class[T2]) {
    def withComputeFunction(f: (T1, T2, VmContext, ExecutionContext) => Variable OrElse VmError) =
      new PartialSignature(
        variables =>
          if ((variables.size == 2) &&
            variableTypeCompliesWith(variables.head, vt1) &&
            variableTypeCompliesWith(variables.tail.head, vt2))
            Some((vmContext: VmContext, executionContext: ExecutionContext) => {
              val x = variables.head.asInstanceOf[T1]
              val y = variables.tail.head.asInstanceOf[T2]
              f(x, y, vmContext, executionContext) && {
                result =>
                  executionContext.logger ~> s"IN ${x.name} ${x.getValueString}"
                  executionContext.logger ~> s"IN ${y.name} ${y.getValueString}"
                  executionContext.logger ~> s"=> ${result.getValueString}"
                  result
              }
            })
          else None,
        List(vt1, vt2)
      )
  }

  private def variableTypeCompliesWith[T <: Variable](variable: Variable, vt: Class[T]): Boolean =
    vt.isAssignableFrom(variable.getClass)
}
