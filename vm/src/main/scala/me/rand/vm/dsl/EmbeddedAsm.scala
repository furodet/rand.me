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
package me.rand.vm.dsl

import me.rand.commons.idioms.Status._
import me.rand.vm.dsl.EmbeddedAsm.VariableLocation.{HeapVariable, StackVariable}
import me.rand.vm.engine.Instruction.Operand.{DestinationOperand, Source}
import me.rand.vm.engine.Instruction.{Operand, Operands}
import me.rand.vm.engine.VmProgram.InstructionInstance
import me.rand.vm.engine.{Instruction, VmContext, VmTypes, VmWord}
import me.rand.vm.main.VmError.SyntaxError

import scala.language.implicitConversions

// The goal of this embedded assembler is to provide a simple DSL to quickly
// create instructions and programs for rand.me virtual machine in Scala.
// One may import all the elements defined by this object to forge instructions,
// operands...
// It's also a quick assembly language reference for a higher level of abstraction
// and function.
// The assembler has its own set of errors, independent from the Virtual Machine
// errors, to keep a good level of abstraction between the two layers.
object EmbeddedAsm {

  // In order to forge a series of operands, given that each construction potentially
  // fails, define intermediate builders, which are "tryForEach" when the expression
  // is complete.
  sealed trait AsmSourceOperandBuilder {
    def build: Operand.Source OrElse SyntaxError
  }

  sealed trait AsmDestinationOperandBuilder {
    def build: Operand.DestinationOperand
  }

  sealed trait VariableLocation

  object VariableLocation {

    object HeapVariable extends VariableLocation

    object StackVariable extends VariableLocation

  }

  object AsmOperandBuilder {
    // Need to forge a specific set of VM types to understand type strings.
    private val anyVmType = VmTypes.forMachineWordByteLength(VmContext.maximumByteSizeAllowed)

    object In {

      case class Scalar(value: BigInt) {
        def /(typeString: Symbol): AsmSourceOperandBuilder =
          new AsmSourceOperandBuilder {
            override def build: OrElse[Source, SyntaxError] =
              anyVmType.valueOf(typeString.name) match {
                case Ok(machineType) =>
                  Ok(Source.Immediate(new VmWord(machineType, value)))

                case Err(vmError) =>
                  Err(SyntaxError.InvalidTypeDefinition(typeString.name, vmError))
              }
          }
      }

      case class Variable(location: VariableLocation, index: Int) extends AsmSourceOperandBuilder {
        override def build: Source.Variable OrElse SyntaxError =
          location match {
            case HeapVariable =>
              Ok(Operand.Source.Variable.InTheHeap(index))

            case StackVariable =>
              Ok(Operand.Source.Variable.InTheStack(index))
          }
      }

      case class Indirect(depth: Int) {
        def * : Indirect = copy(depth + 1)

        def *(variable: AsmOperandBuilder.In.Variable): AsmSourceOperandBuilder =
          new AsmSourceOperandBuilder {
            def build: Source OrElse SyntaxError =
              variable.build && (v => Source.Indirect(v, depth + 1))
          }
      }

      case class Reference(variable: AsmOperandBuilder.In.Variable) extends AsmSourceOperandBuilder {
        override def build: Source OrElse SyntaxError =
          variable.build match {
            case Ok(Source.Variable.InTheHeap(index)) =>
              Ok(Source.Reference.InTheHeap(index))

            case Ok(Source.Variable.InTheStack(index)) =>
              Ok(Source.Reference.InTheStack(index))

            case Ok(anyOtherKindOfOperand) =>
              Err(SyntaxError.InvalidReferenceDefinition(None))

            case Err(somethingWrong) =>
              Err(SyntaxError.InvalidReferenceDefinition(Some(somethingWrong)))
          }
      }

    }

    object Out {

      case class Variable(location: VariableLocation, index: Int) extends AsmDestinationOperandBuilder {
        override def build: DestinationOperand.TargetVariable =
          location match {
            case HeapVariable =>
              Operand.DestinationOperand.TargetVariable.InTheHeap(index)

            case StackVariable =>
              Operand.DestinationOperand.TargetVariable.InTheStack(index)
          }
      }

      case object NoDestination extends AsmDestinationOperandBuilder {
        override def build: DestinationOperand = DestinationOperand.NoDestination
      }

      case class Redirect(depth: Int) {
        def * : Redirect = copy(depth + 1)

        def *-(variable: AsmOperandBuilder.Out.Variable): AsmDestinationOperandBuilder =
          new AsmDestinationOperandBuilder {
            def build: DestinationOperand =
              DestinationOperand.Redirect(variable.build, depth + 1)
          }
      }

    }

  }

  // Example: 8/'u16
  // Need to redefine for every elementary type, otherwise compiler will
  // get confused.
  implicit def CharToScalarOperandBuilder(value: Char): AsmOperandBuilder.In.Scalar =
    AsmOperandBuilder.In.Scalar(BigInt(value.toInt))

  implicit def ByteToScalarOperandBuilder(value: Byte): AsmOperandBuilder.In.Scalar =
    AsmOperandBuilder.In.Scalar(BigInt(value.toInt))

  implicit def ShortToScalarOperandBuilder(value: Short): AsmOperandBuilder.In.Scalar =
    AsmOperandBuilder.In.Scalar(BigInt(value.toInt))

  implicit def IntToScalarOperandBuilder(value: Int): AsmOperandBuilder.In.Scalar =
    AsmOperandBuilder.In.Scalar(BigInt(value))

  implicit def LongToScalarOperandBuilder(value: Long): AsmOperandBuilder.In.Scalar =
    AsmOperandBuilder.In.Scalar(BigInt(value))

  implicit def BigIntToScalarOperandBuilder(value: BigInt): AsmOperandBuilder.In.Scalar =
    AsmOperandBuilder.In.Scalar(value)

  def -%(heapVariableIndex: Int): AsmOperandBuilder.In.Variable =
    AsmOperandBuilder.In.Variable(HeapVariable, heapVariableIndex)

  def -%%(stackVariableIndex: Int): AsmOperandBuilder.In.Variable =
    AsmOperandBuilder.In.Variable(StackVariable, stackVariableIndex)

  def %-(heapVariableIndex: Int): AsmOperandBuilder.Out.Variable =
    AsmOperandBuilder.Out.Variable(HeapVariable, heapVariableIndex)

  def %%-(stackVariableIndex: Int): AsmOperandBuilder.Out.Variable =
    AsmOperandBuilder.Out.Variable(StackVariable, stackVariableIndex)

  def -* : AsmOperandBuilder.In.Indirect = AsmOperandBuilder.In.Indirect(1)

  def -*(variable: AsmOperandBuilder.In.Variable): AsmSourceOperandBuilder =
    AsmOperandBuilder.In.Indirect(0).*(variable)

  def -&(variable: AsmOperandBuilder.In.Variable): AsmSourceOperandBuilder =
    AsmOperandBuilder.In.Reference(variable)

  def * : AsmOperandBuilder.Out.Redirect = AsmOperandBuilder.Out.Redirect(1)

  def *-(variable: AsmOperandBuilder.Out.Variable): AsmDestinationOperandBuilder =
    AsmOperandBuilder.Out.Redirect(0).*-(variable)

  class AsmInstructionBuilder(instruction: Instruction,
                              sourceOperands: List[AsmSourceOperandBuilder],
                              destinationOperand: AsmDestinationOperandBuilder) {
    def -(sourceOperandBuilder: AsmSourceOperandBuilder): AsmInstructionBuilder =
      new AsmInstructionBuilder(instruction, sourceOperands :+ sourceOperandBuilder, destinationOperand)

    def -(destination: AsmDestinationOperandBuilder): AsmInstructionBuilder =
      new AsmInstructionBuilder(instruction, sourceOperands, destination)

    // Note: the paren is here not to confuse idea... It looks like a nasty workaround,
    // but it works, and not so ugly at the end: << ... >> ()
    def >>(): InstructionInstance OrElse SyntaxError = {
      sourceOperands.tryFoldLeft((0, Operands.none)) {
        case ((currentOperandIndex, operands), eachOperandBuilder) =>
          eachOperandBuilder.build && {
            sourceOperand =>
              (currentOperandIndex + 1, operands.addSource(currentOperandIndex -> sourceOperand))
          }
      } && {
        case (_, builtSourceOperands) =>
          new InstructionInstance(instruction,
            builtSourceOperands.setDestination(destinationOperand.build))
      }
    }

  }

  def <<(instruction: Instruction): AsmInstructionBuilder =
    new AsmInstructionBuilder(instruction, List.empty, AsmOperandBuilder.Out.NoDestination)
}
