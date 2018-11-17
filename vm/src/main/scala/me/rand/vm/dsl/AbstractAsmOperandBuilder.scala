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

import java.nio.ByteBuffer

import me.rand.commons.idioms.Status._
import me.rand.vm.engine.Instruction.Operand
import me.rand.vm.engine.{Instruction, VmContext, VmTypes, VmWord}
import me.rand.vm.main.VmError
import me.rand.vm.main.VmError.SyntaxError

import scala.language.implicitConversions

// Documentation: doc/vmeasm.md
sealed trait AbstractAsmOperandBuilder {
  def toSourceOperand: Instruction.Operand.Source OrElse VmError.SyntaxError

  def toDestinationOperand: Operand.Destination OrElse VmError.SyntaxError
}

object AbstractAsmOperandBuilder {

  private trait AbstractAsmVariableLocation

  private object AbstractAsmVariableLocation {

    case object InTheStack extends AbstractAsmVariableLocation

    case object InTheHeap extends AbstractAsmVariableLocation

  }

  class AbstractAsmScalarBuilder(value: Array[Byte]) {
    // Need to forge a specific set of VM types to understand type strings.
    private val anyVmType = VmTypes.forMachineWordByteLength(VmContext.maximumByteSizeAllowed)

    def /(typeSymbol: Symbol): AbstractAsmOperandBuilder =
      new AbstractAsmOperandBuilder {
        override def toSourceOperand: Operand.Source.Immediate OrElse VmError.SyntaxError =
          anyVmType.valueOf(typeSymbol.name) match {
            case Ok(machineType) =>
              Ok(Operand.Source.Immediate(VmWord.ofType(machineType).withValue(value)))

            case Err(vmError) =>
              Err(SyntaxError.InvalidTypeDefinition(typeSymbol.name, vmError))
          }

        override def toDestinationOperand: Operand.Destination OrElse VmError.SyntaxError =
          Err(SyntaxError.NotADestinationOperand(s"${value.toString} / ${typeSymbol.name}"))
      }
  }

  // Example: 8/'u16
  // Need to redefine for every elementary type, otherwise compiler will
  // get confused.
  implicit def CharToScalarOperandBuilder(value: Char): AbstractAsmOperandBuilder.AbstractAsmScalarBuilder =
    new AbstractAsmOperandBuilder.AbstractAsmScalarBuilder(ByteBuffer.allocate(1).put(value.toByte).array())

  implicit def ByteToScalarOperandBuilder(value: Byte): AbstractAsmOperandBuilder.AbstractAsmScalarBuilder =
    new AbstractAsmOperandBuilder.AbstractAsmScalarBuilder(ByteBuffer.allocate(1).put(value).array())

  implicit def ShortToScalarOperandBuilder(value: Short): AbstractAsmOperandBuilder.AbstractAsmScalarBuilder =
    new AbstractAsmOperandBuilder.AbstractAsmScalarBuilder(ByteBuffer.allocate(2).putShort(value).array())

  implicit def IntToScalarOperandBuilder(value: Int): AbstractAsmOperandBuilder.AbstractAsmScalarBuilder =
    new AbstractAsmOperandBuilder.AbstractAsmScalarBuilder(ByteBuffer.allocate(4).putInt(value).array())

  implicit def LongToScalarOperandBuilder(value: Long): AbstractAsmOperandBuilder.AbstractAsmScalarBuilder =
    new AbstractAsmOperandBuilder.AbstractAsmScalarBuilder(ByteBuffer.allocate(8).putLong(value).array())

  implicit def BigIntToScalarOperandBuilder(value: BigInt): AbstractAsmOperandBuilder.AbstractAsmScalarBuilder =
    new AbstractAsmOperandBuilder.AbstractAsmScalarBuilder(value.toByteArray)

  class AbstractAsmVariableBuilder(location: AbstractAsmVariableLocation, val index: Int) extends AbstractAsmOperandBuilder {
    override def toSourceOperand: Operand.Source.Variable OrElse SyntaxError =
      location match {
        case AbstractAsmVariableLocation.InTheStack =>
          Ok(Operand.Source.Variable.InTheStack(index))

        case AbstractAsmVariableLocation.InTheHeap =>
          Ok(Operand.Source.Variable.InTheHeap(index))
      }

    override def toDestinationOperand: Operand.Destination.Variable OrElse SyntaxError =
      location match {
        case AbstractAsmVariableLocation.InTheStack =>
          Ok(Operand.Destination.Variable.InTheStack(index))
        case AbstractAsmVariableLocation.InTheHeap =>
          Ok(Operand.Destination.Variable.InTheHeap(index))
      }
  }

  def %(heapIndex: Int) = new AbstractAsmVariableBuilder(AbstractAsmVariableLocation.InTheHeap, heapIndex)

  def $(stackIndex: Int) = new AbstractAsmVariableBuilder(AbstractAsmVariableLocation.InTheStack, stackIndex)

  class AbstractAsmReferenceBuilder(variable: AbstractAsmVariableBuilder) extends AbstractAsmOperandBuilder {
    override def toSourceOperand: Operand.Source.Reference OrElse SyntaxError =
      variable.toSourceOperand && {
        case Operand.Source.Variable.InTheHeap(heapIndex) =>
          Operand.Source.Reference.InTheHeap(heapIndex)

        case Operand.Source.Variable.InTheStack(stackIndex) =>
          Operand.Source.Reference.InTheStack(stackIndex)
      }

    override def toDestinationOperand: Operand.Destination OrElse SyntaxError =
      Err(SyntaxError.NotADestinationOperand(""))
  }

  def &(variable: AbstractAsmVariableBuilder) = new AbstractAsmReferenceBuilder(variable)

  class AbstractAsmAnyDirectBuilder(depth: Int) {
    def * = new AbstractAsmAnyDirectBuilder(depth + 1)

    def *(variable: AbstractAsmVariableBuilder): AbstractAsmOperandBuilder =
      new AbstractAsmOperandBuilder {
        override def toSourceOperand: Operand.Source.Indirect OrElse SyntaxError =
          variable.toSourceOperand && (source => Operand.Source.Indirect(source, depth + 1))

        override def toDestinationOperand: Operand.Destination.Redirect OrElse SyntaxError =
          variable.toDestinationOperand && (destination => Operand.Destination.Redirect(destination, depth + 1))
      }
  }

  def * = new AbstractAsmAnyDirectBuilder(1)

  def *(variable: AbstractAsmVariableBuilder): AbstractAsmOperandBuilder =
    new AbstractAsmAnyDirectBuilder(0).*(variable)

}
