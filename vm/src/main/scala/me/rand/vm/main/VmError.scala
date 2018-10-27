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
package me.rand.vm.main

import me.rand.vm.is.InstructionSetVersion

sealed trait VmError

object VmError {

  sealed trait VmContextError extends VmError

  object VmContextError {

    case class VariableIndexOutOfBounds(index: Int) extends VmContextError {
      override def toString: String = s"variable index out of bounds: $index"
    }

    case class InvalidVmTypeString(string: String) extends VmContextError {
      override def toString: String = s"invalid type string '$string'"
    }

    case class EmptyStackAccess(operation: String) extends VmContextError {
      override def toString: String = s"invalid $operation to empty stack"
    }

    case class ProgramCounterOutOfBounds(index: Int) extends VmContextError {
      override def toString: String = s"program counter index out of bounds: $index"
    }

    case object ProgramCounterOutOfBlock extends VmContextError {
      override def toString: String = s"program counter not on any basic block"
    }

    case class NoSuchBasicBlock(blockName: String) extends VmContextError {
      override def toString: String = s"no such basic block '$blockName'"
    }

  }

  sealed trait VmExecutionError extends VmError

  object VmExecutionError {

    sealed trait IllegalEncodingError extends VmExecutionError

    object IllegalEncodingError {

      case object UnspecifiedDestinationOperand extends IllegalEncodingError {
        override def toString: String = "destination operand is not specified"
      }

      case class UnspecifiedSourceOperand(operandId: Int) extends IllegalEncodingError {
        override def toString: String = s"source operand #$operandId is not specified"
      }

    }

    sealed trait VmFetchOperandError extends IllegalEncodingError

    object VmFetchOperandError {

      case class NotAnImmediateOperand(operandId: Int) extends VmFetchOperandError {
        override def toString: String = s"operand #$operandId is not an immediate"
      }

      case class NotAnImmediateOrScalarVariableOperand(operandId: Int) extends VmFetchOperandError {
        override def toString: String = s"operand #$operandId is neither an immediate nor a scalar variable"
      }

      case class NotAnImmediateOrScalarVariableOperandAfterRedirections(operandId: Int) extends VmFetchOperandError {
        override def toString: String = s"operand #$operandId redirects to neither an immediate nor a scalar variable"
      }

      sealed trait InvalidPointerValue extends VmFetchOperandError

      object InvalidPointerValue {

        case class InvalidTargetReference(pointerName: String, variableIndex: Int, cause: Option[VmError]) extends InvalidPointerValue {
          override def toString: String = {
            val explanation = cause match {
              case Some(error) =>
                error.toString

              case None =>
                "value not set"
            }
            s"pointer [$pointerName=$variableIndex] is invalid: $explanation"
          }
        }

        case class InvalidRedirection(operandId: Int) extends IllegalEncodingError {
          override def toString: String = s"source operand #$operandId redirects to an unexpected type of variable"
        }

        case object IllegalDestinationPointer extends IllegalEncodingError {
          override def toString: String = "destination is neither a heap nor a stack variable"
        }

      }

    }

  }

  sealed trait VmProfileStringError extends VmError

  object VmProfileStringError {

    case class InvalidFormat(profile: String, expected: String) extends VmProfileStringError {
      override def toString: String =
        s"invalid profile string '$profile': expected '$expected'"
    }

    case class NotAPositiveNumber(profile: String, fieldName: String) extends VmProfileStringError {
      override def toString: String =
        s"invalid profile string '$profile': field '$fieldName'"
    }

    case class ValueExceedsMaximumAllowed(profile: String, fieldName: String, maxValue: Int) extends VmProfileStringError {
      override def toString: String =
        s"profile value '$fieldName' exceeds maximum allowed ($maxValue)"
    }

    case class NotAPowerOfTwo(value: Int, fieldName: String) extends VmProfileStringError {
      override def toString: String =
        s"profile value '$fieldName'=$value is not a power of two"
    }

  }

  case class IncompatibleInstructionSetVersion(v0: InstructionSetVersion) extends VmError {
    override def toString: String =
      s"instruction set version $v0 is incompatible with current version ${InstructionSetVersion.current}"
  }

}
