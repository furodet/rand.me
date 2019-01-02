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

import me.rand.vm.engine.Variable
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

      case class IllegalCast(source: Variable, destination: Variable) extends IllegalEncodingError {
        override def toString: String = s"could not cast source $source to type of $destination"
      }

    }

    sealed trait VmFetchOperandError extends IllegalEncodingError

    object VmFetchOperandError {

      sealed trait InvalidPointerValue extends VmFetchOperandError

      object InvalidPointerValue {

        case class InvalidTargetReference(pointerName: Option[String], variableIndex: Int, cause: Option[VmError]) extends InvalidPointerValue {
          override def toString: String = {
            val explanation = cause match {
              case Some(error) =>
                error.toString

              case None =>
                "value not set"
            }

            s"pointer [${pointerName.getOrElse("<undef>")}=$variableIndex] is invalid: $explanation"
          }
        }

        case class InvalidSourceReference(varSetName: String, variableIndex: Int, cause: Option[VmError]) extends InvalidPointerValue {
          override def toString: String = {
            val explanation = cause match {
              case Some(error) =>
                error.toString

              case None =>
                "value not set"
            }
            s"pointer [$varSetName=$variableIndex] is invalid: $explanation"
          }
        }

        case class InvalidIndirect(operandId: Int) extends IllegalEncodingError {
          override def toString: String =
            s"source operand #$operandId redirects to an unexpected type of variable"
        }

        case object InvalidRedirect extends IllegalEncodingError {
          override def toString: String = "destination operand redirects to an unexpected type of variable"
        }

        case class InvalidArrayBase(name: String) extends IllegalEncodingError {
          override def toString: String =
            s"operand '$name' is not a valid base for indexing"
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

  }

  case class IncompatibleInstructionSetVersion(v0: InstructionSetVersion) extends VmError {
    override def toString: String =
      s"instruction set version $v0 is incompatible with current version ${InstructionSetVersion.current}"
  }

  // Embedded asm errors

  sealed trait SyntaxError extends VmError

  object SyntaxError {

    case class InvalidTypeDefinition(typeString: String, cause: VmError) extends SyntaxError {
      override def toString: String = s"invalid type definition '$typeString': $cause"
    }

    case class NotADestinationOperand(operandString: String) extends SyntaxError {
      override def toString: String = s"not a destination operand '$operandString'"
    }

    case class NoMatchingProfile(instructionName: String, variables: Iterable[Variable]) extends SyntaxError {
      private lazy val variableList = variables.map(v => v.name).mkString("(", ",", ")")

      override def toString: String =
        s"no matching profile found for instruction '$instructionName' with variables $variableList"
    }

  }

}
