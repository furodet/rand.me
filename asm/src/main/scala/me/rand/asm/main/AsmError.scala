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
package me.rand.asm.main

import me.rand.vm.main.VmError
import me.rand.vm.main.VmError.VmProfileStringError

sealed trait AsmError

object AsmError {

  sealed trait AsmArgumentError extends AsmError

  object AsmArgumentError {

    case class CantOpenFile(fileName: String, cause: Throwable) extends AsmArgumentError {
      override def toString: String =
        s"could not open '$fileName': ${cause.getMessage}"
    }

  }

  sealed trait AsmParserError extends AsmError {
    def lineNumber: Int
  }

  object AsmParserError {

    case class DuplicateMachineSpecification(lineNumber: Int) extends AsmParserError {
      override def toString: String =
        s"$lineNumber: machine profile already defined"
    }

    case object MissingMachineSpecification extends AsmParserError {
      override def toString: String =
        s"no machine specification found"

      override def lineNumber: Int = 0
    }

    case class UnknownInstruction(name: String, lineNumber: Int) extends AsmParserError {
      override def toString: String =
        s"$lineNumber: unknown instruction '$name'"
    }

    case class UnknownSourceOperandType(name: String, lineNumber: Int) extends AsmParserError {
      override def toString: String =
        s"$lineNumber: unknown type of source operand '$name'"
    }

    case class UnknownDestinationOperandType(name: String, lineNumber: Int) extends AsmParserError {
      override def toString: String =
        s"$lineNumber: unknown type of destination operand '$name'"
    }

    case class InvalidIndirection(name: String, lineNumber: Int) extends AsmParserError {
      override def toString: String =
        s"$lineNumber: invalid indirection '$name'"
    }

    case class InvalidReference(name: String, lineNumber: Int) extends AsmParserError {
      override def toString: String =
        s"$lineNumber: invalid variable reference '$name'"
    }

    case class InvalidMachineSpecification(text: String, cause: VmProfileStringError, lineNumber: Int) extends AsmParserError {
      override def toString: String =
        s"$lineNumber: invalid machine specification '$text': $cause"
    }

    case class InvalidPointerType(text: String, lineNumber: Int) extends AsmParserError {
      override def toString: String =
        s"$lineNumber: invalid type of pointer '$text'"
    }

    case class UnspecifiedMachineProfile(lineNumber: Int) extends AsmParserError {
      override def toString: String =
        s"$lineNumber: no machine specification provided - could not decode type"
    }

    case class InvalidTypeLen(text: String, lineNumber: Int) extends AsmParserError {
      override def toString: String =
        s"$lineNumber: invalid type length specification '$text'"
    }

    case class InvalidDirectiveSpecification(keyword: String, lineNumber: Int) extends AsmParserError {
      override def toString: String =
        s"$lineNumber: invalid directive specification '$keyword'"
    }

    case class InvalidVariableSpecification(text: String, lineNumber: Int) extends AsmParserError {
      override def toString: String =
        s"$lineNumber: invalid variable specification '$text'"
    }

    case class InvalidInstructionSetVersionSpecification(string: String, lineNumber: Int) extends AsmParserError {
      override def toString: String =
        s"$lineNumber: invalid instruction set version specification '$string'"
    }

    case class IncompatibleInstructionSet(error: VmError.IncompatibleInstructionSetVersion, lineNumber: Int) extends AsmParserError {
      override def toString: String = s"$lineNumber: $error"
    }

  }

  sealed trait AsmProgramBuilderError extends AsmError

  object AsmProgramBuilderError {

    case class NoBasicBlockDeclared(lineNumber: Int) extends AsmProgramBuilderError {
      override def toString: String = s"$lineNumber: could not set instruction: no basic block declared yet"
    }

    case object UndefinedBootstrapBlock extends AsmProgramBuilderError {
      override def toString: String = "undefined bootstrap"
    }

    case class NoSuchBootstrapBlock(name: String, lineNumber: Int) extends AsmProgramBuilderError {
      override def toString: String = s"$lineNumber: invalid bootstrap: no basic block called '$name' declared"
    }

    case class DuplicateBootstrapDefinition(lineNumber: Int) extends AsmProgramBuilderError {
      override def toString: String = s"$lineNumber: duplicate bootstrap definition"
    }

    case class DuplicateBasicBlockDefinition(name: String, lineNumber: Int) extends AsmProgramBuilderError {
      override def toString: String = s"$lineNumber: basic block '$name' already declared"
    }

  }

}
