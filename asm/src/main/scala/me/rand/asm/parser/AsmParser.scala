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
package me.rand.asm.parser

import me.rand.asm.main.AsmError.AsmParserError
import me.rand.commons.idioms.Logger
import me.rand.commons.idioms.Status._
import me.rand.vm.alu.VmRegister
import me.rand.vm.engine.Operands.OperandsBuilder
import me.rand.vm.engine.VmProgram.InstructionInstance
import me.rand.vm.engine.{Operand, Operands, VmContext}
import me.rand.vm.is.InstructionSet

class AsmParser(input: Iterable[String]) {

  private case class AsmParserContext(lineNumber: Int = 0, vmContext: Option[VmContext] = None, tokens: List[AsmToken] = List.empty) {
    def nextLine: AsmParserContext = copy(lineNumber = lineNumber + 1)

    def recordNewToken(token: AsmToken): AsmParserContext = copy(lineNumber = lineNumber + 1, tokens = tokens :+ token)

    def recordMachIfNotYetDefined(token: AsmToken.Mach): AsmParserContext OrElse AsmParserError =
      vmContext match {
        case None =>
          Ok(copy(lineNumber = lineNumber + 1, vmContext = Some(token.vmContext), tokens = tokens :+ token))

        case Some(_) =>
          Err(AsmParserError.DuplicateMachineSpecification(lineNumber))
      }
  }

  def execute(implicit logger: Logger): (List[AsmToken], VmContext) OrElse AsmParserError =
    input.tryFoldLeft(AsmParserContext()) {
      case (context, eachLine) =>
        logger >> s"${context.lineNumber}: $eachLine"
        translateLine(eachLine)(logger, context) & {
          case None =>
            Ok(context.nextLine)

          case Some(token: AsmToken.Mach) =>
            context.recordMachIfNotYetDefined(token)

          case Some(asmToken) =>
            Ok(context.recordNewToken(asmToken))
        }
    } & {
      case AsmParserContext(_, Some(vmContext), tokens) =>
        Ok((tokens, vmContext))

      case _ =>
        Err(AsmParserError.MissingMachineSpecification)
    }

  private def translateLine(text: String)(implicit logger: Logger, asmParserContext: AsmParserContext): Option[AsmToken] OrElse AsmParserError =
    text.trim.split("\\s+").toList match {
      case Nil | "" :: _ =>
        // Empty line
        Ok(None)

      case k :: _ if k.startsWith("//") =>
        Ok(None)

      case key :: args =>
        translateCode(key, args) && (Some(_))
    }

  private def translateCode(keyword: String, args: List[String])(implicit logger: Logger, asmParserContext: AsmParserContext): AsmToken OrElse AsmParserError =
    keyword match {
      case ".mach" =>
        VmContext.usingProfileString(args.head) match {
          case Ok(vmContext) =>
            Ok(AsmToken.Mach(vmContext, asmParserContext.lineNumber))

          case Err(cause) =>
            Err(AsmParserError.InvalidMachineSpecification(args.head, cause, asmParserContext.lineNumber))
        }

      case directive if directive.startsWith(".") =>
        throw new NotImplementedError("directives not implemented (yet)")

      case instructionName =>
        InstructionSet.map.get(instructionName) match {
          case None =>
            Err(AsmParserError.UnknownInstruction(keyword, asmParserContext.lineNumber))

          case Some(instruction) =>
            logger >> s"found instruction $instruction"
            translateInstructionOperands(args) &&
              (operands => AsmToken.Instruction(new InstructionInstance(instruction, operands), asmParserContext.lineNumber))
        }
    }

  private def translateInstructionOperands(args: List[String])(implicit asmParserContext: AsmParserContext): Operands OrElse AsmParserError =
    args.tryFoldLeft((OperandsBuilder.none, false)) {
      case ((builder, isFetchingOutput), eachArg) =>
        (eachArg, isFetchingOutput) match {
          case (">", _) =>
            Ok(builder, true)

          case (_, false) =>
            translateInstructionSourceOperand(eachArg) && (x => (builder + x, isFetchingOutput))

          case (_, true) =>
            translateInstructionDestinationOperand(eachArg) && (x => (builder + x, isFetchingOutput))
        }
    } && (_._1.build)

  private def translateInstructionSourceOperand(arg: String)(implicit asmParserContext: AsmParserContext): Operand.Source OrElse AsmParserError =
    arg match {
      case aHeapVariable if aHeapVariable.startsWith("%") =>
        getOperandIndexFromName(aHeapVariable.tail) && Operand.Source.Variable.InTheHeap

      case aStackVariable if aStackVariable.startsWith("$") =>
        getOperandIndexFromName(aStackVariable.tail) && Operand.Source.Variable.InTheStack

      case aPointer if aPointer.startsWith("*") =>
        val (depth, operand) = countIndirectionsAndFetchOperandName(aPointer, "*")
        operand match {
          case aHeapVariable if aHeapVariable.startsWith("%") =>
            getOperandIndexFromName(aHeapVariable.tail) &&
              (index => Operand.Source.Indirect(Operand.Source.Variable.InTheHeap(index), depth))

          case aStackVariable if aStackVariable.startsWith("$") =>
            getOperandIndexFromName(aStackVariable.tail) &&
              (index => Operand.Source.Indirect(Operand.Source.Variable.InTheStack(index), depth))

          case _ =>
            Err(AsmParserError.InvalidIndirection(aPointer, asmParserContext.lineNumber))
        }

      case anAddress if anAddress.startsWith("&") =>
        anAddress.tail match {
          case aHeapVariable if aHeapVariable.startsWith("%") =>
            getOperandIndexFromName(aHeapVariable.tail) && Operand.Source.Reference.InTheHeap

          case aStackVariable if aStackVariable.startsWith("$") =>
            getOperandIndexFromName(aStackVariable.tail) && Operand.Source.Reference.InTheStack

          case _ =>
            Err(AsmParserError.InvalidReference(anAddress, asmParserContext.lineNumber))
        }

      case anImmediate if anImmediate.startsWith("=") =>
        translateImmediateValue(anImmediate.tail) && Operand.Source.Immediate

      case somethingElse =>
        Err(AsmParserError.UnknownOperandType(somethingElse, asmParserContext.lineNumber))
    }

  private def translateInstructionDestinationOperand(arg: String)(implicit asmParserContext: AsmParserContext): Operand.Destination OrElse AsmParserError =
    arg match {
      case "_" =>
        Ok(Operand.Destination.NoDestination)

      case aHeapVariable if aHeapVariable.startsWith("%") =>
        getOperandIndexFromName(aHeapVariable.tail) && Operand.Destination.Variable.InTheHeap

      case aStackVariable if aStackVariable.startsWith("$") =>
        getOperandIndexFromName(aStackVariable.tail) && Operand.Destination.Variable.InTheStack

      case aPointer if aPointer.startsWith("*") =>
        val (depth, operand) = countIndirectionsAndFetchOperandName(aPointer, "*")
        operand match {
          case aHeapVariable if aHeapVariable.startsWith("%") =>
            getOperandIndexFromName(aHeapVariable.tail) &&
              (index => Operand.Destination.Redirect(Operand.Destination.Variable.InTheHeap(index), depth))

          case aStackVariable if aStackVariable.startsWith("$") =>
            getOperandIndexFromName(aStackVariable.tail) &&
              (index => Operand.Destination.Redirect(Operand.Destination.Variable.InTheStack(index), depth))

          case _ =>
            Err(AsmParserError.InvalidIndirection(aPointer, asmParserContext.lineNumber))
        }

      case somethingElse =>
        Err(AsmParserError.UnknownOperandType(somethingElse, asmParserContext.lineNumber))
    }

  private def countIndirectionsAndFetchOperandName(name: String, matchPrefix: String, depth: Int = 0): (Int, String) =
    if (name.startsWith(matchPrefix))
      countIndirectionsAndFetchOperandName(name.tail, matchPrefix, depth + 1)
    else (depth, name)

  private def getOperandIndexFromName(name: String)(implicit asmParserContext: AsmParserContext): Int OrElse AsmParserError =
    try {
      Ok(Integer.valueOf(name))
    } catch {
      case _: NumberFormatException =>
        Err(AsmParserError.InvalidOperandIndex(name, asmParserContext.lineNumber))
    }

  private def translateImmediateValue(text: String)(implicit asmParserContext: AsmParserContext): VmRegister OrElse AsmParserError =
  // TODO
    ???
}

object AsmParser {
  def read(input: Iterator[String]) = new AsmParser(input.toIterable)
}
