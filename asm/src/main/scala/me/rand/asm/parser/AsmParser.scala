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
import me.rand.commons.idioms.NormalizedNumber._
import me.rand.commons.idioms.Status._
import me.rand.commons.idioms.{Logger, NormalizedNumber}
import me.rand.vm.alu.VmRegister
import me.rand.vm.engine.Operands.OperandsBuilder
import me.rand.vm.engine.VmProgram.InstructionInstance
import me.rand.vm.engine.VmTypes.VmType
import me.rand.vm.engine.{Operand, Operands, VmContext}
import me.rand.vm.is.{InstructionSet, InstructionSetVersion}

class AsmParser(input: Iterable[String], prefix: Option[String]) {
  def withPrefix(newPrefix: Option[String]): AsmParser = new AsmParser(input, newPrefix)

  private case class AsmParserContext(lineNumber: Int = 1, vmContext: Option[VmContext] = None, tokens: List[AsmToken] = List.empty) {
    def nextLine: AsmParserContext = copy(lineNumber = lineNumber + 1)

    def recordNewToken(token: AsmToken)(implicit logger: Logger): AsmParserContext = {
      logger >> s"$lineNumber: registering token $token"
      copy(lineNumber = lineNumber + 1, tokens = tokens :+ token)
    }

    def recordMachIfNotYetDefined(token: AsmToken.Mach)(implicit logger: Logger): AsmParserContext OrElse AsmParserError =
      vmContext match {
        case None =>
          logger >> s"$lineNumber: registering token $token"
          Ok(copy(lineNumber = lineNumber + 1, vmContext = Some(token.vmContext), tokens = tokens :+ token))

        case Some(_) =>
          Err(AsmParserError.DuplicateMachineSpecification(lineNumber))
      }
  }

  def execute(implicit logger: Logger): (VmContext, List[AsmToken]) OrElse AsmParserError =
    for {
      asmParserContext <- parseInput
      result <- getParserResultIfVmContextIsDefined(asmParserContext)
    } yield result

  private def parseInput(implicit logger: Logger): AsmParserContext OrElse AsmParserError =
    input.tryFoldLeft(AsmParserContext()) {
      case (context, eachLine) if matchesPrefix(eachLine) =>
        logger >> s"${context.lineNumber}: $eachLine"
        translateLine(eachLine)(logger, context) & {
          case None =>
            Ok(context.nextLine)

          case Some(token: AsmToken.Mach) =>
            context.recordMachIfNotYetDefined(token)

          case Some(asmToken) =>
            Ok(context.recordNewToken(asmToken))
        }

      case (context, _) =>
        Ok(context.nextLine)
    }

  private def matchesPrefix(text: String): Boolean =
    prefix match {
      case None =>
        true

      case Some(prefixText) =>
        text.startsWith(prefixText)
    }

  private def getParserResultIfVmContextIsDefined(asmParserContext: AsmParserContext): (VmContext, List[AsmToken]) OrElse AsmParserError =
    asmParserContext match {
      case AsmParserContext(_, Some(vmContext), tokens) =>
        Ok((vmContext, tokens))

      case _ =>
        Err(AsmParserError.MissingMachineSpecification)
    }

  private def translateLine(text: String)(implicit logger: Logger, asmParserContext: AsmParserContext): Option[AsmToken] OrElse AsmParserError = {
    val cleanText = (if (prefix.isDefined) text.substring(prefix.get.length) else text).trim.split("\\s+").toList
    cleanText match {
      case Nil | "" :: _ =>
        // Empty line
        Ok(None)

      case k :: _ if k.startsWith("//") =>
        Ok(None)

      case key :: args =>
        translateCode(key, args) && (Some(_))
    }
  }

  private def translateCode(keyword: String, args: List[String])(implicit logger: Logger, asmParserContext: AsmParserContext): AsmToken OrElse AsmParserError =
    keyword match {
      case ".mach" if args.length == 2 =>
        logger >> s"applying machine definition 'version=${args.head} ${args(1)}'"
        verifyInstructionSetVersion(args.head, asmParserContext.lineNumber) & (_ => VmContext.usingProfileString(args(1)) match {
          case Ok(vmContext) =>
            Ok(AsmToken.Mach(vmContext, asmParserContext.lineNumber))

          case Err(cause) =>
            Err(AsmParserError.InvalidMachineSpecification(args.head, cause, asmParserContext.lineNumber))
        })

      case directive if directive.startsWith(".") =>
        logger >> s"applying directive '$directive'"
        directive match {
          case ".bb" if args.nonEmpty =>
            Ok(AsmToken.Directive.DeclareBasicBlock(args.head, asmParserContext.lineNumber))

          case ".var" =>
            translateVariableDeclarationDirective(args)

          case ".boot" if args.nonEmpty =>
            Ok(AsmToken.Directive.DefineBootBasicBlock(args.head, asmParserContext.lineNumber))

          case _ =>
            Err(AsmParserError.InvalidDirectiveSpecification(keyword, asmParserContext.lineNumber))
        }

      case instructionName =>
        logger >> s"searching for instruction '$instructionName'"
        InstructionSet.map.get(instructionName) match {
          case None =>
            Err(AsmParserError.UnknownInstruction(keyword, asmParserContext.lineNumber))

          case Some(instruction) =>
            translateInstructionOperands(args) &&
              (operands => AsmToken.Instruction(InstructionInstance(instruction, operands), asmParserContext.lineNumber))
        }
    }

  private def verifyInstructionSetVersion(string: String, lineNumber: Int): Unit OrElse AsmParserError =
    InstructionSetVersion.fromString(string) match {
      case None =>
        Err(AsmParserError.InvalidInstructionSetVersionSpecification(string, lineNumber))

      case Some(version) =>
        InstructionSetVersion.assertThatVirtualMachineVersionIsCompatibleWith(version) match {
          case Err(error) =>
            Err(AsmParserError.IncompatibleInstructionSet(error, lineNumber))

          case _ =>
            Ok(())
        }
    }

  private def matchesHeapVariable(string: String) = string.matches("%[0-9]+")

  private def matchesStackVariable(string: String) = string.matches("\\$[0-9]+")

  private def matchesImmediateValue(string: String) = string.matches("\\(([0-9a-fA-F][0-9a-fA-F])+:[su][0-9]+\\)")

  private def matchesIndexedHeapVariable(string: String) = string.matches("%[0-9]+\\[[0-9]+\\]")

  private def matchesIndexedStackVariable(string: String) = string.matches("\\$[0-9]+\\[[0-9]+\\]")

  private def translateVariableDeclarationDirective(args: List[String])(implicit asmParserContext: AsmParserContext): AsmToken.Directive.TagVariable OrElse AsmParserError =
    args match {
      case name :: id :: value :: Nil if matchesHeapVariable(id) && matchesImmediateValue(value) =>
        translateImmediateValue(value.tail.init) && (v => AsmToken.Directive.TagVariable.InTheHeap(name, id.tail.toInt, v, asmParserContext.lineNumber))

      case name :: id :: value :: Nil if matchesStackVariable(id) && matchesImmediateValue(value) =>
        translateImmediateValue(value.tail.init) && (v => AsmToken.Directive.TagVariable.InTheStack(name, id.tail.toInt, v, asmParserContext.lineNumber))

      case _ =>
        Err(AsmParserError.InvalidVariableSpecification(args.mkString(" "), asmParserContext.lineNumber))
    }

  private def translateInstructionOperands(args: List[String])(implicit asmParserContext: AsmParserContext): Operands OrElse AsmParserError =
    args.tryFoldLeft((OperandsBuilder.none, false)) {
      case ((builder, isFetchingOutput), eachArg) =>
        (eachArg, isFetchingOutput) match {
          case (">", _) =>
            Ok((builder, true))

          case (_, false) =>
            translateInstructionSourceOperand(eachArg) && (x => (builder + x, isFetchingOutput))

          case (_, true) =>
            translateInstructionDestinationOperand(eachArg) && (x => (builder + x, isFetchingOutput))
        }
    } && (_._1.build)

  private def translateInstructionSourceOperand(arg: String)(implicit asmParserContext: AsmParserContext): Operand.Source OrElse AsmParserError =
    arg match {
      case aHeapVariable if matchesHeapVariable(aHeapVariable) =>
        Ok(Operand.Source.Variable.InTheHeap(aHeapVariable.tail.toInt))

      case aStackVariable if matchesStackVariable(aStackVariable) =>
        Ok(Operand.Source.Variable.InTheStack(aStackVariable.tail.toInt))

      case aPointer if aPointer.startsWith("*") =>
        val (depth, operand) = countIndirectionsAndFetchOperandName(aPointer, "*")
        operand match {
          case aHeapVariable if matchesHeapVariable(aHeapVariable) =>
            val index = aHeapVariable.tail.toInt
            Ok(Operand.Source.Indirect(Operand.Source.Variable.InTheHeap(index), depth))

          case aStackVariable if matchesStackVariable(aStackVariable) =>
            val index = aStackVariable.tail.toInt
            Ok(Operand.Source.Indirect(Operand.Source.Variable.InTheStack(index), depth))

          case _ =>
            Err(AsmParserError.InvalidIndirection(aPointer, asmParserContext.lineNumber))
        }

      case anIndexedVariable if matchesIndexedHeapVariable(anIndexedVariable) =>
        val fields = anIndexedVariable.tail.split("\\[")
        val index = fields.head.toInt
        val offset = fields(1).init.toInt
        Ok(Operand.Source.Indexed(Operand.Source.Variable.InTheHeap(index), offset))

      case anIndexedVariable if matchesIndexedStackVariable(anIndexedVariable) =>
        val fields = anIndexedVariable.tail.split("\\[")
        val index = fields.head.toInt
        val offset = fields(1).init.toInt
        Ok(Operand.Source.Indexed(Operand.Source.Variable.InTheStack(index), offset))

      case anAddress if anAddress.startsWith("&") =>
        anAddress.tail match {
          case aHeapVariable if matchesHeapVariable(aHeapVariable) =>
            Ok(Operand.Source.Reference.InTheHeap(aHeapVariable.tail.toInt))

          case aStackVariable if matchesStackVariable(aStackVariable) =>
            Ok(Operand.Source.Reference.InTheStack(aStackVariable.tail.toInt))

          case _ =>
            Err(AsmParserError.InvalidReference(anAddress, asmParserContext.lineNumber))
        }

      case anImmediate if matchesImmediateValue(anImmediate) =>
        translateImmediateValue(anImmediate.tail.init) && Operand.Source.Immediate

      case somethingElse =>
        Err(AsmParserError.UnknownSourceOperandType(somethingElse, asmParserContext.lineNumber))
    }

  private def translateInstructionDestinationOperand(arg: String)(implicit asmParserContext: AsmParserContext): Operand.Destination OrElse AsmParserError =
    arg match {
      case "_" =>
        Ok(Operand.Destination.NoDestination)

      case aHeapVariable if matchesHeapVariable(aHeapVariable) =>
        Ok(Operand.Destination.Variable.InTheHeap(aHeapVariable.tail.toInt))

      case aStackVariable if matchesStackVariable(aStackVariable) =>
        Ok(Operand.Destination.Variable.InTheStack(aStackVariable.tail.toInt))

      case aPointer if aPointer.startsWith("*") =>
        val (depth, operand) = countIndirectionsAndFetchOperandName(aPointer, "*")
        operand match {
          case aHeapVariable if matchesHeapVariable(aHeapVariable) =>
            val index = aHeapVariable.tail.toInt
            Ok(Operand.Destination.Redirect(Operand.Destination.Variable.InTheHeap(index), depth))

          case aStackVariable if matchesStackVariable(aStackVariable) =>
            val index = aStackVariable.tail.toInt
            Ok(Operand.Destination.Redirect(Operand.Destination.Variable.InTheStack(index), depth))

          case _ =>
            Err(AsmParserError.InvalidIndirection(aPointer, asmParserContext.lineNumber))
        }

      case anIndexedVariable if matchesIndexedHeapVariable(anIndexedVariable) =>
        val fields = anIndexedVariable.tail.split("\\[")
        val index = fields.head.toInt
        val offset = fields(1).init.toInt
        Ok(Operand.Destination.Indexed(Operand.Destination.Variable.InTheHeap(index), offset))

      case anIndexedVariable if matchesIndexedStackVariable(anIndexedVariable) =>
        val fields = anIndexedVariable.tail.split("\\[")
        val index = fields.head.toInt
        val offset = fields(1).init.toInt
        Ok(Operand.Destination.Indexed(Operand.Destination.Variable.InTheStack(index), offset))

      case somethingElse =>
        Err(AsmParserError.UnknownDestinationOperandType(somethingElse, asmParserContext.lineNumber))
    }

  private def countIndirectionsAndFetchOperandName(name: String, matchPrefix: String, depth: Int = 0): (Int, String) =
    if (name.startsWith(matchPrefix))
      countIndirectionsAndFetchOperandName(name.tail, matchPrefix, depth + 1)
    else (depth, name)

  private def translateImmediateValue(text: String)(implicit asmParserContext: AsmParserContext): VmRegister OrElse AsmParserError = {
    val fields = text.split(":")
    for {
      vmType <- decodeType(fields(1), asmParserContext.vmContext, asmParserContext.lineNumber)
      value = decodeNumber(fields(0))
    } yield VmRegister.normalize(vmType, value)
  }

  private def decodeNumber(text: String): NormalizedNumber =
    text.sliding(2, 2).map(byteAsString => Integer.parseInt(byteAsString, 16))

  private def decodeType(text: String, vmContext: Option[VmContext], lineNumber: Int): VmType OrElse AsmParserError =
    if (vmContext.isDefined) decodeType(text.tail, isSigned = text.head == 's', vmContext.get, lineNumber)
    else Err(AsmParserError.UnspecifiedMachineProfile(lineNumber))

  private def decodeType(radix: String, isSigned: Boolean, vmContext: VmContext, lineNumber: Int): VmType OrElse AsmParserError =
    decodeTypeLen(radix, lineNumber) & {
      vmContext.vmTypes.select(_, isSigned) match {
        case None =>
          Err(AsmParserError.InvalidTypeLen(radix, lineNumber))

        case Some(vmType) =>
          Ok(vmType)
      }
    }

  private def decodeTypeLen(text: String, lineNumber: Int): Int OrElse AsmParserError =
    try {
      text.toInt match {
        case bitLen if bitLen > 0 =>
          Ok((bitLen + 7) / 8)

        case _ =>
          Err(AsmParserError.InvalidTypeLen(text, lineNumber))
      }
    } catch {
      case _: NumberFormatException =>
        Err(AsmParserError.InvalidTypeLen(text, lineNumber))
    }
}

object AsmParser {
  def read(input: Iterator[String]) = new AsmParser(input.toIterable, None)
}
