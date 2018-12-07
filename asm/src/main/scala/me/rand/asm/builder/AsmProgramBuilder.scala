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
package me.rand.asm.builder

import me.rand.asm.main.AsmError
import me.rand.asm.main.AsmError.AsmProgramBuilderError
import me.rand.asm.parser.AsmToken
import me.rand.commons.idioms.Logger
import me.rand.commons.idioms.Status._
import me.rand.vm.engine.VmProgram.{BasicBlockBuilder, InstructionInstance}
import me.rand.vm.engine.{Variable, VmContext}

class AsmProgramBuilder(initialContext: VmContext) {

  private class AsmProgramBuilderContext(val vmContext: VmContext, val currentBasicBlock: Option[BasicBlockBuilder]) {
    def getCurrentBasicBlockOrError(lineNumber: Int): BasicBlockBuilder OrElse AsmProgramBuilderError =
      currentBasicBlock match {
        case None =>
          Err(AsmError.AsmProgramBuilderError.NoBasicBlockDeclared(lineNumber))

        case Some(basicBlockBuilder) =>
          Ok(basicBlockBuilder)
      }

    def withVmContext(newContext: VmContext): AsmProgramBuilderContext =
      new AsmProgramBuilderContext(newContext, currentBasicBlock)

    def withNewBasicBlockCalled(name: String): AsmProgramBuilderContext =
      new AsmProgramBuilderContext(registerLastBasicBlockUnderConstructionIfAny, Some(BasicBlockBuilder.aBasicBlockCalled(name)))

    def withInstruction(instruction: InstructionInstance, intoBasicBlock: BasicBlockBuilder): AsmProgramBuilderContext =
      new AsmProgramBuilderContext(vmContext, Some(intoBasicBlock + instruction))

    def withBootBasicBlock(name: String, lineNumber: Int): AsmProgramBuilderContext OrElse AsmProgramBuilderError = {
      val safeVmContext = registerLastBasicBlockUnderConstructionIfAny
      if (safeVmContext.program.pc.basicBlock.isDefined)
        Err(AsmProgramBuilderError.DuplicateBootstrapDefinition(lineNumber))
      else safeVmContext.setPcToBlockCalled(name) match {
        case Ok(newVmContext) =>
          Ok(new AsmProgramBuilderContext(newVmContext, currentBasicBlock))

        case Err(_) =>
          Err(AsmProgramBuilderError.NoSuchBootstrapBlock(name, lineNumber))
      }
    }

    private def registerLastBasicBlockUnderConstructionIfAny: VmContext =
      currentBasicBlock match {
        case None =>
          vmContext

        case Some(basicBlockBuilder) =>
          vmContext.setProgram(vmContext.program ++ basicBlockBuilder.build)
      }
  }

  def applyProgram(tokens: List[AsmToken])(implicit logger: Logger): VmContext OrElse AsmProgramBuilderError =
    tokens.tryFoldLeft(new AsmProgramBuilderContext(initialContext, None)) {
      case (context, token) =>
        logger.>>(s"processing token $token")
        token match {
          case _: AsmToken.Mach =>
            // The VM is already setup with its specification
            Ok(context)

          case instruction: AsmToken.Instruction =>
            context.getCurrentBasicBlockOrError(instruction.lineNumber) && (context.withInstruction(instruction.instance, _))

          case AsmToken.Directive.DeclareBasicBlock(name, _) =>
            Ok(context.withNewBasicBlockCalled(name))

          case AsmToken.Directive.DefineBootBasicBlock(name, lineNumber) =>
            context.withBootBasicBlock(name, lineNumber)

          case AsmToken.Directive.DeclareVariable.InTheHeap(name, heapIndex, initialValue, lineNumber) =>
            context.vmContext.putHeapVariable(heapIndex, Variable.Scalar(name, initialValue)) match {
              case Err(error) =>
                Err(AsmProgramBuilderError.CantPutHeapVariable(error, lineNumber))

              case Ok(newContext) =>
                Ok(context.withVmContext(newContext))
            }
        }
    } & returnVmContextIfBootstrapIsDefined

  private def returnVmContextIfBootstrapIsDefined(builderContext: AsmProgramBuilderContext): VmContext OrElse AsmProgramBuilderError =
    builderContext.vmContext.program.pc.basicBlock match {
      case None =>
        Err(AsmProgramBuilderError.UndefinedBootstrapBlock)

      case Some(_) =>
        Ok(builderContext.vmContext)
    }
}

object AsmProgramBuilder {
  def startingFromContext(vmContext: VmContext) = new AsmProgramBuilder(vmContext)
}
