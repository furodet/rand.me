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
package me.rand.asm.builder

import me.rand.asm.main.AsmError
import me.rand.asm.main.AsmError.AsmProgramBuilderError
import me.rand.asm.parser.AsmToken
import me.rand.commons.idioms.Logger
import me.rand.commons.idioms.Status._
import me.rand.vm.engine.VmProgram.{BasicBlockBuilder, InlineDirective, InstructionInstance}
import me.rand.vm.engine.{Operand, Variable, VmContext, VmControl}

class AsmProgramBuilder(initialContext: VmContext) {

  private class AsmProgramBuilderContext(val vmContext: VmContext,
                                         val currentBasicBlock: Option[BasicBlockBuilder],
                                         val bootBasicBlockName: Option[(String, Int)]) {
    def getCurrentBasicBlockOrError(lineNumber: Int): BasicBlockBuilder OrElse AsmProgramBuilderError =
      currentBasicBlock match {
        case None =>
          Err(AsmError.AsmProgramBuilderError.NoBasicBlockDeclared(lineNumber))

        case Some(basicBlockBuilder) =>
          Ok(basicBlockBuilder)
      }

    def withVmContext(newContext: VmContext): AsmProgramBuilderContext =
      new AsmProgramBuilderContext(newContext, currentBasicBlock, bootBasicBlockName)

    def withNewBasicBlockCalled(name: String): AsmProgramBuilderContext =
      new AsmProgramBuilderContext(vmContext, Some(BasicBlockBuilder.aBasicBlockCalled(name)), bootBasicBlockName)

    def withInstruction(instruction: InstructionInstance, intoBasicBlock: BasicBlockBuilder): AsmProgramBuilderContext =
      new AsmProgramBuilderContext(vmContext, Some(intoBasicBlock + instruction), bootBasicBlockName)

    def withInlineDirective(inlineDirective: InlineDirective, intoBasicBlock: BasicBlockBuilder): AsmProgramBuilderContext =
      new AsmProgramBuilderContext(vmContext, Some(intoBasicBlock + inlineDirective), bootBasicBlockName)

    def withBootBasicBlock(name: String, lineNumber: Int): AsmProgramBuilderContext OrElse AsmProgramBuilderError =
      if (bootBasicBlockName.isDefined) Err(AsmProgramBuilderError.DuplicateBootstrapDefinition(lineNumber))
      else Ok(new AsmProgramBuilderContext(vmContext, currentBasicBlock, Some((name, lineNumber))))

    def registerLastBasicBlockUnderConstructionIfAny: AsmProgramBuilderContext =
      new AsmProgramBuilderContext(registerLastBasicBlockUnderConstructionInVmContextIfNeeded, None, bootBasicBlockName)

    private def registerLastBasicBlockUnderConstructionInVmContextIfNeeded: VmContext =
      currentBasicBlock match {
        case None =>
          vmContext

        case Some(basicBlockBuilder) =>
          vmContext.setProgram(vmContext.program ++ basicBlockBuilder.build)
      }
  }

  def applyProgram(tokens: List[AsmToken])(implicit logger: Logger): VmContext OrElse AsmProgramBuilderError =
    tokens.tryFoldLeft(new AsmProgramBuilderContext(initialContext, None, None)) {
      case (context, token) =>
        logger.>>(s"processing token $token")
        token match {
          case _: AsmToken.Mach =>
            // The VM is already setup with its specification
            Ok(context)

          case AsmToken.MachPtr.ToInstruction(nativeType, _) =>
            val newPointerTypes = context.vmContext.profile.pointerTypes.withInstructionPointerType(nativeType)
            Ok(context.withVmContext(context.vmContext.withPointerTypes(newPointerTypes)))

          case AsmToken.MachPtr.ToHeapVariable(nativeType, _) =>
            val newPointerTypes = context.vmContext.profile.pointerTypes.withHeapPointerType(nativeType)
            Ok(context.withVmContext(context.vmContext.withPointerTypes(newPointerTypes)))

          case AsmToken.MachPtr.ToStackVariable(nativeType, _) =>
            val newPointerTypes = context.vmContext.profile.pointerTypes.withStackPointerType(nativeType)
            Ok(context.withVmContext(context.vmContext.withPointerTypes(newPointerTypes)))

          case instruction: AsmToken.Instruction =>
            context.getCurrentBasicBlockOrError(instruction.lineNumber) && (context.withInstruction(instruction.instance, _))

          case AsmToken.Directive.DeclareBasicBlock(name, lineNumber) =>
            val newContext = context.registerLastBasicBlockUnderConstructionIfAny
            newContext.vmContext.program.basicBlocks.get(name) match {
              case None =>
                Ok(newContext.registerLastBasicBlockUnderConstructionIfAny.withNewBasicBlockCalled(name))

              case Some(_) =>
                Err(AsmError.AsmProgramBuilderError.DuplicateBasicBlockDefinition(name, lineNumber))
            }

          case AsmToken.Directive.DefineBootBasicBlock(name, lineNumber) =>
            context.withBootBasicBlock(name, lineNumber)

          case AsmToken.Directive.TagVariable.InTheHeap(name, heapIndex, vmt, lineNumber) =>
            context.getCurrentBasicBlockOrError(lineNumber) &&
              (context.withInlineDirective(forgeTagVariable(name, Operand.Source.Variable.InTheHeap(heapIndex), vmt), _))

          case AsmToken.Directive.TagVariable.InTheStack(name, stackIndex, vmt, lineNumber) =>
            context.getCurrentBasicBlockOrError(lineNumber) &&
              (context.withInlineDirective(forgeTagVariable(name, Operand.Source.Variable.InTheStack(stackIndex), vmt), _))

          case AsmToken.Directive.FrameOperation.Push(nrVariables, lineNumber) =>
            context.getCurrentBasicBlockOrError(lineNumber) &&
              (context.withInlineDirective(InlineDirective(VmControl.FrameOperation.Push(nrVariables)), _))

          case AsmToken.Directive.FrameOperation.Pop(lineNumber) =>
            context.getCurrentBasicBlockOrError(lineNumber) &&
              (context.withInlineDirective(InlineDirective(VmControl.FrameOperation.Pop), _))
        }
    } & updateVmContextWithBootstrap

  private def forgeTagVariable(tag: String, operand: Operand.Source.Variable, vmt: Variable.BasicType): InlineDirective =
    InlineDirective(VmControl.TagVariable(tag, operand, vmt))

  private def updateVmContextWithBootstrap(builderContext: AsmProgramBuilderContext): VmContext OrElse AsmProgramBuilderError = {
    val completed = builderContext.registerLastBasicBlockUnderConstructionIfAny
    completed.bootBasicBlockName match {
      case None =>
        Err(AsmProgramBuilderError.UndefinedBootstrapBlock)

      case Some((basicBlockName, lineNumber)) =>
        completed.vmContext.resetPcToBlockCalled(basicBlockName) ||
          (_ => AsmProgramBuilderError.NoSuchBootstrapBlock(basicBlockName, lineNumber))
    }
  }
}

object AsmProgramBuilder {
  def startingFromContext(vmContext: VmContext) = new AsmProgramBuilder(vmContext)
}
