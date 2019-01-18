/*-
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
import me.rand.vm.engine.VmProgram.{BasicBlock, Counter, VirtualInstruction}
import me.rand.vm.main.VmError.VmContextError
import me.rand.vm.main.VmError.VmContextError.{NoSuchBasicBlock, ProgramCounterOutOfBlock, ProgramCounterOutOfBounds}
import me.rand.vm.main.{ExecutionContext, VmError}

import scala.language.postfixOps

// Documentation: doc/vmarchitecture.md
class VmProgram(val basicBlocks: Map[String, BasicBlock], val pc: Counter) {
  def ++(basicBlock: BasicBlock): VmProgram =
    new VmProgram(basicBlocks + (basicBlock.name -> basicBlock), pc)

  def nextInstruction: VirtualInstruction OrElse VmContextError =
    pc.basicBlock match {
      case None =>
        Err(ProgramCounterOutOfBlock)

      case Some(basicBlock) =>
        try {
          Ok(basicBlock.instructions(pc.index))
        } catch {
          case _: ArrayIndexOutOfBoundsException =>
            Err(ProgramCounterOutOfBounds(pc.index))
        }
    }

  private[engine] def incrementPc: VmProgram = setPc(pc ++)

  private[engine] def setPcToBlockCalled(blockName: String): VmProgram OrElse VmContextError =
    basicBlocks.get(blockName) match {
      case Some(basicBlock) =>
        Ok(setPc(Counter.atTheBeginningOf(basicBlock)))

      case None =>
        Err(NoSuchBasicBlock(blockName))
    }

  def setPc(pc: Counter) = new VmProgram(basicBlocks, pc)
}

object VmProgram {
  def empty: VmProgram = new VmProgram(Map.empty, Counter.reset)

  sealed trait VirtualInstruction {
    def execute(vmContext: VmContext)(implicit executionContext: ExecutionContext): VmContext OrElse VmError
  }

  case class InlineDirective(vmControl: VmControl) extends VirtualInstruction {
    override def execute(vmContext: VmContext)(implicit executionContext: ExecutionContext): OrElse[VmContext, VmError] =
      vmControl.execute(vmContext)
  }

  case class InstructionInstance(instruction: Instruction, operands: Operands) extends VirtualInstruction {
    override def execute(vmContext: VmContext)(implicit executionContext: ExecutionContext): VmContext OrElse VmError =
      instruction.execute(operands)(vmContext, executionContext)

    override def toString: String =
      s"$instruction(${operands.sources.mkString(",")} > ${operands.destination})"
  }

  class BasicBlock(val name: String, val instructions: Array[VirtualInstruction])

  class BasicBlockBuilder(val name: String, val instructions: Vector[VirtualInstruction]) {
    def +(instruction: InstructionInstance): BasicBlockBuilder =
      new BasicBlockBuilder(name, instructions :+ instruction)

    def +(directive: InlineDirective): BasicBlockBuilder =
      new BasicBlockBuilder(name, instructions :+ directive)

    def build: BasicBlock = {
      new BasicBlock(name, instructions.toArray)
    }
  }

  object BasicBlockBuilder {
    def aBasicBlockCalled(name: String) = new BasicBlockBuilder(name, Vector.empty)
  }

  class Counter(val basicBlock: Option[BasicBlock], val index: Int) {
    private[engine] def ++ = new Counter(basicBlock, index + 1)

    override def toString: String = basicBlock match {
      case None =>
        s"<undef>:$index"

      case Some(block) =>
        s"${block.name}:$index"
    }
  }

  object Counter {
    def reset = new Counter(None, 0)

    def atTheBeginningOf(basicBlock: BasicBlock) =
      new Counter(Some(basicBlock), 0)

    def inBlockWithIndex(basicBlock: BasicBlock, index: Int) =
      new Counter(Some(basicBlock), index)
  }

}
