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
package me.rand.vm.engine

import me.rand.commons.idioms.Status._
import me.rand.vm.engine.VmProgram.{VmProgramBasicBlock, VmProgramCounter}
import me.rand.vm.main.VmError.VmContextError
import me.rand.vm.main.VmError.VmContextError.{NoSuchBasicBlock, ProgramCounterOutOfBlock, ProgramCounterOutOfBounds}

import scala.language.postfixOps

// Documentation: doc/vmarchitecture.md
class VmProgram(val basicBlocks: Map[String, VmProgramBasicBlock], val pc: VmProgramCounter) {
  def +(basicBlock: VmProgramBasicBlock): VmProgram =
    new VmProgram(basicBlocks + (basicBlock.name -> basicBlock), pc)

  def getCurrentInstruction: Instruction OrElse VmContextError =
    pc.basicBlock match {
      case None =>
        Err(ProgramCounterOutOfBlock())

      case Some(basicBlock) =>
        try {
          Ok(basicBlock.instructions(pc.index))
        } catch {
          case _: ArrayIndexOutOfBoundsException =>
            Err(ProgramCounterOutOfBounds(pc.index))
        }
    }

  def incrementPc: VmProgram = setPc(pc ++)

  def setPcToBlockCalled(blockName: String): VmProgram OrElse VmContextError =
    basicBlocks.get(blockName) match {
      case Some(basicBlock) =>
        Ok(setPc(VmProgramCounter.atTheBeginningOf(basicBlock)))

      case None =>
        Err(NoSuchBasicBlock(blockName))
    }

  private def setPc(pc: VmProgramCounter) = new VmProgram(basicBlocks, pc)
}

object VmProgram {
  def empty: VmProgram = new VmProgram(Map.empty, VmProgramCounter.reset)

  class VmProgramBasicBlock(val name: String, val instructions: Array[Instruction])

  class VmProgramBasicBlockBuilder(val name: String, val instructions: Vector[Instruction]) {
    def +(instruction: Instruction): VmProgramBasicBlockBuilder =
      new VmProgramBasicBlockBuilder(name, instructions :+ instruction)

    def build: VmProgramBasicBlock = {
      new VmProgramBasicBlock(name, instructions.toArray)
    }
  }

  object VmProgramBasicBlockBuilder {
    def aBasicBlockCalled(name: String) = new VmProgramBasicBlockBuilder(name, Vector.empty)
  }

  class VmProgramCounter(val basicBlock: Option[VmProgramBasicBlock], val index: Int) {
    private[engine] def ++ = new VmProgramCounter(basicBlock, index + 1)
  }

  object VmProgramCounter {
    def reset = new VmProgramCounter(None, 0)

    def atTheBeginningOf(basicBlock: VmProgramBasicBlock) =
      new VmProgramCounter(Some(basicBlock), 0)
  }

}
