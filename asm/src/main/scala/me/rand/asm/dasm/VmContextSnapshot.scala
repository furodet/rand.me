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
package me.rand.asm.dasm

import me.rand.vm.engine.{VmContext, VmProgram}

import scala.collection.mutable.ListBuffer

class VmContextSnapshot(vmContext: VmContext) {
  def generics: List[String] = List(
    s"TYPES: ${vmContext.vmTypes}",
    s"STATE: ${explainState(vmContext.exitCode)}"
  )

  def heap: List[String] = {
    val list = ListBuffer.newBuilder[String]
    list += "HEAP"
    vmContext.heap.zipWithIndex.foreach {
      case (Some(variable), index) =>
        list += "%%%-16d%s %s".format(index, variable.name, variable.getValueString)
      case _ => // Nothing to do
    }
    list.result().toList
  }

  def stack: List[String] = {
    val list = ListBuffer.newBuilder[String]
    list += "STACK"
    vmContext.stack.frames.zipWithIndex.foreach {
      case (eachFrame, frameIndex) =>
        list += s"FRAME#$frameIndex"
        eachFrame.vars.zipWithIndex.foreach {
          case (Some(variable), index) =>
            list += "$%-16d%s %s".format(index, variable.name, variable.getValueString)
          case _ => // Nothing to do
        }
    }
    list.result().toList
  }

  def program: List[String] =
    "PROGRAM" +: explainPc(vmContext.program.pc) +: disassembleBasicBlocks(vmContext.program.basicBlocks)

  def all: List[String] = generics ++ heap ++ stack ++ program

  private def explainState(maybeExitCode: Option[Int]): String =
    maybeExitCode match {
      case None =>
        "running"

      case Some(exitCode) =>
        s"exit:$exitCode"
    }

  private def explainPc(pc: VmProgram.Counter): String =
    pc.basicBlock match {
      case Some(basicBlock) =>
        s"${basicBlock.name}:${pc.index}"

      case None =>
        s"???:???"
    }

  private def disassembleBasicBlocks(basicBlocks: Map[String, VmProgram.BasicBlock]): List[String] = {
    val list = ListBuffer.newBuilder[String]
    for (eachBasicBlock <- basicBlocks.values) {
      list += s".bb ${eachBasicBlock.name}"
      list ++= disassembleBasicBlockInstructions(eachBasicBlock)
    }
    list.result().toList
  }

  private def disassembleBasicBlockInstructions(basicBlock: VmProgram.BasicBlock): List[String] = {
    val list = ListBuffer.newBuilder[String]
    for (eachInstruction <- basicBlock.instructions) {
      val disassembled = Disassemble.instruction(eachInstruction).toInstructionKeywordAndOperandStrings
      list += s"\t$disassembled"
    }
    list.result().toList
  }
}

object VmContextSnapshot {
  def of(vmContext: VmContext) = new VmContextSnapshot(vmContext)
}