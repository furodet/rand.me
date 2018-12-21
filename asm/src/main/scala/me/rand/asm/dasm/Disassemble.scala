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

import me.rand.asm.dasm.Disassemble.DisassembledInstruction
import me.rand.vm.engine.VmControl
import me.rand.vm.engine.VmProgram.{InlineDirective, InstructionInstance, VirtualInstruction}

class Disassemble(instruction: VirtualInstruction) {
  def toInstructionKeywordAndOperandStrings: DisassembledInstruction =
    instruction match {
      case InlineDirective(VmControl.TagVariable(name, id, initialValue)) =>
        new DisassembledInstruction(VmControl.TagVariable.name, List(name, id.toString, initialValue.toString), "_")

      case InlineDirective(VmControl.FrameOperation.Push(length)) =>
        new DisassembledInstruction(VmControl.FrameOperation.Push.name, List(length.toString), "_")

      case InlineDirective(VmControl.FrameOperation.Pop) =>
        new DisassembledInstruction(VmControl.FrameOperation.Pop.name, List.empty, "_")

      case InstructionInstance(i, operands) =>
        new DisassembledInstruction(i.name, operands.sources.map(_.toString), operands.destination.toString)
    }
}

object Disassemble {
  def instruction(instruction: VirtualInstruction) = new Disassemble(instruction)

  class DisassembledInstruction(val keyword: String, val operands: List[String], val destination: String) {
    override def toString: String = s"$keyword ${operands.mkString(" ")} > $destination"
  }

}
