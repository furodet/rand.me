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
package me.rand.asm.out

import java.io.PrintWriter

import me.rand.vm.engine.VmProgram.{InlineDirective, InstructionInstance}
import me.rand.vm.engine.{Operand, VmContext, VmProgram}

class VmContextDump(vmContext: VmContext) {
  def into(out: PrintWriter): Unit = {
    out.println(s"PC:${vmContext.program.pc}")
    vmContext.program.basicBlocks.values.foreach {
      eachBasicBlock =>
        out.println(s"${eachBasicBlock.name}:")
        eachBasicBlock.instructions.foreach {
          eachInstruction =>
            printInstructionInto(out, eachInstruction)
        }
    }
  }

  private def printInstructionInto(out: PrintWriter, instruction: VmProgram.VirtualInstruction): Unit =
    instruction match {
      case InlineDirective(vmControl) =>
        out.println(s"\t$vmControl")

      case InstructionInstance(vmInstruction, operands) =>
        val sources = operands.sources.mkString(" ")
        val destination = operands.destination match {
          case Operand.Destination.NoDestination =>
            "_"

          case aDestination =>
            aDestination.toString
        }
        out.println("\t%-10s %s > %s".format(vmInstruction.name, sources, destination))
    }
}

object VmContextDump {
  def forContext(vmContext: VmContext) = new VmContextDump(vmContext)
}
