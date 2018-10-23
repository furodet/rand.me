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

import me.rand.vm.engine.VmProgram.VmProgramBasicBlock

// Documentation: doc/vmarchitecture.md
class VmProgram(val basicBlocks: Map[String, VmProgramBasicBlock]) {
  def +(basicBlock: VmProgramBasicBlock): VmProgram =
    new VmProgram(basicBlocks + (basicBlock.name -> basicBlock))
}

object VmProgram {

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

  def empty: VmProgram = new VmProgram(Map.empty)
}
