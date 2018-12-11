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

import me.rand.vm.alu.VmRegister
import me.rand.vm.engine.VmContext
import me.rand.vm.engine.VmProgram.InstructionInstance

sealed trait AsmToken {
  def lineNumber: Int
}

object AsmToken {

  case class Mach(vmContext: VmContext, lineNumber: Int) extends AsmToken {
    override def toString: String =
      s"@$lineNumber ${vmContext.vmTypes}"
  }

  case class Instruction(instance: InstructionInstance, lineNumber: Int) extends AsmToken {
    override def toString: String =
      s"@$lineNumber $instance"
  }

  sealed trait Directive extends AsmToken

  object Directive {

    case class DeclareBasicBlock(name: String, lineNumber: Int) extends Directive {
      override def toString: String = s".bb($name)"
    }

    case class DefineBootBasicBlock(name: String, lineNumber: Int) extends Directive {
      override def toString: String = s".boot($name)"
    }

    sealed trait TagVariable extends Directive

    object TagVariable {

      case class InTheHeap(name: String, heapIndex: Int, initialValue: VmRegister, lineNumber: Int) extends TagVariable {
        override def toString: String = s".var(%$heapIndex:$name=$initialValue)"
      }

      case class InTheStack(name: String, stackIndex: Int, initialValue: VmRegister, lineNumber: Int) extends TagVariable {
        override def toString: String = s".var($$$stackIndex:$name=$initialValue)"
      }

    }

  }

}
