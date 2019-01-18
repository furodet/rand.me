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
package me.rand.asm.parser

import me.rand.vm.engine.VmProgram.InstructionInstance
import me.rand.vm.engine.VmTypes.VmType
import me.rand.vm.engine.{Variable, VmContext}

sealed trait AsmToken {
  def lineNumber: Int
}

object AsmToken {

  case class Mach(vmContext: VmContext, lineNumber: Int) extends AsmToken {
    override def toString: String =
      s"@$lineNumber ${vmContext.profile.vmTypes}"
  }

  sealed trait MachPtr[PTRT <: Variable.Pointer] extends AsmToken {
    def nativeType: VmType

    def lineNumber: Int

    override def toString: String =
      s"@$lineNumber $nativeType"
  }

  object MachPtr {

    case class ToInstruction(nativeType: VmType, lineNumber: Int) extends MachPtr[Variable.Pointer.ToInstruction]

    case class ToHeapVariable(nativeType: VmType, lineNumber: Int) extends MachPtr[Variable.Pointer.ToVariable.InTheHeap]

    case class ToStackVariable(nativeType: VmType, lineNumber: Int) extends MachPtr[Variable.Pointer.ToVariable.InTheStack]

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

      case class InTheHeap(name: String, heapIndex: Int, vmt: Variable.BasicType, lineNumber: Int) extends TagVariable {
        override def toString: String = s".var(%$heapIndex:$name:$vmt)"
      }

      case class InTheStack(name: String, stackIndex: Int, vmt: Variable.BasicType, lineNumber: Int) extends TagVariable {
        override def toString: String = s".var($$$stackIndex:$name:$vmt)"
      }

    }

    sealed trait FrameOperation extends Directive

    object FrameOperation {

      case class Push(nrVariables: Int, lineNumber: Int) extends FrameOperation {
        override def toString: String = s".push $nrVariables"
      }

      case class Pop(lineNumber: Int) extends FrameOperation {
        override def toString: String = s".pop"
      }

    }

  }

}
