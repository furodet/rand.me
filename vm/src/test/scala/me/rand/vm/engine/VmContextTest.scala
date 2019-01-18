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
package me.rand.vm.engine

import me.rand.commons.idioms.NormalizedNumber._
import me.rand.commons.idioms.Status._
import me.rand.vm.alu.VmRegister
import me.rand.vm.engine.Variable.Scalar
import me.rand.vm.engine.VmProgram.InstructionInstance
import me.rand.vm.is.{Exit, InstructionSet}
import me.rand.vm.main.VmError.VmContextError._
import org.scalatest.FlatSpec

import scala.language.postfixOps

class VmContextTest extends FlatSpec {
  // just a decorating nit.
  private def ok(): Unit = ()

  private val aStandardHeapSize = 10

  "VM context" should "not allow to read variables from an empty stack" in {
    givenAValidVmContext {
      vmContext =>
        vmContext.stack.getVariable(0) match {
          case Err(EmptyStackAccess(_)) =>
            ok()

          case whatever =>
            fail(s"unexpected result from get variable from empty stack: $whatever")
        }
    }
  }

  "VM context" should "not allow to write variables into an empty stack" in {
    givenAValidVmContext {
      vmContext =>
        vmContext.stack.putVariable(0, aVariable(vmContext)) match {
          case Err(EmptyStackAccess(_)) =>
            ok()

          case whatever =>
            fail(s"unexpected result from put variable from empty stack: $whatever")
        }
    }
  }

  "VM context" should "allow to write then read variables in stack" in {
    givenAValidVmContext {
      vmContext =>
        val myVariable = aVariable(vmContext)
        val hasFrame = vmContext.createFrameOfSize(1)
        (for {
          _ <- hasFrame.stack.putVariable(0, myVariable)
          v <- hasFrame.stack.getVariable(0)
        } yield v) match {
          case Ok(Some(variable)) if variable == myVariable =>
            ok()

          case whatever =>
            fail(s"unexpected result from put/get variable from stack: $whatever")
        }
    }
  }

  "VM program" should "not return instruction if no basic block is defined" in {
    givenAValidVmContext {
      vmContext =>
        vmContext.program.nextInstruction match {
          case Err(ProgramCounterOutOfBlock) =>
            ok()

          case whatever =>
            fail(s"unexpected result when reading empty program memory: $whatever")
        }
    }
  }

  "VM program" should "return an instruction if basic block is not entirely parsed" in {
    givenAValidVmContext {
      vmContext =>
        val c0 = vmContext.setProgram(aDummyProgramWithOneBasicBlockOfOneInstruction)
        (for {
          c1 <- c0.resetPcToBlockCalled("b0")
          instruction <- c1.program.nextInstruction
        } yield instruction) match {
          case Ok(InstructionInstance(instruction, _)) if instruction == InstructionSet.map(Exit.shortName) =>
            ok()

          case whatever =>
            fail(s"unexpected result when reading valid program memory: $whatever")
        }
    }
  }

  "VM program" should "not return instruction if basic block is entirely parsed" in {
    givenAValidVmContext {
      vmContext =>
        val c0 = vmContext.setProgram(aDummyProgramWithOneBasicBlockOfOneInstruction)
        (
          for {
            c1 <- c0.resetPcToBlockCalled("b0")
            c2 = c1.incrementPc.incrementPc
            instruction <- c2.program.nextInstruction
          } yield instruction) match {
          case Err(ProgramCounterOutOfBounds(2)) =>
            ok()

          case whatever =>
            fail(s"unexpected result when reading invalid program memory: $whatever")
        }
    }
  }

  "VM program" should "not allow to jump to an unknown block" in {
    givenAValidVmContext {
      vmContext =>
        val c0 = vmContext.setProgram(aDummyProgramWithOneBasicBlockOfOneInstruction)
        c0.resetPcToBlockCalled("b1") match {
          case Err(NoSuchBasicBlock("b1")) =>
            ok()

          case whatever =>
            fail(s"unexpected result when jumping to unknown block: $whatever")
        }
    }
  }

  "VM program" should "allow to jump to a known block and reset PC offset" in {
    givenAValidVmContext {
      vmContext =>
        val c0 = vmContext.setProgram(aDummyProgramWithTwoBasicBlocksOfOneInstruction)
        c0.resetPcToBlockCalled("b1") match {
          case Ok(c1) =>
            c1.program.pc.basicBlock match {
              case Some(basicBlock) if basicBlock.name == "b1" =>
                ok()

              case whatever =>
                fail(s"unexpected result from valid jump: $whatever")
            }
            assert(c1.program.pc.index == 0)

          case whatever =>
            fail(s"unexpected result when jumping to unknown block: $whatever")
        }
    }
  }

  private def aVariable(c: VmContext) = {
    Scalar("x", VmRegister.ofType(c.profile.vmTypes.select(1, isSigned = true).get).withValue(0L))
  }

  private def givenAValidVmContext(action: VmContext => Unit): Unit =
    VmContext.usingProfileString(s"bl:1:heap:$aStandardHeapSize") match {
      case Ok(vmContext) =>
        action(vmContext)

      case whatever =>
        fail(s"unexpected result from valid profile definition: $whatever")
    }

  private def aDummyProgramWithOneBasicBlockOfOneInstruction: VmProgram =
    VmProgram.empty ++ aDummyBasicBlockCalled("b0")

  private def aDummyProgramWithTwoBasicBlocksOfOneInstruction: VmProgram =
    VmProgram.empty ++ aDummyBasicBlockCalled("b0") ++ aDummyBasicBlockCalled("b1")

  private def aDummyBasicBlockCalled(name: String) =
    VmProgram.BasicBlockBuilder
      .aBasicBlockCalled(name) + InstructionInstance(InstructionSet.map(Exit.shortName), Operands.none) build
}
