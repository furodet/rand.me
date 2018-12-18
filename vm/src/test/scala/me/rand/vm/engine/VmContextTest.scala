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

  "VM types" should "understand legal type strings and reject illegal type strings" in {
    VmContext.usingProfileString(s"bl:1:heap:10") match {
      case Ok(vmContext) =>
        vmContext.vmTypes.valueOf("u8") match {
          case Ok(vmType) if vmType.byteLen == 1 && vmType.isUnsigned =>
            ok()

          case Err(whatever) =>
            fail(s"unexpected result from value of (u8): $whatever")
        }

        vmContext.vmTypes.valueOf("s8") match {
          case Ok(vmType) if vmType.byteLen == 1 && vmType.isSigned =>
            ok()

          case whatever =>
            fail(s"unexpected result from value of (s8): $whatever")
        }

        vmContext.vmTypes.valueOf("u16") match {
          case Err(InvalidVmTypeString("u16")) =>
            ok()

          case whatever =>
            fail(s"unexpected result from value of (u16): $whatever")
        }
      case whatever =>
        fail(s"unexpected result from valid profile definition: $whatever")
    }
  }

  "VM context" should "not allow to pop frame from empty stack" in {
    givenAValidVmContext {
      vmContext =>
        vmContext.popFrame() match {
          case Err(EmptyStackAccess(_)) =>
            ok()

          case whatever =>
            fail(s"unexpected result for pop frame from empty stack: $whatever")
        }
    }
  }

  "VM context" should "allow to push and pop frames in the stack" in {
    givenAValidVmContext {
      c0 =>
        val c1 = c0.createFrameOfSize(1)
        assert(c1.stack.frames.length == 1)
        val c2 = c1.createFrameOfSize(2)
        assert(c2.stack.frames.length == 2)
        (for {
          c3 <- c2.popFrame()
          _ = assert(c3.stack.frames.length == 1)
          finalContext <- c3.popFrame()
        } yield finalContext) match {
          case Ok(context) if context.stack.frames isEmpty =>
            ok()

          case whatever =>
            fail(s"unexpected result from push/pop frames: $whatever")
        }
    }
  }

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

  "VM context" should "prevent from illegal read to a VarSet (negative index)" in {
    givenAValidVmContext {
      vmContext =>
        vmContext.heap.getVariable(-1) match {
          case Err(VariableIndexOutOfBounds(-1)) =>
            ok()

          case whatever =>
            fail(s"unexpected result from illegal access to heap: $whatever")
        }
    }
  }

  "VM context" should "prevent from illegal read to a VarSet (index too large)" in {
    givenAValidVmContext {
      vmContext =>
        vmContext.heap.getVariable(aStandardHeapSize) match {
          case Err(VariableIndexOutOfBounds(s)) if s == aStandardHeapSize =>
            ok()

          case whatever =>
            fail(s"unexpected result from illegal access to heap: $whatever")
        }
    }
  }

  "VM context" should "prevent from illegal write to a VarSet (negative index)" in {
    givenAValidVmContext {
      vmContext =>
        vmContext.heap.putVariable(Int.MinValue, aVariable(vmContext)) match {
          case Err(VariableIndexOutOfBounds(Int.MinValue)) =>
            ok()

          case whatever =>
            fail(s"unexpected result from illegal access to heap: $whatever")
        }
    }
  }

  "VM context" should "prevent from illegal write to a VarSet (index too large)" in {
    givenAValidVmContext {
      vmContext =>
        vmContext.heap.putVariable(Int.MaxValue, aVariable(vmContext)) match {
          case Err(VariableIndexOutOfBounds(Int.MaxValue)) =>
            ok()

          case whatever =>
            fail(s"unexpected result from illegal access to heap: $whatever")
        }
    }
  }

  "VM context" should "allow to read undefined VarSet variables" in {
    givenAValidVmContext {
      vmContext =>
        vmContext.heap.getVariable(0) match {
          case Ok(None) =>
            ok()

          case whatever =>
            fail(s"unexpected result from access to uninitialized heap variable: $whatever")
        }
    }
  }

  "VM context" should "allow to write then read a variable in a VarSet" in {
    givenAValidVmContext {
      vmContext =>
        val myVariable = aVariable(vmContext)
        (for {
          _ <- vmContext.heap.putVariable(2, myVariable)
          v <- vmContext.heap.getVariable(2)
        } yield v) match {
          case Ok(Some(variable)) if variable == myVariable =>
            ok()

          case whatever =>
            fail(s"unexpected result from put/get variable in heap: $whatever")
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
          c1 <- c0.setPcToBlockCalled("b0")
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
            c1 <- c0.setPcToBlockCalled("b0")
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
        c0.setPcToBlockCalled("b1") match {
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
        c0.setPcToBlockCalled("b1") match {
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
    Scalar("x", VmRegister.ofType(c.vmTypes.select(1, isSigned = true).get).withValue(0L))
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
