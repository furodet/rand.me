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
package me.rand.vm.is

import java.io.PrintWriter

import me.rand.commons.idioms.Logger._
import me.rand.commons.idioms.Status._
import me.rand.vm.engine.Instruction.Operand.Source
import me.rand.vm.engine.Instruction.Operands
import me.rand.vm.engine.VmProgram.BasicBlockBuilder.aBasicBlockCalled
import me.rand.vm.engine.VmProgram.{Counter, InstructionInstance}
import me.rand.vm.engine.{Variable, VmContext, VmProgram, VmWord}
import me.rand.vm.main.ExecutionContext
import org.scalatest._

class BaseIsSpec extends FlatSpec with BeforeAndAfterEach {

  protected trait VariableLocation

  protected case object HeapVariable extends VariableLocation

  protected case object StackVariable extends VariableLocation

  private val prErr = new PrintWriter(System.err)
  private val prOut = new PrintWriter(System.out)
  implicit val executionContext: ExecutionContext =
    new ExecutionContext(forConfiguration(
      Seq(
        LogError -> ("E| " to prErr),
        LogWarning -> ("W| " to prErr),
        LogInfo -> ("I| " to prOut),
        LogDebug -> ("D| " to prOut),
        LogTrace -> ("-| " to prOut)
      )
    ))

  override def afterEach(): Unit = {
    prOut.flush()
    prErr.flush()
  }

  protected def givenAnEmptyVmContext: VmContext =
    VmContext.usingProfileString("bl:4:heap:1024") match {
      case Ok(emptyContext) =>
        implicit val c: VmContext = emptyContext
        (for {
          _ <- c.heap.putVariable(0, createScalarVariable("hp0", "u32", 0))
        } yield c) match {
          case Ok(context) =>
            context

          case Err(error) =>
            fail(s"could not create VM context: $error")
        }

      case Err(error) =>
        fail(s"could not create VM context: $error")
    }

  protected def givenABareMinimalVmContext: VmContext =
    VmContext.usingProfileString("bl:4:heap:1024") match {
      case Ok(emptyContext) =>
        implicit val c: VmContext = emptyContext.createFrameOfSize(3)
        // Here's what we build:
        // HEAP:
        //
        //  +--------------------+
        //  |                    |
        //  |   2 &&stk0       >-+
        //  +-> 1 &stk0        >--------+
        //  +-> 0 hp0       ...:u32     |
        //  |                           |
        //  +--------------------+      |
        // STACK:                |      |
        //      2 stk2    Iptr to 'foo' |
        //      1 &hp0         >-+      |
        //  +-> 0 stk0      ...:u32     |
        //  |                           |
        //  +---------------------------+
        //
        // So that we have:
        //   **(&&stk0) = *(&stk0) = stk0 = 12345678
        //    *(hp0) = hp0 = 87654321
        val fooBasicBlock = aBasicBlockCalled("foo")
          .+(new InstructionInstance(Exit, Operands.none.addSource(0 -> imm_("u8", 123))))
          .build
        (for {
          _ <- c.stack.putVariable(0, createScalarVariable("stk0", "u32", 12345678))
          _ <- c.stack.putVariable(1, Variable.Pointer.ToVariable.InTheHeap("&hp0", 0))
          _ <- c.stack.putVariable(2, Variable.Pointer.ToInstruction("stk2", Counter.atTheBeginningOf(fooBasicBlock)))
          _ <- c.heap.putVariable(0, createScalarVariable("hp0", "u32", 87654321))
          _ <- c.heap.putVariable(1, Variable.Pointer.ToVariable.InTheStack("&stk0", 0))
          _ <- c.heap.putVariable(2, Variable.Pointer.ToVariable.InTheHeap("&&stk0", 1))
        } yield c.setProgram(VmProgram.empty.++(fooBasicBlock))) match {
          case Ok(context) =>
            context

          case Err(error) =>
            fail(s"could not create VM context: $error")
        }
      case Err(error) =>
        fail(s"could not create VM context: $error")
    }

  private def imm_(typeString: String, value: Int)(implicit vmContext: VmContext): Source.Immediate =
    Source.Immediate(createVmWord(typeString, value, vmContext))

  private def createScalarVariable(name: String, typeString: String, value: Int)(implicit vmContext: VmContext): Variable.Scalar =
    Variable.Scalar(name, createVmWord(typeString, value, vmContext))

  private def createVmWord(typeString: String, value: Int, vmContext: VmContext): VmWord =
    vmContext.vmTypes.valueOf(typeString) && {
      t => VmWord.ofType(t).withValue(BigInt(value))
    } match {
      case Err(error) =>
        fail(s"could not create value of type '$typeString': $error")

      case Ok(variable) =>
        variable
    }
}
