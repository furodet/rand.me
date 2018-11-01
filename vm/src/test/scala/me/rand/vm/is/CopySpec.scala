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

import me.rand.commons.idioms.Status._
import me.rand.vm.dsl.AbstractAsmInstructionBuilder._
import me.rand.vm.dsl.AbstractAsmOperandBuilder._
import me.rand.vm.engine.Variable.{Pointer, Scalar}
import me.rand.vm.engine.VmContext
import me.rand.vm.main.VmError.VmExecutionError.IllegalEncodingError.{UnspecifiedDestinationOperand, UnspecifiedSourceOperand}

class CopySpec extends BaseIsSpec {

  "copy" should "pass 'copy 123:u32 > %0'" in {
    implicit val vmContext: VmContext = givenABareMinimalVmContext
    (for {
      command <- Copy < 123 / 'u32 > %(0)
      context <- command.execute(vmContext)
      result <- context.heap.getVariable(0)
    } yield result) match {
      case Ok(Some(Scalar(variableName, value))) =>
        assert(variableName == "hp0")
        assert(value.data.toInt == 123)
        assert(value.vmType.isUnsigned)
        assert(value.vmType.byteLen == 4)

      case whatever =>
        fail(s"unexpected result of 'copy 123:u32 > %0': $whatever")
    }
  }

  "copy" should "pass 'copy %0 > $0'" in {
    implicit val vmContext: VmContext = givenABareMinimalVmContext
    (for {
      command <- Copy < %(0) > $(0)
      context <- command.execute(vmContext)
      result <- context.stack.getVariable(0)
    } yield result) match {
      case Ok(Some(Scalar(variableName, value))) =>
        assert(variableName == "stk0")
        assert(value.data.toInt == 87654321)
        assert(value.vmType.isUnsigned)
        assert(value.vmType.byteLen == 4)

      case whatever =>
        fail(s"unexpected result of 'copy %0 > $$0': $whatever")
    }
  }

  "copy" should "pass 'copy %1 > %0 (pointer to stack)'" in {
    implicit val vmContext: VmContext = givenABareMinimalVmContext
    (for {
      command <- Copy < %(1) > %(0)
      context <- command.execute(vmContext)
      result <- context.heap.getVariable(0)
    } yield result) match {
      case Ok(Some(Pointer.ToVariable.InTheStack(pointerName, variableIndex))) =>
        assert(pointerName == "hp0")
        assert(variableIndex == 0)

      case whatever =>
        fail(s"unexpected result of 'copy %1 > %0 (pointer to stack)': $whatever")
    }
  }

  "copy" should "pass 'copy $1 > %0 (pointer to heap)'" in {
    implicit val vmContext: VmContext = givenABareMinimalVmContext
    (for {
      command <- Copy < $(1) > %(0)
      context <- command.execute(vmContext)
      result <- context.heap.getVariable(0)
    } yield result) match {
      case Ok(Some(Pointer.ToVariable.InTheHeap(pointerName, variableIndex))) =>
        assert(pointerName == "hp0")
        assert(variableIndex == 0)

      case whatever =>
        fail(s"unexpected result of 'copy $$1 > %0 (pointer to heap)': $whatever")
    }
  }

  "copy" should "pass 'copy $2 > %0 (pointer to instruction)'" in {
    implicit val vmContext: VmContext = givenABareMinimalVmContext
    (for {
      command <- Copy < $(2) > %(0)
      context <- command.execute(vmContext)
      result <- context.heap.getVariable(0)
    } yield result) match {
      case Ok(Some(Pointer.ToInstruction(pointerName, destination))) =>
        assert(pointerName == "hp0")
        assert(destination.index == 0)
        assert(destination.basicBlock.isDefined)
        assert(destination.basicBlock.get.name == "foo")

      case whatever =>
        fail(s"unexpected result of 'copy $$2 > %0 (pointer to instruction)': $whatever")
    }
  }

  "copy" should "pass 'copy **%2 > $0'" in {
    implicit val vmContext: VmContext = givenABareMinimalVmContext
    (for {
      command <- Copy < *.*(%(2)) > $(0)
      context <- command.execute(vmContext)
      result <- context.stack.getVariable(0)
    } yield result) match {
      case Ok(Some(Scalar(name, value))) =>
        assert(name == "stk0")
        assert(value.data.toInt == 12345678)
        assert(value.vmType.isUnsigned)
        assert(value.vmType.byteLen == 4)

      case whatever =>
        fail(s"unexpected result of 'copy **%2 $$0': $whatever")
    }
  }

  "copy" should "pass 'copy 12345:u32 > **%2'" in {
    implicit val vmContext: VmContext = givenABareMinimalVmContext
    (for {
      command <- Copy < 12345 / 'u32 > *.*(%(2))
      context <- command.execute(vmContext)
      result <- context.stack.getVariable(0)
    } yield result) match {
      case Ok(Some(Scalar(variableName, value))) =>
        assert(variableName == "stk0")
        assert(value.data.toInt == 12345)
        assert(value.vmType.isUnsigned)
        assert(value.vmType.byteLen == 4)

      case whatever =>
        fail(s"unexpected result of 'copy 12345:u32 > **%2': $whatever")
    }
  }

  "copy" should "pass 'copy &%0 > $2'" in {
    implicit val vmContext: VmContext = givenABareMinimalVmContext
    (for {
      command <- Copy < &(%(0)) > $(2)
      context <- command.execute(vmContext)
      result <- context.stack.getVariable(2)
    } yield result) match {
      case Ok(Some(Pointer.ToVariable.InTheHeap(pointerName, referenceIndex))) =>
        assert(pointerName == "stk2")
        assert(referenceIndex == 0)

      case whatever =>
        fail(s"unexpected result of 'copy &%0 > $$2': $whatever")
    }
  }

  "copy" should "not pass 'copy 123 > _'" in {
    implicit val vmContext: VmContext = givenABareMinimalVmContext
    (for {
      command <- Copy < 123 / 'u32 > ()
      context <- command.execute(vmContext)
    } yield context) match {
      case Err(error@UnspecifiedDestinationOperand) =>
        executionContext.logger > error.toString

      case whatever =>
        fail(s"unexpected result of 'copy 123 > _': $whatever")
    }
  }

  "copy" should "not pass 'copy _ > %0'" in {
    implicit val vmContext: VmContext = givenABareMinimalVmContext
    (for {
      command <- Copy < () > %(0)
      context <- command.execute(vmContext)
    } yield context) match {
      case Err(error@UnspecifiedSourceOperand(0)) =>
        executionContext.logger > error.toString

      case whatever =>
        fail(s"unexpected result of 'copy %0 _': $whatever")
    }
  }
}
