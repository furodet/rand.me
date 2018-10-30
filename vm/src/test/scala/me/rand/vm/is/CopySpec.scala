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
import me.rand.vm.engine.Instruction.Operand.DestinationOperand._
import me.rand.vm.engine.Variable.{Pointer, Scalar}
import me.rand.vm.engine.VmContext
import me.rand.vm.main.VmError.VmExecutionError.IllegalEncodingError.UnspecifiedDestinationOperand

class CopySpec extends BaseIsSpec {
  private implicit val vmContext: VmContext = givenABareMinimalVmContext

  "copy" should "pass 'copy var imm'" in {
    Copy.execute(vmContext, ops_(ToHeapVariable(0), 0 -> imm_("u32", 123))) & (
      _.heap.getVariable(0)
      ) match {
      case Ok(Some(Scalar(variableName, value))) =>
        assert(variableName == "hp0")
        assert(value.data.toInt == 123)
        assert(value.vmType.isUnsigned)
        assert(value.vmType.byteLen == 4)

      case whatever =>
        fail(s"unexpected result of 'copy %0 123': $whatever")
    }
  }

  "copy" should "pass 'copy var var'" in {
    Copy.execute(vmContext, ops_(ToHeapVariable(0), 0 -> var_(StackVariable, 0))) & (
      _.heap.getVariable(0)
      ) match {
      case Ok(Some(Scalar(variableName, value))) =>
        assert(variableName == "hp0")
        assert(value.data.toInt == 0x12345678)
        assert(value.vmType.isUnsigned)
        assert(value.vmType.byteLen == 4)

      case whatever =>
        fail(s"unexpected result of 'copy %0 stk0': $whatever")
    }
  }

  "copy" should "pass 'copy var stack_ptr'" in {
    Copy.execute(vmContext, ops_(ToHeapVariable(0), 0 -> var_(HeapVariable, 1))) & (
      _.heap.getVariable(0)
      ) match {
      case Ok(Some(Pointer.ToVariable.InTheStack(pointerName, variableIndex))) =>
        assert(pointerName == "hp0")
        assert(variableIndex == 0)

      case whatever =>
        fail(s"unexpected result of 'copy %0 stk1': $whatever")
    }
  }

  "copy" should "pass 'copy var heap_ptr'" in {
    Copy.execute(vmContext, ops_(ToHeapVariable(0), 0 -> var_(StackVariable, 1))) & (
      _.heap.getVariable(0)
      ) match {
      case Ok(Some(Pointer.ToVariable.InTheHeap(pointerName, variableIndex))) =>
        assert(pointerName == "hp0")
        assert(variableIndex == 0)

      case whatever =>
        fail(s"unexpected result of 'copy %0 hp1': $whatever")
    }
  }

  "copy" should "pass 'copy var **ptr'" in {
    Copy.execute(vmContext, ops_(ToHeapVariable(0), 0 -> ind_(HeapVariable, 2, 2))) & (
      _.heap.getVariable(0)
      ) match {
      case Ok(Some(Scalar(name, value))) =>
        assert(name == "hp0")
        assert(value.data.toInt == 0x12345678)
        assert(value.vmType.isUnsigned)
        assert(value.vmType.byteLen == 4)

      case whatever =>
        fail(s"unexpected result of 'copy %0 **hp2': $whatever")
    }
  }

  "copy" should "pass 'copy var inst_ptr'" in {
    Copy.execute(vmContext, ops_(ToHeapVariable(0), 0 -> var_(StackVariable, 2))) & (
      _.heap.getVariable(0)
      ) match {
      case Ok(Some(Pointer.ToInstruction(pointerName, destination))) =>
        assert(pointerName == "hp0")
        assert(destination.index == 0)
        assert(destination.basicBlock.isDefined)
        assert(destination.basicBlock.get.name == "foo")

      case whatever =>
        fail(s"unexpected result of 'copy %0 stk2': $whatever")
    }
  }

  "copy" should "pass 'copy **var imm'" in {
    Copy.execute(vmContext, ops_(red_(HeapVariable, 2, 2), 0 -> imm_("u32", 12345))) & (
      _.stack.getVariable(0)
      ) match {
      case Ok(Some(Scalar(variableName, value))) =>
        assert(variableName == "stk0")
        assert(value.data.toInt == 12345)
        assert(value.vmType.isUnsigned)
        assert(value.vmType.byteLen == 4)

      case whatever =>
        fail(s"unexpected result of 'copy **(&&stk0) 12345': $whatever")
    }
  }

  "copy" should "not pass 'copy _ imm'" in {
    Copy.execute(vmContext, ops_(NoDestination, 0 -> imm_("u32", 123))) match {
      case Err(error@UnspecifiedDestinationOperand) =>
        executionContext.logger > error.toString

      case whatever =>
        fail(s"unexpected result of 'copy _ 123': $whatever")
    }
  }

  "copy" should "not pass 'copy var <undef>'" in {
    // TODO:
  }

  "copy" should "not pass 'copy <undef> var'" in {
    // TODO:
  }

}
