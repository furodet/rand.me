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

import me.rand.commons.idioms.NormalizedNumber._
import me.rand.commons.idioms.Status._
import me.rand.vm.dsl.AbstractAsmInstructionBuilder._
import me.rand.vm.dsl.AbstractAsmOperandBuilder._
import me.rand.vm.engine.Variable.Scalar
import me.rand.vm.engine.VmContext
import me.rand.vm.main.VmError

class BitFlipSpec extends BaseIsSpec {
  private lazy val bitflip = InstructionSet.map(BitFlip.shortName)

  "bitflip" should "pass ~0xaaaaaaaa:u32 > %0" in {
    implicit val vmContext: VmContext = givenABareMinimalVmContext
    (for {
      command <- bitflip < !!(0xaaaaaaaa, 'u32) > %(0)
      context <- command.execute(vmContext)
      result <- context.heap.getVariable(0)
    } yield result) match {
      case Ok(Some(Scalar(variableName, value))) =>
        assert(variableName == "hp0")
        assert(value.toInt == 0x55555555)
        assert(value.vmType.isUnsigned)
        assert(value.vmType.byteLen == 4)

      case whatever =>
        fail(s"unexpected result of '~0xaaaaaaaa:u32 > %0': $whatever")
    }
  }

  "bitflip" should "pass ~%0 > $0" in {
    implicit val vmContext: VmContext = givenABareMinimalVmContext
    (for {
      command <- bitflip < %(0) > $(0)
      context <- command.execute(vmContext)
      result <- context.stack.getVariable(0)
    } yield result) match {
      case Ok(Some(Scalar(variableName, value))) =>
        assert(variableName == "stk0")
        assert(value.vmType.isUnsigned)
        assert(value.toInt == ~0x87654321)

      case whatever =>
        fail(s"unexpected result of '~%0 > $$0': $whatever")
    }
  }

  "bitflip" should "pass ~$0 > %0" in {
    implicit val vmContext: VmContext = givenABareMinimalVmContext
    (for {
      command <- bitflip < $(0) > %(0)
      context <- command.execute(vmContext)
      result <- context.heap.getVariable(0)
    } yield result) match {
      case Ok(Some(Scalar(variableName, value))) =>
        assert(variableName == "hp0")
        assert(value.vmType.isUnsigned)
        assert(value.toInt == ~0x12345678)

      case whatever =>
        fail(s"unexpected result of '~$$0 > %0 > %0': $whatever")
    }
  }

  "bitflip" should "pass '~**%2 > $0'" in {
    implicit val vmContext: VmContext = givenABareMinimalVmContext
    (for {
      command <- bitflip < *.*(%(2)) > %(0)
      context <- command.execute(vmContext)
      result <- context.heap.getVariable(0)
    } yield result) match {
      case Ok(Some(Scalar(variableName, value))) =>
        assert(variableName == "hp0")
        assert(value.vmType.isUnsigned)
        assert(value.toInt == ~0x12345678)

      case whatever =>
        fail(s"unexpected result of '~**%2 > $$0': $whatever")
    }
  }

  "copy" should "pass '~12345:u32 > **%2'" in {
    implicit val vmContext: VmContext = givenABareMinimalVmContext
    (for {
      command <- bitflip < !!(12345, 'u32) > *.*(%(2))
      context <- command.execute(vmContext)
      result <- context.stack.getVariable(0)
    } yield result) match {
      case Ok(Some(Scalar(variableName, value))) =>
        assert(variableName == "stk0")
        assert(value.toInt == ~12345)
        assert(value.vmType.isUnsigned)
        assert(value.vmType.byteLen == 4)

      case whatever =>
        fail(s"unexpected result of '~12345:u32 > **%2': $whatever")
    }
  }

  "bitflip" should "pass '~%1 > %0 (pointer to instruction)'" in {
    implicit val vmContext: VmContext = givenABareMinimalVmContext
    (for {
      command <- bitflip < %(1) > %(0)
      context <- command.execute(vmContext)
      result <- context.heap.getVariable(0)
    } yield result) match {
      case Err(err: VmError.SyntaxError.NoMatchingProfile) =>
        println(err)

      case whatever =>
        fail(s"unexpected result of '~$$2 > %0 (pointer to instruction)': $whatever")
    }
  }
}
