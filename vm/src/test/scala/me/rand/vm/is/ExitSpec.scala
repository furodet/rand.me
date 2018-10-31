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
import me.rand.vm.dsl.EmbeddedAsm._
import me.rand.vm.engine.VmContext
import me.rand.vm.main.VmError.VmExecutionError.IllegalEncodingError.UnspecifiedSourceOperand
import me.rand.vm.main.VmError.VmExecutionError.VmFetchOperandError.NotAnImmediateOperand

class ExitSpec extends BaseIsSpec {
  "exit" should "pass 'exit imm'" in {
    implicit val vmContext: VmContext = givenAnEmptyVmContext
    (for {
      command <- <<(Exit) - (42 / 'u8) >> ()
      context <- command.execute(vmContext)
    } yield context) match {
      case Ok(newContext) =>
        assert(newContext.exitCode.getOrElse(-1) == 42)

      case whatever =>
        fail(s"unexpected result of 'exit 42': $whatever")
    }
  }

  "exit" should "fail 'exit'" in {
    implicit val vmContext: VmContext = givenAnEmptyVmContext
    (for {
      command <- <<(Exit) >> ()
      context <- command.execute(vmContext)
    } yield context) match {
      case Err(err: UnspecifiedSourceOperand) =>
        executionContext.logger > err.toString

      case whatever =>
        fail(s"unexpected result of 'exit': $whatever")
    }
  }

  "exit" should "fail 'exit %0'" in {
    implicit val vmContext: VmContext = givenAnEmptyVmContext
    (for {
      command <- <<(Exit) - -%(0) >> ()
      context <- command.execute(vmContext)
    } yield context) match {
      case Err(err: NotAnImmediateOperand) =>
        executionContext.logger > err.toString

      case whatever =>
        fail(s"unexpected result of 'exit %0': $whatever")
    }
  }

  "exit" should "fail 'exit *%2'" in {
    implicit val vmContext: VmContext = givenABareMinimalVmContext
    (for {
      command <- <<(Exit) - -*(-%(2)) >> ()
      context <- command.execute(vmContext)
    } yield context) match {
      case Err(err: NotAnImmediateOperand) =>
        executionContext.logger > err.toString

      case whatever =>
        fail(s"unexpected result of 'exit ind': $whatever")
    }
  }
}
