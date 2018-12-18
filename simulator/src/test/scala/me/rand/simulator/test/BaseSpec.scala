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
package me.rand.simulator.test

import java.io.PrintWriter

import me.rand.commons.idioms.Status._
import me.rand.simulator.main.{Main, SimulatorError, SimulatorOptions}
import me.rand.vm.engine.VmContext
import org.scalatest.FlatSpec

class BaseSpec extends FlatSpec {
  protected def successfulAssemblyAndExecutionOf(source: String)(testResult: VmContext => Unit): Unit =
    Main.main(setupSimulatorOptionsToAssembleAndRun(source)) match {
      case Err(what) =>
        fail(s"simulator error: $what")

      case Ok(vmContext) =>
        testResult(vmContext)
    }

  protected def failureOfAssemblyOrExecutionOf(source: String)(testResult: PartialFunction[SimulatorError, Boolean]): Unit =
    Main.main(setupSimulatorOptionsToAssembleAndRun(source)) match {
      case Ok(_) =>
        fail("unexpected success")

      case Err(what) =>
        testResult.lift(what) match {
          case None =>
            fail(s"unexpected failure: $what")

          case Some(_) =>
            println(what)
        }
    }

  private def setupSimulatorOptionsToAssembleAndRun(source: String): SimulatorOptions = {
    val fileName = writeSourceIntoTemporaryTestFile(source)
    SimulatorOptions.fromUserArgs(Array("--vv:asm", "--vv:run", "-t", ".", "-s", fileName)) match {
      case Ok(options) =>
        options

      case Err(what) =>
        throw new RuntimeException(s"BUG: could not setup options: $what")
    }
  }

  // Intentionally write the current test into a file, so that one can easily replay it
  // offline, if needed.
  private def writeSourceIntoTemporaryTestFile(source: String): String = {
    val output = new java.io.File("test.rasm")
    val writer = new PrintWriter(output)
    try {
      writer.println(source)
    } catch {
      case ioe: java.io.IOException =>
        throw new RuntimeException(s"BUG: $ioe")
    } finally {
      writer.close()
    }
    output.getAbsoluteFile.toString
  }
}
