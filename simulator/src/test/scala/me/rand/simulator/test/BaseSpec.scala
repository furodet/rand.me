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

import me.rand.asm.dasm.VmContextSnapshot
import me.rand.commons.idioms.Status._
import me.rand.simulator.main.{Main, SimulatorError, SimulatorOptions}
import me.rand.vm.engine.Variable.Scalar
import me.rand.vm.engine.{Variable, VmContext}
import me.rand.vm.is.InstructionSetVersion
import org.scalatest.FlatSpec

class BaseSpec extends FlatSpec {

  sealed trait TestVerification[IN] {
    def thenVerify(testFunction: PartialFunction[IN, Boolean]): Unit
  }

  protected def successfullyAssembleAndExecute(source: String): TestVerification[VmContext] =
    Main.main(setupSimulatorOptionsToAssembleAndRun(source)) match {
      case Err(what) =>
        fail(s"simulator error: $what")

      case Ok(vmContext) =>
        new TestVerification[VmContext] {
          override def thenVerify(testFunction: PartialFunction[VmContext, Boolean]): Unit =
            testFunction.lift(vmContext) match {
              case Some(true) =>
                println(VmContextSnapshot.of(vmContext).all.mkString("\n"))

              case _ =>
                fail(s"unexpected result: ${VmContextSnapshot.of(vmContext).all.mkString("\n")}")
            }
        }
    }

  protected def failToAssembleOrExecute(source: String): TestVerification[SimulatorError] =
    Main.main(setupSimulatorOptionsToAssembleAndRun(source)) match {
      case Ok(vmContext) =>
        fail(s"unexpected success: ${VmContextSnapshot.of(vmContext).all.mkString("\n")}")

      case Err(what) =>
        new TestVerification[SimulatorError] {
          override def thenVerify(testFunction: PartialFunction[SimulatorError, Boolean]): Unit =
            testFunction.lift(what) match {
              case Some(true) =>
                println(what)

              case _ =>
                fail(s"unexpected failure: $what")
            }
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

  // Pre-defined .mach spec
  protected def aMachDirectiveWithSpecification(specification: String): String =
    s".mach ${InstructionSetVersion.current} $specification"

  protected def aMachDirectiveWithMachineWordLengthSetTo(nrBytes: Int): String =
    aMachDirectiveWithSpecification(s"bl:$nrBytes:heap:${VmContext.maximumNumberOfVariablesInHeap}")

  protected def aStandardMachineConfiguration: String =
    aMachDirectiveWithMachineWordLengthSetTo(8)

  // A standard "main" function that will be used in many tests.
  // May specify optional machine specifications (machptr), otherwise rely on the default VM configuration.
  // May specify a given machine word optionally, otherwise use default 64 bytes.
  // The main function is surrounded by mach spec plus a declaration of the main block on one end,
  // an exit instruction and a boot directive on the other.
  protected def main(body: String, optionalMachSpec: Option[String] = None, machineWordLength: Int = 8): String = {
    val head = optionalMachSpec match {
      case None =>
        StringBuilder.newBuilder
          .append(aMachDirectiveWithMachineWordLengthSetTo(machineWordLength)).append('\n')
          .append(".bb main").append('\n')

      case Some(text) =>
        StringBuilder.newBuilder
          .append(aMachDirectiveWithMachineWordLengthSetTo(machineWordLength)).append('\n')
          .append(text).append('\n')
          .append(".bb main").append('\n')
    }
    val tail =
      s"""
         | exit (00:u8)
         | .boot main
       """.stripMargin

    head + body + tail
  }

  // Straightforward validation of a variable value in the heap, with no detailed check
  protected def hasHeapVariable(index: Int, value: Int, vmContext: VmContext): Boolean =
    vmContext.heap.getVariable(index) match {
      case Ok(Some(Scalar(_, result))) =>
        result.toInt == value

      case whatever =>
        println(s"wrong! $whatever")
        false
    }

  // Straightforward validation of a variable value in the stack, with no detailed check
  protected def hasStackVariable(index: Int, value: Int, vmContext: VmContext): Boolean =
    vmContext.stack.getVariable(index) match {
      case Ok(Some(Scalar(_, result))) =>
        result.toInt == value

      case whatever =>
        println(s"wrong! $whatever")
        false
    }

  // Straightforward validation of a pointer in the heap, with no detailed check
  protected def hasHeapVariable(index: Int, value: Variable.Pointer.ToVariable, vmContext: VmContext): Boolean =
    (vmContext.heap.getVariable(index), value) match {
      case (Ok(Some(Variable.Pointer.ToVariable.InTheHeap(_, address))), Variable.Pointer.ToVariable.InTheHeap(_, expected)) =>
        expected == address

      case (Ok(Some(Variable.Pointer.ToVariable.InTheStack(_, address))), Variable.Pointer.ToVariable.InTheStack(_, expected)) =>
        expected == address

      case (whatever, _) =>
        println(s"wrong! $whatever")
        false
    }
}
