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
package me.rand.simulator.main

import java.io.{IOException, PrintWriter}

import me.rand.asm.main.{AsmError, AsmOptions}
import me.rand.commons.idioms.Logger._
import me.rand.commons.idioms.Status._
import me.rand.vm.engine.{VmContext, VmRunner}
import me.rand.vm.main.{ExecutionContext, VmError}

import scala.io.Source

object Main {

  private class SimulatorContext(val asmSource: Source, val vmExecutionContext: ExecutionContext, val asmOptions: AsmOptions)

  def main(args: Array[String]): Unit =
    (for {
      options <- SimulatorOptions.fromUserArgs(args)
      simulatorContext <- setup(options)
      result <- assembleAndRun(simulatorContext)
    } yield result) match {
      case Err(error) =>
        System.err.println(error)
        System.exit(1)

      case Ok(_) =>
        // TODO: dump final context?
        println("ok")
    }

  private def setup(options: SimulatorOptions): SimulatorContext OrElse SimulatorError =
    (for {
      source <- getReaderForFile(options.asmOptions.in)
      executionContext <- setupExecutionContext(options)
    } yield new SimulatorContext(source, executionContext, options.asmOptions)) || {
      error =>
        System.err.println(error)
        error
    }

  private def getReaderForFile(file: java.io.File): Source OrElse SimulatorError =
    try {
      Ok(Source.fromFile(file))
    } catch {
      case err: IOException =>
        Err(SimulatorError.CantOpenAsmFile(file.getName, err))
    }

  private def setupExecutionContext(options: SimulatorOptions): ExecutionContext OrElse SimulatorError = {
    val loggerSpec = StandardLoggerConfiguration().withDebugLogs(options.asmOptions.verbose)
    setupTraceExecutionContext(loggerSpec, options) && (logger => new ExecutionContext(logger.build))
  }

  private def setupTraceExecutionContext(from: StandardLoggerConfiguration, options: SimulatorOptions): StandardLoggerConfiguration OrElse SimulatorError =
    options.traceOut match {
      case None =>
        Ok(from)

      case Some(None) =>
        Ok(from.withTracesToStdout)

      case Some(Some(file)) =>
        try {
          Ok(from.withTracesTo(new PrintWriter(file)))
        } catch {
          case ioe: IOException =>
            Err(SimulatorError.CantOpenTraceFile(file.getName, ioe))
        }
    }

  private def assembleAndRun(context: SimulatorContext): VmContext OrElse SimulatorError =
    (for {
      initialVmContext <- me.rand.asm.main.Main.assemble(context.asmOptions, context.asmSource)
      finalContext <- VmRunner.forAProgram(initialVmContext.program).execute(initialVmContext)(context.vmExecutionContext)
    } yield finalContext) || {
      case error: AsmError =>
        SimulatorError.FromAsmError(error)

      case error: VmError =>
        SimulatorError.FromVmError(error)
    }
}
