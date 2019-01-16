package me.rand.simulator.main

import java.io.{IOException, PrintWriter}

import me.rand.asm.dasm.VmContextSnapshot
import me.rand.asm.main.{AsmError, AsmOptions}
import me.rand.commons.idioms.Logger.StandardLoggerConfiguration
import me.rand.commons.idioms.Status._
import me.rand.vm.engine.{VmContext, VmRunner}
import me.rand.vm.main.{ExecutionContext, VmError}

import scala.io.Source

object Main {

  private class SimulatorContext(val asmSource: Source, val vmExecutionContext: ExecutionContext, val asmOptions: AsmOptions)

  def main(args: Array[String]): Unit =
    (for {
      options <- SimulatorOptions.fromUserArgs(args)
      result <- main(options)
    } yield result) match {
      case Err(error) =>
        System.err.println(error)
        System.exit(1)

      case Ok(vmContext) =>
        println(VmContextSnapshot.of(vmContext).all.mkString("\n"))
    }

  def main(options: SimulatorOptions): VmContext OrElse SimulatorError =
    for {
      simulatorContext <- setup(options)
      result <- assembleAndRun(simulatorContext)
    } yield result

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
