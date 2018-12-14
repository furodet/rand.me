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
package me.rand.commons.idioms

import java.io.PrintWriter

import me.rand.commons.idioms.Logger.LoggerConfiguration

import scala.language.implicitConversions

/** Logs messages issued by programs.
  *
  * Idiom to build a Logger is: {{{Logger.forConfiguration(Seq(T -> CONFIG, T -> CONFIG...))}}}
  * Where {{{CONFIG}}} specifies what to do with each of the following types of message {{{T}}}:
  * {{{LogError, LogWarning, LogInfo, LogDebug}}} plus the special type {{{LogTrace}}}
  * which reflects a message traced by the program for further usage.
  *
  * Syntax of a {{{CONFIG}}} is {{{PREFIX >>> OUTPUT}}}, where:
  *   - {{{PREFIX}}} is a string printed before each new message
  *   - {{{OUTPUT}}} is a {{{PrintWriter}}} (e.g. {{{System.out}}})
  *
  * Once configured, a {{{Logger}}} can print a message of a given type with the
  * following functions:
  *   - {{{!!}}} : print an error
  *   - {{{!}}} : print a warning
  *   - {{{>}}} : print an info
  *   - {{{~>}}} : print a debug information
  *   - {{{>>}}} : print a trace
  *
  */
class Logger(configuration: LoggerConfiguration) {
  private def printIfNeeded(message: String, out: Option[(String, PrintWriter)]): Unit =
    out match {
      case None =>
      // Ignore

      case Some((prefix, printWriter)) =>
        printWriter.println(prefix + message)
        printWriter.flush()
    }

  def !!(errorMessage: String): Unit = printIfNeeded(errorMessage, configuration.errorOut)

  def !(warningMessage: String): Unit = printIfNeeded(warningMessage, configuration.warningOut)

  def >(infoMessage: String): Unit = printIfNeeded(infoMessage, configuration.infoOut)

  def ~>(traceMessage: String): Unit = printIfNeeded(traceMessage, configuration.traceOut)

  def >>(debugMessage: String): Unit = printIfNeeded(debugMessage, configuration.debugOut)
}

object Logger {

  sealed trait LogType

  case object LogError extends LogType

  case object LogWarning extends LogType

  case object LogInfo extends LogType

  case object LogDebug extends LogType

  case object LogTrace extends LogType

  class LoggerOutputSpecification(val prefix: String, val writer: PrintWriter)

  class LoggerOutputSpecificationBuilder(prefix: String) {
    def to(writer: PrintWriter): LoggerOutputSpecification =
      new LoggerOutputSpecification(prefix, writer)
  }

  implicit def StringToLoggerOutputSpecificationBuilder(prefix: String): LoggerOutputSpecificationBuilder =
    new LoggerOutputSpecificationBuilder(prefix)

  class LoggerConfiguration(val errorOut: Option[(String, PrintWriter)],
                            val warningOut: Option[(String, PrintWriter)],
                            val infoOut: Option[(String, PrintWriter)],
                            val debugOut: Option[(String, PrintWriter)],
                            val traceOut: Option[(String, PrintWriter)]
                           ) {
    def setOutput(logType: LogType, output: LoggerOutputSpecification): LoggerConfiguration = {
      val spec = (output.prefix, output.writer)
      logType match {
        case LogError =>
          new LoggerConfiguration(Some(spec), warningOut, infoOut, debugOut, traceOut)
        case LogWarning =>
          new LoggerConfiguration(errorOut, Some(spec), infoOut, debugOut, traceOut)
        case LogInfo =>
          new LoggerConfiguration(errorOut, warningOut, Some(spec), debugOut, traceOut)
        case LogDebug =>
          new LoggerConfiguration(errorOut, warningOut, infoOut, Some(spec), traceOut)
        case LogTrace =>
          new LoggerConfiguration(errorOut, warningOut, infoOut, debugOut, Some(spec))
      }
    }
  }

  private object LoggerConfiguration {
    def silent = new LoggerConfiguration(None, None, None, None, None)
  }

  def forConfiguration(configuration: Seq[(LogType, LoggerOutputSpecification)]): Logger = {
    val loggerConfiguration = configuration.foldLeft(LoggerConfiguration.silent) {
      case (underConstruction, (eachType, eachOutput)) =>
        underConstruction.setOutput(eachType, eachOutput)
    }
    new Logger(loggerConfiguration)
  }

  // For practical application context, standard logging is:
  //  - log info to stdout
  //  - log errors and warnings to stderr
  //  - debug log is optional to stdout
  //  - traces are optional and sent to a given output file
  // Provide a ready-made builder for that.
  class StandardLoggerConfiguration(setup: Seq[(LogType, LoggerOutputSpecification)], errWriter: PrintWriter, outWriter: PrintWriter) {
    def withDebugLogs(allowed: Boolean = false): StandardLoggerConfiguration =
      if (allowed) new StandardLoggerConfiguration(setup :+ (LogDebug -> ("Debug: " to outWriter)), errWriter, outWriter)
      else this

    def withTracesTo(writer: PrintWriter): StandardLoggerConfiguration =
      new StandardLoggerConfiguration(setup :+ (LogTrace -> ("[] " to writer)), errWriter, outWriter)

    def withTracesToStdout: StandardLoggerConfiguration =
      new StandardLoggerConfiguration(setup :+ (LogTrace -> ("[] " to outWriter)), errWriter, outWriter)

    def build: Logger = forConfiguration(setup)
  }

  object StandardLoggerConfiguration {
    lazy val errWriter = new PrintWriter(System.err)
    lazy val outWriter = new PrintWriter(System.out)

    def apply(): StandardLoggerConfiguration = {
      new StandardLoggerConfiguration(Seq(
        LogError -> ("Error: " to errWriter),
        LogWarning -> ("Warning: " to errWriter),
        LogInfo -> ("Info: " to outWriter)),
        errWriter, outWriter)
    }
  }

}

