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
package me.rand.asm.main

import java.io.{IOException, PrintWriter}

import me.rand.asm.builder.AsmProgramBuilder
import me.rand.asm.parser.{AsmParser, AsmToken}
import me.rand.commons.idioms.Logger
import me.rand.commons.idioms.Logger._
import me.rand.commons.idioms.Status._
import me.rand.vm.engine.VmContext

import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    (for {
      options <- AsmOptions.fromUserArgs(args)
      source <- getReaderForFile(options.in)
      vmContext <- main(options, source)
    } yield vmContext) match {
      case Ok(result) =>
        println(result)

      case Err(error) =>
        System.err.println(error)
        System.exit(1)
    }
  }

  def main(options: AsmOptions, source: Source): VmContext OrElse Unit = {
    implicit val logger: Logger = setupLogger(options.verbose)
    (for {
      parsed <- parse(source, options.prefix)
      vmContext <- buildProgram(parsed._1, parsed._2)
    } yield vmContext) match {
      case Err(error) =>
        logger !! error.toString
        Err(())

      case Ok(context) =>
        Ok(context)
    }
  }

  private def setupLogger(isVerbose: Boolean): Logger = {
    val errWriter = new PrintWriter(System.err)
    val outWriter = new PrintWriter(System.out)
    val defaultConfiguration = Seq(
      LogError -> ("Error: " to errWriter),
      LogWarning -> ("Warning: " to errWriter),
      LogInfo -> ("Info: " to outWriter)
    )
    Logger.forConfiguration(
      if (isVerbose) defaultConfiguration :+ (LogDebug -> ("debug " to outWriter))
      else defaultConfiguration
    )
  }

  private def getReaderForFile(file: java.io.File): Source OrElse AsmError =
    try {
      Ok(Source.fromFile(file))
    } catch {
      case err: IOException =>
        Err(AsmError.AsmArgumentError.CantOpenFile(file.getName, err))
    }

  private def parse(source: Source, prefix: Option[String])(implicit logger: Logger): (VmContext, List[AsmToken]) OrElse AsmError.AsmParserError = {
    val result = AsmParser.read(source.getLines()).withPrefix(prefix).execute
    source.close()
    result
  }

  private def buildProgram(initialContext: VmContext, tokens: List[AsmToken])(implicit logger: Logger): VmContext OrElse AsmError.AsmProgramBuilderError =
    AsmProgramBuilder.startingFromContext(initialContext).applyProgram(tokens)
}
