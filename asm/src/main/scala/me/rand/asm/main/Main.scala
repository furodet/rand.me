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

import java.io.{FileReader, IOException}

import me.rand.asm.parser.AsmParser
import me.rand.commons.idioms.Status._

object Main {
  def main(args: Array[String]): Unit = {
    optionParser.parse(args, AsmOptions()) match {
      case None =>
        // Too bad
        optionParser.showUsageAsError()

      case Some(options) =>
        (for {
          reader <- getReaderForFile(options.in)
          parsed = parse(reader)
        } yield parsed) match {
          case Ok(result) =>
            println(result)

          case Err(error) =>
            System.err.println(error)
            System.exit(1)
        }
    }
  }

  private lazy val optionParser = new scopt.OptionParser[AsmOptions]("asm") {
    opt[java.io.File]('s', "assemble").action {
      (file, options) =>
        options.copy(in = file)
    }.valueName("file").required().maxOccurs(1).text("assemble source file")

    help("help").text("prints this help")
  }

  private def getReaderForFile(file: java.io.File): java.io.Reader OrElse AsmError =
    try {
      Ok(new FileReader(file))
    } catch {
      case err: IOException =>
        Err(AsmError.AsmArgumentError.CantOpenFile(file.getName, err))
    }

  def parse(reader: java.io.Reader) = {
    val result = AsmParser.read(reader).execute
    reader.close()
    result
  }
}
