/*
 * Copyright (c) 2018-2019 rand.me project
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
package me.rand.randasm.main

import me.rand.commons.idioms.Status._

case class RandAsmOptions(configurationFile: java.io.File = null, outputFile: Option[java.io.File] = None,
                          verboseGeneration: Boolean = false, verboseRun: Boolean = false)

object RandAsmOptions {
  def fromUserArgs(args: Array[String]): RandAsmOptions OrElse String =
    optionParser.parse(args, RandAsmOptions()) match {
      case None =>
        optionParser.showUsageAsError()
        Err("")

      case Some(options) =>
        Ok(options)
    }

  private lazy val optionParser = new scopt.OptionParser[RandAsmOptions]("randasm") {
    opt[java.io.File]('c', "config").action {
      (file, options) =>
        options.copy(configurationFile = file)
    }.valueName("file").required().maxOccurs(1).text("generator configuration")

    opt[java.io.File]('o', "out").action {
      (file, options) =>
        options.copy(outputFile = Some(file))
    }.valueName("file").optional().maxOccurs(1).text("write generated code into specified file")

    opt[Unit]("vv:gen").action {
      (_, options) =>
        options.copy(verboseGeneration = true)
    }.optional().text("generate verbosely")

    opt[Unit]("vv:run").action {
      (_, options) =>
        options.copy(verboseRun = true)
    }.optional().text("simulate verbosely")


    help("help").text("prints this help")
  }
}