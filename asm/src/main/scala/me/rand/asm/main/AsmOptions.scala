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

import me.rand.commons.idioms.Status._

case class AsmOptions(in: java.io.File = null, out: Option[java.io.File] = None, verbose: Boolean = false, prefix: Option[String] = None)

object AsmOptions {
  def fromUserArgs(args: Array[String]): AsmOptions OrElse String =
    optionParser.parse(args, AsmOptions()) match {
      case None =>
        optionParser.showUsageAsError()
        Err("")

      case Some(options) =>
        Ok(options)
    }

  private lazy val optionParser = new scopt.OptionParser[AsmOptions]("asm") {
    opt[java.io.File]('s', "assemble").action {
      (file, options) =>
        options.copy(in = file)
    }.valueName("file").required().maxOccurs(1).text("assemble source file")

    opt[java.io.File]('o', "output").action {
      (file, options) =>
        options.copy(out = Some(file))
    }.valueName("file").optional().maxOccurs(1).text("write into output file")

    opt[Unit]("vv").action {
      (_, options) =>
        options.copy(verbose = true)
    }.optional().text("assemble verbosely")

    opt[String](name = "prefix").action {
      (prefix, options) =>
        options.copy(prefix = Some(prefix))
    }.optional().maxOccurs(1).valueName("text").text("text prefixing assembly directives (none by default)")

    help("help").text("prints this help")
  }
}