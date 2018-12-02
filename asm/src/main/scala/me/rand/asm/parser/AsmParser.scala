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
package me.rand.asm.parser

import me.rand.asm.main.AsmError.AsmParserError
import me.rand.commons.idioms.Logger
import me.rand.commons.idioms.Status._

class AsmParser(input: Iterable[String]) {
  def execute(implicit logger: Logger) =
    input.tryFoldLeft(0) {
      case (lineNumber, eachLine) =>
        logger >> s"$lineNumber: $eachLine"
        translateLine(lineNumber, eachLine) && (_ => lineNumber + 1)
    }

  private def translateLine(lineNumber: Int, text: String): Option[String] OrElse AsmParserError =
    text.trim.split("\\s+").toList match {
      case Nil | "" :: _ =>
        // Empty line
        Ok(None)

      case k :: _ if k.startsWith("//") =>
        Ok(None)

      case key :: args =>
        translateCode(lineNumber, key, args) && (Some(_))
    }

  private def translateCode(lineNumber: Int, keyword: String, args: List[String]): String OrElse AsmParserError =
    Err(AsmParserError.UnknownInstruction(keyword, lineNumber))

}

object AsmParser {
  def read(input: Iterator[String]) = new AsmParser(input.toIterable)
}
