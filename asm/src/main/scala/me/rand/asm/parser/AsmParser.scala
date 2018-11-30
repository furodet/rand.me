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

import scala.util.parsing.combinator.RegexParsers

class AsmParser(input: java.io.Reader) extends RegexParsers {
  def execute = parseAll(main, input)

  def main: Parser[List[String]] = skip ~> rep(token <~ skip) <~ eof

  def skip: Parser[Unit] = rep(whiteSpace | comment) ^^^ Unit

  def comment: Parser[Unit] = "//" ~ rep(not("\n") ~ ".".r) ^^^ Unit

  def eof: Parser[String] = "\\z".r | failure("unexpected character")

  def token: Parser[String] = instruction

  def instruction: Parser[String] =
    instructionName ^^ (">" + _)

  def instructionName: Parser[String] = "hello"

  //val number: Regex = "[1-9][0-9]+".r
}

object AsmParser {
  def read(input: java.io.Reader) = new AsmParser(input)
}
