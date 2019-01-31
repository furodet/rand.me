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
package me.rand.genconfig

import java.io.{IOException, InputStream}

import me.rand.commons.idioms.Status._

import scala.collection.mutable.ListBuffer
import scala.io.Source

class CreateConfig(options: GenConfigOptions) {
  private lazy val translationMap = Map[String, String](
    "@bytes@" -> options.bytes.toString,
    "@bits@" -> (options.bytes * 8).toString
  )

  def getText: List[String] OrElse Exception =
    try {
      val in = getClass.getResourceAsStream("template_config.yml")
      if (in == null)
        Err(new RuntimeException("BUG: could not open template configuration resource file"))
      else {
        val out = createConfigurationWithOptionValues(in)
        Ok(out)
      }
    } catch {
      case err: IOException =>
        Err(new RuntimeException("BUG: could not find template configuration resource file", err))
    }

  private def createConfigurationWithOptionValues(in: InputStream): List[String] = {
    val source = Source.fromInputStream(in)
    try {
      val out = ListBuffer.newBuilder[String]
      source.getLines().foreach {
        eachLine =>
          out += injectOptionValuesInto(eachLine)
      }
      out.result().toList
    } finally {
      source.close()
    }
  }

  private def injectOptionValuesInto(string: String): String =
    translationMap.foldLeft(string) {
      case (out, (key, value)) =>
        out.replaceAllLiterally(key, value)
    }
}

object CreateConfig {
  def usingOptions(options: GenConfigOptions) = new CreateConfig(options)
}
