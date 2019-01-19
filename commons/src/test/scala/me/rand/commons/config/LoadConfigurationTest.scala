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
package me.rand.commons.config

import java.io.{File, PrintWriter}

import me.rand.commons.idioms.Status._
import org.scalatest.FlatSpec

import scala.collection.JavaConverters._

class LoadConfigurationTest extends FlatSpec {

  "a file that does not exist" should "not be loaded" in {
    val thisFileDoesNotExist = List("this", "is", "not", "a", "valid", "file", "path").mkString(File.separator)
    LoadConfiguration.forClass(classOf[TestClass]).fromFile(thisFileDoesNotExist) match {
      case Err(LoadConfigurationError.FileNotFound(name)) =>
        assert(name == thisFileDoesNotExist)

      case whatever =>
        fail(s"unexpected result: $whatever")
    }
  }

  "a valid file" should "be successfully loaded" in {
    val file = writeToTestFile(
      """
        |# A test
        |  name : John Doe
        |  id : 6767
        |  info :
        |    text : this is info
        |  properties:
        |    - name : boring
        |      description : waiting for time passing by
        |    - name : swimming
        |      description : having fun in the water
      """.stripMargin
    )
    LoadConfiguration.forClass(classOf[TestClass]).fromFile(file) & (_.get) match {
      case Ok(test) =>
        assert(test.name == "John Doe")
        assert(test.id == 6767)
        assert(test.info.text == "this is info")
        val properties = test.properties.asScala
        assert(properties.size == 2)
        assert(properties(0).name == "boring")
        assert(properties(0).description == "waiting for time passing by")
        assert(properties(1).name == "swimming")
        assert(properties(1).description == "having fun in the water")

      case Err(err) =>
        fail(s"unexpected failure: $err")
    }
  }

  "a file with invalid configuration" should "not be loaded" in {
    val file = writeToTestFile(
      """
        |# Next is non sense
        |  name : John Doe
        |  what : undefined
        |  id   : 777
      """.stripMargin
    )
    LoadConfiguration.forClass(classOf[TestClass]).fromFile(file) & (_.get) match {
      case Err(LoadConfigurationError.ConfigurationError(err)) =>
        println(err)

      case whatever =>
        fail(s"unexpected result: $whatever")
    }
  }

  private def writeToTestFile(text: String): String = {
    val output = new java.io.File("test.yaml")
    val writer = new PrintWriter(output)
    try {
      writer.println(text)
    } catch {
      case ioe: java.io.IOException =>
        throw new RuntimeException(s"BUG: $ioe")
    } finally {
      writer.close()
    }
    output.getAbsoluteFile.toString
  }
}
