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
package me.rand.commons.idioms

import java.io.{PrintWriter, StringWriter}

import me.rand.commons.idioms.Logger._
import org.scalatest.FlatSpec

class LoggerTest extends FlatSpec {

  class TestWriter {
    private def newOut = {
      val writer = new StringWriter()
      (new PrintWriter(writer), writer)
    }

    val (errors, errors_) = newOut
    val (warnings, warnings_) = newOut
    val (infos, infos_) = newOut
    val (debugs, debugs_) = newOut
    val (traces, traces_) = newOut

    def close(): Unit = {
      errors.close()
      warnings.close()
      infos.close()
      debugs.close()
      traces.close()
    }

    def flush(): Unit = {
      errors.flush()
      warnings.flush()
      infos.flush()
      debugs.flush()
      traces.flush()
    }
  }

  "a logger " should "print errors when asked to" in {
    val writer = new TestWriter
    val logger = Logger.forConfiguration(Seq(LogError -> ("E" to writer.errors)))
    writeMessagesAndAssertThatBufferIsOk("E", writer, logger.!!, writer.errors_)
  }

  "a logger" should "not print errors when not asked to" in {
    val writer = new TestWriter
    val logger = Logger.forConfiguration(Seq())
    writeMessagesAndAssertThatBufferIsEmpty(writer, logger.!!, writer.errors_)
  }

  "a logger " should "print warnings when asked to" in {
    val writer = new TestWriter
    val logger = Logger.forConfiguration(Seq(LogWarning -> ("W" to writer.warnings)))
    writeMessagesAndAssertThatBufferIsOk("W", writer, logger.!, writer.warnings_)
  }

  "a logger" should "not print warnings when not asked to" in {
    val writer = new TestWriter
    val logger = Logger.forConfiguration(Seq())
    writeMessagesAndAssertThatBufferIsEmpty(writer, logger.!!, writer.warnings_)
  }

  "a logger " should "print infos when asked to" in {
    val writer = new TestWriter
    val logger = Logger.forConfiguration(Seq(LogInfo -> ("I" to writer.infos)))
    writeMessagesAndAssertThatBufferIsOk("I", writer, logger.>, writer.infos_)
  }

  "a logger" should "not print infos when not asked to" in {
    val writer = new TestWriter
    val logger = Logger.forConfiguration(Seq())
    writeMessagesAndAssertThatBufferIsEmpty(writer, logger.>, writer.infos_)
  }

  "a logger " should "print debugs when asked to" in {
    val writer = new TestWriter
    val logger = Logger.forConfiguration(Seq(LogDebug -> ("D" to writer.debugs)))
    writeMessagesAndAssertThatBufferIsOk("D", writer, logger.>>, writer.debugs_)
  }

  "a logger" should "not print debugs when not asked to" in {
    val writer = new TestWriter
    val logger = Logger.forConfiguration(Seq())
    writeMessagesAndAssertThatBufferIsEmpty(writer, logger.>>, writer.debugs_)
  }

  "a logger " should "print traces when asked to" in {
    val writer = new TestWriter
    val logger = Logger.forConfiguration(Seq(LogTrace -> ("//" to writer.traces)))
    writeMessagesAndAssertThatBufferIsOk("//", writer, logger.~>, writer.traces_)
  }

  "a logger" should "not print traces when not asked to" in {
    val writer = new TestWriter
    val logger = Logger.forConfiguration(Seq())
    writeMessagesAndAssertThatBufferIsEmpty(writer, logger.~>, writer.traces_)
  }

  private def writeMessagesAndAssertThatBufferIsOk(expectedPrefix: String,
                                                   writer: TestWriter,
                                                   logFunction: String => Unit,
                                                   getter: => StringWriter): Unit = {
    logFunction("message 1")
    logFunction("message 2")
    writer.flush()
    assert(getter.toString.contains(expectedPrefix + "message 1"))
    assert(getter.toString.contains(expectedPrefix + "message 2"))
    writer.close()
  }

  private def writeMessagesAndAssertThatBufferIsEmpty(writer: TestWriter,
                                                      logFunction: String => Unit,
                                                      getter: => StringWriter): Unit = {
    logFunction("message 1")
    logFunction("message 2")
    writer.flush()
    assert(getter.toString.isEmpty)
    writer.close()
  }
}
