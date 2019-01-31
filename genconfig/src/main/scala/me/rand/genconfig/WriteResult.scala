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

import java.io.{File, PrintWriter}

import me.rand.commons.idioms.Status._

class WriteResult(out: PrintWriter, outFileName: Option[String], closeWriterWhenDone: Boolean) {
  def <<(in: List[String]): String OrElse Exception =
    try {
      in.foreach(out.println)
      out.flush()
      if (closeWriterWhenDone) out.close()
      Ok(prettyPrintWriteComplete)
    } catch {
      case err: Exception =>
        Err(err)
    }

  private def prettyPrintWriteComplete: String =
    outFileName match {
      case None =>
        ""

      case Some(fileName) =>
        s"wrote $fileName"
    }
}

object WriteResult {
  def into(out: Option[File]): WriteResult =
    out match {
      case None =>
        new WriteResult(new PrintWriter(System.out), outFileName = None, closeWriterWhenDone = false)

      case Some(file) =>
        new WriteResult(new PrintWriter(file), outFileName = Some(file.getAbsolutePath), closeWriterWhenDone = true)
    }
}
