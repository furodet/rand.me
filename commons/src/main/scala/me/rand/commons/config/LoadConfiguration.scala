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

import java.io.IOException
import java.net.URI
import java.nio.file.{Files, Paths}

import me.rand.commons.idioms.Status._
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor

import scala.io.Source

class LoadConfiguration[T](source: Source, constructor: Constructor) {
  def get: T OrElse LoadConfigurationError = try {
    val yaml = new Yaml(constructor)
    Ok(yaml.load[T](source.getLines().mkString("\n")))
  } catch {
    case err: Exception =>
      Err(LoadConfigurationError.ConfigurationError(err))
  } finally {
    source.close()
  }
}

object LoadConfiguration {

  class LoadConfigurationFileMapper[T](targetClass: Class[T], constructor: Constructor) {
    def fromFile(fileName: String): LoadConfiguration[T] OrElse LoadConfigurationError =
      locateSourceCalled(fileName) && (source => new LoadConfiguration[T](source, constructor))

    private def locateSourceCalled(name: String): Source OrElse LoadConfigurationError =
      ensureThatFileExists(name) & createSource

    private def ensureThatFileExists(name: String): URI OrElse LoadConfigurationError = {
      val path = Paths.get(name)
      if (Files.exists(path) && Files.isRegularFile(path) && Files.isReadable(path)) {
        Ok(path.toUri)
      } else {
        Err(LoadConfigurationError.FileNotFound(name))
      }
    }

    private def createSource(uri: URI): Source OrElse LoadConfigurationError =
      try {
        Ok(Source.fromFile(uri))
      } catch {
        case err: IOException =>
          Err(LoadConfigurationError.CouldNotLoadFile(uri.getPath, err))
      }
  }

  def forClass[T](targetClass: Class[T]) = new LoadConfigurationFileMapper[T](targetClass, new Constructor(targetClass))
}
