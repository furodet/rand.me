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

import java.beans.BeanProperty

import me.rand.commons.config.RandMeConfiguration.Machine
import me.rand.commons.idioms.Status._

class RandMeConfiguration extends MustBeValidConfiguration {
  @BeanProperty var machine: Machine = new Machine

  override def assertIsOk: Unit OrElse RandMeConfigurationError = machine.assertIsOk

  private[config] def validate: RandMeConfiguration OrElse RandMeConfigurationError =
    assertIsOk && (_ => this)
}

object RandMeConfiguration {

  class Machine extends MustBeValidConfiguration {
    @BeanProperty var version: String = ""
    @BeanProperty var bytes: Int = 8
    @BeanProperty var hpBytes: Int = undefinedIntByDefault
    @BeanProperty var spBytes: Int = undefinedIntByDefault
    @BeanProperty var ipBytes: Int = undefinedIntByDefault
    @BeanProperty var heapSize: Int = 256

    override def assertIsOk: Unit OrElse RandMeConfigurationError =
      verify(
        ifNot(version.nonEmpty).thenReturn(RandMeConfigurationError.UndefinedMandatoryField("version")),
        ifNot(hpBytes >= 0).thenReturn(RandMeConfigurationError.InvalidValue("hpBytes", hpBytes, "must be greater than 0")),
        ifNot(spBytes >= 0).thenReturn(RandMeConfigurationError.InvalidValue("spBytes", spBytes, "must be greater than 0")),
        ifNot(ipBytes >= 0).thenReturn(RandMeConfigurationError.InvalidValue("ipBytes", ipBytes, "must be greater than 0")),
        ifNot(heapSize >= 0).thenReturn(RandMeConfigurationError.InvalidValue("heapSize", heapSize, "must be greater than 0"))
      )
  }

  def loadFromFileAndValidate(file: String): RandMeConfiguration OrElse RandMeConfigurationError =
    LoadConfiguration.forClass(classOf[RandMeConfiguration]).fromFile(file) & (_.get) match {
      case Ok(configuration) =>
        configuration.validate

      case Err(error) =>
        Err(RandMeConfigurationError.FromLoaderError(error))
    }
}
