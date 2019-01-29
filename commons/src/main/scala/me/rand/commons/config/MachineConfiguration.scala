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

import me.rand.commons.config.RandMeConfigurationError.InvalidValue
import me.rand.commons.idioms.Status.OrElse

class MachineConfiguration extends MustBeValidConfiguration {
  @BeanProperty var version: String = ""
  @BeanProperty var bytes: Int = 8
  @BeanProperty var hpBytes: Int = undefinedIntByDefault
  @BeanProperty var spBytes: Int = undefinedIntByDefault
  @BeanProperty var ipBytes: Int = undefinedIntByDefault
  @BeanProperty var heapSize: Int = 256

  override def assertIsOk: Unit OrElse RandMeConfigurationError =
    verify(
      ifNot(version.nonEmpty).thenReturn(RandMeConfigurationError.UndefinedMandatoryField("version")),
      ifNot(isAValidWordLength(bytes, 1)).thenReturn(InvalidValue("bytes", bytes, "must be a valid byte length")),
      ifNot(isAValidWordLength(hpBytes)).thenReturn(InvalidValue("hpBytes", hpBytes, "must be a valid byte length or 0")),
      ifNot(isAValidWordLength(spBytes)).thenReturn(InvalidValue("spBytes", spBytes, "must be a valid byte length or 0")),
      ifNot(isAValidWordLength(ipBytes)).thenReturn(InvalidValue("ipBytes", ipBytes, "must be a valid byte length or 0")),
      ifNot(heapSize >= 0).thenReturn(InvalidValue("heapSize", heapSize, "must be greater than 0"))
    )

  private def isAValidWordLength(value: Int, minAllowedValue: Int = 0): Boolean =
  // Verify that we have a power of two
    (value >= minAllowedValue) && ((value == 0) || BigInt.int2bigInt(value).bitCount == 1)
}
