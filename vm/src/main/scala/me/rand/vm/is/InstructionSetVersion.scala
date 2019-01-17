/*-
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
package me.rand.vm.is

import me.rand.commons.idioms.Status._
import me.rand.vm.main.VmError.IncompatibleInstructionSetVersion

class InstructionSetVersion(val major: Int, val minor: Int) {
  def isOlderThan(other: InstructionSetVersion): Boolean =
    (major < other.major) || (minor < other.minor)

  override def toString: String = s"$major.$minor"
}

object InstructionSetVersion {
  val current = new InstructionSetVersion(0, 1)

  def fromString(string: String): Option[InstructionSetVersion] =
    string.split("\\.").toList match {
      case major :: minor :: _ if major.matches("[0-9]+") && minor.matches("[0-9]+") =>
        Some(new InstructionSetVersion(major.toInt, minor.toInt))

      case _ =>
        None
    }

  def assertThatVirtualMachineVersionIsCompatibleWith(version: InstructionSetVersion): Unit OrElse IncompatibleInstructionSetVersion =
    if (current.isOlderThan(version) || (current.major != version.major)) Err(IncompatibleInstructionSetVersion(version))
    else Ok(())
}
