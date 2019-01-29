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
package me.rand.randasm.main

import me.rand.asm.dasm.VmContextSnapshot
import me.rand.commons.config.RandMeConfiguration
import me.rand.commons.idioms.Status._
import me.rand.randasm.main.process.CreateEmptyVmContext
import me.rand.vm.engine.VmContext

object Main {
  def main(args: Array[String]): Unit = {
    (for {
      options <- RandAsmOptions.fromUserArgs(args)
      result <- main(options)
    } yield result) match {
      case Ok(vmContext) =>
        println(VmContextSnapshot.of(vmContext).all.mkString("\n"))

      case Err(error) =>
        System.err.println(error)
        System.exit(1)
    }
  }

  def main(options: RandAsmOptions): VmContext OrElse RandAsmError = {
    for {
      configuration <- tryLoadConfiguration(options.configurationFile.getAbsolutePath)
      initialVmContext <- tryInitializeVmContext(configuration.machine)
    } yield initialVmContext
  }

  private def tryLoadConfiguration(path: String): RandMeConfiguration OrElse RandAsmError = {
    RandMeConfiguration.loadFromFileAndValidate(path) || (error => RandAsmError.FromConfigurationError(error))
  }

  private def tryInitializeVmContext(machine: RandMeConfiguration.Machine): VmContext OrElse RandAsmError =
    CreateEmptyVmContext.fromConfiguration(machine) || (error => RandAsmError.FromVmError(error))
}
