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
package me.rand.randasm.main.process

import me.rand.commons.config.{MachineConfiguration, RandMeConfigurationError}
import me.rand.commons.idioms.Status._
import me.rand.randasm.main.RandAsmError
import me.rand.vm.engine.VmContext
import me.rand.vm.engine.VmContext.VmContextProfile.PointerTypes
import me.rand.vm.engine.VmTypes.VmType

object CreateEmptyVmContext {
  def fromConfiguration(configuration: MachineConfiguration): VmContext OrElse RandAsmError = for {
    init <- createInitialVmContext(configuration)
    vmContext <- setupPointerTypes(init, configuration)
  } yield vmContext

  private def createInitialVmContext(configuration: MachineConfiguration): VmContext OrElse RandAsmError =
    VmContext.usingProfileString(s"bytes:${configuration.bytes}:heap:${configuration.heapSize}") ||
      (error => RandAsmError.FromVmConfigurationError(error))

  private def setupPointerTypes(vmContext: VmContext, configuration: MachineConfiguration): VmContext OrElse RandAsmError =
    for {
      v0 <- fetchAndSetPointerType(vmContext, configuration.ipBytes, "ipBytes", (pt, t) => pt.withInstructionPointerType(t))
      v1 <- fetchAndSetPointerType(vmContext, configuration.hpBytes, "hpBytes", (pt, t) => pt.withHeapPointerType(t))
      v2 <- fetchAndSetPointerType(vmContext, configuration.spBytes, "spBytes", (pt, t) => pt.withStackPointerType(t))
    } yield v2

  private def fetchAndSetPointerType(vmContext: VmContext, pointerBytes: Int, fieldName: String,
                                     update: (PointerTypes, VmType) => PointerTypes): VmContext OrElse RandAsmError =
    if (pointerBytes == 0) Ok(vmContext)
    else {
      vmContext.profile.vmTypes.select(pointerBytes, isSigned = false) match {
        case None =>
          Err(RandAsmError.FromConfigurationError(RandMeConfigurationError.InvalidValue(fieldName, pointerBytes, "not defining a known machine type")))

        case Some(vmType) =>
          val newPointerTypes = update(vmContext.profile.pointerTypes, vmType)
          Ok(vmContext.withPointerTypes(newPointerTypes))
      }
    }
}
