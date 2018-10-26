/*-
 * Copyright (c) 2018 rand.me project
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
package me.rand.vm.engine

import me.rand.commons.idioms.Status._
import me.rand.vm.main.VmError._

// Documentation: doc/vmarchitecture.md
case class VmContext(vmTypes: VmTypes,
                     heap: VarSet,
                     stack: VmStack,
                     program: VmProgram,
                     exitCode: Option[Int]) {
  def createFrameOfSize(nrVariables: Int): VmContext = copy(stack = stack.createFrameOfSize(nrVariables))

  def popFrame(): VmContext OrElse VmContextError =
    for {
      poppedStack <- stack.popFrame()
    } yield copy(stack = poppedStack)

  def incrementPc: VmContext =
    setProgram(program.incrementPc)

  def setPcToBlockCalled(blockName: String): VmContext OrElse VmContextError =
    program.setPcToBlockCalled(blockName) && setProgram

  def setProgram(newProgram: VmProgram): VmContext = copy(program = newProgram)

  def halt(exitCode: Int): VmContext = copy(exitCode = Some(exitCode))
}

object VmContext {
  // Maximums are totally arbitrary
  val maximumByteSizeAllowed: Int = 2048 / 8
  val maximumNumberOfVariablesInHeap: Int = 1 << 20

  class VmProfile(val machineWordByteLen: Int, val varSetSize: Int)

  object VmProfile {
    def fromString(profile: String): VmProfile OrElse VmProfileStringError = {
      val asStrings = profile.trim.toLowerCase.split(":").toList
      asStrings match {
        case "bl" :: bl :: "heap" :: hsz :: Nil =>
          for {
            byteLen <- readPositiveIntOrElse(profile, bl, maximumByteSizeAllowed, "bl")
            _ <- verifyThatNumberIsAPowerOfTwo(byteLen, "bl")
            heapSize <- readPositiveIntOrElse(profile, hsz, maximumNumberOfVariablesInHeap, "heap")
          } yield new VmProfile(byteLen, heapSize)

        case _ =>
          Err(VmProfileStringError.InvalidFormat(profile, "bl:...:heap:..."))
      }
    }

    private def readPositiveIntOrElse(profile: String, stringValue: String, maxValue: Int, fieldName: String): Int OrElse VmProfileStringError =
      try {
        val result = stringValue.toInt
        result match {
          case i if i > 0 && i <= maxValue =>
            Ok(i)

          case i if i <= 0 =>
            Err(VmProfileStringError.NotAPositiveNumber(profile, fieldName))

          case _ =>
            Err(VmProfileStringError.ValueExceedsMaximumAllowed(profile, fieldName, maxValue))
        }
      } catch {
        case _: IllegalArgumentException =>
          Err(VmProfileStringError.NotAPositiveNumber(profile, fieldName))
      }

    private def verifyThatNumberIsAPowerOfTwo(value: Int, fieldName: String): Unit OrElse VmProfileStringError =
      if (((value - 1) & value) == 0) Ok(()) else Err(VmProfileStringError.NotAPowerOfTwo(value, fieldName))
  }

  def usingProfile(vmProfile: VmProfile): VmContext = {
    val vmTypes = VmTypes.forMachineWordByteLength(vmProfile.machineWordByteLen)
    new VmContext(vmTypes, VarSet.ofSize(vmProfile.varSetSize), VmStack.empty, VmProgram.empty, None)
  }

  def usingProfileString(profileString: String): VmContext OrElse VmProfileStringError =
    VmProfile.fromString(profileString) && { profile => VmContext.usingProfile(profile) }
}
