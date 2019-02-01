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
package me.rand.vm.engine

import me.rand.commons.idioms.Status._
import me.rand.vm.engine
import me.rand.vm.engine.VmTypes.VmType
import me.rand.vm.main.VmError._

// Documentation: doc/vmarchitecture.md
case class VmContext(heap: VarSet,
                     stack: VmStack,
                     program: VmProgram,
                     state: VmRunState,
                     profile: VmContext.VmContextProfile) {
  def createFrameOfSize(nrVariables: Int): VmContext = copy(stack = stack.createFrameOfSize(nrVariables))

  def popFrame(): VmContext OrElse VmContextError =
    for {
      poppedStack <- stack.popFrame()
    } yield copy(stack = poppedStack)

  def incrementPc: VmContext =
    setProgram(program.incrementPc)

  def resetPcToBlockCalled(blockName: String): VmContext OrElse VmContextError =
    program.setPcToBlockCalled(blockName) && setProgram

  def setProgram(newProgram: VmProgram): VmContext = copy(program = newProgram)

  def movePc(programCounter: VmProgram.Counter): VmContext = {
    val newProgram = program.setPc(programCounter)
    setProgram(newProgram)
  }

  def halt(exitCode: Int): VmContext = copy(state = VmRunState.Stopped(exitCode))

  def pause(): VmContext = copy(state = VmRunState.Paused)

  def putHeapVariable(id: Int, v: Variable): VmContext OrElse VmContextError =
    heap.putVariable(id, v) && (_ => this)

  def putStackVariable(id: Int, v: Variable): VmContext OrElse VmContextError =
    stack.putVariable(id, v) && (_ => this)

  def withProfile(newProfile: VmContext.VmContextProfile): VmContext = copy(profile = newProfile)

  def withPointerTypes(newTypes: VmContext.VmContextProfile.PointerTypes): VmContext =
    withProfile(profile.withPointerTypes(newTypes))
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
        case "bytes" :: bytes :: "heap" :: hsz :: Nil =>
          for {
            byteLen <- readPositiveIntOrElse(profile, bytes, maximumByteSizeAllowed, "bytes")
            heapSize <- readPositiveIntOrElse(profile, hsz, maximumNumberOfVariablesInHeap, "heap")
          } yield new VmProfile(byteLen, heapSize)

        case _ =>
          Err(VmProfileStringError.InvalidFormat(profile, "bytes:...:heap:..."))
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
  }

  def usingProfile(vmProfile: VmProfile): VmContext = {
    val vmTypes = VmTypes.forMachineWordByteLength(vmProfile.machineWordByteLen)
    new VmContext(VarSet.InArray.called("heap").ofSize(vmProfile.varSetSize),
      VmStack.empty,
      VmProgram.empty,
      VmRunState.Running,
      VmContextProfile.default(vmTypes)
    )
  }

  def usingProfileString(profileString: String): VmContext OrElse VmProfileStringError =
    VmProfile.fromString(profileString) && { profile => VmContext.usingProfile(profile) }

  class VmContextProfile(val vmTypes: VmTypes, val pointerTypes: VmContextProfile.PointerTypes) {
    def withPointerTypes(sizes: VmContextProfile.PointerTypes): VmContextProfile =
      new VmContextProfile(vmTypes, sizes)

    override def toString: String = s"${vmTypes.toString}:${pointerTypes.toString}"
  }

  object VmContextProfile {
    def default(vmTypes: VmTypes) = new VmContextProfile(
      vmTypes,
      new engine.VmContext.VmContextProfile.PointerTypes(vmTypes.maxUnsignedType, vmTypes.maxUnsignedType, vmTypes.maxUnsignedType)
    )

    class PointerTypes(val toInstruction: VmType, val toHeap: VmType, val toStack: VmType) {
      def withInstructionPointerType(newSize: VmType): PointerTypes = new PointerTypes(newSize, toHeap, toStack)

      def withHeapPointerType(newSize: VmType): PointerTypes = new PointerTypes(toInstruction, newSize, toStack)

      def withStackPointerType(newSize: VmType): PointerTypes = new PointerTypes(toInstruction, toHeap, newSize)

      override def toString: String = s"iptr=$toInstruction:hptr=$toHeap:sptr=$toStack)"
    }

  }

}
