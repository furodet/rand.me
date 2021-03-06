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
import me.rand.vm.engine.VmTypes.VmType
import me.rand.vm.main.VmError.VmContextError.InvalidVmTypeString

import scala.annotation.tailrec

// Documentation: doc/vmarchitecture.md
class VmTypes(val typeMap: Map[String, VmType]) {
  // Whatever the virtual machine is, there are always "minimal types" representing
  // one byte signed and unsigned.
  lazy val minUnsignedType: VmType = select(1, isSigned = false).get
  // And there is a type with the maximum length... Easy to lazily guess.
  lazy val maxUnsignedType: VmType = {
    val maxLen = typeMap.values.map(vmType => vmType.byteLen).max
    select(maxLen, isSigned = false).get
  }

  def valueOf(s: String): VmType OrElse InvalidVmTypeString =
    typeMap.get(s) match {
      case None =>
        Err(InvalidVmTypeString(s))

      case Some(vmType) =>
        Ok(vmType)
    }

  def select(byteLen: Int): Seq[VmType] =
    typeMap.values.collect {
      case t if t.byteLen == byteLen => t
    }.toSeq

  def select(byteLen: Int, isSigned: Boolean): Option[VmType] =
    typeMap.values.collect {
      case t if t.byteLen == byteLen && t.isSigned == isSigned => t
    } match {
      case firstMatch :: _ =>
        Some(firstMatch)

      case _ =>
        None
    }

  private lazy val textual = typeMap.values.map(_.name).toList.sorted.mkString("[", ",", "]")

  override def toString: String = textual
}

object VmTypes {
  def forMachineWordByteLength(byteLen: Int): VmTypes =
    new VmTypes(new VmTypeFactory(byteLen).build)

  class VmType(val byteLen: Int, val isSigned: Boolean, unsignedCounterpart: Option[VmType]) {
    def bitLen: Int = byteLen * 8

    lazy val isUnsigned: Boolean = !isSigned

    val name: String = if (isSigned) s"s$bitLen" else s"u$bitLen"

    def hasSameByteLenAs(other: VmType): Boolean = byteLen ==
      other.byteLen

    def hasSameSignAs(other: VmType): Boolean =
      isSigned == other.isSigned

    def <=>(other: VmType): Boolean = hasSameByteLenAs(other) && hasSameSignAs(other)

    def asUnsigned: VmType = unsignedCounterpart.getOrElse(this)

    override def toString: String = name
  }

  private class VmTypeFactory(machineWordByteLen: Int) {
    private val _unsigned = false
    private val _signed = true

    def build: Map[String, VmType] =
      buildAndRegisterSignedAndUnsignedVmType(1, Map.empty[String, VmType])

    @tailrec
    private def buildAndRegisterSignedAndUnsignedVmType(byteLen: Int, vmTypes: Map[String, VmType]): Map[String, VmType] =
      if (byteLen > machineWordByteLen) vmTypes
      else {
        val unsignedType = new VmType(byteLen, _unsigned, None)
        val signedType = new VmType(byteLen, _signed, Some(unsignedType))
        val newVmTypes = (vmTypes + (unsignedType.name -> unsignedType)) + (signedType.name -> signedType)
        buildAndRegisterSignedAndUnsignedVmType(byteLen * 2, newVmTypes)
      }
  }

}
