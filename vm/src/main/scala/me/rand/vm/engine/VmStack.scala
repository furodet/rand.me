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
import me.rand.vm.engine.VmStack.VmFrame
import me.rand.vm.main.VmError.VmContextError
import me.rand.vm.main.VmError.VmContextError.EmptyStackAccess

import scala.language.postfixOps

// Documentation: doc/vmarchitecture.md
class VmStack(val frames: List[VmFrame]) extends VarSet {
  override def name: String = "stack"

  private def push(newFrame: VmFrame): VmStack =
  // Note: scala List prepend is the most efficient.
    new VmStack(newFrame +: frames)

  private[engine] def createFrameOfSize(nrVariables: Int): VmStack =
    push(new VmFrame(VarSet.InArray.called(s"frame#${frames.length}").ofSize(nrVariables)))

  private[engine] def popFrame(): VmStack OrElse VmContextError =
    for {
      _ <- getTopFrameOrErrorForAction("pop")
      r = new VmStack(frames tail)
    } yield r

  override def getVariable(id: Int): Option[Variable] OrElse VmContextError =
    for {
      frame <- getTopFrameOrErrorForAction("read")
      r <- frame.vars.getVariable(id)
    } yield r

  override private[engine] def putVariable(id: Int, v: Variable): Unit OrElse VmContextError =
    for {
      frame <- getTopFrameOrErrorForAction("write")
      r <- frame.vars.putVariable(id, v)
    } yield r

  private def getTopFrameOrErrorForAction(action: String): VmFrame OrElse EmptyStackAccess =
    if (frames nonEmpty) Ok(frames head) else Err(EmptyStackAccess(action))

  override def iterator: Iterator[Option[Variable]] = frames match {
    case head :: _ =>
      head.vars.toIterator

    case _ =>
      List.empty.iterator
  }
}

object VmStack {
  def empty = new VmStack(List.empty)

  class VmFrame(val vars: VarSet)

}
