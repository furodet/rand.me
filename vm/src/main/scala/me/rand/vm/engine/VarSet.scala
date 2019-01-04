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
import me.rand.vm.engine
import me.rand.vm.main.VmError.VmContextError
import me.rand.vm.main.VmError.VmContextError.VariableIndexOutOfBounds

// Documentation: doc/vmarchitecture.md
trait VarSet extends Iterable[Option[Variable]] {
  def name: String

  private[engine] def putVariable(id: Int, v: Variable): Unit OrElse VmContextError

  def getVariable(id: Int): Option[Variable] OrElse VmContextError
}

object VarSet {

  class InArray(val data: Array[Option[Variable]], val name: String) extends VarSet {
    override private[engine] def putVariable(id: Int, v: Variable): Unit OrElse VariableIndexOutOfBounds =
      try {
        data(id) = Some(v)
        Ok(())
      } catch {
        case _: ArrayIndexOutOfBoundsException =>
          Err(VariableIndexOutOfBounds(id))
      }

    override def getVariable(id: Int): Option[Variable] OrElse VariableIndexOutOfBounds =
      try {
        Ok(data(id))
      } catch {
        case _: ArrayIndexOutOfBoundsException =>
          Err(VariableIndexOutOfBounds(id))
      }

    override def iterator: Iterator[Option[Variable]] = data.toIterator
  }

  object InArray {

    class InArrayBuilder(name: String) {
      // Note: we assume that nrVariables will never be negative or null.
      def ofSize(nrVariables: Int) = new engine.VarSet.InArray(Array.fill(nrVariables)(None), name)
    }

    def called(name: String) = new InArrayBuilder(name)
  }

}
