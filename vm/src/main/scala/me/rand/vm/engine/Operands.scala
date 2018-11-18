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

class Operands(val destination: Operand.Destination, val sources: List[Operand.Source]) {
  def setDestination(destinationOperand: Operand.Destination): Operands =
    new Operands(destination = destinationOperand, sources)

  def addSource(operand: Operand.Source): Operands =
    new Operands(destination, sources = sources :+ operand)
}

object Operands {
  def none = new Operands(Operand.Destination.NoDestination, List.empty)

  class OperandsBuilder(operands: Operands) {
    def +(destination: Operand.Destination): OperandsBuilder =
      new OperandsBuilder(new Operands(destination, operands.sources))

    def +(source: Operand.Source): OperandsBuilder =
      new OperandsBuilder(new Operands(operands.destination, operands.sources :+ source))

    def build: Operands = operands
  }

  object OperandsBuilder {
    def none = new OperandsBuilder(Operands.none)
  }

}
