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
package me.rand.vm.alu

import me.rand.commons.idioms.Status._
import me.rand.vm.engine.Variable
import me.rand.vm.main.VmError.VmExecutionError.AluError

sealed trait Comparator {
  def execute(op1: Variable, op2: Variable): Boolean OrElse AluError

  protected def standardComparator(op1: Variable, op2: Variable,
                                   comparisonName: String,
                                   aluCompareOperation: (VmRegister, VmRegister) => Boolean,
                                   intCompareOperation: (Int, Int) => Boolean): Boolean OrElse AluError =
    (op1, op2) match {
      case (Variable.Scalar(_, x), Variable.Scalar(_, y)) =>
        Ok(aluCompareOperation(x.data, y.data))

      case (Variable.Pointer.ToVariable.InTheHeap(_, x), Variable.Pointer.ToVariable.InTheHeap(_, y)) =>
        Ok(intCompareOperation(x, y))

      case (Variable.Pointer.ToVariable.InTheStack(_, x), Variable.Pointer.ToVariable.InTheStack(_, y)) =>
        Ok(intCompareOperation(x, y))

      case (Variable.Pointer.ToInstruction(_, x), Variable.Pointer.ToInstruction(_, y)) =>
        Ok((x.basicBlock == y.basicBlock) && intCompareOperation(x.index, y.index))

      case (x: Variable, y: Variable) =>
        Err(AluError.InconsistentTypesForComparison(comparisonName, x, y))
    }
}

object Comparator {

  object ?= extends Comparator {
    override def execute(op1: Variable, op2: Variable): Boolean OrElse AluError =
      standardComparator(op1, op2, toString, Alu.isEqual, _ == _)

    override def toString: String = "?="
  }

  object ?≠ extends Comparator {
    override def execute(op1: Variable, op2: Variable): Boolean OrElse AluError =
      standardComparator(op1, op2, toString, Alu.isNotEqual, _ != _)

    override def toString: String = "?≠"
  }

  object ?< extends Comparator {
    override def execute(op1: Variable, op2: Variable): Boolean OrElse AluError =
      standardComparator(op1, op2, toString, Alu.isLower, _ < _)

    override def toString: String = "?<"
  }

  object ?> extends Comparator {
    override def execute(op1: Variable, op2: Variable): Boolean OrElse AluError =
      standardComparator(op1, op2, toString, Alu.isGreater, _ > _)

    override def toString: String = "?>"
  }

  object ?≤ extends Comparator {
    override def execute(op1: Variable, op2: Variable): Boolean OrElse AluError =
      standardComparator(op1, op2, toString, Alu.isLowerOrEqual, _ < _)

    override def toString: String = "?≤"
  }

  object ?≥ extends Comparator {
    override def execute(op1: Variable, op2: Variable): Boolean OrElse AluError =
      standardComparator(op1, op2, toString, Alu.isGreaterOrEqual, _ >= _)

    override def toString: String = "?≥"
  }

}
