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
package me.rand.simulator.test.instructions

import me.rand.commons.idioms.Status._
import me.rand.simulator.test.BaseSpec
import me.rand.vm.engine.Variable.Scalar
import me.rand.vm.engine.VmContext

class SizeofSpec extends BaseSpec {
  // TODO: for types u/s24,40,48;56 I'm quite sure that sizeof does not return the number of bytes! To be tested
  "sizeof" should "pass %x > %y (scalar types)" in {
    successfullyAssembleAndExecute(
      s"""
         | $aStandardMachineConfiguration
         | .bb main
         |   .var xu8  %0 u8
         |   .var xs8  %1 s8
         |   .var xu16 %2 u16
         |   .var xs16 %3 s16
         |   .var xu24 %4 u24
         |   .var xs24 %5 s24
         |   .var xu32 %6 u32
         |   .var xs32 %7 s32
         |   .var xu40 %8 u40
         |   .var xs40 %9 s40
         |   .var xu48 %10 u48
         |   .var xs48 %11 s48
         |   .var xu56 %12 u56
         |   .var xs56 %13 s56
         |   .var xu64 %14 u64
         |   .var xs64 %15 s64
         |   sizeof %0 > %0
         |   sizeof %1 > %1
         |   sizeof %2 > %2
         |   sizeof %3 > %3
         |   sizeof %4 > %4
         |   sizeof %5 > %5
         |   sizeof %6 > %6
         |   sizeof %7 > %7
         |   sizeof %8 > %8
         |   sizeof %9 > %9
         |   sizeof %10 > %10
         |   sizeof %11 > %11
         |   sizeof %12 > %12
         |   sizeof %13 > %13
         |   sizeof %14 > %14
         |   sizeof %15 > %15
         |   exit (00:u8)
         | .boot main
      """.stripMargin
    ).thenVerify {
      case vmContext =>
        hasHeapVariable(0, 1, vmContext) &&
          hasHeapVariable(1, 1, vmContext) &&
          hasHeapVariable(2, 2, vmContext) &&
          hasHeapVariable(3, 2, vmContext) &&
          hasHeapVariable(4, 3, vmContext) &&
          hasHeapVariable(5, 3, vmContext) &&
          hasHeapVariable(6, 4, vmContext) &&
          hasHeapVariable(7, 4, vmContext) &&
          hasHeapVariable(8, 5, vmContext) &&
          hasHeapVariable(9, 5, vmContext) &&
          hasHeapVariable(10, 6, vmContext) &&
          hasHeapVariable(11, 6, vmContext) &&
          hasHeapVariable(12, 7, vmContext) &&
          hasHeapVariable(13, 7, vmContext) &&
          hasHeapVariable(14, 8, vmContext) &&
          hasHeapVariable(15, 8, vmContext)
    }
  }

  "sizeof" should "pass %x > %y (instruction pointer)" in {
    successfullyAssembleAndExecute(
      s"""
         | $aStandardMachineConfiguration
         | .machptr instruction u16
         | .bb main
         |   .var x %0 ptr
         |   .var y %1 u32
         |   copy &@main > %0
         |   sizeof %0 > %1
         |   exit (00:u8)
         | .boot main
       """.stripMargin
    ).thenVerify {
      case vmContext =>
        hasHeapVariable(1, 2, vmContext)
    }
  }

  "sizeof" should "pass %x > %y (heap pointer)" in {
    successfullyAssembleAndExecute(
      s"""
         | $aStandardMachineConfiguration
         | .machptr heap u32
         | .bb main
         |   .var x %0 ptr
         |   .var y %1 u32
         |   copy &%1 > %0
         |   sizeof %0 > %1
         |   exit (00:u8)
         | .boot main
       """.stripMargin
    ).thenVerify {
      case vmContext =>
        hasHeapVariable(1, 4, vmContext)
    }
  }

  "sizeof" should "pass %x > %$y (stack pointer)" in {
    successfullyAssembleAndExecute(
      s"""
         | $aStandardMachineConfiguration
         | .machptr stack u16
         | .bb main
         |   .push 1
         |   .var x %0 ptr
         |   .var y $$0 u32
         |   copy &$$0 > %0
         |   sizeof %0 > $$0
         |   exit (00:u8)
         | .boot main
       """.stripMargin
    ).thenVerify {
      case vmContext =>
        hasStackVariable(0, 2, vmContext)
    }
  }

  "sizeof" should "pass %0 > _" in {
    successfullyAssembleAndExecute(
      s"""
         | $aStandardMachineConfiguration
         | .bb main
         |   .var x %0 u32
         |   sizeof %0 > _
         |   exit (00:u8)
         | .boot main
       """.stripMargin
    ).thenVerify {
      case _ =>
        // Nothing to check
        true
    }
  }

  private def hasHeapVariable(index: Int, value: Int, vmContext: VmContext): Boolean =
    vmContext.heap.getVariable(index) match {
      case Ok(Some(Scalar(_, result))) =>
        result.toInt == value

      case _ =>
        false
    }

  private def hasStackVariable(index: Int, value: Int, vmContext: VmContext): Boolean =
    vmContext.stack.getVariable(index) match {
      case Ok(Some(Scalar(_, result))) =>
        result.toInt == value

      case _ =>
        false
    }
}
