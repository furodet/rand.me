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

class IncrementSpec extends BaseSpec {
  private def main(body: String): String =
    s"""
       | $aStandardMachineConfiguration
       | .bb main
     """.stripMargin +
      body +
      s"""
         |  exit (00:u8)
         | .boot main
     """.stripMargin

  "++" should "pass %x > %y" in {
    successfullyAssembleAndExecute(
      main(body =
        s"""
           | .var xu8 %0 u8
           | .var xs8 %1 s8
           | copy (ff:u8) > %0
           | copy (fe:s8) > %1
           | ++ %0 > %0
           | ++ %1 > %1
         """.stripMargin
      )
    ).thenVerify {
      case vmContext =>
        hasHeapVariable(0, 0, vmContext) &&
          hasHeapVariable(1, -1, vmContext)
    }
  }

  private def hasHeapVariable(index: Int, value: Int, vmContext: VmContext): Boolean =
    vmContext.heap.getVariable(index) match {
      case Ok(Some(Scalar(_, result))) =>
        result.toInt == value

      case _ =>
        false
    }
}
