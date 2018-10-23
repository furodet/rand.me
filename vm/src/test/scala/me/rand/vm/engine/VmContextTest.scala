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
import me.rand.vm.engine.Variable.ScalarBuilder.aScalarCalled
import me.rand.vm.engine.VmContext.VmProfile
import me.rand.vm.engine.VmTypes.VmType
import me.rand.vm.main.VmError.VmContextError.{EmptyStackAccess, InvalidVmTypeString, VariableIndexOutOfBounds}
import me.rand.vm.main.VmError._
import org.scalatest.FlatSpec

import scala.language.postfixOps

class VmContextTest extends FlatSpec {
  // just a decorating nit.
  private def ok(): Unit = ()

  private val aStandardHeapSize = 10

  "VM profile" should "not be created from invalid specification (illegal string)" in {
    VmProfile.fromString("babeubi") match {
      case Err(VmProfileStringError.InvalidFormat(_, _)) =>
        ok()

      case whatever =>
        fail(s"unexpected result from profile with illegal string: $whatever")
    }
  }

  "VM profile" should "not be created from invalid specification (illegal string integer)" in {
    VmProfile.fromString("bl:what?:heap:10") match {
      case Err(VmProfileStringError.NotAPositiveNumber(_, "bl")) =>
        ok()

      case whatever =>
        fail(s"unexpected result from profile with illegal string: $whatever")
    }
  }

  "VM profile" should "not be created from invalid specification (invalid byte len)" in {
    VmProfile.fromString("bl:0:heap:10") match {
      case Err(VmProfileStringError.NotAPositiveNumber(_, "bl")) =>
        ok()

      case whatever =>
        fail(s"unexpected result from profile with illegal string: $whatever")
    }
  }

  "VM profile" should "not be created from invalid specification (excessive byte len)" in {
    VmProfile.fromString("bl:257:heap:10") match {
      case Err(VmProfileStringError.ValueExceedsMaximumAllowed(_, "bl", VmContext.maximumByteSizeAllowed)) =>
        ok()

      case whatever =>
        fail(s"unexpected result from profile with illegal string: $whatever")
    }
  }

  "VM profile" should "not be created from invalid specification (byte len is not a power of two)" in {
    VmProfile.fromString("bl:3:heap:10") match {
      case Err(VmProfileStringError.NotAPowerOfTwo(3, "bl")) =>
        ok()

      case whatever =>
        fail(s"unexpected result from profile with illegal string: $whatever")
    }
  }

  "VM profile" should "not be created from invalid specification (invalid varsetsize)" in {
    VmProfile.fromString("bl:1:heap:-1") match {
      case Err(VmProfileStringError.NotAPositiveNumber(_, "heap")) =>
        ok()

      case whatever =>
        fail(s"unexpected result from profile with illegal string: $whatever")
    }
  }

  "VM profile" should "not be created from invalid specification (excessive varsetsize)" in {
    VmProfile.fromString(s"bl:1:heap:${VmContext.maximumNumberOfVariablesInHeap + 1}") match {
      case Err(VmProfileStringError.ValueExceedsMaximumAllowed(_, "heap", VmContext.maximumNumberOfVariablesInHeap)) =>
        ok()

      case whatever =>
        fail(s"unexpected result from profile with illegal string: $whatever")
    }
  }

  "VM context" should "properly initialize for 8-bits machine" in {
    VmContext.usingProfileString(s"bl:1:heap:${VmContext.maximumNumberOfVariablesInHeap}") match {
      case Ok(vmContext) =>
        assertThatVmTypesMapExactly(vmContext.vmTypes, 1)

      case whatever =>
        fail(s"unexpected result from profile/8-bits: $whatever")
    }
  }

  "VM context" should "properly initialize for 64-bits machine" in {
    VmContext.usingProfileString(s"bl:8:heap:${VmContext.maximumNumberOfVariablesInHeap}") match {
      case Ok(vmContext) =>
        assertThatVmTypesMapExactly(vmContext.vmTypes, 1, 2, 4, 8)

      case whatever =>
        fail(s"unexpected result from profile/64-bits: $whatever")
    }
  }

  "VM types" should "understand legal type strings and reject illegal type strings" in {
    VmContext.usingProfileString(s"bl:1:heap:10") match {
      case Ok(vmContext) =>
        vmContext.vmTypes.valueOf("u8") match {
          case Ok(vmType) if vmType.byteLen == 1 && vmType.isUnsigned =>
            ok()

          case Err(whatever) =>
            fail(s"unexpected result from value of (u8): $whatever")
        }

        vmContext.vmTypes.valueOf("s8") match {
          case Ok(vmType) if vmType.byteLen == 1 && vmType.isSigned =>
            ok()

          case whatever =>
            fail(s"unexpected result from value of (s8): $whatever")
        }

        vmContext.vmTypes.valueOf("u16") match {
          case Err(InvalidVmTypeString("u16")) =>
            ok()

          case whatever =>
            fail(s"unexpected result from value of (u16): $whatever")
        }
      case whatever =>
        fail(s"unexpected result from valid profile definition: $whatever")
    }
  }

  "VM context" should "not allow to pop frame from empty stack" in {
    givenAValidVmContext {
      vmContext =>
        vmContext.popFrame() match {
          case Err(EmptyStackAccess(_)) =>
            ok()

          case whatever =>
            fail(s"unexpected result for pop frame from empty stack: $whatever")
        }
    }
  }

  "VM context" should "allow to push and pop frames in the stack" in {
    givenAValidVmContext {
      c0 =>
        val c1 = c0.createFrameOfSize(1)
        assert(c1.stack.frames.length == 1)
        val c2 = c1.createFrameOfSize(2)
        assert(c2.stack.frames.length == 2)
        (for {
          c3 <- c2.popFrame()
          _ = assert(c3.stack.frames.length == 1)
          finalContext <- c3.popFrame()
        } yield finalContext) match {
          case Ok(context) if context.stack.frames isEmpty =>
            ok()

          case whatever =>
            fail(s"unexpected result from push/pop frames: $whatever")
        }
    }
  }

  "VM context" should "not allow to read variables from an empty stack" in {
    givenAValidVmContext {
      vmContext =>
        vmContext.stack.getVariable(0) match {
          case Err(EmptyStackAccess(_)) =>
            ok()

          case whatever =>
            fail(s"unexpected result from get variable from empty stack: $whatever")
        }
    }
  }

  "VM context" should "not allow to write variables into an empty stack" in {
    givenAValidVmContext {
      vmContext =>
        vmContext.stack.putVariable(0, aVariable(vmContext)) match {
          case Err(EmptyStackAccess(_)) =>
            ok()

          case whatever =>
            fail(s"unexpected result from put variable from empty stack: $whatever")
        }
    }
  }

  "VM context" should "allow to write then read variables in stack" in {
    givenAValidVmContext {
      vmContext =>
        val myVariable = aVariable(vmContext)
        val hasFrame = vmContext.createFrameOfSize(1)
        (for {
          _ <- hasFrame.stack.putVariable(0, myVariable)
          v <- hasFrame.stack.getVariable(0)
        } yield v) match {
          case Ok(Some(variable)) if variable == myVariable =>
            ok()

          case whatever =>
            fail(s"unexpected result from put/get variable from stack: $whatever")
        }
    }
  }

  "VM context" should "prevent from illegal read to a VarSet (negative index)" in {
    givenAValidVmContext {
      vmContext =>
        vmContext.heap.getVariable(-1) match {
          case Err(VariableIndexOutOfBounds(-1)) =>
            ok()

          case whatever =>
            fail(s"unexpected result from illegal access to heap: $whatever")
        }
    }
  }

  "VM context" should "prevent from illegal read to a VarSet (index too large)" in {
    givenAValidVmContext {
      vmContext =>
        vmContext.heap.getVariable(aStandardHeapSize) match {
          case Err(VariableIndexOutOfBounds(s)) if s == aStandardHeapSize =>
            ok()

          case whatever =>
            fail(s"unexpected result from illegal access to heap: $whatever")
        }
    }
  }

  "VM context" should "prevent from illegal write to a VarSet (negative index)" in {
    givenAValidVmContext {
      vmContext =>
        vmContext.heap.putVariable(Int.MinValue, aVariable(vmContext)) match {
          case Err(VariableIndexOutOfBounds(Int.MinValue)) =>
            ok()

          case whatever =>
            fail(s"unexpected result from illegal access to heap: $whatever")
        }
    }
  }

  "VM context" should "prevent from illegal write to a VarSet (index too large)" in {
    givenAValidVmContext {
      vmContext =>
        vmContext.heap.putVariable(Int.MaxValue, aVariable(vmContext)) match {
          case Err(VariableIndexOutOfBounds(Int.MaxValue)) =>
            ok()

          case whatever =>
            fail(s"unexpected result from illegal access to heap: $whatever")
        }
    }
  }

  "VM context" should "allow to read undefined VarSet variables" in {
    givenAValidVmContext {
      vmContext =>
        vmContext.heap.getVariable(0) match {
          case Ok(None) =>
            ok()

          case whatever =>
            fail(s"unexpected result from access to uninitialized heap variable: $whatever")
        }
    }
  }

  "VM context" should "allow to write then read a variable in a VarSet" in {
    givenAValidVmContext {
      vmContext =>
        val myVariable = aVariable(vmContext)
        (for {
          _ <- vmContext.heap.putVariable(2, myVariable)
          v <- vmContext.heap.getVariable(2)
        } yield v) match {
          case Ok(Some(variable)) if variable == myVariable =>
            ok()

          case whatever =>
            fail(s"unexpected result from put/get variable in heap: $whatever")
        }
    }
  }

  private def aVariable(c: VmContext) =
    aScalarCalled("x").ofType(c.vmTypes.select(1, isSigned = true).get).setTo(0)

  private def givenAValidVmContext(action: VmContext => Unit): Unit =
    VmContext.usingProfileString(s"bl:1:heap:$aStandardHeapSize") match {
      case Ok(vmContext) =>
        action(vmContext)

      case whatever =>
        fail(s"unexpected result from valid profile definition: $whatever")
    }

  private def assertThatVmTypesMapExactly(types: VmTypes, byteLens: Int*): Unit = {
    // Has every expected type
    byteLens.foreach {
      eachLen =>
        assertThatVmTypesSelectAllTypesOfByteLen(eachLen, types)
    }
    // Has no other type
    assert(2 * byteLens.length == types.typeMap.size)
    // Just test with one stupid type at least once
    types.select(128, isSigned = true) match {
      case Some(unexpectedType) =>
        fail(s"type u128 should not be known but got $unexpectedType")

      case None =>
      // Ok
    }
  }

  private def assertThatVmTypesSelectAllTypesOfByteLen(byteLen: Int, types: VmTypes): Unit = {
    assert(types.select(byteLen, isSigned = true).isDefined)
    assert(types.select(byteLen, isSigned = false).isDefined)
    types.select(byteLen) match {
      case seq if vmTypeSequenceContainsSignedAndUnsignedTypes(seq, byteLen) =>
        ()

      case whatever =>
        fail(s"failed to select signed or unsigned type with byte length $byteLen: $whatever")
    }
  }

  private def vmTypeSequenceContainsSignedAndUnsignedTypes(seq: Seq[VmType], byteLen: Int): Boolean =
    (seq.length == 2) &&
      vmTypeSeqContains(seq, byteLen, isSigned = true) &&
      vmTypeSeqContains(seq, byteLen, isSigned = false) &&
      vmTypeSeqContainsTypeNameForLength(seq, byteLen)

  private def vmTypeSeqContains(seq: Seq[VmType], byteLen: Int, isSigned: Boolean): Boolean =
    seq.exists(t => (t.isSigned == isSigned) && (t.byteLen == byteLen))

  private def vmTypeSeqContainsTypeNameForLength(seq: Seq[VmType], byteLen: Int): Boolean =
    (seq.length == 2) &&
      seq.exists(t => t.toString == s"s${8 * byteLen}") &&
      seq.exists(t => t.toString == s"u${8 * byteLen}")

}
