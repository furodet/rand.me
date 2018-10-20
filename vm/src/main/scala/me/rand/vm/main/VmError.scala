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
package me.rand.vm.main

sealed trait VmError

object VmError {

  sealed trait VmContextError extends VmError

  object VmContextError {

    case class VariableIndexOutOfBounds(index: Int) extends VmContextError {
      override def toString: String = s"variable index out of bounds: $index"
    }

    case class InvalidVmTypeString(string: String) extends VmContextError {
      override def toString: String = s"invalid type string '$string'"
    }

  }

  sealed trait VmProfileStringError extends VmError

  object VmProfileStringError {

    case class InvalidFormat(profile: String, expected: String) extends VmProfileStringError {
      override def toString: String =
        s"invalid profile string '$profile': expected '$expected'"
    }

    case class NotAPositiveNumber(profile: String, fieldName: String) extends VmProfileStringError {
      override def toString: String =
        s"invalid profile string '$profile': field '$fieldName'"
    }

    case class ValueExceedsMaximumAllowed(profile: String, fieldName: String, maxValue: Int) extends VmProfileStringError {
      override def toString: String =
        s"profile value '$fieldName' exceeds maximum allowed ($maxValue)"
    }

    case class NotAPowerOfTwo(value: Int, fieldName: String) extends VmProfileStringError {
      override def toString: String =
        s"profile value '$fieldName'=$value is not a power of two"
    }

  }

}
