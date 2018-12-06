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

package me.rand.commons.idioms

import scala.language.implicitConversions
import scala.reflect.{ClassTag, classTag}

object Status {
  type Status[+WhenOk, +WhenErr] = Either[WhenErr, WhenOk]
  type OrElse[+WhenOk, +WhenErr] = Status[WhenOk, WhenErr]
  type StatusOk[+WhenErr, +WhenOk] = Left[WhenErr, WhenOk]
  val Ok: Right.type = Right
  type StatusError[+WhenErr, +WhenOk] = Right[WhenErr, WhenOk]
  val Err: Left.type = Left

  def isOk[WhenOk, WhenErr](result: Status[WhenOk, WhenErr]): Boolean = result.isRight

  def isErr[WhenOk, WhenErr](result: Status[WhenOk, WhenErr]): Boolean = !isOk(result)

  class IterableWithStatus[T](iterable: Iterable[T]) {
    def tryForEach[U, WhenErr](f: T => Status[U, WhenErr]): Status[Unit, WhenErr] = {
      for (eachItem <- iterable) {
        f(eachItem) match {
          case Err(error) =>
            return Err(error)

          case Ok(_) =>
          // Continue
        }
      }

      Ok(())
    }

    def tryFoldLeft[WhenOk, WhenErr](ctor: WhenOk)(f: (WhenOk, T) => Status[WhenOk, WhenErr]): Status[WhenOk, WhenErr] = {
      var underConstruction = ctor
      for (eachItem <- iterable) {
        f(underConstruction, eachItem) match {
          case Ok(nextValue) =>
            underConstruction = nextValue

          case Err(error) =>
            return Err(error)
        }
      }

      Ok(underConstruction)
    }
  }

  implicit def Iterable2IterableWithStatus[T](iterable: Iterable[T]): IterableWithStatus[T] = new IterableWithStatus(iterable)

  class PartialFunctionReturningStatusBuilder {

    class PartialStatementBuilder[WhenOk, WhenErr](f: Status[WhenOk, WhenErr]) {
      def &&[ReturnType](g: WhenOk => ReturnType): Status[ReturnType, WhenErr] = f.map(g)

      def &[ReturnType](g: WhenOk => Status[ReturnType, WhenErr]): Status[ReturnType, WhenErr] = f.flatMap(g)

      def |[ReturnErrorType](g: WhenErr => Status[WhenOk, ReturnErrorType]): Status[WhenOk, ReturnErrorType] =
        f match {
          case Ok(x) =>
            Ok(x)

          case Err(err) =>
            g(err)
        }

      def ||[ReturnErrorType](g: WhenErr => ReturnErrorType): Status[WhenOk, ReturnErrorType] =
        f | (x => Err(g(x)))
    }

    def >[WhenOk, WhenErr](f: Status[WhenOk, WhenErr]): PartialStatementBuilder[WhenOk, WhenErr] =
      new PartialStatementBuilder(f)
  }

  implicit def StatusToPartialStatementBuilder[WhenOk, WhenErr](c: Status[WhenOk, WhenErr]):
  PartialFunctionReturningStatusBuilder#PartialStatementBuilder[WhenOk, WhenErr] =
    new PartialFunctionReturningStatusBuilder > c

  def tryTo[T, E <: Exception : ClassTag](f: => T): Status[T, E] =
    try {
      Ok(f)
    } catch {
      case e if classTag[E].runtimeClass.isInstance(e) =>
        Err(e.asInstanceOf[E])
    }
}
