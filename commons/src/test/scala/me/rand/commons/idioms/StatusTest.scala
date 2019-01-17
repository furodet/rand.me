/*-
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
package me.rand.commons.idioms

import me.rand.commons.idioms.Status._
import org.scalatest.FlatSpec

class StatusTest extends FlatSpec {
  "a status" should "allow to report ok" in {
    def f: Int OrElse Exception = Ok(0)

    f match {
      case r@Ok(0) =>
        assert(isOk(r))
        assert(!isErr(r))

      case Ok(invalid) =>
        fail(s"invalid value $invalid returned by f")

      case Err(err) =>
        fail(s"invalid error $err returned by f")
    }
  }

  "a status" should "allow to report err" in {
    def f: Int OrElse Exception = Err(new RuntimeException())

    f match {
      case Ok(invalid) =>
        fail(s"invalid value $invalid returned by f")

      case r@Err(_: RuntimeException) =>
        assert(isErr(r))
        assert(!isOk(r))

      case Err(invalid) =>
        fail(s"invalid error $invalid returned by f")
    }
  }

  "an iterable" should "pass all elements when try for each with success" in {
    // Use a var to be sure that we parse every element, since a for each has
    // no explicit effect
    val out = StringBuilder.newBuilder
    Seq("elephants", "can", "speak", "easy").tryForEach {
      eachWord =>
        out.append(eachWord)
        Ok(eachWord)
    } match {
      case Ok(()) =>
        assert(out.result() == "elephantscanspeakeasy")

      case somethingElse =>
        fail(s"invalid result returned by tryForEach: $somethingElse")
    }
  }

  "an iterable" should "stop when try for each and an error occurs" in {
    // Use a var to be sure that we parse every element, since a for each has
    // no explicit effect
    val out = StringBuilder.newBuilder
    Seq("elephants", "can", "speak", "easy").tryForEach {
      case eachWord if eachWord == "speak" =>
        Err(eachWord.length)

      case anyOtherWord =>
        out.append(anyOtherWord)
        Ok(anyOtherWord)
    } match {
      case Err(5) =>
        assert(out.result() == "elephantscan")

      case somethingElse =>
        fail(s"invalid result returned by tryForEach: $somethingElse")
    }
  }

  "an iterable" should "process all elements when successfully try fold left" in {
    Seq("elephants", "can", "speak", "easy").tryFoldLeft("") {
      case (underConstruction, eachWord) =>
        Ok(underConstruction + eachWord)
    } match {
      case Ok("elephantscanspeakeasy") =>
        succeed

      case somethingElse =>
        fail(s"invalid result returned by tryFoldLeft: $somethingElse")
    }
  }

  "an iterable" should "stop when try fold left and an error occurs" in {
    Seq("elephants", "can", "speak", "easy").tryFoldLeft("") {
      case (underConstruction, "easy") =>
        Err(underConstruction)

      case (underConstruction, anyOtherWord) =>
        Ok(underConstruction + anyOtherWord)
    } match {
      case Err("elephantscanspeak") =>
        succeed

      case somethingElse =>
        fail(s"invalid result returned by tryFoldLeft: $somethingElse")
    }
  }

  "a status" should "allow to map and succeed" in {
    def okIfHello(string: String): String OrElse Int =
      if (string == "hello") Ok("aloha") else Err(66)

    okIfHello("hello") && (_ + "0") match {
      case Ok("aloha0") =>
        succeed

      case somethingElse =>
        fail(s"invalid result returned by map: $somethingElse")
    }
  }

  "a status" should "allow to left map and fail" in {
    def okIfHello(string: String): String OrElse Int =
      if (string == "hello") Ok("aloha") else Err(66)

    okIfHello("helloo") || (_ + 8) match {
      case Err(74) =>
        succeed

      case somethingElse =>
        fail(s"invalid result returned by map: $somethingElse")
    }
  }

  "a status" should "allow to map and fail" in {
    def okIfHello(string: String): String OrElse Int =
      if (string == "hello") Ok("aloha") else Err(66)

    okIfHello("goodbye") && (_ + "0") match {
      case Err(66) =>
        succeed

      case somethingElse =>
        fail(s"invalid result returned by map: $somethingElse")
    }
  }

  "a status" should "allow to left map and succeed" in {
    def okIfHello(string: String): String OrElse Int =
      if (string == "hello") Ok("aloha") else Err(66)

    okIfHello("hello") || (_ + 8) match {
      case Ok("aloha") =>
        succeed

      case somethingElse =>
        fail(s"invalid result returned by map: $somethingElse")
    }
  }

  "a status" should "allow to flat map and succeed" in {
    def okIfHello(string: String): String OrElse Int =
      if (string == "hello") Ok("aloha") else Err(66)

    okIfHello("hello") & { x => Ok(x.head) } match {
      case Ok('a') =>
        succeed

      case somethingElse =>
        fail(s"invalid result returned by flat map: $somethingElse")
    }
  }

  "a status" should "allow to flat left map and fail" in {
    def okIfHello(string: String): String OrElse Int =
      if (string == "hello") Ok("aloha") else Err(66)

    okIfHello("helloo") | { x => Err(x + 8) } match {
      case Err(74) =>
        succeed

      case somethingElse =>
        fail(s"invalid result returned by flat map: $somethingElse")
    }
  }

  "a status" should "allow to flat map and fail" in {
    def okIfHello(string: String): String OrElse Int =
      if (string == "hello") Ok("aloha") else Err(66)

    okIfHello("hello") & { _ => Err(77) } match {
      case Err(77) =>
        succeed

      case somethingElse =>
        fail(s"invalid result returned by flat map: $somethingElse")
    }
  }

  "a status" should "allow to flat left map and succeed" in {
    def okIfHello(string: String): String OrElse Int =
      if (string == "hello") Ok("aloha") else Err(66)

    okIfHello("hello") | { x => Err(x + 8) } match {
      case Ok("aloha") =>
        succeed

      case somethingElse =>
        fail(s"invalid result returned by flat map: $somethingElse")
    }
  }

  "a status" should "allow to flat map left" in {
    def okIfHello(string: String): String OrElse Int =
      if (string == "hello") Ok("aloha") else Err(66)

    okIfHello("goodbye") & { x => Ok(x + "0") } match {
      case Err(66) =>
        succeed

      case somethingElse =>
        fail(s"invalid result returned by flat map: $somethingElse")
    }
  }

  "a tryTo" should "allow to hide ugly try/catches" in {
    def incrementOrThrowIfZero(x: Int): Int = if (x == 0) throw new RuntimeException("igotya") else x + 1

    tryTo[Int, RuntimeException](incrementOrThrowIfZero(0)) match {
      case Err(err) =>
        assert(err.getMessage == "igotya")

      case somethingElse =>
        fail(s"invalid result returned by tryTo: $somethingElse")
    }
  }

  "a tryTo" should "wrap successful operations in status" in {
    def incrementOrThrowIfZero(x: Int): Int = if (x == 0) throw new RuntimeException("igotya") else x + 1

    tryTo[Int, RuntimeException](incrementOrThrowIfZero(1)) match {
      case Ok(2) =>
        succeed

      case somethingElse =>
        fail(s"invalid result returned by tryTo: $somethingElse")
    }
  }
}
