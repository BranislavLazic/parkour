/*
 * Copyright (c) 2022 Branislav Lazic
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package io.parkour

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

class ChunkedStreamSpec extends AnyWordSpec with Matchers {
  "ChunkedStream" should {
    "materialize stream of integers" in {
      val stream      = ChunkedStream("123".iterator)
      val (seq, rest) = stream.takeWhile(Character.isDigit)
      seq should have size 3
      rest.hasNext shouldBe false
    }

    "return an empty sequence and an empty iterator" in {
      val stream      = ChunkedStream("".iterator)
      val (seq, rest) = stream.takeWhile(Character.isDigit)
      seq should have size 0
      rest.hasNext shouldBe false
    }

    "materialize stream of integers and return the rest of the iterator" in {
      val stream      = ChunkedStream("123abc".iterator)
      val (seq, rest) = stream.takeWhile(Character.isDigit)
      seq should have size 3
      rest should have size 3
    }
    "drop integer elements" in {
      val stream = ChunkedStream("123abc".iterator)
      val rest   = stream.skipWhile(Character.isDigit)
      rest.mkString shouldBe "abc"
    }
  }
}
