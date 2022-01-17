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

class ParserSpec extends AnyWordSpec with Matchers {
  import Parkour._

  case class NumberPair(first: Int, second: Int)

  "Parser" should {
    "parse an integer" in {
      val parsed       = integer.run(TextInput("25"))
      val parseSuccess = parsed.toOption.get
      parseSuccess.result shouldBe 25
      parseSuccess.rest.toIterator should have size 0
    }

    "skip whitespaces" in {
      val parsed       = (skipManySatisfy(Character.isWhitespace)).run(TextInput("    abc"))
      val parseSuccess = parsed.toOption.get
      parseSuccess.result shouldBe ()
      parseSuccess.rest.toIterator should have size 3
    }

    "parse a whitespace" in {
      val parsed       = ws.run(TextInput(" "))
      val parseSuccess = parsed.toOption.get
      parseSuccess.result shouldBe ()
      parseSuccess.rest.toIterator should have size 0
    }

    "not parse an integer" in {
      val parsed = integer.run(TextInput("a25"))
      parsed shouldBe Left(ParseError("not a digit 'a25'", TextInput("a25")))
    }

    "parse combined integers delimited by a space" in {
      val parsedFirst       = (integer <* ws <* integer).run(TextInput("2    5"))
      val parseFirstSuccess = parsedFirst.toOption.get
      parseFirstSuccess.result shouldBe 5
      parseFirstSuccess.rest.toIterator should have size 0

      val parsedSecond       = (integer *> ws *> integer).run(TextInput("2    5"))
      val parseSecondSuccess = parsedSecond.toOption.get
      parseSecondSuccess.result shouldBe 2
      parseSecondSuccess.rest.toIterator should have size 0

      val parsedWs        = (integer <* ws *> integer).run(TextInput("2    5"))
      val parsedWsSuccess = parsedWs.toOption.get
      parsedWsSuccess.result shouldBe ()
      parsedWsSuccess.rest.toIterator should have size 0
    }

    "parse integers delimited by a space" in {
      val parsed       = pipe3(integer, ws, integer).run(TextInput("2    5"))
      val parseSuccess = parsed.toOption.get
      parseSuccess.result shouldBe (2, (), 5)
      parseSuccess.rest.toIterator should have size 0
    }

    "parse an integer between spaces" in {
      val parsed       = Parkour.between(ws, ws, integer).run(TextInput("   255   "))
      val parseSuccess = parsed.toOption.get
      parseSuccess.result shouldBe 255
      parseSuccess.rest.toIterator should have size 0
    }
  }
}
