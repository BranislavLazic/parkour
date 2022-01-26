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

class ParkourSpec extends AnyWordSpec with Matchers:
  import Parkour.*

  private def ws: Parser[Unit] = skipManySatisfy(Character.isWhitespace)

  "Parkour" should {
    "parse an integer" in {
      val parsed       = int.run(CharSeq("25"))
      val parseSuccess = parsed.toOption.get
      parseSuccess.result `shouldBe` 25
      parseSuccess.rest.chars `should` have `size` 0
    }

    "skip white spaces" in {
      val parsed       = (skipManySatisfy(Character.isWhitespace)).run(CharSeq("    abc"))
      val parseSuccess = parsed.toOption.get
      parseSuccess.result `shouldBe` ()
      parseSuccess.rest.chars `should` have `size` 3
    }

    "parse a whitespace" in {
      val parsed       = ws.run(CharSeq(" "))
      val parseSuccess = parsed.toOption.get
      parseSuccess.result `shouldBe` ()
      parseSuccess.rest.chars `should` have `size` 0
    }

    "not parse an integer" in {
      val parsed = int.run(CharSeq("a25"))
      parsed `shouldBe` Left(ParseError("Not an integer 'a25'"))
    }

    "parse combined integers delimited by a space" in {
      val parsedFirst       = (int <* ws <* int).run(CharSeq("2    5"))
      val parseFirstSuccess = parsedFirst.toOption.get
      parseFirstSuccess.result `shouldBe` 5
      parseFirstSuccess.rest.chars `should` have `size` 0

      val parsedSecond       = (int *> ws *> int).run(CharSeq("2    5"))
      val parseSecondSuccess = parsedSecond.toOption.get
      parseSecondSuccess.result `shouldBe` 2
      parseSecondSuccess.rest.chars `should` have `size` 0

      val parsedWs        = (int <* ws *> int).run(CharSeq("2    5"))
      val parsedWsSuccess = parsedWs.toOption.get
      parsedWsSuccess.result `shouldBe` ()
      parsedWsSuccess.rest.chars `should` have `size` 0
    }

    "parse integers delimited by a space" in {
      val parsed       = pipe3(int, ws, int).run(CharSeq("2    5"))
      val parseSuccess = parsed.toOption.get
      parseSuccess.result `shouldBe` (2, (), 5)
      parseSuccess.rest.chars `should` have `size` 0
    }

    "parse a string" in {
      val parsed       = string("test1").run(CharSeq("test1"))
      val parseSuccess = parsed.toOption.get
      parseSuccess.result `shouldBe` "test1"
      parseSuccess.rest.chars `should` have `size` 0
    }

    "not parse a string" in {
      val parsed = string("test").run(CharSeq(" test"))
      parsed `shouldBe` Left(ParseError("Not a string character at position 0: ' test'"))
    }

    "parse a string starting with a whitespace, followed by a tab" in {
      val parsed       = (ws <* string("test")).run(CharSeq("   test1 "))
      val parseSuccess = parsed.toOption.get
      parseSuccess.result `shouldBe` "test"
      parseSuccess.rest.chars `should` have `size` 2
    }

    "parse all 'a' characters" in {
      val parsed       = manySatisfy(_ == 'a').run(CharSeq("aaabb"))
      val parseSuccess = parsed.toOption.get
      parseSuccess.result.mkString `shouldBe` "aaa"
      parseSuccess.rest.chars `should` have `size` 2
    }

    "not parse all 'a' characters if the string begins with 'b'" in {
      val parsed = manySatisfy(_ == 'a').run(CharSeq("baaabb"))
      parsed `shouldBe` Left(ParseError("Unexpected character at position 0: 'baaabb'."))
    }

    "parse an optionally negative integer" in {
      val parser = pipe2(opt(satisfy(_ == '-')), int)
        .map {
          case (Some(_), i) => -1 * i
          case (None, i)    => i
        }

      val parsedInt       = parser.run(CharSeq("3"))
      val parseSuccessInt = parsedInt.toOption.get
      parseSuccessInt.result `shouldBe` 3

      val parsedNegativeInt       = parser.run(CharSeq("-3"))
      val parseSuccessNegativeInt = parsedNegativeInt.toOption.get
      parseSuccessNegativeInt.result `shouldBe` -3
    }

    "parse one of two strings" in {
      val parsed       = (string("true") <|> string("false")).run(CharSeq("false"))
      val parseSuccess = parsed.toOption.get
      parseSuccess.result `shouldBe` "false"
      parseSuccess.rest.chars `should` have `size` 0
    }

    "parse comma separated integers" in {
      val parsed =
        sepBy(int, satisfy(_ == ',')).run(CharSeq("1,1,1,122abc"))
      val parseSuccess = parsed.toOption.get
      parseSuccess.result `shouldBe` List(1, 1, 1, 122)
      parseSuccess.rest.chars `should` have `size` 3
    }

    "parse many letters" in {
      val parsed =
        many(string("bla")).run(CharSeq("blablabla123"))
      val parseSuccess = parsed.toOption.get
      parseSuccess.result `shouldBe` List("bla", "bla", "bla")
      parseSuccess.rest.chars `should` have `size` 3
    }
  }
