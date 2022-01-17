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

object Parkour {

  def integer: Parser[Int] = Parser[Int] { input =>
    val (digitsSeq, rest) =
      input.toIterator.span(Character.isDigit)
    if (digitsSeq.isEmpty) Left(ParseError(s"Not an integer '$input'"))
    else Right(ParseSuccess(digitsSeq.mkString.toInt, StreamInput(rest)))
  }

  def string: Parser[String] = Parser[String] { input =>
    val (str, rest) =
      input.toIterator.span(ch => !Character.isSpaceChar(ch))
    Right(ParseSuccess(str.mkString, StreamInput(rest)))
  }

  def satisfy(cond: Char => Boolean) = Parser[Char] { input =>
    val it = input.toIterator
    if (it.hasNext) {
      val ch = it.next()
      if (cond(ch)) Right(ParseSuccess(ch, StreamInput(it)))
      else Left(ParseError(s"Unexpected character at the beginning of '$input'."))
    } else {
      Left(ParseError(s"No characters to parse."))
    }
  }

  def manySatisfy(cond: Char => Boolean) = Parser[String] { input =>
    val (str, rest) = input.toIterator.span(cond)
    if (str.isEmpty) Left(ParseError(s"Unexpected character at the beginning of '$input'."))
    else Right(ParseSuccess(str.mkString, StreamInput(rest)))
  }

  def skipManySatisfy(cond: Char => Boolean) = Parser[Unit] { input =>
    val rest = input.toIterator.span(cond)._2
    Right(ParseSuccess((), StreamInput(rest)))
  }

  def ws: Parser[Unit] = skipManySatisfy(Character.isWhitespace)

  def opt[T](p: Parser[T]): Parser[Option[T]] = Parser[Option[T]] { input =>
    p.run(input) match {
      case Left(ParseError(message))         => Right(ParseSuccess(None, input))
      case Right(ParseSuccess(result, rest)) => Right(ParseSuccess(Some(result), rest))
    }
  }

  def pipe2[P1, P2](p1: Parser[P1], p2: Parser[P2]): Parser[(P1, P2)] = Parser[(P1, P2)] { input =>
    for {
      r1 <- p1.run(input)
      r2 <- p2.run(r1.rest)
    } yield ParseSuccess((r1.result, r2.result), r2.rest)
  }

  def pipe3[P1, P2, P3](p1: Parser[P1], p2: Parser[P2], p3: Parser[P3]): Parser[(P1, P2, P3)] =
    Parser[(P1, P2, P3)] { input =>
      for {
        r1 <- pipe2(p1, p2).run(input)
        r2 <- p3.run(r1.rest)
      } yield ParseSuccess((r1.result._1, r1.result._2, r2.result), r2.rest)
    }

  def pipe4[P1, P2, P3, P4](
      p1: Parser[P1],
      p2: Parser[P2],
      p3: Parser[P3],
      p4: Parser[P4]
  ): Parser[(P1, P2, P3, P4)] =
    Parser[(P1, P2, P3, P4)] { input =>
      for {
        r1 <- pipe3(p1, p2, p3).run(input)
        r2 <- p4.run(r1.rest)
      } yield ParseSuccess((r1.result._1, r1.result._2, r1.result._3, r2.result), r2.rest)
    }
}
