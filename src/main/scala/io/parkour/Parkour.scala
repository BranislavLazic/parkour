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
      ChunkedStream(input.toIterator).takeWhile(Character.isDigit)
    if (digitsSeq.isEmpty) Left(ParseError(s"not a digit '$input'", input))
    else Right(ParseSuccess(digitsSeq.mkString.toInt, StreamInput(rest)))
  }

  def skipManySatisfy(cond: Char => Boolean) = Parser[Unit] { input =>
    val rest = ChunkedStream(input.toIterator).skipWhile(cond)
    Right(ParseSuccess((), StreamInput(rest)))
  }

  def ws: Parser[Unit] = skipManySatisfy(Character.isWhitespace)

  def between[L, R, T](lhs: Parser[L], rhs: Parser[R], self: Parser[T]): Parser[T] =
    lhs <* self *> rhs

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
}
