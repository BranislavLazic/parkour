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

sealed trait ParserInput {
  def toIterator: Iterator[Char]
}

final case class TextInput(text: String) extends ParserInput {
  override def toIterator: Iterator[Char] = text.iterator

  override def toString(): String = text
}

final case class StreamInput(stream: Iterator[Char]) extends ParserInput {
  override def toIterator: Iterator[Char] = stream
  override def toString(): String         = stream.mkString
}

final case class ParseError(message: String, input: ParserInput)
final case class ParseSuccess[T](result: T, rest: ParserInput)

final case class Parser[T](val run: ParserInput => Either[ParseError, ParseSuccess[T]]) {

  /**
    * Combines two parsers where a succeeding value is the one from
    * the left-hand-side parser.
    */
  def *>[R](rhs: Parser[R]): Parser[T] = Parser[T] { input =>
    run(input).flatMap { case ParseSuccess(value, rest) =>
      rhs.run(rest).map { case ParseSuccess(_, r) => ParseSuccess(value, r) }
    }
  }

  /**
    * Combines two parsers where a succeeding value is the one from
    * the right-hand-side parser.
    */
  def <*[R](rhs: Parser[R]): Parser[R] = Parser[R] { input =>
    run(input).flatMap { case ParseSuccess(_, rest) =>
      rhs.run(rest)
    }
  }

  def map[R](f: T => R): Parser[R] = Parser[R] { input =>
    run(input).map { case ParseSuccess(value, rest) => ParseSuccess(f(value), rest) }
  }
}
