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

object Parkour:
  /**
    * Parses a single character that satisfies the given condition.
    */
  def satisfy(cond: Char => Boolean): Parser[Char] = Parser[Char] { input =>
    val it = input.toIterator
    if (it.hasNext)
      val ch = it.next()
      if (cond(ch))
        Right(ParseSuccess(ch, StreamInput(it)))
      else
        Left(ParseError(s"Unexpected character at the beginning of '$input'."))
    else Left(ParseError(s"No characters to parse."))
  }

  /**
    * Parses a sequence of characters that satisfy the given condition.
    */
  def manySatisfy(cond: Char => Boolean): Parser[List[Char]] = Parser[List[Char]] { input =>
    val (str, rest) = input.toIterator.span(cond)
    if (str.isEmpty)
      Left(ParseError(s"Unexpected character at the beginning of '$input'."))
    else
      Right(ParseSuccess(str.toList, StreamInput(rest)))
  }

  /**
    * Skips a sequence of characters that satisfy the given condition.
    */
  def skipManySatisfy(cond: Char => Boolean): Parser[Unit] =
    manySatisfy(cond).map(_ => ()) <|> Parser.pure(())

  /**
    * Parses a sequence of characters with the parser "p".
    */
  def many[T](p: Parser[T]): Parser[List[T]] =
    p.flatMap(v => many(p).map(list => v :: list)) <|> Parser.pure(Nil)

  /**
    * Parses an unsigned 64 bit integer.
    */
  def unsignedInt64 = Parser[Long] { input =>
    many(satisfy(Character.isDigit)).run(input).flatMap {
      case ParseSuccess(v, rest) if v.nonEmpty => Right(ParseSuccess(v.mkString.toLong, rest))
      case ParseSuccess(v, rest)               => Left(ParseError(s"Not an integer '$rest'"))
    }
  }

  /**
    * Parses a signed 64 bit integer.
    */
  def int64: Parser[Long] = pipe2(opt(satisfy(_ == '-')), unsignedInt64).map {
    case (Some(_), v) => -1 * v
    case (None, v)    => v
  }

  /**
    * Parses a signed 32 bit integer.
    */
  def int: Parser[Int] = int64.map(_.toInt)

  /**
    * Parses a string.
    */
  def string(str: String): Parser[String] = Parser[String] { input =>
    val strIt = str.iterator
    val (strResult, rest) =
      input.toIterator.span { ch =>
        if (strIt.hasNext) ch == strIt.next else false
      }
    if (strResult.isEmpty)
      Left(ParseError(s"Not a string at '$input'"))
    else
      Right(ParseSuccess(strResult.mkString, StreamInput(rest)))
  }

  /**
    * Parses an optional occurrence of the given parser.
    */
  def opt[T](p: Parser[T]): Parser[Option[T]] = p.map(v => Some(v)) <|> Parser.pure(None)

  /**
    * Parses multiple occurrences of parser "p" separated by "sep" parser.
    */
  def sepBy[L, R](p: Parser[L], sep: Parser[R]): Parser[List[L]] =
    bindCons(p, opt(sep).flatMap(_.map(_ => sepBy(p, sep)).getOrElse(Parser.pure(Nil))))

  /**
    * Combine two parsers and return their values as a tuple.
    */
  def pipe2[P1, P2](p1: Parser[P1], p2: Parser[P2]): Parser[(P1, P2)] =
    p1.flatMap(v1 => p2.map(v2 => (v1, v2)))

  def pipe3[P1, P2, P3](p1: Parser[P1], p2: Parser[P2], p3: Parser[P3]): Parser[(P1, P2, P3)] =
    pipe2(p1, p2).flatMap(v1 => p3.map(v2 => (v1._1, v1._2, v2)))

  def pipe4[P1, P2, P3, P4](
      p1: Parser[P1],
      p2: Parser[P2],
      p3: Parser[P3],
      p4: Parser[P4]
  ): Parser[(P1, P2, P3, P4)] =
    pipe3(p1, p2, p3).flatMap(v1 => p4.map(v2 => (v1._1, v1._2, v1._3, v2)))

  private def bindCons[T](p: Parser[T], tail: Parser[List[T]]): Parser[List[T]] =
    p.flatMap(h => tail.map(t => h :: t))
