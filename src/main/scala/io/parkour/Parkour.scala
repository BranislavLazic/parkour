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

  def integer: Parser[Int] = Parser[Int] { input =>
    val (digitsSeq, rest) =
      input.toIterator.span(Character.isDigit)
    if (digitsSeq.isEmpty)
      Left(ParseError(s"Not an integer '${input}'", StreamInput(rest)))
    else
      Right(ParseSuccess(digitsSeq.mkString.toInt, StreamInput(rest)))
  }

  def string(str: String): Parser[String] = Parser[String] { input =>
    val strIt = str.iterator
    val (strResult, rest) =
      input.toIterator.span { ch =>
        if (strIt.hasNext) ch == strIt.next else false
      }
    if (strResult.isEmpty)
      Left(ParseError(s"Not a string at '${input}'", StreamInput(rest)))
    else
      Right(ParseSuccess(strResult.mkString, StreamInput(rest)))
  }

  def satisfy(cond: Char => Boolean) = Parser[Char] { input =>
    val it = input.toIterator
    if (it.hasNext)
      val ch = it.next()
      if (cond(ch))
        Right(ParseSuccess(ch, StreamInput(it)))
      else
        Left(ParseError(s"Unexpected character at the beginning of '$input'.", StreamInput(it)))
    else Left(ParseError(s"No characters to parse.", StreamInput(it)))
  }

  def manySatisfy(cond: Char => Boolean) = Parser[Seq[Char]] { input =>
    val (str, rest) = input.toIterator.span(cond)
    if (str.isEmpty)
      Left(ParseError(s"Unexpected character at the beginning of '$input'.", input))
    else
      Right(ParseSuccess(str.toSeq, StreamInput(rest)))
  }

  def skipManySatisfy(cond: Char => Boolean) = Parser[Unit] { input =>
    val rest = input.toIterator.span(cond)._2
    Right(ParseSuccess((), StreamInput(rest)))
  }

  def opt[T](p: Parser[T]): Parser[Option[T]] = Parser[Option[T]] { input =>
    p.run(input) match
      case Left(ParseError(message, _))      => Right(ParseSuccess(None, input))
      case Right(ParseSuccess(result, rest)) => Right(ParseSuccess(Some(result), rest))
  }

  def reps[T](p: Parser[T]): Parser[List[T]] = Parser[List[T]] { input =>
    p.run(input) match
      case Right(ParseSuccess(v, rest)) =>
        reps(p).map(l => v :: l).run(rest)
      case l @ Left(ParseError(msg, rest)) => Right(ParseSuccess(Nil, rest))
  }

  // def sepBy[L, R](p: Parser[L], sep: Parser[R]): Parser[List[L]] =
  //   Parser[List[L]] { input =>
  //     pipe2(p, opt(sep)).run(input) match
  //       case Right(ParseSuccess((v, Some(_)), rest)) =>
  //         sepBy(p, sep).run(rest).map {
  //           case ParseSuccess(list, rst) => ParseSuccess(v :: list, rst)
  //         }
  //       case Right(ParseSuccess((v, None), rest)) =>
  //         Right(ParseSuccess(v :: Nil, rest))
  //       case Left(_) => Left(ParseError("bla"))
  //   }
  // val first = for {
  //   f <- opt(p)
  //   s <- opt(sep)
  // } yield (f, s)

  // def remaining: Parser[List[L]] =
  //   bindCons(p, opt(sep).flatMap(_.map(_ => remaining).getOrElse(Parser.pure(Nil))))

  // first.flatMap {
  //   case (optFirst, optSep) =>
  //     optFirst
  //       .map(f =>
  //         println(f)
  //         optSep.map(_ => bindCons(Parser.pure(f), remaining)).getOrElse(Parser.pure(List(f)))
  //       )
  //       .getOrElse(Parser.pure(Nil))
  // }

  def bindCons[T](p: Parser[T], tail: Parser[List[T]]): Parser[List[T]] =
    p.flatMap(h => tail.map(t => h :: t))

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
