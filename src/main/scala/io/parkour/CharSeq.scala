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

final case class CharSeqSlice(
    chars: Iterator[Char],
    materialized: Seq[Char] = Seq.empty[Char],
    pos: Int = 0
):
  def isMaterializedEmpty: Boolean = materialized.isEmpty

  override def toString: String = chars.mkString

object CharSeq:
  def apply(text: String): CharSeq        = new CharSeq(CharSeqSlice(text.iterator))
  def apply(it: Iterator[Char]): CharSeq  = new CharSeq(CharSeqSlice(it))
  def apply(slice: CharSeqSlice): CharSeq = new CharSeq(slice)

class CharSeq(private val seqSlice: CharSeqSlice):

  def chars: Iterator[Char] = seqSlice.chars

  def takeWhile(cond: Char => Boolean): CharSeqSlice =
    val (materialized, rest) = seqSlice.chars.span(cond)
    val materializedSlice    = materialized.toSeq
    CharSeqSlice(rest, materializedSlice, pos = seqSlice.pos + materializedSlice.size)

  def duplicate: (CharSeq, CharSeq) =
    val (orig, copy) = seqSlice.chars.duplicate
    (CharSeq(orig), CharSeq(copy))

  def size: Int = seqSlice.chars.size

  override def toString: String = seqSlice.chars.mkString
