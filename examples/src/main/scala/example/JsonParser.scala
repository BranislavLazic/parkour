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

package example

import io.parkour.Parkour.*
import io.parkour.Parser
import io.parkour.CharSeq

sealed trait JsValue
case object JsNull                                extends JsValue
case class JsInt(value: Int)                      extends JsValue
case class JsString(value: String)                extends JsValue
case class JsBoolean(value: Boolean)              extends JsValue
case class JsArray(values: List[JsValue])         extends JsValue
case class JsObject(values: Map[String, JsValue]) extends JsValue

object JsonParser:
  def betweenChars[T](open: Char, close: Char, p: Parser[T]): Parser[T] =
    satisfy(_ == open) <* p *> satisfy(_ == close)

  val ws: Parser[Unit] = skipManySatisfy(Character.isWhitespace)

  val jsBoolean: Parser[JsValue] =
    (string("true") <|> string("false")).map(bool => JsBoolean(bool.toBoolean))
  val jsInt: Parser[JsValue]  = int.map(i => JsInt(i))
  val jsNull: Parser[JsValue] = string("null").map(_ => JsNull)
  val jsString: Parser[JsValue] = betweenChars(
    '"',
    '"',
    manySatisfy(ch => Character.isLetterOrDigit(ch) || Character.isWhitespace(ch))
  ).map(s => JsString(s.mkString))

  val jsKey: Parser[String] =
    betweenChars('"', '"', manySatisfy(Character.isLetterOrDigit)).map(_.mkString)
  val jsKeyWs: Parser[String] = ws <* jsKey *> ws

  val jsArray: Parser[JsValue] =
    betweenChars('[', ']', sepBy(jsValueWs, satisfy(_ == ','))).map(list => JsArray(list))

  def jsValueWs: Parser[JsValue] =
    ws <* (jsInt <|> jsString <|> jsBoolean <|> jsNull <|> jsArray) *> ws

  val jsField: Parser[(String, JsValue)] = pipe2(jsKeyWs *> satisfy(_ == ':'), jsValueWs)

  val jsObject: Parser[JsValue] =
    (ws <* betweenChars('{', '}', sepBy(jsField, satisfy(_ == ','))) *> ws).map(pairs =>
      JsObject(pairs.toMap)
    )

def main(args: Array[String]): Unit =
  import JsonParser.*
  println(
    jsObject.run(
      CharSeq(
        """
        {
          "name": "John Doe",
          "drivingLicense": true,
          "age": 18,
          "tags": [1, true, "test", null]
        }
        """
      )
    )
  )
