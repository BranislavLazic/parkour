# parkour

Welcome to parkour, a Scala 3 proof-of-concept parser combinator.

## Import combinators

```scala
import Parkour.*
```

## Use them

Parse a simple integer:

```scala
int.run(TextInput("2"))

Right(ParseSuccess(2, ""))
```

Parse it surrounded with white spaces:

```scala
val ws = skipManySatisfy(Character.isWhitespace)
val intWs = (ws <* int *> ws)
intWs.run(TextInput("  2  "))

Right(ParseSuccess(2, ""))
```

Parse an arithmetic expression (use `pipe2` to keep both right-hand-side and left-hand-side values):

```scala
val addition = pipe2(intWs *> satisfy(_ == '+'), intWs)
addition.run(TextInput("2 + 3"))

Right(ParseSuccess((2,3), ""))
```

Map over the parser:

```scala
case class PlusExpression(lhs: Int, rhs: Int)
addition.map { case (lhs, rhs) => PlusExpression(lhs, rhs) }.run(TextInput("2 + 3"))

Right(ParseSuccess(PlusExpression(2,3), ""))
```

Parse repeating sequence:

```scala
many(string("12")).run(TextInput("1212121212"))

Right(ParseSuccess(List(12, 12, 12, 12, 12), ""))
```

Parse sequence of characters separated by:

```scala
sepBy(int, satisfy(_ == ',')).run(TextInput("1,2,3,4,5,6"))

Right(ParseSuccess(List(1, 2, 3, 4, 5, 6), ""))
```

Parse optionally:

```scala
val optional = opt(satisfy(_ == '+')) <* int

optional.run(TextInput("+3"))

Right(ParseSuccess(3, ""))

optional.run(TextInput("3"))

Right(ParseSuccess(3, ""))
```

## Warning!

The project is not production ready!

## Contribution policy

Contributions via GitHub pull requests are gladly accepted from their original author. Along with
any pull requests, please state that the contribution is your original work and that you license
the work to the project under the project's open source license. Whether or not you state this
explicitly, by submitting any copyrighted material via pull request, email, or other means you
agree to license the material under the project's open source license and warrant that you have the
legal authority to do so.

## License

This code is open source software licensed under the
[MIT](https://opensource.org/licenses/MIT) license.
