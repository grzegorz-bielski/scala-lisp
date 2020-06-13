package lisp

import cats.Applicative

object Parser {
  import atto._, Atto._
  import cats.implicits._
  import cats.syntax.flatMap._

  import LispVal._

  val symbol = oneOf("!#$%&|*+-/:<=>?@^_~")
  val spaces = skipMany1(spaceChar)

  val lispStr: Parser[LispVal] = for {
    _ <- char('"')
    s <- many(noneOf("\""))
    _ <- char('"')
  } yield LispStr(s.mkString)

  val lispBool: Parser[LispVal] = for {
    _ <- char('#')
    b <- char('t') | char('f')
  } yield LispBool((b == 't'))

  val lispAtom: Parser[LispVal] = for {
    first <- letter | symbol
    rest <- many(letter | digit | symbol)
  } yield {
    val atom = first +: rest

    LispStr("") // TODO
  }

  val parser = spaces ~> symbol

  def readExp(str: String): String =
    (parser parseOnly str).either match {
      case Left(reason) => s"Err: $reason"
      case Right(x)     => s"OK: $x"
    }
}
