package lisp

object Parser {
  import atto._, Atto._
  import cats.instances.all._
  import scala.util.Try
  import cats.implicits._

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
    val atom = (first +: rest).mkString
    (lispBool parseOnly atom).either getOrElse LispAtom(atom)
  }

  val lispNum: Parser[LispVal] = for {
    s <- many1(digit) map (_.toList.mkString)
    d <- Try(s.toInt).toEither match {
      case Left(e)  => err(e.toString)
      case Right(a) => ok(a)
    }
  } yield LispNum(d)

  // val parser = spaces ~> symbol

  val parser: Parser[LispVal] = lispAtom | lispBool | lispStr | lispNum

  def readExp(str: String): String =
    (parser parseOnly str).either match {
      case Left(reason) => s"Err: $reason"
      case Right(x)     => s"OK: $x"
    }
}
