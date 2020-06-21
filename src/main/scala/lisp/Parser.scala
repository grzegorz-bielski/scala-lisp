package lisp

object Parser {
  import atto._, Atto._
  import scala.util.Try
  import cats.syntax.show._

  import LispVal._

  val symbol = oneOf("!#$%&|*+-/:<=>?@^_~")
  val spaces = skipMany1(spaceChar)

  val lispStr: Parser[LispVal] = for {
    s <- char('"') ~> many(noneOf("\"")) <~ char('"')
  } yield LispStr(s.mkString)

  val lispBool: Parser[LispVal] = for {
    b <- char('#') ~> (char('t') | char('f'))
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

  val lispExp: Parser[LispVal] =
    lispAtom | lispNil | lispBool | lispStr | lispNum | lispQuoted | lispSExp

  val lispSExp: Parser[LispVal] = char('(') ~> lispList <~ char(')')

  val lispList: Parser[LispVal] = for {
    ll <- many(lispExp) sepBy spaces
  } yield LispList(ll.flatten)

  val lispQuoted: Parser[LispVal] = for {
    e <- char('\'') ~> lispExp
  } yield LispList(List(LispAtom("quote"), e))

  val lispNil: Parser[LispVal] = string("Nil") map (_ => LispNil)

  def readExpToStr(str: String): String =
    (lispExp parseOnly str).either match {
      case Left(reason)  => s"Err: ${reason}"
      case Right(result) => s"Ok: ${result.show}"
    }

  def contents[A](p: Parser[A]): Parser[A] = skipWhitespace ~> p <~ endOfInput

  def readExpr(str: String): ParseResult[LispVal] =
    (contents(lispExp)) parse str

  def readExprFile(str: String): ParseResult[LispVal] =
    (contents(lispList)) parse str

}
