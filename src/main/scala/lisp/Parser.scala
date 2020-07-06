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

  val lispNum: Parser[LispVal] = for {
    sign <- opt(char('-'))
    s <- many1(digit) map (_.toList.mkString)
    d <- Try(s.toInt).toEither fold (e => err(e.toString), ok(_))
    n = sign map (_ => d * -1) getOrElse d
  } yield LispNum(n)

  val lispAtom: Parser[LispVal] = for {
    first <- letter | symbol
    rest <- many(letter | digit | symbol)
  } yield {
    val atom = (first +: rest).mkString
    (lispBool parseOnly atom).either getOrElse LispAtom(atom)
  }

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

  def contents[A](p: Parser[A]): Parser[A] = skipWhitespace ~> p <~ endOfInput
  // def contents[A](p: Parser[A]): Parser[A] = skipWhitespace ~> p <~ skipWhitespace

  def readExpr(str: String): ParseResult[LispVal] =
    (contents(lispExp)) parse str

  def readExprFile(str: String): ParseResult[LispVal] =
    (contents(lispList)) parse str

}
