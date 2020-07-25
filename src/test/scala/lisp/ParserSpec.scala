package lisp

import atto.{Parser => AttoParser, _}, Atto._
import org.scalatest.FreeSpec
import org.scalatest._
import org.scalatest.prop.TableDrivenPropertyChecks

import LispVal._

class ParserSpec extends FreeSpec with TableDrivenPropertyChecks with Matchers {
  "string parser" - {
    "should successfully parse valid strings" in {
      val cases = Table(
        ("text", "expected result"),
        (raw""""a"""", ParseResult.Done("", LispStr("a"))),
        (raw""""abc"""", ParseResult.Done("", LispStr("abc"))),
        (raw"""""""", ParseResult.Done("", LispStr("")))
      )

      forAll(cases) { (text, expected) => (Parser.lispStr parseOnly text) shouldBe expected }
    }

    "should not successfully parse invalid strings" in {
      val cases = Table(
        ("text", "expected result"),
        (
          raw"""a""",
          ParseResult.Fail("a", List(), raw"""Failure reading:'"'""")
        ),
        (
          raw""""""",
          ParseResult.Fail("", List(), raw"""Failure reading:'"'""")
        ),
        (
          raw"""ad""",
          ParseResult.Fail("ad", List(), raw"""Failure reading:'"'""")
        )
      )

      forAll(cases) { (text, expected) => (Parser.lispStr parseOnly text) shouldBe expected }
    }
  }

  "bool parser" - {
    "should successfully parse valid booleans" in {
      val cases = Table(
        ("text", "expected result"),
        (raw"""#t""", ParseResult.Done("", LispBool(true))),
        (raw"""#f""", ParseResult.Done("", LispBool(false)))
      )

      forAll(cases) { (text, expected) => (Parser.lispBool parseOnly text) shouldBe expected }
    }
  }

  "num parser" - {
    "should successfully parse valid numbers" in {
      val cases = Table(
        ("text", "expected result"),
        (raw"""1""", ParseResult.Done("", LispNum(1))),
        (raw"""2""", ParseResult.Done("", LispNum(2))),
        (raw"""0""", ParseResult.Done("", LispNum(0))),
        (raw"""-5""", ParseResult.Done("", LispNum((-5)))),
        (raw"""10""", ParseResult.Done("", LispNum(10)))
      )

      forAll(cases) { (text, expected) => (Parser.lispNum parseOnly text) shouldBe expected }
    }
  }

  "atom parser" - {
    "should successfully parse valid atoms" in {
      val cases = Table(
        ("text", "expected result"),
        (raw"""define""", ParseResult.Done("", LispAtom("define"))),
        (raw"""begin""", ParseResult.Done("", LispAtom("begin"))),
        (raw"""xd""", ParseResult.Done("", LispAtom("xd")))
      )

      forAll(cases) { (text, expected) => (Parser.lispAtom parseOnly text) shouldBe expected }
    }
  }

  "quoted parser" - {
    "should successfully parse quoted exp" in {
      val cases = Table(
        ("text", "expected result"),
        ("\'x", ParseResult.Done("", LispList(List(LispAtom("quote"), LispAtom("x")))))
      )

      forAll(cases) { (text, expected) => (Parser.lispQuoted parseOnly text) shouldBe expected }
    }
  }

  "list parser" - {
    "should successfully parse valid lists" in {
      val cases = Table(
        ("text", "expected result"),
        (raw"""""", ParseResult.Done("", LispList(List()))),
        (raw"""1""", ParseResult.Done("", LispList(List(LispNum(1))))),
        (raw"""()""", ParseResult.Done("", LispList(List(LispList(List()))))),
        (
          raw"""(+ 1 2)""",
          ParseResult
            .Done("", LispList(List(LispList(List(LispAtom("+"), LispNum(1), LispNum(2))))))
        ),
        (
          raw"""(define compose (lambda (f g) (lambda (arg) (f (g arg)))))""",
          ParseResult
            .Done(
              "",
              LispList(
                List(
                  LispList(
                    List(
                      LispAtom("define"),
                      LispAtom("compose"),
                      LispList(
                        List(
                          LispAtom("lambda"),
                          LispList(List(LispAtom("f"), LispAtom("g"))),
                          LispList(
                            List(
                              LispAtom("lambda"),
                              LispList(List(LispAtom("arg"))),
                              LispList(
                                List(LispAtom("f"), LispList(List(LispAtom("g"), LispAtom("arg"))))
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
        )
      )

      forAll(cases) { (text, expected) => (Parser.lispList parseOnly text) shouldBe expected }
    }
  }

  "readExpr parser" - {
    "should successfully parse lisp exp" in {
      val cases = Table(
        ("text", "expected result"),
        (raw"""  () """, ParseResult.Done("", LispList(List()))),
        (raw"""
              1
            """, ParseResult.Done("", LispNum(1)))
      )

      forAll(cases) { (text, expected) => Parser.readExpr(text) shouldBe expected }
    }
  }

  "comments parser" - {
    "should successfully parse lisp comment" in {
      val cases = Table(
        ("text", "expected result"),
        (raw"""
            ;; sth is written here
            """, ParseResult.Done("            ", ()))
      )

      forAll(cases) { (text, expected) => Parser.comment parseOnly text shouldBe expected }
    }
  }

  "readExprFile parser" - {
    "should successfully parse lisp exp file" in {
      val cases = Table(
        ("text", "expected result"),
        (
          """|
             |(define id (lambda (obj) obj))
             |(define compose (lambda (f g) (lambda (arg) (f (g arg)))))
             |""".stripMargin,
          ParseResult.Done(
            "",
            LispList(
              List(
                LispList(
                  List(
                    LispAtom("define"),
                    LispAtom("id"),
                    LispList(
                      List(LispAtom("lambda"), LispList(List(LispAtom("obj"))), LispAtom("obj"))
                    )
                  )
                ),
                LispList(
                  List(
                    LispAtom("define"),
                    LispAtom("compose"),
                    LispList(
                      List(
                        LispAtom("lambda"),
                        LispList(List(LispAtom("f"), LispAtom("g"))),
                        LispList(
                          List(
                            LispAtom("lambda"),
                            LispList(List(LispAtom("arg"))),
                            LispList(
                              List(LispAtom("f"), LispList(List(LispAtom("g"), LispAtom("arg"))))
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        ),
        ("""|;; 2
            |1
            |""".stripMargin, ParseResult.Done("", LispList(List(LispNum(1))))),
        ("""|1
            |;; 2
            |""".stripMargin, ParseResult.Done("", LispList(List(LispNum(1))))),
        (
          """|(define xd 4)
             |;; a
             |;; b
             |(define xdd 4)
             |""".stripMargin,
          ParseResult.Done(
            "",
            LispList(
              List(
                LispList(List(LispAtom("define"), LispAtom("xd"), LispNum(4))),
                LispList(List(LispAtom("define"), LispAtom("xdd"), LispNum(4)))
              )
            )
          )
        )
      )

      forAll(cases) { (text, expected) => Parser.readExprFile(text) shouldBe expected }
    }
  }
}
