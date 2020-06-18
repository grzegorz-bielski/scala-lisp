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

      forAll(cases) { (text, expected) =>
        (Parser.lispStr parseOnly text) shouldBe expected
      }
    }

    "should no successfully parse invalid strings" in {
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

      forAll(cases) { (text, expected) =>
        (Parser.lispStr parseOnly text) shouldBe expected
      }
    }
  }

}
