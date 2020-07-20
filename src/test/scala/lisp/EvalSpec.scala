package lisp

import org.scalatest.FreeSpec
import org.scalatest._
import org.scalatest.prop.TableDrivenPropertyChecks
import cats.effect.IO
import scala.util.Try
import scala.util.Failure
import scala.util.Success

class EvalSpec extends FreeSpec with TableDrivenPropertyChecks with Matchers {
  import LispVal._
  import cats.implicits._

  val emptyEnv: Env = Map()

  "eval identity" - {
    "should return the input" in {
      val cases = List(
        LispNum(1),
        LispStr("abc"),
        LispBool(true),
        LispNil
      )

      cases.foreach { lv => Eval.eval(lv).unEval(emptyEnv).unsafeRunSync shouldBe lv }
    }
  }

  "eval quote" - {
    "should successfully evaluate a valid quote" in {
      val cases = Table(
        ("lisp val", "expected result"),
        (
          LispList(List(LispAtom("quote"), LispStr("kek"))),
          LispStr("kek")
        ),
        (
          LispList(List(LispAtom("quote"), LispList(List(LispNum(1))))),
          LispList(List(LispNum(1)))
        )
      )

      forAll(cases) { (lv, expected) =>
        Eval.eval(lv).unEval(emptyEnv).unsafeRunSync shouldBe expected
      }
    }
  }

  "eval atom" - {
    val scope: Env = Map(
      "xd" -> LispNum(1),
      "todos" -> LispList(List(LispStr("milk"), LispStr("cornflakes")))
    )

    "should successfully evaluate valid lisp atoms defined in the environment" in {
      val cases = Table(
        ("lisp val", "expected result"),
        (
          LispAtom("xd"),
          scope.get("xd").get
        ),
        (
          LispAtom("todos"),
          scope.get("todos").get
        )
      )

      forAll(cases) { (lv, expected) =>
        Eval.eval(lv).unEval(scope).unsafeRunSync shouldBe expected
      }
    }

    "should not successfully evaluate valid lisp atoms not defined in the environment" in {
      val cases = Table(
        ("lisp val", "expected result"),
        (
          LispAtom("xd"),
          scope.get("xd").get
        ),
        (
          LispAtom("todos"),
          scope.get("todos").get
        ),
        (
          LispAtom("whatever"),
          LispNil
        )
      )

      forAll(cases) { (lv, expected) =>
        val program = Eval.eval(lv).unEval(emptyEnv)

        an[LispVal.LispError.VariableNotInScope] should be thrownBy program.unsafeRunSync
      }
    }
  }

  "eval write" - {
    "should successfully evaluate a valid write syntax" in {
      val cases = Table(
        ("lisp val", "expected result"),
        (
          LispList(List(LispAtom("write"), LispStr("kek"))),
          LispStr(raw""""kek"""")
        ),
        (
          LispList(List(LispAtom("write"), LispList(List(LispNum(1), LispNum(2))))),
          LispStr("(1 2)")
        ),
        (
          LispList(
            List(
              LispAtom("write"),
              LispList(
                List(
                  LispAtom("define"),
                  LispAtom("id"),
                  LispList(
                    List(LispAtom("lambda"), LispList(List(LispAtom("obj"))), LispAtom("obj"))
                  )
                )
              )
            )
          ),
          LispStr("(define id (lambda (obj) obj))")
        )
      )

      forAll(cases) { (lv, expected) =>
        Eval.eval(lv).unEval(emptyEnv).unsafeRunSync shouldBe expected
      }
    }
  }

  "eval if" - {
    "should successfully evaluate a valid if expression" in {
      val cases = Table(
        ("lisp val", "expected result")
        // (
        //   LispList(List(LispAtom("write"), LispStr("kek"))),
        //   LispStr(raw""""kek"""")
        // ),
        // (
        //   LispList(List(LispAtom("write"), LispList(List(LispNum(1), LispNum(2))))),
        //   LispStr("(1 2)")
        // ),
        // (
        //   LispList(
        //     List(
        //       LispAtom("write"),
        //       LispList(
        //         List(
        //           LispAtom("define"),
        //           LispAtom("id"),
        //           LispList(
        //             List(LispAtom("lambda"), LispList(List(LispAtom("obj"))), LispAtom("obj"))
        //           )
        //         )
        //       )
        //     )
        //   ),
        //   LispStr("(define id (lambda (obj) obj))")
        // )
      )

      forAll(cases) { (lv, expected) =>
        Eval.eval(lv).unEval(emptyEnv).unsafeRunSync shouldBe expected
      }
    }
  }
}
