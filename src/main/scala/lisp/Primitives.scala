package lisp

import lisp.LispVal._
import cats.effect.{IO, LiftIO}

object Primitives {
  type Prim = List[(String, LispVal)]
  type Unary = LispVal => LispEval[LispVal]
  type Binary = LispVal => LispVal => LispEval[LispVal]

  val primEnv: Prim = List()

  def unaryOp(op: Unary)(vals: List[LispVal]): LispEval[LispVal] =
    vals match {
      case List(x) => op(x)
      case args =>
        LispEval.raiseError(
          new IllegalArgumentException("Need at least one arg")
        )
    }

}
