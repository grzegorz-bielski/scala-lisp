package lisp

import lisp.LispVal._
import cats.effect.{IO, LiftIO}

object Primitives {
  type Prim = List[(String, LispVal)]
  type Unary = LispVal => LispEval[LispVal]
  type Binary = LispVal => LispVal => LispEval[LispVal]

  val primEnv: Prim = List(
    )

  def unaryOp(op: Unary)(vs: List[LispVal]): LispEval[LispVal] = vs match {
    case List(x) => op(x)
    case args    => LispError.IncorrectArgsNum(1, args).raise
  }

}
