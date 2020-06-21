package lisp

import io.estatico.newtype.macros.newtype
import cats.data.Kleisli
import cats.{Monad, Functor, Applicative, Show}
import cats.implicits._
import cats.effect.{LiftIO, IO, Effect, Async}

sealed trait LispVal {
  // `A` type on the `LispEval` cannot be covariant
  // so I'm just casting it 🤷‍♂️
  def base: LispVal = this

  def of[F[_]: Applicative] = base.pure[F]
}
object LispVal {

  case class LispAtom(v: String) extends LispVal
  case class LispList(v: List[LispVal]) extends LispVal
  case class LispNum(v: Int) extends LispVal
  case class LispStr(v: String) extends LispVal
  case class LispFunc(v: Func) extends LispVal
  case class LispLambda(v: Func, c: Env) extends LispVal
  case object LispNil extends LispVal
  case class LispBool(v: Boolean) extends LispVal

  implicit val s: Show[LispVal] = Show.show({
    case LispAtom(v)      => v
    case LispList(v)      => s"(${v.map(Show[LispVal].show).mkString(" ")})"
    case LispNum(v)       => Show[Int].show(v)
    case LispFunc(v)      => "(internal function)"
    case LispLambda(v, c) => "(lambda function)"
    case LispStr(v)       => s"""\"$v\""""
    case LispNil          => "Nil"
    case LispBool(v)      => if (v) "#t" else "#f"
  })

  type Env = Map[String, LispVal]
  type Func = List[LispVal] => LispEval[LispVal]
  type LispError = Throwable

  @newtype
  sealed case class LispEval[A](unEval: Kleisli[IO, Env, A])
  object LispEval {
    // I'm not sure how to derive `ApplicativeLocal`
    // and `Effect` doesn't provide instance for `Kleisli`

    implicit val f: Functor[LispEval] = derivingK
    implicit val a: Applicative[LispEval] = derivingK
    implicit val m: Monad[LispEval] = derivingK
    implicit val ea: Async[LispEval] = derivingK

    def raiseError[A](e: Throwable): LispEval[A] =
      LiftIO[LispEval].liftIO(IO.raiseError(e))
  }

}
