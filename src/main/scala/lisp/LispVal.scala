package lisp

import io.estatico.newtype.macros.newtype

object LispVal {
  import cats.data.Kleisli
  import scala.annotation.tailrec
  import cats.{Monad, Functor, Applicative, FlatMap, Show}
  import cats.instances.int
  import cats.implicits._
  import cats.syntax.functor._
  import cats.syntax.flatMap._
  import cats.mtl.{ApplicativeAsk, ApplicativeLocal}
  import cats.effect.{LiftIO, IO}

  sealed trait LispVal
  case class LispAtom(v: String) extends LispVal
  case class LispList(v: List[LispVal]) extends LispVal
  case class LispNum(v: Int) extends LispVal
  case class LispStr(v: String) extends LispVal
  case class LispFunc(v: Func) extends LispVal
  case class LispLambda(v: Func, c: Env) extends LispVal
  case object LispNil extends LispVal
  case class LispBool(v: Boolean) extends LispVal

  object LispVal {
    implicit val s: Show[LispVal] = Show.show({
      case LispAtom(v)      => v
      case LispList(v)      => s"( ${v.map(Show[LispVal].show).mkString} )"
      case LispNum(v)       => Show[Int].show(v)
      case LispFunc(v)      => "(internal function)"
      case LispLambda(v, c) => "(lambda function)"
      case LispStr(v)       => s"""\"$v\""""
      case LispNil          => "Nil"
      case LispBool(v)      => if (v) "#t" else "#f"
    })
  }

  type Env = Map[String, LispVal]
  type Func = List[LispVal] => LispEval[LispVal]
  type Effect[A] = IO[A]

  @newtype case class LispEval[A](val unEval: Kleisli[Effect, Env, A])
  object LispEval {
    implicit val f: Functor[LispEval] = derivingK
    implicit val a: Applicative[LispEval] = derivingK
    implicit val m: Monad[LispEval] = derivingK
    implicit val lio: LiftIO[LispEval] = derivingK
    // don't know how to derive `ApplicativeLocal` :/
  }
}
