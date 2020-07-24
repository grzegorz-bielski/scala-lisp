package lisp

import io.estatico.newtype.macros.newtype
import cats.data.Kleisli
import cats.data.StateT
import cats.{Monad, Functor, Applicative, Show}
import cats.implicits._
import cats.effect.{LiftIO, IO, Effect, Async}
import cats.mtl.{ApplicativeLocal, DefaultApplicativeLocal, ApplicativeAsk}
import cats.Foldable

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
  case class LispFunc(v: Fn) extends LispVal
  case class LispLambda(v: Fn, e: Env) extends LispVal
  case object LispNil extends LispVal
  case class LispBool(v: Boolean) extends LispVal

  implicit val s: Show[LispVal] = Show.show {
    case LispAtom(v)      => v
    case LispList(v)      => s"(${v map Show[LispVal].show mkString " "})"
    case LispNum(v)       => Show[Int].show(v)
    case LispFunc(v)      => "(internal function)"
    case LispLambda(v, c) => "(lambda function)"
    case LispStr(v)       => s"""\"$v\""""
    case LispNil          => "Nil"
    case LispBool(v)      => if (v) "#t" else "#f"
  }

  type Env = Map[String, LispVal]
  type Fn = List[LispVal] => LispEval[LispVal]

  @newtype
  sealed case class LispEval[A](unEval: Kleisli[IO, Env, A])
  object LispEval {
    implicit val f: Functor[LispEval] = derivingK
    implicit val a: Applicative[LispEval] = derivingK
    implicit val m: Monad[LispEval] = derivingK

    // `Effect` doesn't provide instance for `Kleisli`, so we are sticking with only `Async`
    implicit val ea: Async[LispEval] = derivingK

    // I'm not sure how to automatically derive `ApplicativeLocal`
    implicit val al: ApplicativeLocal[LispEval, Env] =
      new DefaultApplicativeLocal[LispEval, Env]() {
        val applicative: Applicative[LispEval] = Applicative[LispEval]
        def ask: LispEval[Env] = LispEval(Kleisli.ask[IO, Env])
        def local[A](f: Env => Env)(fa: LispEval[A]): LispEval[A] =
          LispEval(Kleisli.local(f)(fa.unEval))
      }

    def putStrLn(a: String) = LiftIO[LispEval].liftIO(IoOps.putStrLn(a))
  }

  sealed trait LispError extends Throwable {
    def raise[A](): LispEval[A] = LiftIO[LispEval].liftIO(IO.raiseError(this))
  }
  object LispError {
    case class IncorrectArgsNum(n: Int, v: List[LispVal]) extends LispError
    case class IncorrectListLength(n: Int, a: String) extends LispError
    case class ExpectedList(a: String) extends LispError
    case class IncorrectType(a: String) extends LispError
    case class IncorrectSpecialForm(a: String) extends LispError
    case class NotAFunction(v: LispVal) extends LispError
    case class VariableNotInScope(a: LispVal) extends LispError
    case class CouldNotParse(a: String) extends LispError
    case class IOError(a: String) extends LispError
    case class UnrecognizedError(a: String) extends LispError

    implicit val s: Show[LispError] = Show.show {
      case IncorrectArgsNum(n, v) =>
        s"Error: Incorrect number of arguments, expected: ${n}, received: ${v.length}"
      case IncorrectListLength(n, a) => s"Error: The List in ${a} has a length of: ${n}"
      case ExpectedList(a)           => s"Error: Expected List in :${a}"
      case IncorrectType(a)          => s"Error: Mismatched type, ${a}"
      case IncorrectSpecialForm(a)   => s"Error: The special form is not correct: ${a}"
      case NotAFunction(v)           => s"Error: Expected a function, got: ${v.show}"
      case VariableNotInScope(v)     => s"Error: Variable '${v.show}' is not in scope"
      case CouldNotParse(a)          => s"Error: Couldn't parse the ${a}"
      case IOError(a)                => s"Error: I/O exception while reading the ${a}"
      case UnrecognizedError(a)      => s"Error: Encountered unrecognized error: ${a}"
    }
  }
}
