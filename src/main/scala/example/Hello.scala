package example

object Hello extends Greeting with App {
  println(greeting)
}

trait Greeting {
  lazy val greeting: String = "hello"
}

object Lisp {
  import cats.data.Kleisli
  import scala.annotation.tailrec
  import cats.{Monad, Functor, Applicative, FlatMap}
  import cats.syntax.functor._
  import cats.syntax.flatMap._
  import cats.effect.IO

  type EnvCtx = Map[String, LispVal]
  type Func = List[LispVal] => LispEval[LispVal]

  sealed trait LispVal
  case class LispAtom(v: String) extends LispVal
  case class LispList(v: List[LispVal]) extends LispVal
  case class LispNum(v: Int) extends LispVal
  case class LispStr(v: String) extends LispVal
  case class LispFunc(v: Func) extends LispVal
  case class LispLambda(v: Func, c: EnvCtx) extends LispVal
  case object ListpNil extends LispVal
  case class LispBool(v: Boolean) extends LispVal

  case class LispEval[A](fn: EnvCtx => IO[A]) {
    def apply(): Kleisli[IO, EnvCtx, A] = Kleisli(fn)
  }

  object LispEval {
    implicit val mle: Monad[LispEval] = new Monad[LispEval] {
      override def pure[A](x: A): LispEval[A] = LispEval((a: EnvCtx) => IO(x))
      override def flatMap[A, B](fa: LispEval[A])(
          f: A => LispEval[B]
      ): LispEval[B] = LispEval(fa().flatMap(f(_)()).run)

      // @tailrec - not sure how make it tailrec-safe :/
      override def tailRecM[B, C](b: B)(
          f: B => LispEval[Either[B, C]]
      ): LispEval[C] =
        LispEval((a) => implicitly[FlatMap[IO]].tailRecM(b) { f(_)().run(a) })
    }

    implicit val fle: Functor[LispEval] = new Functor[LispEval] {
      override def map[A, B](fa: LispEval[A])(f: A => B): LispEval[B] =
        fa.flatMap(a => implicitly[Monad[LispEval]].pure(f(a)))

      // implicit val fla:

    }
  }

  val x = LispEval((a) => IO("")) >> LispEval(a => IO("aa"))
}
