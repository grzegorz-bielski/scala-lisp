package lisp

// import cats.mtl.syntax.ask
import cats.data.Kleisli
import cats.mtl.ApplicativeLocal

object Eval {
  import cats.implicits._
  import cats.effect.{IO}

  import LispVal._

  def basicEnv() =
    Primitives.primEnv.toMap
      .combineK(Map("read" -> LispFunc(Primitives.unaryOp(readFn))))

  def run(a: String): IO[Unit] = ???

  def runParser(input: String): LispEval[LispVal] =
    Parser.readExprFile(input).either match {
      case Left(value)  => LispEval.raiseError(new Error(value))
      case Right(value) => evalBody(value)
    }

  def runProgram[A](code: Env)(action: LispEval[A]): IO[A] =
    action.unEval(code)

  def readFn(a: LispVal): LispEval[LispVal] = ???

  def evalBody(a: LispVal): LispEval[LispVal] = ???

  def eval(a: LispVal): LispEval[LispVal] = {
    def evalList(l: List[LispVal]): LispEval[LispVal] = l match {
      case List(LispAtom("quote"), v) => v.pure[LispEval]
      case List(LispAtom("write"), v) => LispStr(v.show).of[LispEval]
      case LispAtom("write") :: rest  => LispList(rest).of[LispEval]

      case List(LispAtom("if"), pred, onTrue, onFalse) =>
        eval(pred).flatMap({
          case LispBool(true)  => eval(onTrue)
          case LispBool(false) => eval(onFalse)
          case _               => LispEval.raiseError(new Error("Could not eval the 'if'"))
        })

      case (List(LispAtom("let"), LispList(pairs), expr)) =>
        for {
          env <- ApplicativeLocal[LispEval, Env].ask

        } yield ???
    }

    def lookupAtom(atom: LispAtom): LispEval[LispVal] =
      for {
        env <- ApplicativeLocal[LispEval, Env].ask
        value <- env.get(atom.v) match {
          case Some(v) => v.of[LispEval]
          case None    => LispEval.raiseError(new Error("Unbound variable"))
        }
      } yield value

    val res = a match {
      case v @ LispNum(_)  => v.of[LispEval]
      case v @ LispStr(_)  => v.of[LispEval]
      case v @ LispBool(_) => v.of[LispEval]
      case v @ LispNil     => v.of[LispEval]
      case v @ LispAtom(_) => lookupAtom(v)
      case LispList(Nil)   => LispNil.of[LispEval]
      case LispList(l)     => evalList(l)
    }

    ???
  }
}
