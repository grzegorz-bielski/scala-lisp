package lisp

import cats.data.Kleisli
import cats.mtl.ApplicativeLocal

object Eval {
  import cats.implicits._
  import cats.effect.IO
  import LispVal._

  private lazy val AL = ApplicativeLocal[LispEval, Env]

  def basicEnv() =
    Primitives.primEnv.toMap combineK Map("read" -> LispFunc(Primitives.unaryOp(readFn)))

  def run(a: String): IO[Unit] = ???

  def runParser(input: String): LispEval[LispVal] = Parser.readExprFile(input).either match {
    case Left(a)  => LispError.UnrecognizedError(a).raise
    case Right(v) => evalBody(v)
  }

  def runProgram[A](code: Env)(action: LispEval[A]): IO[A] = action.unEval(code)

  private def readFn(a: LispVal): LispEval[LispVal] = ???

  def eval(a: LispVal): LispEval[LispVal] = a match {
    case v @ LispNum(_)                                             => v.of[LispEval]
    case v @ LispStr(_)                                             => v.of[LispEval]
    case v @ LispBool(_)                                            => v.of[LispEval]
    case v @ LispNil                                                => v.of[LispEval]
    case v @ LispAtom(_)                                            => evalAtom(v)
    case LispList(Nil)                                              => LispNil.of[LispEval]
    case LispList(List(LispAtom("quote"), v))                       => v.pure[LispEval]
    case LispList(List(LispAtom("write"), v))                       => LispStr(v.show).of[LispEval]
    case LispList(LispAtom("write") :: rest)                        => LispList(rest).of[LispEval]
    case LispList(List(LispAtom("if"), pred, onT, onF))             => evalIf(pred)(onT)(onF)
    case LispList(List(LispAtom("let"), LispList(pairs), expr))     => evalLet(pairs)(expr)
    case LispList(List(LispAtom("begin"), v))                       => evalBody(v)
    case LispList(LispAtom("begin") :: rest)                        => evalBody(LispList(rest))
    case LispList(List(LispAtom("define"), varExp, expr))           => evalDefine(varExp)(expr)
    case LispList(List(LispAtom("lambda"), LispList(params), expr)) => evalLambda(params)(expr)
    case LispList(x :: xs)                                          => evalApplication(x)(xs)
  }

  def evalBody(a: LispVal): LispEval[LispVal] = a match {
    case LispList(List(LispList(LispAtom("define") :: List(LispAtom(v), expr)), rest)) =>
      for {
        env <- AL.ask
        evaled <- eval(expr)

        envPrim = env + (v -> evaled)
        modEnv = (_: Env) => envPrim

        v <- AL.local(modEnv)(eval(rest))
      } yield v

    case LispList(LispList(LispAtom("define") :: List(LispAtom(v), expr)) :: rest) =>
      for {
        env <- AL.ask
        evaled <- eval(expr)

        envPrim = env + (v -> evaled)
        modEnv = (_: Env) => envPrim

        v <- AL.local(modEnv)(evalBody(LispList(rest)))
      } yield v

    case v => eval(v)
  }

  def evalAtom(atom: LispAtom): LispEval[LispVal] =
    for {
      env <- AL.ask
      value <- env.get(atom.v) match {
        case Some(v) => v.of[LispEval]
        case None    => LispError.VariableNotInScope(atom).raise
      }
    } yield value

  def evalIf(pred: LispVal)(onT: LispVal)(onF: LispVal): LispEval[LispVal] =
    eval(pred).flatMap({
      case LispBool(true)  => eval(onT)
      case LispBool(false) => eval(onF)
      case _               => LispError.IncorrectSpecialForm("if").raise
    })

  def evalLet(pairs: List[LispVal])(expr: LispVal): LispEval[LispVal] = {
    def evenSide[A](v: List[A]): List[A] = v match {
      case Nil     => Nil
      case x :: xs => x :: oddSide(xs)
    }

    def oddSide[A](v: List[A]): List[A] = v match {
      case Nil     => Nil
      case x :: xs => evenSide(xs)
    }

    applyLambda(expr)(evenSide(pairs))(oddSide(pairs))
  }

  def evalDefine(varExpr: LispVal)(expr: LispVal): LispEval[LispVal] =
    for {
      env <- AL.ask
      atom <- extractAtom(varExpr)
      evaled <- eval(expr)

      envPrim = env + (atom.v -> evaled)
      modEnv = (_: Env) => envPrim

      v <- AL.local(modEnv)(varExpr.of[LispEval])
    } yield v

  def evalLambda(params: List[LispVal])(expr: LispVal): LispEval[LispVal] =
    AL.ask.map(LispLambda(applyLambda(expr)(params) _, _))

  def evalApplication(x: LispVal)(xs: List[LispVal]): LispEval[LispVal] =
    for {
      fn <- eval(x)
      arg <- xs traverse eval
      r <- fn match {
        case LispFunc(f)      => f(arg)
        case LispLambda(f, e) => AL.local(_ => e)(f(arg))
        case v                => LispError.NotAFunction(v).raise
      }
    } yield r

  def extractAtom(v: LispVal): LispEval[LispAtom] = v match {
    case v @ LispAtom(_) => v.pure[LispEval]
    case e               => LispError.VariableNotInScope(e).raise
  }

  def applyLambda(expr: LispVal)(params: List[LispVal])(args: List[LispVal]) =
    for {
      env <- AL.ask
      atoms <- params traverse extractAtom
      evaled <- args traverse eval

      nextPairs = (atoms zip evaled) map { case (LispAtom(v), b) => (v, b) }
      envPrim = nextPairs.toMap combineK env
      modEnv = (_: Env) => envPrim

      v <- AL.local(modEnv)(evalBody(expr))
    } yield v
}
