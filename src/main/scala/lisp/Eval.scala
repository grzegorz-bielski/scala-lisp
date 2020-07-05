package lisp

import cats.data.Kleisli
import cats.mtl.ApplicativeLocal
import cats.implicits._
import cats.effect.IO
import cats.Show

object Eval {
  import LispVal._

  private lazy val AL = ApplicativeLocal[LispEval, Env]

  type LEval = LispEval[LispVal]
  type LList = List[LispVal]

  def run(expr: String): IO[Unit] =
    for {
      stdLib <- IoOps.readFile("std/lib/scm")
      evalStr = parseWithStdLib(stdLib)(expr) fold ((_.raise), evalBody)
      toRun = evalStr.unEval(basicEnv) // can't do it in one line for some reason (type erasure?)
      r <- toRun map (_.show)
      _ <- IoOps.putStrLn(r)
    } yield ()

  def basicEnv() =
    Primitives.primEnv.toMap combineK Map("read" -> LispFunc(Primitives.unaryOp(readFn)))

  def parseWithStdLib(lib: String)(expr: String): Either[LispError, LispVal] = {
    def incorrectType[A: Show](n: A) = LispError.IncorrectType(s"Failed to get variable: ${n.show}")

    val parsed = for {
      l <- Parser.readExprFile(lib).either
      e <- Parser.readExpr(expr).either
    } yield (l, e)

    parsed
      .leftMap(incorrectType(_))
      .flatMap {
        case (LispList(v), expr) => Right(LispList(v |+| List(expr)))
        case (n, _)              => Left(incorrectType(n))
      }
  }

  def runParserForASTPreview(str: String): String = Parser.readExpr(str).either foldMap (_.show)

  def readFn(a: LispVal): LEval = ???
  def parseFn(a: LispVal): LEval = a match {
    case LispStr(str) =>
      Parser.readExpr(str).either fold (LispError.CouldNotParse(_).raise, _.of[LispEval])
    case _ => LispError.IncorrectType(s"Expected string").raise
  }

  def eval(a: LispVal): LEval = a match {
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
    case LispList(List(LispAtom("cdr"), LispList(List(LispAtom("quote"), LispList(x :: xs))))) =>
      LispList(xs).of[LispEval]
    case LispList(List(LispAtom("cdr"), arg @ LispList(x :: xs))) => evalCdrComposition(x)(xs)(arg)
    case LispList(List(LispAtom("car"), LispList(List(LispAtom("quote"), LispList(x :: xs))))) =>
      LispList(xs).of[LispEval]
    case LispList(List(LispAtom("car"), arg @ LispList(x :: xs))) => evalCarComposition(x)(xs)(arg)

    case LispList(x :: xs) => evalApplication(x)(xs)
  }

  def evalCarComposition(x: LispVal)(xs: List[LispVal])(arg: LispList): LispEval[LispVal] =
    x match {
      case LispAtom(_) => eval(arg) flatMap (a => eval(LispList(List(LispAtom("car"), a))))
      case _           => LispList(xs).of[LispEval]
    }

  def evalCdrComposition(x: LispVal)(xs: List[LispVal])(arg: LispList): LispEval[LispVal] =
    x match {
      case LispAtom(_) => eval(arg) flatMap (a => eval(LispList(List(LispAtom("cdr"), a))))
      case _           => x.of[LispEval]
    }

  def evalBody(a: LispVal): LEval = a match {
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

  def evalAtom(atom: LispAtom): LEval =
    for {
      env <- AL.ask
      value <- env.get(atom.v) match {
        case Some(v) => v.of[LispEval]
        case None    => LispError.VariableNotInScope(atom).raise
      }
    } yield value

  def evalIf(pred: LispVal)(onT: LispVal)(onF: LispVal): LEval =
    eval(pred).flatMap({
      case LispBool(true)  => eval(onT)
      case LispBool(false) => eval(onF)
      case _               => LispError.IncorrectSpecialForm("if").raise
    })

  def evalLet(pairs: LList)(expr: LispVal): LEval = {
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

  def evalDefine(varExpr: LispVal)(expr: LispVal): LEval =
    for {
      env <- AL.ask
      atom <- extractAtom(varExpr)
      evaled <- eval(expr)

      envPrim = env + (atom.v -> evaled)
      modEnv = (_: Env) => envPrim

      v <- AL.local(modEnv)(varExpr.of[LispEval])
    } yield v

  def evalLambda(params: LList)(expr: LispVal): LEval =
    AL.ask.map(LispLambda(applyLambda(expr)(params) _, _))

  def evalApplication(x: LispVal)(xs: LList): LEval =
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

  def applyLambda(expr: LispVal)(params: LList)(args: LList) =
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
