package lisp

import lisp.LispVal._
import cats.effect.{IO, LiftIO}
import cats.implicits._
import cats.Foldable

object Primitives {
  type Prim = List[(String, LispVal)]
  type Unary = LispVal => LispEval[LispVal]
  type Binary = LispVal => LispVal => LispEval[LispVal]

  val primEnv: Prim = List(
    ("+", LispFunc(binaryOpFold(numericOp(_ + _))(LispNum(0)))),
    ("*", LispFunc(binaryOpFold(numericOp(_ * _))(LispNum(1)))),
    ("<>", LispFunc(binaryOpFold(stringOp(_ |+| _))(LispStr("")))),
    ("-", LispFunc(binaryOp(numericOp(_ - _)))),
    ("<", LispFunc(binaryOp(numericCmp(_ < _)))),
    ("<=", LispFunc(binaryOp(numericCmp(_ <= _)))),
    (">", LispFunc(binaryOp(numericCmp(_ > _)))),
    (">=", LispFunc(binaryOp(numericCmp(_ >= _)))),
    ("==", LispFunc(binaryOp(numericCmp(_ == _)))),
    ("even?", LispFunc(unaryOp(numericBool(_ % 2 == 0)))),
    ("odd?", LispFunc(unaryOp(numericBool(a => !(a % 2 == 0))))),
    ("neg?", LispFunc(unaryOp(numericBool(_ < 0)))),
    ("pos?", LispFunc(unaryOp(numericBool(_ > 0)))),
    ("eq?", LispFunc(binaryOp(eqCmd))),
    ("bl-eq?", LispFunc(binaryOp(eqOp(_ == _)))),
    ("and", LispFunc(binaryOpFold(eqOp(_ && _))(LispBool(true)))),
    ("or", LispFunc(binaryOpFold(eqOp(_ || _))(LispBool(true))))
  )

  def unaryOp(op: Unary)(vs: List[LispVal]): LispEval[LispVal] = vs match {
    case List(x) => op(x)
    case args    => LispError.IncorrectArgsNum(1, args).raise
  }

  def binaryOp(op: Binary)(vs: List[LispVal]): LispEval[LispVal] = vs match {
    case List(x, y) => op(x)(y)
    case args       => LispError.IncorrectArgsNum(2, args).raise
  }

  def numericOp(op: (Int, Int) => Int)(a: LispVal)(b: LispVal): LispEval[LispVal] = (a, b) match {
    case (LispNum(x), LispNum(y)) => LispNum(op(x, y)).of[LispEval]
    case (LispNil, LispNum(y))    => LispNum(y).of[LispEval]
    case (LispNum(x), LispNil)    => LispNum(x).of[LispEval]
    case (x, y)                   => LispError.IncorrectType(s"numeric op ${x.show}, ${y.show}").raise
  }

  def eqOp(op: (Boolean, Boolean) => Boolean)(a: LispVal)(b: LispVal): LispEval[LispVal] =
    (a, b) match {
      case (LispBool(x), LispBool(y)) => LispBool(op(x, y)).of[LispEval]
      case (x, y)                     => LispError.IncorrectType(s"bool op ${x.show}, ${y.show}").raise
    }

  def eqCmd(a: LispVal)(b: LispVal): LispEval[LispVal] = (a, b) match {
    case (LispAtom(x), LispAtom(y)) => LispBool(x == y).of[LispEval]
    case (LispNum(x), LispNum(y))   => LispBool(x == y).of[LispEval]
    case (LispBool(x), LispBool(y)) => LispBool(x == y).of[LispEval]
    case (LispStr(x), LispStr(y))   => LispBool(x == y).of[LispEval]
    case (LispNil, LispNil)         => LispBool(true).of[LispEval]
    case _                          => LispBool(false).of[LispEval]
  }

  def numericCmp(op: (Int, Int) => Boolean)(a: LispVal)(b: LispVal): LispEval[LispVal] =
    (a, b) match {
      case (LispNum(x), LispNum(y)) => LispBool(op(x, y)).of[LispEval]
      case (x, y)                   => LispError.IncorrectType(s"string op ${x.show} ${y.show}").raise
    }

  def numericBool(op: Int => Boolean)(a: LispVal): LispEval[LispVal] = a match {
    case LispNum(x) => LispBool(op(x)).of[LispEval]
    case arg        => LispError.IncorrectType(s"numeric bool op ${arg.show}").raise
  }

  def stringOp(op: (String, String) => String)(a: LispVal)(b: LispVal): LispEval[LispVal] =
    (a, b) match {
      case (LispStr(x), LispStr(y)) => LispStr(op(x, y)).of[LispEval]
      case (LispNil, LispStr(y))    => LispStr(y).of[LispEval]
      case (LispStr(x), LispNil)    => LispStr(x).of[LispEval]
      case (x, y)                   => LispError.IncorrectType(s"string op ${x.show} ${y.show}").raise
    }

  def binaryOpFold(op: Binary)(zeroValue: LispVal)(args: List[LispVal]): LispEval[LispVal] =
    args match {
      case List(a, b) => op(a)(b)
      case x :: xs    => Foldable[List].foldM(args, zeroValue)(op(_)(_))
      case Nil        => LispError.IncorrectArgsNum(2, args).raise
    }

  def cons(vs: List[LispVal]): LispEval[LispVal] = vs match {
    case List(v, (rest @ LispList(xs))) => LispList(v +: xs).of[LispEval]
    case List(v)                        => LispList(List(v)).of[LispEval]
    case Nil                            => LispList(Nil).of[LispEval]
    case _                              => LispError.ExpectedList("cons").raise
  }

  def car(vs: List[LispVal]): LispEval[LispVal] = vs match {
    case List(LispList(Nil)) | Nil => LispList(Nil).of[LispEval]
    case List(LispList(v :: _))    => v.of[LispEval]
    case _                         => LispError.ExpectedList("car").raise
  }

  def cdr(vs: List[LispVal]): LispEval[LispVal] = vs match {
    case List(LispList(_ :: xs))   => LispList(xs).of[LispEval]
    case List(LispList(Nil)) | Nil => LispList(Nil).of[LispEval]
    case _                         => LispError.ExpectedList("cdr").raise
  }

  def fileExists(v: LispVal): LispEval[LispVal] = v match {
    case LispAtom(a) => fileExists(LispStr(a))
    case LispStr(a)  => LiftIO[LispEval].liftIO(doesFileExist(a)) map LispBool
    case v           => LispError.IncorrectType("expected str").raise
  }

  // def slurp(v: LispVal): LispEval[LispVal] = v match {
  //   case LispStr(txt) => writeToFile
  // }

  def doesFileExist(path: String): IO[Boolean] = {
    import java.nio.file.{Paths, Files}

    IO(Paths.get("/tmp")) >>= (x => IO(Files.exists(x)))
  }

  // def writeToFile(text: String): IO[LispVal] = {

  // }

}
