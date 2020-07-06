package lisp

import cats.effect._
import cats.implicits._

object Main extends IOApp {
  // override def run(args: List[String]): IO[ExitCode] = args.headOption match {
  //   case Some(str) => Eval.run(str).as(ExitCode.Success)
  //   case None      => IO(System.err.println("Nothing to interpret")).as(ExitCode(2))
  // }

  override def run(a: List[String]): IO[ExitCode] = Repl.run().as(ExitCode.Success)
}
