package lisp

import cats.effect._
import cats.implicits._

import IoOps.putStrLn

object Main extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = args match {
    // these should not always return the `ExitCode.Success`...
    case List("path", path) => (IoOps.readFile(path) >>= Eval.run).as(ExitCode.Success)
    case List("repl")       => Repl.run().as(ExitCode.Success)
    case List("eval", str)  => Eval.run(str).as(ExitCode.Success)
    case Nil | List("help") =>
      (for {
        _ <- putStrLn("No arguments passed.")
        _ <- putStrLn("'repl' - start interactive repl session")
        _ <- putStrLn("'./file/to/interpret.scm' - interpret a file at given path")
      } yield ()).as(ExitCode.Success)
  }
}
