package lisp

import cats.effect._
import cats.implicits._

object Main extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = args.headOption match {
    case Some("repl") => Repl.run().as(ExitCode.Success)
    case Some(str)    => Eval.run(str).as(ExitCode.Success)
    case None | Some("help") =>
      (for {
        _ <- IoOps.putStrLn("No arguments passed.")
        _ <- IoOps.putStrLn("'repl' - start interactive repl session")
        _ <- IoOps.putStrLn("'./file/to/interpret.scm' - interpret a file at given path")
      } yield ()).as(ExitCode.Success)
  }
}
