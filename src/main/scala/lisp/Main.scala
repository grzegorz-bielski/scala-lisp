package lisp

import cats.effect._
import cats.implicits._

object Main extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = args.headOption match {
    case Some(str) =>
      IO(println(Parser.readExp(str))).as(ExitCode.Success)
    case None =>
      IO(System.err.println("Nothing to interpret")).as(ExitCode(2))
  }
}
