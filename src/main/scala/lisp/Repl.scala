package lisp

import cats.effect.IO

import scala.io.StdIn.readLine
import cats.implicits._
import lisp.LispVal.LispError

object Repl {

  def run(): IO[Unit] =
    for {
      _ <- putStrLn("repl: ")
      line <- IO(readLine) map Option.apply
      _ <- line match {
        case None    => putStrLn("👋")
        case Some(v) => process(v) >> run
      }
    } yield ()

  def putStrLn(txt: String) = IO(println(txt))

  def process(str: String): IO[Unit] =
    exec(Eval.evalStr(str)) flatMap {
      case Left(v)  => putStrLn(v)
      case Right(v) => v.pure[IO]
    }

  def processToASTPreview(str: String): IO[Unit] = IO(print(Eval.runParserForASTPreview(str)))

  def exec[A](a: IO[A]): IO[Either[String, A]] =
    a.attempt map (_ leftMap {
      case e: LispError => e.show
      case a            => s"Unknown error: ${a}"
    })
}
