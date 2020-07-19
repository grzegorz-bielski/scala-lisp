package lisp

import cats.effect.IO
import cats.implicits._

import lisp.LispVal.LispError
import IoOps.{putStrLn, putStr, readLn}

object Repl {
  def run(): IO[Unit] =
    for {
      _ <- putStr("λ: ")
      line <- readLn
      _ <- line match {
        case None    => putStrLn("👋")
        case Some(v) => process(v) >> run
      }
    } yield ()

  def process(str: String): IO[Unit] =
    exec(Eval.run(str)) >>= {
      case Left(v)  => putStrLn(v)
      case Right(v) => v.pure[IO]
    }

  def processToASTPreview(str: String): IO[Unit] = putStr(Eval.runParserForASTPreview(str))

  def exec[A](a: IO[A]): IO[Either[String, A]] =
    a.attempt map (_ leftMap {
      case e: LispError => e.show
      case a            => s"Unknown error: ${a}"
    })
}
