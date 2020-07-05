package lisp

import lisp.LispVal._
import cats.effect.{IO, LiftIO}
import cats.implicits._
import cats.Foldable
import cats.effect.Resource
import scala.io.StdIn.readLine

object IoOps {
  def doesFileExist(path: String): IO[Boolean] = {
    import java.nio.file.{Paths, Files}

    IO(Paths.get("/tmp")) >>= (x => IO(Files.exists(x)))
  }

  def writeFile(path: String)(content: String): IO[LispVal] = {
    import java.nio.charset.StandardCharsets
    import java.nio.file.{Paths, Files}

    doesFileExist(path) ifM (
      for {
        p <- IO(Paths.get(path))
        b <- IO(content.getBytes(StandardCharsets.UTF_8))
        _ <- IO(Files.write(p, b))
      } yield LispStr(content),
      IO.raiseError(LispError.IOError(s"File at the '${path}' does not exist"))
    )
  }

  def readFile(path: String): IO[String] = doesFileExist(path) ifM (
    Resource
      .fromAutoCloseable(IO(scala.io.Source.fromFile(path)))
      .use(s => IO(s.mkString)),
    IO.raiseError(LispError.IOError(s"File at the '${path}' does not exist"))
  )

  def putStrLn(txt: String) = IO(println(txt))

  def readLn() = IO(readLine) map Option.apply
}
