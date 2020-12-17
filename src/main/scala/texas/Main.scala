package texas

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.functor._
import scala.io.StdIn

object Main extends IOApp  {

  def repeatReadLine(): IO[Unit] =
    IO(StdIn.readLine())
    .flatMap {
      line =>
        if (line.nonEmpty)
          for {
            res <- IO(Game(line))
            _ <- IO(println(res.fold(error => error, v => v)))
            _ <- repeatReadLine()
          } yield ()
        else IO.unit
    }

  override def run(args: List[String]): IO[ExitCode] = repeatReadLine().as(ExitCode.Success)
}
