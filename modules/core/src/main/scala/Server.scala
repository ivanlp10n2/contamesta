import cats.effect.{ExitCode, IO, IOApp}

import scala.io.StdIn
import cats.syntax.all._

object Server extends IOApp{
  override def run(args: List[String]): IO[ExitCode] =
    program.as(ExitCode.Success)

  val program = 3
    .pure[IO]

  def foo = ???
}