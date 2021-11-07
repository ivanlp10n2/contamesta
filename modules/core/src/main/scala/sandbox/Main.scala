package sandbox

import cats.effect._
import cats.syntax.all._
import scala.concurrent.duration._

object Main extends IOApp {

  import debug._

  override def run(args: List[String]): IO[ExitCode] = {
    (clock, ohNo).parTupled
      .attempt.as(ExitCode.Error)
  }

  val ohNo: IO[Unit] = IO.sleep(2.seconds) *> IO.raiseError(new Exception)
  cats.effect.std.Semaphore

  val clock: IO[Unit] = for {
    _ <- IO.sleep(1.seconds)
    _ <- IO(println(s"Time is ${System.currentTimeMillis()}")).debug
    _ <- clock
  } yield ()
}

