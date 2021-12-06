package kata.telemetry

import cats._
import cats.effect.{ ExitCode, IO, IOApp, _ }
import cats.implicits._

object Algebras extends IOApp {

  override def run(args: List[String]): IO[ExitCode] =
    program(0)
      .as(ExitCode.Success)

  object Incrementer {
    def make[F[_]: LiftIO](ref: Ref[IO, Int]): Increment[F] = new Increment[F] {
      override def addOne(n: Int): F[Int] = ref.updateAndGet(_ + n).to[F]
    }
  }

  def program(i: Int): IO[Int] =
    IO.ref(0)
      .flatMap(ref => {
        implicit val incrementer = Incrementer.make[IO](ref)
        op[IO](i)
      })

  def op[F[_]: Monad: Increment](i: Int): F[Int] =
    for {
      j <- implicitly[Increment[F]].addOne(i)
      z <- if (j < 10000) op[F](j) else Monad[F].pure(j)
    } yield z

  trait Increment[F[_]] {
    def addOne(n: Int): F[Int]
  }

  trait Telemetryclient[F[_]] {
    def connect: F[Unit]

    def disconnect: F[Unit]
  }

}
