package sandbox

import cats.effect.IO

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

object Continuations {
  def main[F[_], A](args: Array[String]): Unit = {
    val (l, b) = (3, 4)
    IO.async { cb => }

  }

  import cats.effect._
  object Never extends IOApp {
    def run(args: List[String]): IO[ExitCode] =
      never
        .guarantee(IO("i guess never is now").void)
        .as(ExitCode.Success)

    val never: IO[Nothing] =
      IO.async(_ => never)
  }


  def continuatio[A](something: A): (A => Unit) => Unit =
    fun => fun(something)

  def sum[A](s: A)(fun: A => Unit): Unit =
    continuatio(s)(fun)


  import cats.effect.syntax.all._

  trait API {
    def compute: Future[Int] = ???
  }

  def doSomething[A](api: API)(implicit ec: ExecutionContext): IO[Int] =
    IO.async[Int] { cb => {
      api.compute.onComplete {
        {
          case Failure(exception) => cb(Left(exception))
          case Success(value) => cb(Right(value))
        }
      }
    }
    }.guarantee(IO.shift)


}
