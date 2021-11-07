package sandbox

import cats.data.Validated
import cats.effect.{ExitCode, IO, IOApp}
import dev.profunktor.redis4cats.Redis

//object Appasd extends IOApp{
//  override def run(args: List[String]): IO[ExitCode] =
//    IO(println("asdasd"))
//      .flatMap(_ => IO(println("123")))
//      .as(ExitCode.Success)
//
//  Redis
//  def a(b: Option[Int]) : Option[Int] =
//    b match {
//      case Some(value) => value
//      case None =>
//    }
//
//}

import cats.effect.Resource
object t extends App{

  val eitherOptionA = Option("123").toRight("not found a")
  val eitherOptionB = Option("234").toRight("not found b")

  import cats.syntax.all._
  val x: Validated[String, String] =
    (eitherOptionA.toValidated, eitherOptionB.toValidated)
      .mapN( (a,b) => a + b)

  println(x)
}