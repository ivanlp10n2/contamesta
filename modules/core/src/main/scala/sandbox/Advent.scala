package sandbox
import cats.effect._
import cats._
import cats.instances.list._

object Advent extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = Resource.make(IO(scala.io.Source.fromFile("./data")))
  List(1).foldLeft(List.empty[Int]) { (a, l) =>
    l :: a
  }

  //typeclasses:
  // 1- trait
  // 2- instances
  // 3- syntaxis
  // 4- usage

  import cats.syntax.applicative._
  def foo[F[_]: Applicative]: F[Int] = 3.pure[F]
  foo[Id]

  def identity[A]: A => A = a => a
//  type Id[A] = A

  List(1, 2, 3).map(identity)

  //functor: aplica una funcion a un efecto
  //applicative: une dos efectos independientes (ap)
  //monad: une dos efectos dependientes
}
