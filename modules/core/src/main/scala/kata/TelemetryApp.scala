package kata

import cats._
//import cats.implicits.catsSyntaxAlternativeGuard
import cats.implicits._
import cats.effect._

object TelemetryApp extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    IO(test()) *> IO(ExitCode.Success)

  sealed trait Build

  object Build {
    final case object empty extends Build
    final case object full extends Build
  }

  final case class Hash()

  val emptyHash: Hash = Hash()

//  map:: F[A] > A => B > F[B]
//  as = fa.map(_ => fb)

  val a: IO[Unit] = IO( print("hola") )
  val b: IO[Unit] = IO(println("bla"))

  a.flatMap(_ => b)
  a *> b


//  def foo: IO[String] =
//    IO(println("Procesando string"))
//      .as("string final")



  def build[F[_] : ApplicativeThrow](build: Build): F[Hash] = (build == Build.empty)
    .guard[Option]
    .as(emptyHash)
    .liftTo[F](new Throwable("Unsupported build"))


  def builds[F[_]](build: Build)(implicit A: ApplicativeThrow[F]): F[Hash] =
    build match {
      case Build.empty => A.pure(emptyHash)
      case _ => A.raiseError(new Throwable("Unsupported "))
    }

  type Main[A] = Either[Throwable, A]

  import cats.data.State
  type StateCustom = State[Map[String, Int], Unit]

      // State[Map[String, Int], Unit] = S => (Map[String, Int], Unit)
  def test() = assert(build[Main](Build.empty) == builds[Main](Build.full))

  Option("asda").map(s => 3)
  //  State[String, Int](s => ())

//  List(3, 2, 1) => List(1)
  def foo: State[List[Int], Unit] =
    State[List[Int], Unit]( state => (state.tail, ()))

  foo.run(List(3,2,1))


}
