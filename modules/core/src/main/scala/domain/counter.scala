package domain

import cats.arrow.FunctionK
import cats.{FlatMap, Functor, Monad}
import cats.effect.{Async, ExitCode, IO, IOApp, LiftIO}
import domain.Algebras.{Counter, SimplerApi}
import domain.Interpreters.CounterRef

object a {
  final case class Config(name: String, version: Double)
  import cats.data.Kleisli
  type F[X] = Kleisli[IO, Config, X]

  def hello: F[String] =
    Kleisli(c => IO.pure(s"I am ${c.name}, ver.${c.version}"))

  import cats.Applicative
  def hello[F[_]: Applicative]: F[String] = {
    Applicative[F].pure("asd")
  }


  trait ApplicativeReader[F[_], A]{ def read: F[A]}

  import cats.syntax.all._
  final case class Kleisly[F[_], A, B](data: A => F[B]){
    def compose[Z](kleisly: Kleisly[F, Z, A])(implicit F: FlatMap[F]): Kleisly[F, Z, B] =
      Kleisly(z => F.flatMap(kleisly.data(z))(data))

    def map[C](f: B => C)(implicit F: Functor[F]): Kleisly[F, A, C] =
      Kleisly(a => F.map(data(a))(f))
  }

  sealed abstract class Free[S[_], A] {
    def flatMap[B](f: A => Free[S,B]): Free[S, B] =
      Free.FlatMapped(this, f)

    def map[B](f: A => B): Free[S, B] =
      Free.FlatMapped(this, Free.Pure(_))

    def pure(a: A): Free[S, A] =
      Free.Pure[S,A](a)
  }
  object Free{
    def unit[S[_], A](a: A): Free[S, A] = Pure(a)
    def lift[S[_], A](sa: S[A]): Free[S, A] = Suspend(sa)

    case class Suspend[S[_],A](action: S[A]) extends Free[S, A]
    // Do we transform functions like
    // def flatmap[F,A,B](fa: F[A])(f: A => B): F(B)
    // to `case class flatmap(fa, f) extends F[B]
    case class FlatMapped[S[_], A, B](fa: Free[S, A], f: A => Free[S,B]) extends Free[S, B]
    case class Pure[S[_], A](a: A) extends Free[S,A]
  }

  object ApplicativeReader {
    def apply[F[_], A](implicit instance: ApplicativeReader[F,A]): ApplicativeReader[F,A] =
      instance
  }

    // foldea sobre F y mapea F ~> G
    // Free[F,A] => f: F ~> G => G[A]
    def foldMap[F[_], G[_]: Monad, A](
           free: Free[F,A]
         )(
           fk: FunctionK[F,G]
        ): G[A] =
      free match {
        case Free.Pure(a) => a.pure[G]
        case Free.Suspend(action) => fk(action)
        case Free.FlatMapped(fa, f) => foldMap(fa)(fk)
          .flatMap(a => foldMap(f(a))(fk))
      }

  }
}

object Algebras {
  trait Counter[F[_]] {
    def add(n: Int): F[Int]

    def sub(n: Int): F[Int]

    def set(n: Int): F[Int]
  }

  object Counter {
    def apply[F[_]](implicit F: Counter[F]): Counter[F] = F
  }

  trait SimplerApi[F[_]] {
    def increase: F[Int]

    def decrease: F[Int]
  }
}

object Service {
  def make[F[_]](implicit F: Counter[F]): SimplerApi[F] =
    new SimplerApi[F] {
      override def increase: F[Int] = F.add(1)

      override def decrease: F[Int] = F.sub(1)
    }

}

object Program extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    val initial = 0

    IO.ref(initial).flatMap(ref => {
      implicit val c = CounterRef.make[IO](ref)
      val service = Service.make[IO]
      for {
        _ <- service.increase
        y <- service.increase
      } yield y
    })
      .flatMap(a =>
        IO(println(s"current number is $a"))) *>
          IO(ExitCode.Success)
  }
}

object Interpreters {

  import cats.effect.Ref

  object CounterRef {
    def make[F[_] : Async](state: Ref[F, Int]): Counter[F] = {
      Async[F].delay {
        new Counter[F] {
          override def add(n: Int): F[Int] =
            state.updateAndGet(_ + n)

          override def sub(n: Int): F[Int] =
            state.updateAndGet(_ - n)

          override def set(n: Int): F[Int] =
            state.getAndSet(n)
        }
      }
    }
  }
}