package domain

import cats.effect.{ExitCode, IO, IOApp, LiftIO}
import domain.Algebras.Counter
import domain.Interpreters.CounterRef
import domain.Service.SimplerApi

object Algebras {
  trait Counter[F[_]] {
    def add(n: Int): F[Int]

    def sub(n: Int): F[Int]

    def set(n: Int): F[Int]
  }

  object Counter {
    def apply[F[_]](implicit F: Counter[F]): Counter[F] = F
  }
}

object Service {
  class SimplerApi[F[_]](implicit C: Counter[F]){
    def increase: F[Int] = C.add(1)

    def decrease: F[Int] = C.sub(1)

    def set(n: Int): F[Int] = C.sub(n)
  }
}

object Program extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    val initial = 0

    IO.ref(initial).flatMap(ref => {
      implicit val c = CounterRef.make[IO](ref)
      val service = new SimplerApi[IO]()
      for{
        x <- service.increase
        y <- service.increase
      } yield (x,y)
    })
      .flatMap(a => IO(println(s"current number is $a")))
      .as(ExitCode.Success)
  }
}

object Interpreters {
  import cats.effect.Ref

  object CounterRef {
    def make[F[_]: LiftIO](state: Ref[IO, Int]): Counter[F] =
      new Counter[F] {
        override def add(n: Int): F[Int] =
          state.updateAndGet(_ + n).to[F]

        override def sub(n: Int): F[Int] =
          state.updateAndGet(_ - n).to[F]

        override def set(n: Int): F[Int] =
          state.getAndSet(n).to[F]
      }
  }
}