package sandbox

import cats.Functor

object IOMonad {

  type World = String
  type WorldT[A] = World => (A, World)

  trait Console{
    def readStrT : WorldT[String]
    def writeStrT: String => WorldT[Unit]
  }
  trait WorldM[M[_]] {
    def asT[A] : WorldT[A]
  }

}
