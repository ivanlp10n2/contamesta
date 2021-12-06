package sandbox

import cats.Monad
import cats.data.NonEmptyList
import cats.effect.IO

object tofu2 extends App {

//  def fold[A](partida: A)(op: (A,A) => A): A = ???
//  def fold[A, B](partida: A)(op: (A,B) => A): A = ???

  // F[A] => ( (B) => (A,B) => B ) => A

//  trait Perro{
//    def sum(a: Int, b: Int): Int = a + b
//    // sum: (Int, Int) => Int
//    // sum (3)
//    def sum2(a: Int)(b: Int): Int = a + b
//    // sum2: (Int) => (Int) => Int
//    // sum2 (3) : (Int) => Int
//
//  }
  def sum(a: Int, b: Int): Int = ???
  println(List("1", "2", "3").foldLeft("")((a, b) => a.concat(b)))

//  Option(1).foldLeft(0)( (a,b) => a + b)

//  List
//  Option
//  Either
//  Future
//  NonEmptyList
//  IO

//  def persistData(user:String): IO[Int] = ???
//  val persistedUserIO = persistData("Ivan")  // String => Int
//  val persistedUserIO2 = persistData("Ivan")  // String => Int

  trait Perro {
    def name: String
    def amountOfLegs: Int
  }
  case class Chiguagua(name: String, amountOfLegs: Int, owner: String) extends Perro
  case class Gato(name: String, amountOfLegs: Int)
  case class Tigre(name: String, amountOfLegs: Int)

  sealed trait Sum[+A, +B] {
    def flatMap[AA >: A, C](f: B => Sum[AA, C]): Sum[AA, C] =
      this match {
        case Failure(v) => Failure(v)
        case Success(v) => f(v)
      }
  }
  final case class Failure[A](value: A) extends Sum[A, Nothing]
  final case class Success[B](value: B) extends Sum[Nothing, B]
//  Function2[Int, String, String]
//  Function
//    => input contravariant
//    => output covariant

  trait ListH[+A]
  case class Node[A](head: A, tail: ListH[A]) extends ListH[A]
  case class End()                            extends ListH[Nothing]

//  Node[Perro](Perro("firu", 4), Nil())

//  persistedUserIO.unsafeRunSync() // 0
//  val persistedUser2 = 0
  // transparencia referencial: reemplazar el output para el input

//  val a: Int = sum(3,4) // 7
  val map: Map[(Int, Int), Int] = Map((3, 4) -> 7)
//  val a: Int = 7

  sealed trait ErrorApp        extends Throwable
  case class ParseError()      extends Throwable
  case class ArithmeticError() extends Throwable

  trait RaiseError[F[_], E] {
    def raise[A](e: E): F[A]
  }
  import cats._
  import tofu.syntax.raise._
  import cats.syntax.all._
  import tofu._
  def divide[F[_]](x: String, y: String)(
      ParseError: RaiseError[F, ParseError],
      ArithmeticError: RaiseError[F, ArithmeticError]
  ): F[Int] = ???
//    (x.toIntOption.orRaise(ParseError()),
//      y.toIntOption.orRaise[F])
//      .mapN(_ / _)

  def a: String => Either[String, Int] = ???
  def b: Int => Either[String, Unit]   = ???

  type ID = String
  final case class AbstractEvent(
      id: ID,
      name: String,
      typeEvent: EventType
  )
  sealed trait EventType

  final case class PurchaseEvent(details: String) extends EventType
  final case class RefundEvent(orderId: String)   extends EventType

  def makeCopy(e: AbstractEvent, name: String): AbstractEvent = {
    e.copy(name = name)
  }
  //  println("Hello " |+| "Cats!")
}
