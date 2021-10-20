package sandbox

object Main extends App {

  sealed trait ErrorApp extends Throwable
  case class ParseError() extends Throwable
  case class ArithmeticError() extends Throwable

  trait RaiseError[F[_], E] {
    def raise[A](e: E): F[A]
  }
  import cats._
  import tofu.syntax.raise._
  import cats.syntax.all._
  import tofu._
  def divide[F[_]](x: String, y: String)(
      implicit parseError: RaiseError[F, ParseError],
      arithmeticError: RaiseError[F, ArithmeticError]
  ): F[Int] = ???
//    (x.toIntOption.orRaise(ParseError()),
//      y.toIntOption.orRaise[F])
//      .mapN(_ / _)

  def a : String => Either[String, Int] = ???
  def b : Int => Either[String, Unit] = ???

  type ID = String
  final case class AbstractEvent(
    id : ID,
    name : String,
    typeEvent: EventType
  )
  sealed trait EventType


  final case class PurchaseEvent( details: String ) extends EventType
  final case class RefundEvent( orderId: String ) extends EventType

  def makeCopy(e: AbstractEvent, name: String): AbstractEvent = {
    e.copy(name = name)
  }




  //  println("Hello " |+| "Cats!")
}
