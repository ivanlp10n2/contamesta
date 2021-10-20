package sandbox

import cats.Monad
import cats.data.NonEmptyList
import cats.effect.IO

object tofu2 extends App{

  def fold[A](partida: A)(op: (A,A) => A): A = ???
  def fold[A, B](partida: A)(op: (A,B) => A): A = ???

  // F[A] => ( (B) => (A,B) => B ) => A

  trait Perro{
    def sum(a: Int, b: Int): Int = a + b
    // sum: (Int, Int) => Int
    // sum (3)
    def sum2(a: Int)(b: Int): Int = a + b
    // sum2: (Int) => (Int) => Int
    // sum2 (3) : (Int) => Int

  }
  def sum(a: Int, b: Int): Int = ???
  List(1,2,3).foldLeft(0)(new Perro().sum) // Estructura con datos, cantidad indeterminada
  println(List("1","2","3").foldLeft("")( (a,b) => a.concat(b)))

//  Option(1).foldLeft(0)( (a,b) => a + b)

//  List
//  Option
//  Either
//  Future
//  NonEmptyList
//  IO

  def persistData(user:String): IO[Int] = ???
  val persistedUserIO = persistData("Ivan")  // String => Int
  val persistedUserIO2 = persistData("Ivan")  // String => Int

//  persistedUserIO.unsafeRunSync() // 0
//  val persistedUser2 = 0
  // transparencia referencial: reemplazar el output para el input

  def sum(x: Int, y: Int): Int = ???
//  val a: Int = sum(3,4) // 7
  val map: Map[(Int, Int), Int]=  Map( (3, 4) -> 7)
//  val a: Int = 7

  def convertPerro = "asd".map(a)

}
