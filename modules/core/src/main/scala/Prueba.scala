object Prueba {

  case class Perro(name: String) {
    def ladrar: String = "guagu"
  }

  trait Ladrable[A] { def ladrar: String }
  // Descripcion  : llamar 4 servicios
  // Ejecucion    : sequenciales
//  override def run(args: List[String]): IO[ExitCode] =
//    IO(println("asd"))
//    IO(Option(new RuntimeException))
//      .flatMap(_.fold(IO.unit)(IO.raiseError)) *> IO(println("se ejecuto")) *> IO(ExitCode.Success)

}
