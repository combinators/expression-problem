package ep.scala     /*DD:LD:AD*/

object GenerateAll extends App {
  println ("Generating code...")

  val approaches = Seq("functional", "straight", "oo")
  val systems = Seq("e0", "e1", "e2", "e3", "e4", "e5", "e6")

  approaches.foreach(approach =>
    systems.foreach(system =>
      approach match {
        case "straight" => StraightTest.evaluate (system).generatedCode (approach, system, Some("scala_oo"))
        case "oo" => OOTest.evaluate (system).generatedCode (approach, system, Some("odersky"))
        case "functional" => FunctionalTest.evaluate (system).generatedCode (approach, system, Some("scala_func"))

        case _ => ???
      })
  )
}