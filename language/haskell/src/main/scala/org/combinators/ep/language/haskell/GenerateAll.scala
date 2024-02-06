package org.combinators.ep.language.haskell

object GenerateAll extends App {
  println ("Generating code...")
  val approaches = Seq("straight", "alacarte", "grow")
  val systems = Seq("e0", "e1", "e2", "e3", "e4")

  approaches.foreach(approach =>
    systems.foreach(system =>
    approach match {
      case "straight" => StraightTest.evaluate (system).generatedCode (approach, system)
      case "alacarte" => ALaCarteTest.evaluate (system).generatedCode (approach, system)
      case "grow" => GrowTest.evaluate (system).generatedCode (approach, system)

      case _ => ???
    })
  )
}
