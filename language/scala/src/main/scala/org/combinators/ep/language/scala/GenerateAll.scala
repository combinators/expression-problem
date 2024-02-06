package org.combinators.ep.language.scala

object GenerateAll extends App {
  println ("Generating code...")

  val approaches = Seq("straight", "oo", "functional")
  val systems = Seq("e0", "e1", "e2", "e3", "e4", "e5", "e6", "e7", "e8")

  approaches.foreach(approach =>
    systems.foreach(system =>
      approach match {
        case "straight" => StraightTest.evaluate (system).generatedCode (approach, system)
        case "oo" => OOTest.evaluate (system).generatedCode (approach, system)
        case "functional" => FunctionalTest.evaluate (system).generatedCode (approach, system)

        case _ => ???
      })
  )
}