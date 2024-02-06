package org.combinators.ep.language.cpp

object GenerateAll extends App {
  println ("Generating code...")
  val approaches = Seq("oo", "visitor", "visitorTable")
  val systems = Seq("e0", "e1", "e2", "e3", "e4", "e5", "e6", "e7", "e8")

  approaches.foreach(approach =>
    systems.foreach(system =>
      approach match {
        case "oo" => StraightTest.evaluate (system).generatedCode (approach, system)
        case "visitor" => VisitorTest.evaluate (system).generatedCode (approach, system)
        case "visitorTable" => VisitorTableTest.evaluate (system).generatedCode (approach, system)

        case _ => ???
      })
  )
}
