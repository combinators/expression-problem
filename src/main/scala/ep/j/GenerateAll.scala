package ep.j    /*DD:LD:AD*/

object GenerateAll extends App {
  println ("Generating code...")

  val approaches = Seq("oo", "visitor", "extensibleVisitor", "interpreter", "trivially", "algebra")
  val systems = Seq("e0", "e1", "e2", "e3", "e4", "e5", "e6", "c1", "i1", "i2", "s0", "s1")

  approaches.foreach(approach =>
    systems.foreach(system =>
      approach match {
        case "oo" => OOEvaluateTest.evaluate (system).generatedCode (approach, system)
        case "visitor" => VisitorEvaluateTest.evaluate (system).generatedCode (approach, system)
        case "extensibleVisitor" => ExtensibleVisitorEvaluateTest.evaluate (system).generatedCode (approach, system)
        case "interpreter" => InterpreterEvaluateTest.evaluate (system).generatedCode (approach, system)
        case "trivially" => TriviallyEvaluateTest.evaluate (system).generatedCode (approach, system)
        case "algebra" => AlgebraEvaluateTest.evaluate (system).generatedCode (approach, system)
        case _ => ???
      }
    )
  )
}
