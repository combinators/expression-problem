package org.combinators.ep.language.java;

class s1 {
  // GOING AWAY
}
///**
//  * Truly independent of the specific design solution.
//  *
//  * Still Java-based, naturally and JUnit
//  */
//trait s1 extends DomainIndependentJavaGenerator with JUnitTestGenerator with S1 { self:s0 =>
//  val domain:ShapeDomain
//
//  /** Eval operation needs to provide specification for current datatypes, namely Lit and Add. */
//  abstract override def logic(exp:domain.DataType, op:domain.Operation): Seq[Statement] = {
//    op match {
//      case Shrink =>
//        exp match {
//          case Circle =>  // These [Expression] qualifiers need to be here
//            val shrunkRadius = Java(s"${expression(exp, radius)}*pct").expression[Expression]()
//            inst(Circle, shrunkRadius).appendDependent{ case Seq(returnVal) =>
//              CodeBlockWithResultingExpressions(result(returnVal):_*)()
//            }.block
//
//          case Square =>    // These [Expression] qualifiers need to be here
//            val shrunkSide = Java(s"${expression(exp, side)}*pct").expression[Expression]()
//            inst(Square, shrunkSide).appendDependent{ case Seq(returnVal) =>
//              CodeBlockWithResultingExpressions(result(returnVal):_*)()
//            }.block
//
//          case Translate =>
//            val disp = dispatch(expression(exp, shape), op, Java("pct").expression[Expression]())
//            inst(Translate, expression(exp, trans), disp).appendDependent{ case Seq(returnVal) =>
//              CodeBlockWithResultingExpressions(result(returnVal):_*)()
//            }.block
//          }
//      case _ => super.logic(exp, op)
//    }
//  }
//
//  abstract override def testGenerator: Seq[MethodDeclaration] = {
//
//    val s1 = new SquareInst(8.0)
//    val d1 = Java("0.5").expression[Expression]()
//    val instBlock = actual(Shrink, s1, d1).appendDependent { case Seq(inst) =>
//      CodeBlockWithResultingExpressions(
//        Java(s"assertNotNull($inst);").statement()
//      )()
//    }.block
//
//    super.testGenerator ++ Java(
//      s"""
//         |public void test() {
//         |   // without access to the attributes, we can't write meaningful attribute test cases.
//         |   ${instBlock.mkString("\n")}
//         |
//         |   // Handle collect checks
//         |
//         |}""".stripMargin).methodDeclarations()
//  }
//}
