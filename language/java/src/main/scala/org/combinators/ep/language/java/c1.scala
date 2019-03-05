package org.combinators.ep.language.java   /*DD:LD:AI*/

import com.github.javaparser.ast.body.MethodDeclaration
import com.github.javaparser.ast.stmt
import org.combinators.ep.domain.Evolution
import org.combinators.ep.domain.math.{M2, MC1, MathDomain}
import org.combinators.templating.twirl.Java

/**
  * Truly independent of the specific design solution.
  *
  * Still Java-based, naturally and JUnit
  */
trait c1 extends Evolution with JavaGenerator with JUnitTestGenerator with M2 with MC1 {
  self: e0 with e1 with e2 with e3 with i1 with i2 =>
  val domain:MathDomain

  abstract override def logic(exp:domain.DataType, op:domain.Operation): Seq[stmt.Statement] = {
    op match {
      case PrettyP => exp match {
        case Inv =>
          val inv = dispatch(expression(exp, domain.base.inner), PrettyP)
          Java(s"""return "(1.0/" + $inv + ")";""").statements
        case _ => super.logic(exp, op)
      }
      case _ => super.logic(exp, op)
    }
  }

  abstract override def testGenerator: Seq[MethodDeclaration] = {
    super.testGenerator ++ testMethod(MC1_tests)
  }

//  abstract override def testGenerator: Seq[MethodDeclaration] = {
//    val s1 = new domain.BinaryInst(Sub, LitInst(1.0), LitInst(2.0))
//    val d1 = new domain.BinaryInst(Divd, LitInst(1.0),
//      new domain.BinaryInst(Sub, LitInst(1.0), LitInst(2.0)))
//    val s2 = new domain.UnaryInst(Inv ,s1)
//
//      super.testGenerator ++ Java(
//      s"""
//         |public void test() {
//         |   assertEquals("(1.0/(1.0-2.0))", ${dispatch(convert(s2), PrettyP)});
//         |   assertEquals(${dispatch(convert(d1), PrettyP)}, ${dispatch(convert(s2), PrettyP)});
//         |}""".stripMargin).methodDeclarations()
//  }
}
