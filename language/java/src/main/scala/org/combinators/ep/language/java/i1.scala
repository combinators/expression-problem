package org.combinators.ep.language.java   /*DD:LD:AI*/

import com.github.javaparser.ast.body.MethodDeclaration
import org.combinators.ep.domain.Evolution
import org.combinators.ep.domain.math.{I1, M0}
import org.combinators.templating.twirl.Java

class i1 {
// GOING AWAY
}
///**
//  * Independent branch to just contain 'Neg'
//  */
//trait i1 extends Evolution with DomainIndependentJavaGenerator with JUnitTestGenerator with M0 with I1 {
//  self: e0 with e1 =>
//
//  abstract override def logic(exp:domain.DataType, op:domain.Operation): Seq[Statement] = {
//    // generate the actual body
//    op match {
//      case Eval =>
//        exp match {
//          case Inv => result(Java(s" 1 / ${dispatch(expression(exp, domain.base.inner), Eval)} ").expression())
//          case _ => super.logic(exp, op)
//        }
//
//      case _ => super.logic(exp, op)
//    }
//  }
//
//  abstract override def testGenerator: Seq[MethodDeclaration] = {
//
//    val i1 = new domain.UnaryInst(Inv, LitInst(2.0))
//    val i1Block = actual(Eval, i1).appendDependent { case Seq(i1Inst) =>
//        CodeBlockWithResultingExpressions(
//          Java(s"assertEquals(0.5, $i1Inst);").statement()
//        )()
//      }.block
//
//    super.testGenerator ++ Java(
//      s"""
//         |public void test() {
//         |   ${i1Block.mkString("\n")}
//         |}""".stripMargin).methodDeclarations()
//  }
//}
