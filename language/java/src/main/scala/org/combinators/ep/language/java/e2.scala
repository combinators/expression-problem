package org.combinators.ep.language.java   /*DD:LD:AI*/

import com.github.javaparser.ast.body.MethodDeclaration
import org.combinators.ep.domain.Evolution
import org.combinators.ep.domain.math.{M0, M2}
import org.combinators.templating.twirl.Java

class e2 {
// GOING AWAY
}
///**
//  * Truly independent of the specific design solution.
//  *
//  * Still Java-based, naturally and JUnit
//  */
//trait e2 extends Evolution with DomainIndependentJavaGenerator with JUnitTestGenerator with M0 with M2 {
//  self:e0 with e1 =>
//  import domain._
//
//  /** E2 Introduces the concept a String type, used for the 'PrettyPrint' operation. */
//  abstract override def typeConverter(tpe:TypeRep) : Type = {
//    tpe match {
//      case String => Java("String").tpe()
//      case _ => super.typeConverter(tpe)
//    }
//  }
//
//  /** E2 Introduces String values. */
//  abstract override def toTargetLanguage(ei:ExistsInstance) : CodeBlockWithResultingExpressions = {
//    ei.inst match {
//      case s:String => CodeBlockWithResultingExpressions(new com.github.javaparser.ast.expr.StringLiteralExpr(s))
//      case _ => super.toTargetLanguage(ei)
//    }
//  }
//
//  abstract override def logic(exp:DataType, op:Operation): Seq[Statement] = {
//    op match {
//      case PrettyP =>
//        exp match {
//          case Lit => result(Java(s""" "" + ${expression(exp,litValue)} """).expression())
//          case Add => result(Java(s""" "(" + ${dispatch(expression(exp, base.left), PrettyP)} + "+" + ${dispatch(expression(exp, base.right), PrettyP)}+ ")" """).expression())
//          case Sub => result(Java(s""" "(" + ${dispatch(expression(exp, base.left), PrettyP)} + "-" + ${dispatch(expression(exp, base.right), PrettyP)} + ")" """).expression())
//          case _ => super.logic(exp, op)
//        }
//
//      case _ => super.logic(exp, op)
//    }
//  }
//
//  abstract override def testGenerator: Seq[MethodDeclaration] = {
//    super.testGenerator ++ testMethod(M2_tests)
//  }
//}
