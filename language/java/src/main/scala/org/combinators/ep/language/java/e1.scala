package org.combinators.ep.language.java   /*DD:LD:AI*/

import com.github.javaparser.ast.body.MethodDeclaration
import org.combinators.ep.domain.Evolution
import org.combinators.ep.domain.math.{M1, MathDomain}
import org.combinators.templating.twirl.Java

/**
  * Truly independent of the specific design solution.
  *
  * Still Java-based, naturally and JUnit
  */
trait e1 extends Evolution with JavaGenerator with JUnitTestGenerator with M1 {
  self:e0 =>
  val domain:MathDomain

  abstract override def logic(exp:domain.DataType, op:domain.Operation): Seq[Statement] = {
    op match {
      case Eval =>
        exp match {
          case Sub => result(Java(s"${dispatch(expression(exp, domain.base.left), Eval)} - ${dispatch(expression(exp, domain.base.right), Eval)}").expression[Expression]())
          case _ => super.logic(exp, op)
        }

      case _ => super.logic(exp, op)
    }
  }

  abstract override def testGenerator: Seq[MethodDeclaration] = {
    super.testGenerator ++ testMethod(M1_tests)
  }
}
