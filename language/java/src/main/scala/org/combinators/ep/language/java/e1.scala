package org.combinators.ep.language.java   /*DD:LD:AI*/

import com.github.javaparser.ast.body.MethodDeclaration
import org.combinators.ep.domain.Evolution
import org.combinators.ep.domain.math.M1
import org.combinators.templating.twirl.Java

/**
  * Truly independent of the specific design solution.
  *
  * Still Java-based, naturally and JUnit
  */
trait e1 extends Evolution with JavaGenerator with JUnitTestGenerator with M1 {
  self:e0 =>
  import domain._
  abstract override def logic(exp:DataType, op:Operation): Seq[Statement] = {
    op match {
      case Eval =>
        exp match {
          case Sub => result(Java(s"${dispatch(expression(exp, base.left), op)} - ${dispatch(expression(exp, base.right), op)}").expression())
          case _ => super.logic(exp, op)
        }

      case _ => super.logic(exp, op)
    }
  }

  abstract override def testGenerator: Seq[MethodDeclaration] = {
    super.testGenerator ++ testMethod(M1_tests)
  }
}
