package org.combinators.ep.language.java

/*DD:LD:AI*/

import com.github.javaparser.ast.body.MethodDeclaration
import org.combinators.ep.domain.Evolution
import org.combinators.ep.domain.math.{M0, M2, MathDomain}
import org.combinators.templating.twirl.Java

/**
  * Truly independent of the specific design solution.
  *
  * Still Java-based, naturally and JUnit
  */
trait e2 extends Evolution with JavaGenerator with JUnitTestGenerator with M0 with M2 {
  self:e0 with e1 =>
  val domain:MathDomain

  /** E1 Introduces the concept a String type, used for the 'PrettyPrint' operation. */
  abstract override def typeConverter(tpe:domain.TypeRep) : com.github.javaparser.ast.`type`.Type = {
    tpe match {
      case String => Java("String").tpe()
      case _ => super.typeConverter(tpe)
    }
  }

  /** E2 Introduces String values. */
  abstract override def toTargetLanguage(ei:domain.ExistsInstance) : CodeBlockWithResultingExpressions = {
    ei.inst match {
      case s:String => CodeBlockWithResultingExpressions(new com.github.javaparser.ast.expr.StringLiteralExpr(s))
      case _ => super.toTargetLanguage(ei)
    }
  }

  abstract override def logic(exp:domain.DataType, op:domain.Operation): Seq[Statement] = {
    op match {
      case PrettyP =>
        exp match {
          case Lit => result(Java(s""" "" + ${expression(exp,litValue)} + "" """).expression[Expression]())
          case Add => result(Java(s""" "(" + ${dispatch(expression(exp, domain.base.left), PrettyP)} + "+" + ${dispatch(expression(exp, domain.base.right), PrettyP)}+ ")" """).expression[Expression]())
          case Sub => result(Java(s""" "(" + ${dispatch(expression(exp, domain.base.left), PrettyP)} + "-" + ${dispatch(expression(exp, domain.base.right), PrettyP)} + ")" """).expression[Expression]())
          case _ => super.logic(exp, op)
        }

      case _ => super.logic(exp, op)
    }
  }

  abstract override def testGenerator: Seq[MethodDeclaration] = {
    super.testGenerator ++ testMethod(M2_tests)
  }
}
