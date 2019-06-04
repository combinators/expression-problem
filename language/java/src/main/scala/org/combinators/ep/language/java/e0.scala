package org.combinators.ep.language.java    /*DD:LD:AI*/

import com.github.javaparser.ast.body.MethodDeclaration
import org.combinators.ep.domain.math.M0
import org.combinators.ep.generator.LanguageIndependentGenerator
import org.combinators.templating.twirl.Java

/**
  * Truly independent of the specific design solution.
  *
  * Still Java-based, naturally and JUnit
  */
trait e0 extends JavaGenerator with JUnitTestGenerator with M0 {
  import domain._

  /** E0 Introduces the concept a Double and Int type, used for the 'Eval' operation. */
  abstract override def typeConverter(tr:TypeRep) : Type = {
    tr match {
      case Double => Java("Double").tpe
      case Int => Java("Integer").tpe
      case _ => super.typeConverter(tr)
    }
  }

  /** E0 Introduces Double and Int values. */
  abstract override def toTargetLanguage(ei:domain.ExistsInstance) : CodeBlockWithResultingExpressions = {
     ei.inst match {
      case d:scala.Double => CodeBlockWithResultingExpressions(Java(s"$d").expression())
      case i:scala.Int => CodeBlockWithResultingExpressions(Java(s"$i").expression())
      case _ => super.toTargetLanguage(ei)
    }
  }

  /** Eval operation needs to provide specification for current datatypes, namely Lit and Add. */
  abstract override def logic(exp:DataType, op:Operation): Seq[Statement] = {
    op match {
      case Eval =>
        exp match {
          case Lit => result(Java(expression(exp, litValue)).expression[Expression]())
          case Add => result(Java(s"${dispatch(expression(exp, base.left),op)} + ${dispatch(expression(exp, base.right),op)}").expression[Expression]())
          case _ => super.logic(exp, op)
        }

        // all future EXP sub-types can simply return hashcode.
        // move to m5
        //case Identifier => result(Java(exp.hashCode.toString).expression[Expression]())

      case _ => super.logic(exp, op)
    }
  }

  abstract override def testGenerator: Seq[MethodDeclaration] = {
    super.testGenerator ++ testMethod(M0_tests)
  }
}
