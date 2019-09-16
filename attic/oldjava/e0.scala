package org.combinators.ep.language.java    /*DD:LD:AI*/

import com.github.javaparser.ast.body.MethodDeclaration
import org.combinators.ep.domain.abstractions._
import org.combinators.templating.twirl.Java

/**
  * Truly independent of the specific design solution.
  *
  * Still Java-based, naturally and JUnit
  */
class e0(override val gen:DomainIndependentJavaGenerator) extends JUnitTestGenerator(gen) {

  /** E0 Introduces the concept a Double and Int type, used for the 'Eval' operation. */
  override def tpe(tr:TypeRep) : Type = {
    tr match {
      case TypeRep.Double => Java("Double").tpe
      case TypeRep.Int => Java("Integer").tpe
      case _ => super.tpe(tr)
    }
  }

  /**
    * E0 Introduces Double and Int values.
    *
    * Converts instance in ExistsInstance into Java value.
    *
    * Decide to use formal java.lang.Double and java.lang.Integer because of the ambiguity
    * that can exist in JUnit when dealing with primitive types and boxed types.
    */
  override def toTargetLanguage(ei:InstanceModel) : CodeBlockWithResultingExpressions = {
     ei.inst match {
      case d:scala.Double => CodeBlockWithResultingExpressions(Java(s"new Double($d)").expression())
      case i:scala.Int => CodeBlockWithResultingExpressions(Java(s"new Integer($i)").expression())
      case _ => super.toTargetLanguage(ei)
    }
  }

  /** Eval operation needs to provide specification for current datatypes, namely Lit and Add. */
  override def logic(exp:DataType, op:Operation): Seq[Statement] = {
    op match {
      case Eval =>
        exp match {
          case Lit => result(Java(expression(exp, litValue)).expression())
          case Add => result(Java(s"${dispatch(expression(exp, m0.domain.base.left),op)} + ${dispatch(expression(exp, m0.domain.base.right),op)}").expression())
          case _ => super.logic(exp, op)
        }

      case _ => super.logic(exp, op)
    }
  }

  override def testGenerator: Seq[MethodDeclaration] = {
    super.testGenerator ++ testMethod(M0_tests)
  }
}
