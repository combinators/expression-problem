package org.combinators.ep.language.scala    /*DD:LD:AI*/

import org.combinators.ep.domain.Evolution
import org.combinators.ep.domain.math.{M0, M2, MathDomain}

import scala.meta._

/**
  * Truly independent of the specific design solution.
  *
  * Still Java-based, naturally and JUnit
  */
trait e2 extends Evolution with ScalaGenerator with TestGenerator with M0 with M2 {
  self:e0 with e1 =>
  val domain:MathDomain

  abstract override def typeConverter(tpe:domain.TypeRep) : Type = {
    tpe match {
      case String => Type.Name("String")
      case _ => super.typeConverter(tpe)
    }
  }

  /** E2 Introduces String values. */
  abstract override def toTargetLanguage(ei:domain.ExistsInstance) : CodeBlockWithResultingExpressions = {
    ei.inst match {
      case s:String => CodeBlockWithResultingExpressions(Scala(s""""$s"""").expression)
      case _ => super.toTargetLanguage(ei)
    }
  }

  abstract override def logic(exp:domain.DataType, op:domain.Operation): Seq[Statement] = {
    // generate the actual body
    op match {
      case PrettyP =>
        exp match {
          case Lit => result(Scala(s""" "" + ${expression(exp,litValue)} + "" """).expression)
          case Add => result(Scala(s""" "(" + ${dispatch(expression(exp,domain.base.left), PrettyP)} + "+" + ${dispatch(expression(exp,domain.base.right), PrettyP)}+ ")" """).expression)
          case Sub => result(Scala(s""" "(" + ${dispatch(expression(exp,domain.base.left), PrettyP)} + "-" + ${dispatch(expression(exp,domain.base.right), PrettyP)} + ")" """).expression)
          case _ => super.logic(exp, op)
        }

      case _ => super.logic(exp, op)
    }
  }

  abstract override def testGenerator: Seq[Seq[Stat]] = {
    super.testGenerator ++ testMethod(M2_tests)
  }
}
