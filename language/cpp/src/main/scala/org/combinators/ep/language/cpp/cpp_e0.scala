package org.combinators.ep.language.cpp  /*DD:LD:AI*/

import org.combinators.ep.domain.math.M0

/**
  * Truly independent of the specific design solution.
  *
  * Still C++-based, naturally and CPPUnit
  */
trait cpp_e0 extends CPPGenerator with TestGenerator with M0 {
  import domain._

  /** E0 Introduces the concept a Double type, used for the 'Eval' operation. */
  abstract override def typeConverter(tpe:TypeRep) : CPPType = {
    tpe match {
      case Double => new CPPType("double")
      case Int => new CPPType("int")
      case _ => super.typeConverter(tpe)
    }
  }

  /** E0 Introduces Double and Int values. */
  abstract override def toTargetLanguage(ei:domain.ExistsInstance) : CodeBlockWithResultingExpressions = {
    ei.inst match {
      case d:scala.Double => CodeBlockWithResultingExpressions(new CPPExpression(s"$d"))
      case i:scala.Int => CodeBlockWithResultingExpressions(new CPPExpression(s"$i"))
      case _ => super.toTargetLanguage(ei)
    }
  }

  /** Eval operation needs to provide specification for current datatypes, namely Lit and Add. */
  abstract override def logic(exp:DataType, op:Operation): Seq[CPPStatement] = {
    // generate the actual body
    op match {
      case Eval =>
        exp match {
          case Lit => result(valueOf(expression(exp,litValue)))
          case Add => result(new CPPExpression(s"${dispatch(expression(exp,base.left), op)} + ${dispatch(expression(exp,base.right), op)}"))

          case _ => super.logic(exp, op)
        }

      // all future EXP sub-types can simply return hashcode.
      //case Identifier => result(new CPPExpression(exp.hashCode().toString))

      case _ => super.logic(exp, op)
    }
  }

  abstract override def testGenerator: Seq[Seq[CPPElement]] = {
    super.testGenerator ++ testMethod(M0_tests)
  }
}
