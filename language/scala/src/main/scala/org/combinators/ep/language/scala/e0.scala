package org.combinators.ep.language.scala   /*DD:LD:AI*/

import org.combinators.ep.domain.math.M0

import scala.meta.Stat

/**
  * Truly independent of the specific design solution.
  *
  * Still Java-based, naturally and JUnit
  */
trait e0 extends ScalaGenerator with TestGenerator with M0 {
  import domain._

  /** E0 Introduces the concept a Double type, used for the 'Eval' operation. */
  abstract override def typeConverter(tr:TypeRep) : Type = {
    tr match {
      case Double => Scala("Double").tpe
      case Int => Scala("Int").tpe
      case _ => super.typeConverter(tr)
    }
  }

  /** E0 Introduces Double and Int values. */
  abstract override def toTargetLanguage(ei:domain.ExistsInstance) : CodeBlockWithResultingExpressions = {
    ei.inst match {
      case d:scala.Double => CodeBlockWithResultingExpressions(Scala(s"$d").expression)
      case i:scala.Int => CodeBlockWithResultingExpressions(Scala(s"$i").expression)
      case _ => super.toTargetLanguage(ei)
    }
  }

  /** Eval operation needs to provide specification for current datatypes, namely Lit and Add. */
  abstract override def logic(exp:DataType, op:Operation): Seq[Statement] = {
    // generate the actual body
    op match {
      case Eval =>
        exp match {
          case Lit => result(Scala(s"${expression(exp, litValue)}").expression)
          case Add => result(Scala(s"${dispatch(expression(exp, base.left),op)} + ${dispatch(expression(exp, base.right),op)}").expression)
          case _ => super.logic(exp, op)
        }

      // all future EXP sub-types can simply return hashcode.
      case Identifier => result(Scala(s"${exp.hashCode()}").expression)

      case _ => super.logic(exp, op)
    }
  }

  abstract override def testGenerator: Seq[Seq[Stat]] = {
    super.testGenerator ++ testMethod(M0_tests)
  }
}
