package org.combinators.ep.language.haskell  /*DD:LD:AI*/

import org.combinators.ep.domain.math.M0

/**
  * Truly independent of the specific design solution.
  *
  * Still Java-based, naturally and JUnit
  */
trait e0 extends HaskellGenerator with HUnitTestGenerator with M0 {
  import domain._

  /** E0 Introduces Double and Int values. */
  abstract override def toTargetLanguage(ei:domain.ExistsInstance) : CodeBlockWithResultingExpressions = {
    ei.inst match {
      case d:scala.Double => {
        // Can't seem to use unary negation operation in Haskell ?!
        val expect = if (d < 0) {
          s"(0 - ${math.abs(d)})"
        } else {
          d.toString
        }
        CodeBlockWithResultingExpressions(Haskell(s"$expect"))
      }

      case i:scala.Int => CodeBlockWithResultingExpressions(Haskell(s"$i"))

      case _ => super.toTargetLanguage(ei)
    }
  }

  /** E0 Introduces the concept a Double type, used for the 'Eval' operation. */
  abstract override def typeConverter(tpe:TypeRep) : HaskellType = {
    tpe match {
      case Double => new HaskellType("Double")
      case Int => new HaskellType("Int")
      case _ => super.typeConverter(tpe)
    }
  }

  /** Provide reasonable default values for newly defined types. */
  abstract override def standardDefault(tpe:TypeRep) : Haskell = {
    tpe match {
      case Int =>  Haskell("0")
      case Double => new Haskell("0.0")
      case _ => super.standardDefault(tpe)
    }
  }

  /** Eval operation needs to provide specification for current datatypes, namely Lit and Add. */
  abstract override def logic(exp:DataType, op:Operation): Seq[HaskellStatement] = {
    // generate the actual body
    op match {
      case Eval =>
        exp match {
          case Lit => result(new Haskell(s"""${expression(exp,litValue)}"""))
          case Add => result(new Haskell(s"""${dispatch(expression(exp,base.left), op)} + ${dispatch(expression(exp,base.right), op)}"""))
          case _ => super.logic(exp, op)
        }

      // all future EXP sub-types can simply return hashcode.
      // move to m5
      //case Identifier => result(new Haskell(s"${exp.hashCode()}"))
      case _ => super.logic(exp, op)
    }
  }

  abstract override def testGenerator: Seq[UnitTest] = {
    super.testGenerator ++ testMethod(M0_tests)
  }
}
