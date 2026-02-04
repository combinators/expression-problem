package org.combinators.ep.language.haskell        /*DD:LD:AI*/

import org.combinators.ep.domain.Evolution
import org.combinators.ep.domain.math.{M0, M1, M2, MathDomain}

/**
  * Truly independent of the specific design solution.
  */
trait e2 extends Evolution with HaskellGenerator with HUnitTestGenerator with M0 with M1 with M2 {
  self:e0 with e1 =>
  val domain:MathDomain
  import domain._

  /** E2 Introduces String values. */
  abstract override def toTargetLanguage(ei:domain.ExistsInstance) : CodeBlockWithResultingExpressions = {
    ei.inst match {
      case s:String => CodeBlockWithResultingExpressions(Haskell("\"" + s + "\""))
      case _ => super.toTargetLanguage(ei)
    }
  }

  abstract override def typeConverter(tpe:TypeRep) : HaskellType = {
    tpe match {
      case String => new HaskellType("String")
      case _ => super.typeConverter(tpe)
    }
  }

  /** Provide reasonable default values for newly defined types. */
  abstract override def standardDefault(tpe:TypeRep) : Haskell = {
    tpe match {
      case String => new Haskell("\"\"")
      case _ => super.standardDefault(tpe)
    }
  }

  abstract override def logic(exp:DataType, op:Operation): Seq[HaskellStatement] = {
    // generate the actual body
    op match {
      case PrettyP =>
        exp match {
          case Lit => result(Haskell(s"(show ${expression(exp,litValue)})"))
          case Add => result(Haskell(s""""(" ++ ${dispatch(expression(exp,base.left), op)} ++ "+" ++ ${dispatch(expression(exp,base.right), op)} ++ ")""""))
          case Sub => result(Haskell(s""""(" ++ ${dispatch(expression(exp,base.left), op)} ++ "-" ++ ${dispatch(expression(exp,base.right), op)} ++ ")""""))
          case _ => super.logic(exp, op)
        }

      case _ => super.logic(exp, op)
    }
  }

  abstract override def testGenerator: Seq[UnitTest] = {
    super.testGenerator ++ testMethod(M2_tests)
  }
}
