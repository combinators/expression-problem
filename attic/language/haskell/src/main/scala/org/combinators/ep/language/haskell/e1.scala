package org.combinators.ep.language.haskell    /*DD:LD:AI*/

import org.combinators.ep.domain.Evolution
import org.combinators.ep.domain.math.{M1, MathDomain}

/**
  * Truly independent of the specific design solution.
  */
trait e1 extends Evolution with HaskellGenerator with HUnitTestGenerator with M1 {
  self:e0 =>
  val domain:MathDomain
  import domain._

  abstract override def logic(exp:domain.DataType, op:domain.Operation): Seq[HaskellStatement] = {
    op match {
      case Eval =>
        exp match {
          case Sub => result(new Haskell(s"""${dispatch(expression(exp,base.left), op)} - ${dispatch(expression(exp,base.right), op)}"""))
          case _ => super.logic(exp, op)
        }

      case _ => super.logic(exp, op)
    }
  }

  abstract override def testGenerator: Seq[UnitTest] = {
    super.testGenerator ++ testMethod(M1_tests)
  }
}
