package org.combinators.ep.language.gj     /*DD:LD:AI*/

import org.combinators.ep.domain.Evolution
import org.combinators.ep.domain.math.{M1, MathDomain}

/**
  * Truly independent of the specific design solution.
  */
trait e1 extends Evolution with GJGenerator with TestGenerator with M1 {
  self:e0 =>
  val domain:MathDomain

  abstract override def logic(exp:domain.DataType, op:domain.Operation): Seq[GJStatement] = {
    op match {
      case Eval =>
        exp match {
          case Sub => result(GJ(s"return new Double(${dispatch(expression(exp,domain.base.left), Eval)}.doubleValue() - ${dispatch(expression(exp,domain.base.right), Eval)}.doubleValue());"))
          case _ => super.logic(exp, op)
        }

      case _ => super.logic(exp, op)
    }
  }

  abstract override def testGenerator: Seq[UnitTest] = {
    super.testGenerator ++ testMethod(M1_tests)
//    val s1 = new domain.BinaryInst(Sub, LitInst(1.0), LitInst(2.0))
//    val modName = getModel.name
//
//    // TODO: test cases not yet confirmed for GJ
//    super.testGenerator :+ Seq(GJStatement(
//      s"""|   Lang$modName l = new Lang$modName();
//          |   assertEquals(-1.0, ${testDispatch(toTargetLanguage(s1).resultingExpressions.head, Eval)});
//          |""".stripMargin))
  }
}
