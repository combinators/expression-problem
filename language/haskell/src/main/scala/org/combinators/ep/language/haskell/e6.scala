package org.combinators.ep.language.haskell    /*DD:LD:AI*/

import org.combinators.ep.domain.Evolution
import org.combinators.ep.domain.math._

/**
  * Truly independent of the specific design solution.
  */
trait e6 extends Evolution with HaskellGenerator with HUnitTestGenerator with M0 with M1 with M2 with M3 with M4 with M5 with M6 {
  self:e0 with e1 with e2 with e3 with e4 with e5 =>
  val domain:MathDomain
  import domain._

  abstract override def typeConverter(tpe:TypeRep) : HaskellType = {
    tpe match {
      case Boolean => new HaskellType("Bool")
      case _ => super.typeConverter(tpe)
    }
  }

  /**
    * Operations can declare dependencies, which leads to #include extras
    *
    * Haskell implementation no longer needs AsTree (for grow, but what about others?)
    */
  override def dependency(op: domain.Operation): scala.List[domain.Operation] = {
    op match {
      case Equals => scala.List[domain.Operation](domain.AsTree)
      case _ => super.dependency(op)
    }
  }

  /** Provide reasonable default values for newly defined types. */
  abstract override def standardDefault(tpe:TypeRep) : Haskell = {
    tpe match {
      case Boolean => new Haskell("False")
      case _ => super.standardDefault(tpe)
    }
  }

  /**
    * We need to take action with equals operations and provide default fall-through case
    * @param op
    * @return
    */
  abstract override def requireDefault(op:domain.Operation) : Option[(Int,Haskell)] = {
    op match {
      case Equals => Some((2,standardDefault(op.returnType)))
      case _ => super.requireDefault(op)
    }
  }

  abstract override def logic(exp:DataType, op:Operation): Seq[HaskellStatement] = {
    // generate the actual body
    op match {
      case Equals =>
        exp match {
          case Lit  =>
            val value2 =  Haskell(expression(exp, litValue).getCode + "2")
            result(Haskell(s" ${expression(exp, litValue)} == $value2 "))

          case u:Unary  =>
            val inner2 = Haskell(expression(exp,base.inner).getCode + "2")
            result(Haskell(s" ${dispatch(expression(exp,base.inner), op, inner2)} "))

          case b:Binary =>
            val left2 = Haskell(expression(exp, base.left).getCode + "2")
            val right2 = Haskell(expression(exp, base.right).getCode + "2")
            result(Haskell(s" ${dispatch(expression(exp, base.left), op, left2)} && ${dispatch(expression(exp, base.right), op, right2)} "))

          case _ => super.logic(exp, op)
        }

      case _ => super.logic(exp, op)
    }
  }

  override def hunitTestMethod(test:domain.TestCase, idx:Int) : Seq[Statement] = {
    test match {
      case eb: EqualsBinaryMethodTestCase =>
        val leftBlock = toTargetLanguage(eb.inst1)
        val rightBlock = toTargetLanguage(eb.inst2)
        leftBlock.appendDependent { case Seq(leftExp) =>
          rightBlock.appendDependent { case Seq(rightExp) =>
            CodeBlockWithResultingExpressions(
              if (eb.result) {
                HaskellStatement(s"""test_v$idx = TestCase (assertBool "EqualsCheck" (${dispatch(leftExp, Equals, rightExp)}))""")
              } else {
                HaskellStatement(s"""test_v$idx = TestCase (assertBool "NotEqualsCheck" (not (${dispatch(leftExp, Equals, rightExp)})))""")
              }
            )()
          }
        }.block
      case _ => super.hunitTestMethod(test, idx)
    }
  }

  abstract override def testGenerator: Seq[UnitTest] = {
    super.testGenerator ++ testMethod(M6_tests)
  }
}
