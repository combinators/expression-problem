package example.expression.haskell    /*DD:LD:AI*/

import example.expression.domain.{Evolution, M1, MathDomain}

/**
  * Truly independent of the specific design solution.
  *
  * Still Java-based, naturally and JUnit
  */
trait e1 extends Evolution with HaskellGenerator with HUnitTestGenerator with M1 {
  self:e0 =>
  val domain:MathDomain
  import domain._

  abstract override def logic(exp:domain.Atomic, op:domain.Operation): Seq[Haskell] = {
    val atts = subExpressions(exp)
    // generate the actual body
    op match {
      case Eval =>
        exp match {

          case Sub => result(new Haskell(s"""${dispatch(atts(base.left), op)} - ${dispatch(atts(base.right), op)}"""))
          case _ => super.logic(exp, op)
        }

      case _ => super.logic(exp, op)
    }
  }

  abstract override def testGenerator: Seq[Haskell] = {
    super.testGenerator :+ hunitMethod(M1_tests)
  }
}
