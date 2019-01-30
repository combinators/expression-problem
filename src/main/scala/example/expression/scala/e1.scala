package example.expression.scala   /*DD:LD:AI*/

import example.expression.domain.{Evolution, M1, MathDomain}
import org.combinators.templating.twirl.Java

import scala.meta.Stat

/**
  * Truly independent of the specific design solution.
  *
  * Still Java-based, naturally and JUnit
  */
trait e1 extends Evolution with ScalaGenerator with TestGenerator with M1 {
  self:e0 =>
  val domain:MathDomain

  abstract override def logic(exp:domain.Atomic, op:domain.Operation): Seq[Statement] = {
    // generate the actual body
    op match {
      case Eval =>
        exp match {
          case Sub => result(Scala(s"${dispatch(expression(exp, domain.base.left), Eval)} - ${dispatch(expression(exp, domain.base.right), Eval)}").expression)
          case _ => super.logic(exp, op)
        }

      case _ => super.logic(exp, op)
    }
  }

  abstract override def testGenerator: Seq[Seq[Stat]] = {
    super.testGenerator ++ testMethod(M1_tests)
  }
}
