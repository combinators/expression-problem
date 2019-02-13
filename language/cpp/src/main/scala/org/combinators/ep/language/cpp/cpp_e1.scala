package ep.cpp      /*DD:LD:AI*/

import org.combinators.ep.domain.Evolution
import org.combinators.ep.domain.math.M1

/**
  * Truly independent of the specific design solution.
  *
  * Still C++-based, naturally and CPPUnit
  */
trait cpp_e1 extends Evolution with CPPGenerator with TestGenerator with M1 {
  self:cpp_e0 =>

  import domain._

  /** Eval operation needs to provide specification for current datatypes, namely Lit and Add. */
  abstract override def logic(exp:Atomic, op:Operation): Seq[CPPElement] = {
    // generate the actual body
    op match {
      case Eval =>
        exp match {
          case Sub => result(new CPPElement(s"${dispatch(expression(exp, base.left), op)} - ${dispatch(expression(exp, base.right), op)}"))
          case _ => super.logic(exp, op)
        }

      case _ => super.logic(exp, op)
    }
  }

  abstract override def testGenerator: Seq[Seq[CPPElement]] = {
    super.testGenerator ++ testMethod(M1_tests)
  }
}
