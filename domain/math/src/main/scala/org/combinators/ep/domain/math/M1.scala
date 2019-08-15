package org.combinators.ep.domain.math      /*DD:LI:AI*/

import org.combinators.ep.domain._

class M1(val m0:M0) extends Evolution {

  val domain: m0.domain.type = m0.domain
  import domain._
  import m0._

  // m1:model evolution.
  // -------------------
  case object Sub extends Binary("Sub")

  val m1 = Model("m1", Seq(Sub), Seq.empty, last = m0.getModel)

  override def getModel: Model = m1

  // testing
  def M1_tests: Seq[TestCase] = Seq(
    EqualsTestCase(new domain.BinaryInst(Sub, LitInst(1.0), LitInst(2.0)), Eval, ExistsInstance(Double)(-1.0))
  )
}
