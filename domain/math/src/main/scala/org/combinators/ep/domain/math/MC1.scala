package org.combinators.ep.domain.math      /*DD:LI:AI*/

import org.combinators.ep.domain._

class MC1(val m3:M3, i2:I2) extends Evolution {

  val domain:BaseDomain = MathDomain
  import domain._
  import m3._
  import m2.{m1, PrettyP, String}
  import i2._
  import i1._
  import m1._
  import m0._

  // m3 x i2:model evolution. linearize ala topological sort
  // -------------------
  override def getModel = m3.getModel.merge("c1", i2.getModel)

  val mc1_s1 = new domain.BinaryInst(Sub, LitInst(1.0), LitInst(2.0))
  val mc1_d1 = new domain.BinaryInst(Divd, LitInst(1.0),
    new domain.BinaryInst(Sub, LitInst(1.0), LitInst(2.0)))
  val mc1_s2 = new domain.UnaryInst(Inv, mc1_s1)

  // testing
  def MC1_tests: Seq[TestCase] = Seq(
    EqualsTestCase(mc1_s2, PrettyP, ExistsInstance(String)("(1.0/(1.0-2.0))")),
    EqualsTestCase(mc1_d1, PrettyP, ExistsInstance(String)("(1.0/(1.0-2.0))"))
  )

}
