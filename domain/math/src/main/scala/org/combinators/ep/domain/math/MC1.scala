package org.combinators.ep.domain.math      /*DD:LI:AI*/

import org.combinators.ep.domain._
import org.combinators.ep.domain.abstractions.{EqualsTestCase, TestCase}
import org.combinators.ep.domain.math.M0.{Eval, LitInst}
import org.combinators.ep.domain.math.M1.SubInst
import org.combinators.ep.domain.math.M2.{PrettyP,StringInst}
import org.combinators.ep.domain.math.M3.DivdInst
import org.combinators.ep.domain.math.I1.InvInst

object MC1 extends Evolution {
  override implicit def getModel:Model = M3.getModel.merge("c1", I2.getModel)


  // m3 x i2:model evolution. linearize ala topological sort
  // -------------------
  val mc1_s1 = SubInst(LitInst(1.0), LitInst(2.0))
  val mc1_d1 = DivdInst(LitInst(1.0),
    SubInst(LitInst(1.0), LitInst(2.0)))
  val mc1_s2 = InvInst(mc1_s1)

  // testing
  def tests: Seq[TestCase] = Seq(
    EqualsTestCase(getModel.baseDataType, mc1_s2, PrettyP, StringInst("(1.0/(1.0-2.0))")),
    EqualsTestCase(getModel.baseDataType, mc1_d1, PrettyP, StringInst("(1.0/(1.0-2.0))"))
  )
}

