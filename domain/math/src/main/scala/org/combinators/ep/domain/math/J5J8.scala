package org.combinators.ep.domain.math     /*DD:LI:AI*/

import org.combinators.ep.domain._
import org.combinators.ep.domain.abstractions.{EqualsTestCase, TestCase}
import org.combinators.ep.domain.math.J3.{PrettyP, StringInst}
import org.combinators.ep.domain.math.J4.PowerInst
import org.combinators.ep.domain.math.M0.LitInst
import org.combinators.ep.domain.math.M4.{Collect, ListDoubleInst, getModel, m4_d2, m4_s_0, m4_s_00, m4_s_12, m4_s_13}

object J5J8 extends Evolution {
  override implicit def getModel:GenericModel = J8.getModel.merge("j5j8", Seq.empty, Seq.empty, Seq(J5.getModel))

  // testing
  def tests: Seq[TestCase] = Seq(
    EqualsTestCase(getModel.baseDataType, PowerInst(LitInst(2.0), LitInst(5.0)), PrettyP, StringInst("(2.0^5.0)")),

    EqualsTestCase(getModel.baseDataType, m4_d2, Collect, ListDoubleInst(Seq(5.0, 7.0, 7.0, 2.0, 3.0))),
    EqualsTestCase(getModel.baseDataType, m4_s_00, Collect, ListDoubleInst(Seq(0.0, 0.0))),
    EqualsTestCase(getModel.baseDataType, m4_s_0, Collect, ListDoubleInst(Seq(0.0))),
    EqualsTestCase(getModel.baseDataType, m4_s_12, Collect, ListDoubleInst(Seq(1.0, 12.0))),
    EqualsTestCase(getModel.baseDataType, m4_s_13, Collect, ListDoubleInst(Seq(13.0, 1.0))),
  )
}

