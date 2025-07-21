package org.combinators.ep.domain.math     /*DD:LI:AI*/

import org.combinators.ep.domain.instances.{DataTypeInstance, DataTypeInstanceRep}
import org.combinators.cogen.TestCase
import org.combinators.ep.domain.{Evolution, GenericModel}
import org.combinators.ep.domain.abstractions.EqualsCompositeTestCase
import org.combinators.ep.domain.math.M0.{Eval, LitInst}
import org.combinators.ep.domain.math.M3.MultInst
import org.combinators.ep.domain.math.systemI.I1

object A1M3 extends Evolution {
  override implicit def getModel:GenericModel = M3.getModel.extend("a1m3", Seq(A1.getModel))

  // Tests
  val a1m3_t: DataTypeInstance = MultInst(LitInst(5.0), LitInst(2.0))

  /** Could include new tests here, and new ones */
  override def allTests: Map[GenericModel, Seq[TestCase]] = allPastTests(M3, A1)

  def tests: Seq[TestCase] = Seq(
    EqualsCompositeTestCase(getModel.baseDataType, a1m3_t, M0.DoubleInst(30.0), (I1.MultBy, Seq(DataTypeInstanceRep(LitInst(3.0)))), (Eval, Seq.empty))
  )
}

