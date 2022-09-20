package org.combinators.ep.domain.math     /*DD:LI:AI*/

import org.combinators.ep.domain.abstractions.TestCase
import org.combinators.ep.domain.{Evolution, GenericModel}

object I2M3I1N1 extends Evolution {
  override implicit def getModel:GenericModel = M3I1.getModel.extend("i2m3i1n1", Seq(I2.getModel, N1.getModel))

  // testing
  def tests: Seq[TestCase] = Seq(
  )
}

