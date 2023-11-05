package org.combinators.ep.domain.math     /*DD:LI:AI*/

import org.combinators.ep.domain.{Evolution, GenericModel}
import org.combinators.ep.domain.abstractions.TestCase
import org.combinators.ep.domain.math.systemI.I2

object A1M3I2 extends Evolution {
  override implicit def getModel:GenericModel = A1M3.getModel.extend("a1m3i2", Seq(I2.getModel))

  // testing
  def tests: Seq[TestCase] = Seq(
  )
}

