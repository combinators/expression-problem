package org.combinators.ep.domain.math      /*DD:LI:AI*/

import org.combinators.ep.domain._
import org.combinators.ep.domain.abstractions.TestCase

object M7I2 extends Evolution {
  override implicit def getModel:GenericModel = M7.getModel.extend("m7i2", Seq(I2.getModel))

  // testing
  def tests: Seq[TestCase] = Seq(
  )
}

