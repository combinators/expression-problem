package org.combinators.ep.domain.math     /*DD:LI:AI*/

import org.combinators.ep.domain.{Evolution, GenericModel}
import org.combinators.ep.domain.abstractions.TestCase

object A1M3 extends Evolution {
  override implicit def getModel:GenericModel = M3.getModel.extend("a1m3", Seq(A1.getModel))

  // testing
  def tests: Seq[TestCase] = Seq(
  )
}

