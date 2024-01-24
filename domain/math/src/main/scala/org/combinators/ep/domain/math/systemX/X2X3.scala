package org.combinators.ep.domain.math.systemX    /*DD:LI:AI*/

import org.combinators.ep.domain.abstractions.TestCase
import org.combinators.ep.domain.{Evolution, GenericModel}

object X2X3 extends Evolution {
  override implicit def getModel: GenericModel = X2.getModel.extend("x2x3", Seq(X3.getModel))

  // testing
  def tests: Seq[TestCase] = Seq(
  )
}
