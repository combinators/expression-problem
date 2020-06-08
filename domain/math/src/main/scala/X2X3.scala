package org.combinators.ep.domain.math

import org.combinators.ep.domain.{Evolution, GenericModel}
import org.combinators.ep.domain.abstractions.TestCase

object X2X3 extends Evolution {
  override implicit def getModel:GenericModel = X2.getModel.extend("x2x3", Seq(X3.getModel))

  // testing
  def tests: Seq[TestCase] = Seq(
  )
}

