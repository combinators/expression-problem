package org.combinators.ep.domain.math      /*DD:LI:AI*/

import org.combinators.ep.domain.abstractions.TestCase
import org.combinators.ep.domain.{Evolution, GenericModel}

object D1D2 extends Evolution {
  override implicit def getModel:GenericModel = D1.getModel.merge("d1d2", Seq.empty, Seq.empty, Seq(D2.getModel))

  // testing
  def tests: Seq[TestCase] = Seq(
  )
}

