package org.combinators.ep.domain.math   /*DD:LI:AI*/

import org.combinators.ep.domain.abstractions.TestCase
import org.combinators.ep.domain.{Evolution, GenericModel}

object M3I1 extends Evolution {
  override implicit def getModel:GenericModel = M3.getModel.extend("m3i1", Seq(I1.getModel))

  // testing
  def tests: Seq[TestCase] = Seq(
  )
}

