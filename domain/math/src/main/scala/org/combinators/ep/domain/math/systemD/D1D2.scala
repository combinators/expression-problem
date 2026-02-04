package org.combinators.ep.domain.math.systemD    /*DD:LI:AI*/

import org.combinators.cogen.TestCase
import org.combinators.ep.domain.{Evolution, GenericModel}

object D1D2 extends Evolution {
  override implicit def getModel: GenericModel = D1.getModel.merge("d1d2", Seq.empty, Seq.empty, Seq(D2.getModel))

  override def allTests: Map[GenericModel, Seq[TestCase]] = allPastTests(D1, D2)

  // testing
  def tests: Seq[TestCase] = Seq(
  )
}
