package org.combinators.ep.domain.math.systemX    /*DD:LI:AI*/

import org.combinators.cogen.abstractions.TestCase
import org.combinators.ep.domain.{Evolution, GenericModel}

object X2X3 extends Evolution {
  override implicit def getModel: GenericModel = X2.getModel.extend("x2x3", Seq(X3.getModel))

  override def allTests: Map[GenericModel, Seq[TestCase]] = allPastTests(X2, X3)

  // testing
  def tests: Seq[TestCase] = Seq(
  )
}
