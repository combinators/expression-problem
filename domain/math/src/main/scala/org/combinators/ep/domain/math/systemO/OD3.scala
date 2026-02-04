package org.combinators.ep.domain.math.systemO   /*DD:LI:AI*/

import org.combinators.cogen.TestCase
import org.combinators.ep.domain._

object OD3 extends Evolution {
  override implicit def getModel:GenericModel = OD2.getModel.extend("od3", Seq(OD1.getModel))

  override def allTests: Map[GenericModel, Seq[TestCase]] = allPastTests(OD2)

  // testing
  def tests: Seq[TestCase] = Seq(
  )
}
