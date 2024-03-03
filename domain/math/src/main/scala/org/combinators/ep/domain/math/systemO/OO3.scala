package org.combinators.ep.domain.math.systemO   /*DD:LI:AI*/

import org.combinators.ep.domain._
import org.combinators.ep.domain.abstractions._

object OO3 extends Evolution {
  override implicit def getModel:GenericModel = OO2.getModel.extend("oo3", Seq(OO1.getModel))

  // testing
  def tests: Seq[TestCase] = Seq(
  )
}
