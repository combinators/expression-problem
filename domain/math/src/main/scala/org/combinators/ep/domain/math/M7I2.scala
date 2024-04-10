package org.combinators.ep.domain.math      /*DD:LI:AI*/

import org.combinators.ep.domain._
import org.combinators.ep.domain.abstractions.TestCase
import org.combinators.ep.domain.math.systemI.I2

object M7I2 extends Evolution {
  override implicit def getModel:GenericModel = M7.getModel.merge("m7i2", Seq.empty, M6.isOps(Seq(I2.Power)), Seq(I2.getModel))

  /** Should include new tests here..... */
  override def allTests: Map[GenericModel, Seq[TestCase]] = allPastTests(M7, I2)

  // testing
  def tests: Seq[TestCase] = Seq(
  )
}

