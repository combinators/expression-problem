package org.combinators.ep.domain.math.systemO   /*DD:LI:AI*/

import org.combinators.ep.domain._
import org.combinators.ep.domain.abstractions._

/**
 * Combines together two different evolutions, each of which has an independent Op/TypeCase whose implementation
 * has been optimized, as declared by the EIP.
 */
object O1OA extends Evolution {
  override implicit def getModel:GenericModel = O1.getModel.extend("o1oa", Seq(OA.getModel))

  // testing
  def tests: Seq[TestCase] = Seq(
  )
}
