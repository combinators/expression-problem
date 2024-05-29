package org.combinators.ep.domain.math.systemO   /*DD:LI:AI*/

import org.combinators.ep.domain._
import org.combinators.ep.domain.abstractions._
import org.combinators.ep.domain.math.M2
import org.combinators.ep.domain.math.systemO.O1.allPastTests

/**
 * Combines together two different evolutions, each of which has an independent Op/TypeCase whose implementation
 * has been optimized, as declared by the EIP.
 */
object O1OA extends Evolution {
  override implicit def getModel:GenericModel = O1.getModel.extend("o1oa", Seq(OA.getModel))

  /** Could include new tests here, but there are none that come to mind. */
   override def allTests: Map[GenericModel, Seq[TestCase]] = {
    val pastOnes = allPastTests(O1, OA)

    pastOnes.map {case (m,stc) => (m, stc.filterNot(tc => tc.tags.contains(M2.LitPrettyPM2))) }
  }

  // testing
  def tests: Seq[TestCase] = Seq(
  )
}
