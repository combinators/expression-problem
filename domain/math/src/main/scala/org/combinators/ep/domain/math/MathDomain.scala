package org.combinators.ep.domain.math        /*DD:LI:AI*/

import org.combinators.cogen.TestCase
import org.combinators.ep.domain._

/** Mathematical Expressions domain as an instance of EP. */
object MathDomain extends Evolution {
  def getModel:GenericModel = baseModel
  def baseModel:GenericModel = GenericModel.base("MathDomain", "Exp")
  def tests: Seq[TestCase] = Seq.empty
}
