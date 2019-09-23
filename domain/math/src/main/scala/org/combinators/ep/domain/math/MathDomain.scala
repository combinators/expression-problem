package org.combinators.ep.domain.math        /*DD:LI:AI*/

import org.combinators.ep.domain._
import org.combinators.ep.domain.abstractions.TestCase

/**
  * Mathematical Expressions domain as an instance of EP.
  */
object MathDomain extends Evolution {
  def getModel:Model = Model.base("MathDomain", "Exp")
  def tests: Seq[TestCase] = Seq.empty
}
