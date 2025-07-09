package org.combinators.ep.domain.shape    /*DD:LI:AI*/

import org.combinators.cogen.TestCase
import org.combinators.ep.domain._

/**
  * Shape domain as suitable for
  *
  * Synthesizing Object-Oriented and Functional Design to Promote Re-Use
  * Shriram Krishnamurthi, Matthias Felleisen, Daniel Friedman
  *
  * https://www.cs.rice.edu/~cork/teachjava/2003/readings/visitor1.pdf
  */

/**
  * Mathematical Expressions domain as an instance of EP.
  */
object ShapeDomain  extends Evolution {
  def getModel:GenericModel = baseModel
  def baseModel:GenericModel = GenericModel.base("ShapeDomain", "Shape")
  def tests: Seq[TestCase] = Seq.empty
}
