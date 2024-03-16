package org.combinators.ep.domain    /*DI:LI:AI*/

import org.combinators.ep.domain.abstractions.TestCase

/** Models a point in time of an evolving data type. */
trait Evolution {
  /** Returns the domain model associated with this point in time.
    * Any [[org.combinators.ep.domain.GenericModel]] also stores its evolution history. */
  implicit def getModel: GenericModel

  /** These are new tests just for this Evolution, which may be tagged. */
  def tests: Seq[TestCase]

  /** By default, just gives new tests. */
  def allTests: Map[GenericModel, Seq[TestCase]] = Map(getModel -> tests)

  /** Common use case. Include all past tests. */
  def allPastTests(pastEvolution:Evolution): Map[GenericModel, Seq[TestCase]] = pastEvolution.allTests +
    (getModel -> (tests ++ pastEvolution.allTests.values.flatten))
}
