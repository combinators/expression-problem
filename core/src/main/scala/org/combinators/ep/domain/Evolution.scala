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

  /** Common use case: Tests of this evolution are the tests of the immediate predecessors *and* this.tests, while past tests get combined per model  */
  def allPastTests(pastEvolutions:Evolution*): Map[GenericModel, Seq[TestCase]] = {
    val allPriorTests = pastEvolutions.map(_.allTests)
    val mergedPriorTests = allPriorTests.foldLeft[Map[GenericModel, Seq[TestCase]]](Map.empty) { case (s, t) =>
      s ++ t.map { case (k, v) => k -> ((v ++ s.getOrElse(k, Seq.empty)).distinct) }
    }

    mergedPriorTests +
      (getModel -> (tests ++ pastEvolutions.flatMap(e => mergedPriorTests.getOrElse(e.getModel, Seq.empty)).distinct))
  }
}
