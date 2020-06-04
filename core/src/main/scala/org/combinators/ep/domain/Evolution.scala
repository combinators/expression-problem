package org.combinators.ep.domain    /*DI:LI:AI*/

import org.combinators.ep.domain.abstractions.TestCase

/** Models a point in time of an evolving data type. */
trait Evolution {
  /** Returns the domain model associated with this point in time.
    * Any [[org.combinators.ep.domain.Model]] also stores its evolution history. */
  implicit def getModel: GenericModel

  def tests: Seq[TestCase]
}
