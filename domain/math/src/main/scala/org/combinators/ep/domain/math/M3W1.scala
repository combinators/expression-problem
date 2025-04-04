package org.combinators.ep.domain.math   /*DD:LI:AI*/

import org.combinators.ep.domain._
import org.combinators.ep.domain.abstractions.{DataTypeCase, TestCase}

object M3W1 extends Evolution {
  override implicit def getModel:GenericModel = M3.getModel.extend("m3w1", Seq(W1.getModel))

  override def allTests: Map[GenericModel, Seq[TestCase]] = allPastTests(M3, W1)


  // testing
  def tests: Seq[TestCase] = Seq(
  )
}

