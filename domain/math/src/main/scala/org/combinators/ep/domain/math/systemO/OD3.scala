package org.combinators.ep.domain.math.systemO   /*DD:LI:AI*/

import org.combinators.cogen.InstanceRep
import org.combinators.cogen.abstractions.TestCase
import org.combinators.ep.domain.*
import org.combinators.ep.domain.abstractions.*
import org.combinators.ep.domain.instances.DataTypeInstance
import org.combinators.ep.domain.math.M0.LitInst
import org.combinators.ep.domain.math.M2.{PrettyP, StringInst}
import org.combinators.ep.domain.math.{M2, MathDomain}

object OD3 extends Evolution {
  override implicit def getModel:GenericModel = OD2.getModel.extend("od3", Seq(OD1.getModel))

  override def allTests: Map[GenericModel, Seq[TestCase]] = allPastTests(OD2)

  // testing
  def tests: Seq[TestCase] = Seq(
  )
}
