package org.combinators.ep.domain.math.systemO   /*DD:LI:AI*/

import org.combinators.ep.domain._
import org.combinators.ep.domain.abstractions._
import org.combinators.ep.domain.instances.{DataTypeInstance, InstanceRep}
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
