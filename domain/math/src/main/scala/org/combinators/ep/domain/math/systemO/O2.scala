package org.combinators.ep.domain.math.systemO   /*DD:LI:AI*/

import org.combinators.ep.domain._
import org.combinators.ep.domain.abstractions._
import org.combinators.ep.domain.instances.DataTypeInstance
import org.combinators.ep.domain.math.M0.LitInst
import org.combinators.ep.domain.math.M2
import org.combinators.ep.domain.math.M2.{PrettyP, StringInst}

object O2 extends Evolution {
  override implicit def getModel:GenericModel = O1.getModel.evolve("o2", Seq.empty, Seq.empty)

  val o2: DataTypeInstance = LitInst(1.0)

  def tests: Seq[TestCase] = Seq(

    EqualsTestCase(getModel.baseDataType, o2, PrettyP, StringInst("1.01.01.0")),
  )
}
