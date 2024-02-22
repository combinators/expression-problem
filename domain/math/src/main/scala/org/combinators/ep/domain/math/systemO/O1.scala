package org.combinators.ep.domain.math.systemO   /*DD:LI:AI*/

import org.combinators.ep.domain._
import org.combinators.ep.domain.abstractions._
import org.combinators.ep.domain.instances._
import org.combinators.ep.domain.math.M0.{Eval, LitInst}
import org.combinators.ep.domain.math.M2
import org.combinators.ep.domain.math.M2.{PrettyP, StringInst}

object O1 extends Evolution {
  override implicit def getModel:GenericModel = M2.getModel.evolve("o1", Seq.empty, Seq.empty)

  val o1 = LitInst(1.0)

  def tests: Seq[TestCase] = Seq(

    EqualsTestCase(getModel.baseDataType, o1, PrettyP, StringInst("1.01.0")),
  )
}
