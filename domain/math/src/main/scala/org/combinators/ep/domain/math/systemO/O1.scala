package org.combinators.ep.domain.math.systemO   /*DD:LI:AI*/

import org.combinators.ep.domain._
import org.combinators.ep.domain.abstractions._
import org.combinators.ep.domain.instances.DataTypeInstance
import org.combinators.ep.domain.math.M0.LitInst
import org.combinators.ep.domain.math.M2
import org.combinators.ep.domain.math.M2.{PrettyP, StringInst}

/**
 * Provides a corrective implementation for PrettyP/Lit by "doubling" the output. Note that any optimization
 * performed by overwriting an implementation should have output that is indistinguishable from the implementation
 * that it replaces, but this was done to make the effect noticeably visible and see which AIPs can support it
 */
object O1 extends Evolution {
  override implicit def getModel:GenericModel = M2.getModel.evolve("o1", Seq.empty, Seq.empty)

  val o1: DataTypeInstance = LitInst(1.0)

  def tests: Seq[TestCase] = Seq(

    EqualsTestCase(getModel.baseDataType, o1, PrettyP, StringInst("1.01.0")),
  )
}
