package org.combinators.ep.domain.math.systemO   /*DD:LI:AI*/

import org.combinators.ep.domain._
import org.combinators.ep.domain.abstractions._
import org.combinators.ep.domain.instances.DataTypeInstance
import org.combinators.ep.domain.math.M0.{DoubleInst, Eval, LitInst}
import org.combinators.ep.domain.math.M2
import org.combinators.ep.domain.math.M2.{PrettyP, StringInst}

/**
 * Provides a corrective implementation for Eval/Lit though there is no visible different in output.
 */
object OA extends Evolution {
  override implicit def getModel:GenericModel = M2.getModel.evolve("oa", Seq.empty, Seq.empty)

  val oa: DataTypeInstance = LitInst(1.0)

  def tests: Seq[TestCase] = Seq(
    EqualsTestCase(getModel.baseDataType, oa, Eval, DoubleInst(1.0))
  )
}
