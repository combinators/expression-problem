package org.combinators.ep.domain.math.systemO   /*DD:LI:AI*/

import org.combinators.cogen.TestCase
import org.combinators.ep.domain.*
import org.combinators.ep.domain.abstractions.*
import org.combinators.ep.domain.instances.DataTypeInstance
import org.combinators.ep.domain.math.M0.{DoubleInst, Eval, LitInst}
import org.combinators.ep.domain.math.M2
import org.combinators.ep.domain.math.M2.{PrettyP, StringInst}

/**
 * Provides a corrective implementation for Eval/Lit though there is no visible different in output.
 */
object OA extends Evolution {
  override implicit def getModel:GenericModel =
    M2.getModel.evolve("oa", Seq.empty, Seq.empty)
      .optimize(Set((math.M0.Lit,math.M0.Eval)))

  // this optimization has no external impact, so it is fine to allow default test impl

  val oa: DataTypeInstance = LitInst(1.0)

  override def allTests: Map[GenericModel, Seq[TestCase]] = allPastTests(M2)

  def tests: Seq[TestCase] = Seq(
    EqualsTestCase(getModel.baseDataType, oa, Eval, DoubleInst(1.0))
  )
}
