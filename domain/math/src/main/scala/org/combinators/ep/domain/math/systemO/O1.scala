package org.combinators.ep.domain.math.systemO   /*DD:LI:AI*/

import org.combinators.cogen.{Tag, TestCase}
import org.combinators.ep.domain.*
import org.combinators.ep.domain.abstractions.*
import org.combinators.ep.domain.instances.DataTypeInstance
import org.combinators.ep.domain.math.M0.{Eval, LitInst}
import org.combinators.ep.domain.math.{M1, M2}
import org.combinators.ep.domain.math.M2.{PrettyP, StringInst, getModel}

/**
 * Provides a corrective implementation for PrettyP/Lit by "doubling" the output. Note that any optimization
 * performed by overwriting an implementation should have output that is indistinguishable from the implementation
 * that it replaces, but this was done to make the effect noticeably visible and see which AIPs can support it
 */
object O1 extends Evolution {
  override implicit def getModel:GenericModel =
    M2.getModel.evolve("o1", Seq.empty, Seq.empty)
      .optimize(Set((math.M0.Lit,math.M2.PrettyP)))

  val o1: DataTypeInstance = LitInst(1.0)

  case object LitPrettyPO1 extends Tag

  /**
   * Remove past test as no longer being correct, since it was "fixed" in this implementation.
   * These could be in any test case tagged with LitPrettyPM2 in any former tests
   */
  override def allTests: Map[GenericModel, Seq[TestCase]] = {
    val pastOnes = allPastTests(M2)

    pastOnes.map {case (m,stc) => (m, stc.filterNot(tc => tc.tags.contains(M2.LitPrettyPM2))) }
  }

  def tests: Seq[TestCase] = Seq(
    EqualsTestCase(getModel.baseDataType, o1, PrettyP, Seq(LitPrettyPO1), StringInst("1.01.0")),
  )
}
