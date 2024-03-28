package org.combinators.ep.domain.math.systemO   /*DD:LI:AI*/

import org.combinators.ep.domain._
import org.combinators.ep.domain.abstractions._
import org.combinators.ep.domain.instances.DataTypeInstance
import org.combinators.ep.domain.math.M0.LitInst
import org.combinators.ep.domain.math.M2
import org.combinators.ep.domain.math.M2.{PrettyP, StringInst}

object O2 extends Evolution {
  override implicit def getModel:GenericModel =
    O1.getModel.evolve("o2", Seq.empty, Seq.empty)
      .optimize(Set((math.M0.Lit,math.M2.PrettyP)))

  val o2: DataTypeInstance = LitInst(1.0)

  /** Remove past test as no longer being correct, since it was "fixed" in this implementation to be triplicated. */
  override def allTests: Map[GenericModel, Seq[TestCase]] = O1.allTests +
    (getModel -> (tests ++ O1.allTests.values.flatten)
      .filterNot(tc => tc.tags.contains(M2.LitPrettyPM2))          // original
      .filterNot(tc => tc.tags.contains(O1.LitPrettyPO1)))         // first patch that "duplicated"

  def tests: Seq[TestCase] = Seq(

    EqualsTestCase(getModel.baseDataType, o2, PrettyP, StringInst("1.01.01.0")),
  )
}