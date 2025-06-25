package org.combinators.ep.domain.math     /*DD:LI:AI*/

import org.combinators.cogen.InstanceRep
import org.combinators.cogen.abstractions.{Tag, TestCase}
import org.combinators.ep.domain.*
import org.combinators.ep.domain.abstractions.*
import org.combinators.ep.domain.math.M0.{AddInst, LitInst}
import org.combinators.ep.domain.math.M1.SubInst

object M2 extends Evolution {
  override implicit def getModel:GenericModel = M1.getModel.evolve("m2", Seq.empty, Seq(PrettyP))

  def StringInst(s:String): InstanceRep = InstanceRep(TypeRep.String)(s)

  lazy val PrettyP = Operation("prettyp", TypeRep.String)

  // Tests
  val m2_s1 = SubInst(LitInst(1.0), LitInst(2.0))

  case object LitPrettyPM2 extends Tag

  override def allTests: Map[GenericModel, Seq[TestCase]] = allPastTests(M1)

  def tests: Seq[TestCase] = Seq(
    EqualsTestCase(getModel.baseDataType, m2_s1, PrettyP, Seq(LitPrettyPM2), StringInst("(1.0-2.0)")),

    EqualsTestCase(getModel.baseDataType, AddInst(m2_s1, AddInst(LitInst(5.0), LitInst(6.0))),
      PrettyP, Seq(LitPrettyPM2), StringInst("((1.0-2.0)+(5.0+6.0))"))
  )
}
