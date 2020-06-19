package org.combinators.ep.domain.math     /*DD:LI:AI*/

import org.combinators.ep.domain._
import org.combinators.ep.domain.abstractions._
import org.combinators.ep.domain.instances.InstanceRep
import org.combinators.ep.domain.math.M0.{AddInst, LitInst}
import org.combinators.ep.domain.math.M1.SubInst

object M2 extends Evolution {
  override implicit def getModel:GenericModel = M1.getModel.evolve("m2", Seq.empty, Seq(PrettyP))

  def StringInst(s:String): InstanceRep = InstanceRep(TypeRep.String)(s)

  lazy val PrettyP = Operation("prettyp", TypeRep.String)

  // Tests
  val m2_s1 = SubInst(LitInst(1.0), LitInst(2.0))
  def tests: Seq[TestCase] = Seq(
    //EqualsTestCase(m2_s1, PrettyP, ExistsInstance(String)("(1.0-2.0)")),
    EqualsTestCase(getModel.baseDataType, m2_s1, PrettyP, StringInst("(1.0-2.0)")),

    EqualsTestCase(getModel.baseDataType, AddInst(m2_s1, AddInst(LitInst(5.0), LitInst(6.0))),
      PrettyP, StringInst("((1.0-2.0)+(5.0+6.0))"))
  )
}
