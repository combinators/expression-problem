package org.combinators.ep.domain.math.systemD    /*DD:LI:AI*/

import org.combinators.ep.domain.abstractions.{EqualsTestCase, Operation, TestCase, TypeRep}
import org.combinators.ep.domain.instances.InstanceRep
import org.combinators.ep.domain.math.M0.{AddInst, LitInst}
import org.combinators.ep.domain.math.M1.SubInst
import org.combinators.ep.domain.{Evolution, GenericModel}

object D3 extends Evolution {
  override implicit def getModel: GenericModel = D1D2.getModel.evolve("d3", Seq.empty, Seq(PrettyP))

  def StringInst(s: String): InstanceRep = InstanceRep(TypeRep.String)(s)

  lazy val PrettyP = Operation("prettyp", TypeRep.String)

  // Tests
  val m2_s1 = SubInst(LitInst(1.0), LitInst(2.0))

  override def allTests: Map[GenericModel, Seq[TestCase]] = allPastTests(D1D2)

  def tests: Seq[TestCase] = Seq(
    //EqualsTestCase(m2_s1, PrettyP, ExistsInstance(String)("(1.0-2.0)")),
    EqualsTestCase(getModel.baseDataType, m2_s1, PrettyP, StringInst("(1.0-2.0)")),

    EqualsTestCase(getModel.baseDataType, AddInst(m2_s1, AddInst(LitInst(5.0), LitInst(6.0))),
      PrettyP, StringInst("((1.0-2.0)+(5.0+6.0))"))
  )
}
