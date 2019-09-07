package org.combinators.ep.domain.math

import org.combinators.ep.domain._
import org.combinators.ep.domain.abstractions._
import org.combinators.ep.domain.instances.{DataTypeInstance, InstanceRep}
import org.combinators.ep.domain.math.M0.{AddInst, Eval, LitInst}
import org.combinators.ep.domain.math.M1.{Sub, SubInst}

//class M2(val m1:M1) extends Evolution {
object M2 extends Evolution {
  override implicit def getModel:Model = M1.getModel.evolve("m2", Seq.empty, Seq(PrettyP))

  def StringInst(s:String): InstanceRep = InstanceRep(TypeRep.String)(s)

  lazy val PrettyP = Operation("prettyp", TypeRep.String)

  // Tests
  val m2_s1 = SubInst(LitInst(1.0), LitInst(2.0))
  def M2_tests: Seq[TestCase] = Seq(
    //EqualsTestCase(m2_s1, PrettyP, ExistsInstance(String)("(1.0-2.0)")),
    EqualsTestCase(m2_s1, PrettyP, StringInst("(1.0-2.0)")),

    EqualsTestCase(AddInst(m2_s1, AddInst(LitInst(5.0), LitInst(6.0))),
      PrettyP, StringInst("((1.0-2.0)+(5.0+6.0))"))
  )
}
