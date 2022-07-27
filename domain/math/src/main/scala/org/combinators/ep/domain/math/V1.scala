package org.combinators.ep.domain.math    /*DD:LI:AI*/

import org.combinators.ep.domain.abstractions.{DataTypeCase, EqualsCompositeTestCase, EqualsTestCase, TestCase}
import org.combinators.ep.domain.instances.{DataTypeInstance, InstanceRep}
import org.combinators.ep.domain.math.M0.{Eval, LitInst}
import org.combinators.ep.domain.math.M2.{PrettyP, StringInst}
import org.combinators.ep.domain.{Evolution, GenericModel}

object V1 extends Evolution {
  override implicit def getModel:GenericModel = C2.getModel.evolve("v1", Seq(Inv), Seq.empty)

  // M8:model evolution.  INV which is opposite of DIV, or 1/DIV
  // -------------------
  lazy val Inv = DataTypeCase.binary("Inv")(MathDomain.getModel)

  def InvInst(left:DataTypeInstance, right:DataTypeInstance): DataTypeInstance =
    DataTypeInstance(Inv, Seq(InstanceRep(left), InstanceRep(right)))

  // Tests
  val v1_d1 = InvInst(LitInst(5.0),  LitInst(2.0))
  val v1_d2 = InvInst(LitInst(1.0),  LitInst(5.0))

  def tests: Seq[TestCase] = Seq(
    EqualsTestCase(getModel.baseDataType, v1_d1, PrettyP, StringInst("(2.0/5.0)")),
    EqualsTestCase(getModel.baseDataType, v1_d1, Eval, M0.DoubleInst(0.4)),

  )
}
