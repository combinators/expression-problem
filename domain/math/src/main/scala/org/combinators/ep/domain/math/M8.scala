package org.combinators.ep.domain.math    /*DD:LI:AI*/

import org.combinators.ep.domain.abstractions.{DataTypeCase, EqualsTestCase, Operation, TestCase, TypeRep}
import org.combinators.ep.domain.instances.{DataTypeInstance, InstanceRep}
import org.combinators.ep.domain.math.M0.{Eval, LitInst}
import org.combinators.ep.domain.math.M2.{PrettyP, StringInst}
import org.combinators.ep.domain.{Evolution, GenericModel}

object M8 extends Evolution {
  override implicit def getModel:GenericModel = M7I2.getModel.evolve("m8", Seq(Inv), Seq.empty)

  // M8:model evolution.  INV which is opposite of DIV
  // -------------------
  lazy val Inv = DataTypeCase.binary("Inv")(MathDomain.getModel)

  def DivdInst(left:DataTypeInstance, right:DataTypeInstance): DataTypeInstance =
    DataTypeInstance(Inv, Seq(InstanceRep(left), InstanceRep(right)))

  // Tests
  val m8_d1 = DivdInst(LitInst(5.0),  LitInst(2.0))

  def tests: Seq[TestCase] = Seq(

    EqualsTestCase(getModel.baseDataType, m8_d1, PrettyP, StringInst("(2.0/5.0)")),
    EqualsTestCase(getModel.baseDataType, m8_d1, Eval, M0.DoubleInst(0.4)),
  )
}
