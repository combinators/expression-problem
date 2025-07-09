package org.combinators.ep.domain.math    /*DD:LI:AI*/

import org.combinators.cogen.InstanceRep
import org.combinators.ep.domain.instances.DataTypeInstanceRep
import org.combinators.cogen.{TestCase, TypeRep}
import org.combinators.ep.domain.abstractions.{DataTypeCase, EqualsCompositeTestCase, EqualsTestCase, Operation}
import org.combinators.ep.domain.instances.DataTypeInstance
import org.combinators.ep.domain.math.M0.{Eval, LitInst}
import org.combinators.ep.domain.math.M2.{PrettyP, StringInst}
import org.combinators.ep.domain.math.M4.Simplify
import org.combinators.ep.domain.{Evolution, GenericModel}

object M8 extends Evolution {
  override implicit def getModel:GenericModel = M7I2.getModel.evolve("m8", Seq(Inv), M6.isOps(Seq(Inv)))

  // M8:model evolution.  INV which is opposite of DIV, or 1/DIV
  // -------------------
  lazy val Inv = DataTypeCase.binary("Inv")(MathDomain.getModel)

  def InvInst(left:DataTypeInstance, right:DataTypeInstance): DataTypeInstance =
    DataTypeInstance(Inv, Seq(DataTypeInstanceRep(left), DataTypeInstanceRep(right)))

  // Tests
  val m8_d1 = InvInst(LitInst(5.0),  LitInst(2.0))
  val m8_d2 = InvInst(LitInst(1.0),  LitInst(5.0))

  override def allTests: Map[GenericModel, Seq[TestCase]] = allPastTests(M7I2)

  def tests: Seq[TestCase] = Seq(
    EqualsTestCase(getModel.baseDataType, m8_d1, PrettyP, StringInst("(2.0/5.0)")),
    EqualsTestCase(getModel.baseDataType, m8_d1, Eval, M0.DoubleInst(0.4)),

    EqualsCompositeTestCase(getModel.baseDataType, m8_d2, StringInst("5.0"), (Simplify, Seq.empty), (PrettyP, Seq.empty)),
  )
}
