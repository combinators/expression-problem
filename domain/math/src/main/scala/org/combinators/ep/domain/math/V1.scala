package org.combinators.ep.domain.math    /*DD:LI:AI*/

import org.combinators.ep.domain.instances.DataTypeInstanceRep
import org.combinators.cogen.TestCase
import org.combinators.ep.domain.abstractions.{DataTypeCase, EqualsTestCase}
import org.combinators.ep.domain.instances.DataTypeInstance
import org.combinators.ep.domain.math.M0.{Eval, LitInst}
import org.combinators.ep.domain.math.M2.{PrettyP, StringInst}
import org.combinators.ep.domain.{Evolution, GenericModel}

object V1 extends Evolution {
  override implicit def getModel:GenericModel = C2.getModel.evolve("v1", Seq(Inv), Seq.empty)

  // M8:model evolution.  INV which is opposite of DIV, or 1/DIV
  // -------------------
  lazy val Inv: DataTypeCase = DataTypeCase.binary("Inv")(MathDomain.getModel)

  def InvInst(left:DataTypeInstance, right:DataTypeInstance): DataTypeInstance =
    DataTypeInstance(Inv, Seq(DataTypeInstanceRep(left), DataTypeInstanceRep(right)))

  // Tests
  val v1_d1: DataTypeInstance = InvInst(LitInst(5.0),  LitInst(2.0))
  val v1_d2: DataTypeInstance = InvInst(LitInst(1.0),  LitInst(5.0))

  override def allTests: Map[GenericModel, Seq[TestCase]] = allPastTests(C2)

  def tests: Seq[TestCase] = Seq(
    EqualsTestCase(getModel.baseDataType, v1_d1, PrettyP, StringInst("(2.0/5.0)")),
    EqualsTestCase(getModel.baseDataType, v1_d1, Eval, M0.DoubleInst(0.4)),
  )
}
