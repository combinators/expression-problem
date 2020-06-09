package org.combinators.ep.domain.math      /*DD:LI:AI*/

import org.combinators.ep.domain._
import org.combinators.ep.domain.abstractions._
import org.combinators.ep.domain.instances.{DataTypeInstance, InstanceRep}
import org.combinators.ep.domain.math.M0.{Eval, LitInst}

object X4 extends Evolution {
  override implicit def getModel:GenericModel = X2X3.getModel.evolve("x4", Seq(Neg), Seq.empty)

   lazy val Neg = DataTypeCase.unary("Neg")(MathDomain.getModel)

  def NegInst(inner:DataTypeInstance): DataTypeInstance =
    DataTypeInstance(Neg, Seq(InstanceRep(inner)))

  // Tests
  val x4_1 = NegInst(LitInst(5.0))
  def tests: Seq[TestCase] = Seq(
    EqualsTestCase(getModel.baseDataType, x4_1, Eval, M0.DoubleInst(-5.0)),
  )
}
