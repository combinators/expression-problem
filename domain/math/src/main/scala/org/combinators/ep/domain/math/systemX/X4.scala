package org.combinators.ep.domain.math.systemX    /*DD:LI:AI*/

import org.combinators.ep.domain.abstractions.{DataTypeCase, EqualsTestCase, TestCase}
import org.combinators.ep.domain.instances.{DataTypeInstance, InstanceRep}
import org.combinators.ep.domain.math.M0.{Eval, LitInst}
import org.combinators.ep.domain.math.{M0, MathDomain}
import org.combinators.ep.domain.{Evolution, GenericModel}

object X4 extends Evolution {
  override implicit def getModel: GenericModel = X2X3.getModel.evolve("x4", Seq(Neg), Seq.empty)

  lazy val Neg = DataTypeCase.unary("Neg")(MathDomain.getModel)

  def NegInst(inner: DataTypeInstance): DataTypeInstance =
    DataTypeInstance(Neg, Seq(InstanceRep(inner)))

  // Tests
  val x4_1 = NegInst(LitInst(5.0))

  override def allTests: Map[GenericModel, Seq[TestCase]] = allPastTests(X2X3)

  def tests: Seq[TestCase] = Seq(
    EqualsTestCase(getModel.baseDataType, x4_1, Eval, M0.DoubleInst(-5.0)),
  )
}
