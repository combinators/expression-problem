package org.combinators.ep.domain.math.systemI

import org.combinators.ep.domain.abstractions.{DataTypeCase, EqualsTestCase, TestCase}
import org.combinators.ep.domain.instances.{DataTypeInstance, InstanceRep}
import org.combinators.ep.domain.math.M0.{DoubleInst, Eval, LitInst}
import org.combinators.ep.domain.math.MathDomain
import org.combinators.ep.domain.{Evolution, GenericModel}

// TODO: I2 still generates multBy twice in Power.
object I2 extends Evolution {
  override implicit def getModel: GenericModel = I1.getModel.evolve("i2", Seq(Power), Seq.empty)

  // i2:model evolution.
  // -------------------
  lazy val Power: DataTypeCase = DataTypeCase.binary("Power")(MathDomain.getModel)

  def PowerInst(base: DataTypeInstance, exponent: DataTypeInstance): DataTypeInstance =
    DataTypeInstance(Power, Seq(InstanceRep(base), InstanceRep(exponent)))

  // TODO: Model test cases for I2
  def tests: Seq[TestCase] = Seq(
    EqualsTestCase(getModel.baseDataType, PowerInst(LitInst(2.0), LitInst(5.0)), Eval, DoubleInst(32.0)),
  )
}
