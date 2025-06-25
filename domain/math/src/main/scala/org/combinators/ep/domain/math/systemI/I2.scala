package org.combinators.ep.domain.math.systemI    /*DD:LI:AI*/

import org.combinators.cogen.InstanceRep
import org.combinators.cogen.abstractions.TestCase
import org.combinators.ep.domain.abstractions.{DataTypeCase, EqualsTestCase}
import org.combinators.ep.domain.instances.DataTypeInstance
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

  override def allTests: Map[GenericModel, Seq[TestCase]] = allPastTests(I1)

  // TODO: Model test cases for I2
  def tests: Seq[TestCase] = Seq(
    EqualsTestCase(getModel.baseDataType, PowerInst(LitInst(2.0), LitInst(5.0)), Eval, DoubleInst(32.0)),
  )
}
