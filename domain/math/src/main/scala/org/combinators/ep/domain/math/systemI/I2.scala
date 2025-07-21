package org.combinators.ep.domain.math.systemI    /*DD:LI:AI*/

import org.combinators.ep.domain.instances.DataTypeInstanceRep
import org.combinators.cogen.TestCase
import org.combinators.ep.domain.abstractions.{DataTypeCase, EqualsTestCase}
import org.combinators.ep.domain.instances.DataTypeInstance
import org.combinators.ep.domain.math.M0.{DoubleInst, Eval, LitInst}
import org.combinators.ep.domain.math.MathDomain
import org.combinators.ep.domain.{Evolution, GenericModel}

object I2 extends Evolution {
  override implicit def getModel: GenericModel = I1.getModel.evolve("i2", Seq(Power), Seq.empty)

  lazy val Power: DataTypeCase = DataTypeCase.binary("Power")(MathDomain.getModel)

  def PowerInst(base: DataTypeInstance, exponent: DataTypeInstance): DataTypeInstance =
    DataTypeInstance(Power, Seq(DataTypeInstanceRep(base), DataTypeInstanceRep(exponent)))

  override def allTests: Map[GenericModel, Seq[TestCase]] = allPastTests(I1)

  // TODO: Model test cases for I2
  def tests: Seq[TestCase] = Seq(
    EqualsTestCase(getModel.baseDataType, PowerInst(LitInst(2.0), LitInst(5.0)), Eval, DoubleInst(32.0)),
  )
}
