package org.combinators.ep.domain.math    /*DD:LI:AI*/

import org.combinators.ep.domain._
import org.combinators.ep.domain.abstractions.{DataTypeCase, EqualsTestCase, TestCase}
import org.combinators.ep.domain.instances.{DataTypeInstance, InstanceRep}
import org.combinators.ep.domain.math.M0.{DoubleInst, Eval, LitInst}

object W1 extends Evolution {
  override implicit def getModel:GenericModel = M1.getModel.evolve("w1", Seq(Power), Seq.empty)

  // w:model evolution.
  // -------------------
  lazy val Power:DataTypeCase = DataTypeCase.binary("Power")(MathDomain.getModel)

  def PowerInst(base:DataTypeInstance, exponent:DataTypeInstance): DataTypeInstance =
    DataTypeInstance(Power, Seq(InstanceRep(base), InstanceRep(exponent)))

  override def allTests: Map[GenericModel, Seq[TestCase]] = allPastTests(M1)


  // TODO: Model test cases for W1
  def tests: Seq[TestCase] = Seq(
    EqualsTestCase(getModel.baseDataType, PowerInst(LitInst(2.0), LitInst(5.0)), Eval, DoubleInst(32.0)),
  )
}
