package org.combinators.ep.domain.math.systemK    /*DD:LI:AI*/

import org.combinators.ep.domain.instances.DataTypeInstanceRep
import org.combinators.cogen.TestCase
import org.combinators.ep.domain.abstractions.{DataTypeCase, EqualsTestCase}
import org.combinators.ep.domain.instances.DataTypeInstance
import org.combinators.ep.domain.math.M0.{DoubleInst, Eval, LitInst}
import org.combinators.ep.domain.math.MathDomain
import org.combinators.ep.domain.math.systemJ.J1.MultByTestCase
import org.combinators.ep.domain.math.systemJ.J2
import org.combinators.ep.domain.math.systemJ.J2.{eqls, not_eqls, struct_not_eqls}
import org.combinators.ep.domain.{Evolution, GenericModel}

object K1 extends Evolution {
  override implicit def getModel: GenericModel = J2.getModel.evolve("k1", Seq(Power), J2.isOps(Seq(Power)))

  lazy val Power: DataTypeCase = DataTypeCase.binary("Power")(MathDomain.getModel)

  def PowerInst(base: DataTypeInstance, exponent: DataTypeInstance): DataTypeInstance =
    DataTypeInstance(Power, Seq(DataTypeInstanceRep(base), DataTypeInstanceRep(exponent)))

  val powi: DataTypeInstance = PowerInst(LitInst(3.0), LitInst(5.0))

  val powi_same_lhs: DataTypeInstance = PowerInst(LitInst(3.0), LitInst(4.0))
  val powi_same_rhs: DataTypeInstance = PowerInst(LitInst(4.0), LitInst(5.0))

  val all_instances: Seq[DataTypeInstance] = J2.all_instances ++ Seq(powi)
  val lhs: Seq[DataTypeInstance] = J2.lhs ++ Seq(powi_same_lhs) // changes on left hand side
  val rhs: Seq[DataTypeInstance] = J2.rhs ++ Seq(powi_same_rhs) // changes on right hand side

  override def allTests: Map[GenericModel, Seq[TestCase]] = allPastTests(J2)

  def tests: Seq[TestCase] = Seq(
    EqualsTestCase(getModel.baseDataType, PowerInst(LitInst(2.0), LitInst(5.0)), Eval, DoubleInst(32.0)),

    MultByTestCase(powi, DataTypeInstanceRep(LitInst(3.0)), DoubleInst(729.0)),
  ) ++ eqls(all_instances) ++ not_eqls(all_instances) ++ struct_not_eqls(all_instances, lhs, rhs)
}
