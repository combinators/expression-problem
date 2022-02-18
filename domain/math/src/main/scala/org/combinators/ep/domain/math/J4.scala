package org.combinators.ep.domain.math    /*DD:LI:AI*/

import org.combinators.ep.domain._
import org.combinators.ep.domain.abstractions.{DataTypeCase, EqualsTestCase, TestCase}
import org.combinators.ep.domain.instances.{DataTypeInstance, InstanceRep}
import org.combinators.ep.domain.math.J1.MultByTestCase
import org.combinators.ep.domain.math.J2.{eqls, not_eqls, struct_not_eqls}
import org.combinators.ep.domain.math.M0.{DoubleInst, Eval, LitInst}

object J4 extends Evolution {
  override implicit def getModel:GenericModel = J2.getModel.evolve("j4", Seq(Power), J2.isOps(Seq(Power)))

  lazy val Power:DataTypeCase = DataTypeCase.binary("Power")(MathDomain.getModel)

  def PowerInst(base:DataTypeInstance, exponent:DataTypeInstance): DataTypeInstance =
    DataTypeInstance(Power, Seq(InstanceRep(base), InstanceRep(exponent)))

  val powi = PowerInst(LitInst(3.0), LitInst(5.0))

  val powi_same_lhs = PowerInst(LitInst(3.0), LitInst(4.0))
  val powi_same_rhs = PowerInst(LitInst(4.0), LitInst(5.0))

  val all_instances = J2.all_instances ++ Seq(powi)
  val lhs           = J2.lhs ++ Seq(powi_same_lhs)   // changes on left hand side
  val rhs           = J2.rhs ++ Seq(powi_same_rhs)   // changes on right hand side

  def tests: Seq[TestCase] = Seq(
    EqualsTestCase(getModel.baseDataType, PowerInst(LitInst(2.0), LitInst(5.0)), Eval, DoubleInst(32.0)),

    MultByTestCase(powi, InstanceRep(LitInst(3.0)), DoubleInst(729.0)),
  ) ++ eqls(all_instances) ++ not_eqls(all_instances) ++ struct_not_eqls(all_instances, lhs, rhs)
}
