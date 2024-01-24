package org.combinators.ep.domain.math.systemJK    /*DD:LI:AI*/

import org.combinators.ep.domain.abstractions._
import org.combinators.ep.domain.instances.{DataTypeInstance, InstanceRep}
import org.combinators.ep.domain.math.M0.{DoubleInst, Eval, LitInst}
import org.combinators.ep.domain.{Evolution, GenericModel}
import org.combinators.ep.domain.abstractions.{EqualsCompositeTestCase, EqualsTestCase, TestCase}
import org.combinators.ep.domain.math.systemJ.J1.MultBy
import org.combinators.ep.domain.math.{M0, MathDomain}
import org.combinators.ep.domain.math.systemJ.J2
import org.combinators.ep.domain.math.systemJ.J2.{eqls, not_eqls, struct_not_eqls}
import org.combinators.ep.domain.math.systemJ.J3.{PrettyP, StringInst}
import org.combinators.ep.domain.math.systemJ.J5.{op_equals, op_not_equals}
import org.combinators.ep.domain.math.systemJ.J6.PowByTestCase
import org.combinators.ep.domain.math.systemK.K2.{Collect, ListDoubleInst, Simplify}

object J7 extends Evolution {
  override implicit def getModel: GenericModel = K2J6.getModel.evolve("j7", Seq(Inv), J2.isOps(Seq(Inv)))

  lazy val Inv = DataTypeCase.binary("Inv")(MathDomain.getModel)

  def InvInst(left: DataTypeInstance, right: DataTypeInstance): DataTypeInstance =
    DataTypeInstance(Inv, Seq(InstanceRep(left), InstanceRep(right)))

  val invi = InvInst(LitInst(3.0), LitInst(5.0))

  val invi_same_lhs = InvInst(LitInst(3.0), LitInst(4.0))
  val invi_same_rhs = InvInst(LitInst(4.0), LitInst(5.0))

  val all_instances = K2J6.all_instances ++ Seq(invi)
  val lhs = K2J6.lhs ++ Seq(invi_same_lhs) // changes on left hand side
  val rhs = K2J6.rhs ++ Seq(invi_same_rhs) // changes on right hand side

  def tests: Seq[TestCase] = Seq(
    EqualsTestCase(getModel.baseDataType, invi, PrettyP, StringInst("(5.0/3.0)")),
    EqualsTestCase(getModel.baseDataType, invi, Eval, M0.DoubleInst(5.0 / 3.0)),

    EqualsCompositeTestCase(getModel.baseDataType, InvInst(LitInst(1.0), LitInst(5.0)), StringInst("5.0"), (Simplify, Seq.empty), (PrettyP, Seq.empty)),
    EqualsCompositeTestCase(getModel.baseDataType, InvInst(LitInst(-5.0), LitInst(5.0)), StringInst("-1.0"), (Simplify, Seq.empty), (PrettyP, Seq.empty)),
    EqualsCompositeTestCase(getModel.baseDataType, InvInst(LitInst(5.0), LitInst(5.0)), StringInst("1.0"), (Simplify, Seq.empty), (PrettyP, Seq.empty)),
    EqualsCompositeTestCase(getModel.baseDataType, InvInst(LitInst(3.0), LitInst(0.0)), StringInst("0.0"), (Simplify, Seq.empty), (PrettyP, Seq.empty)),
    EqualsCompositeTestCase(getModel.baseDataType, invi, StringInst("(5.0/3.0)"), (Simplify, Seq.empty), (PrettyP, Seq.empty)),

    // these are not possible in SOME EP implementations, because of 'equals' being used.
    //    EqualsTestCase(getModel.baseDataType, InvInst(LitInst(2.0), LitInst(4.0)), PowBy, InstanceRep(PowerInst(InvInst(LitInst(2.0), LitInst(4.0)), LitInst(3.0))),
    //      InstanceRep(LitInst(3.0))),
    //    EqualsTestCase(getModel.baseDataType, MultInst(LitInst(2.0), LitInst(4.0)), PowBy, InstanceRep(PowerInst(MultInst(LitInst(2.0), LitInst(4.0)), LitInst(3.0))),
    //      InstanceRep(LitInst(3.0))),
    //    EqualsTestCase(getModel.baseDataType, InvInst(LitInst(2.0), LitInst(4.0)), MultBy, InstanceRep(MultInst(InvInst(LitInst(2.0), LitInst(4.0)), LitInst(6.0))),
    //      InstanceRep(LitInst(6.0))),

    // do this instead
    EqualsCompositeTestCase(getModel.baseDataType, InvInst(LitInst(3.0), LitInst(6.0)), DoubleInst(6.0), (MultBy, Seq(InstanceRep(LitInst(3.0)))), (Eval, Seq.empty)),

    EqualsTestCase(getModel.baseDataType, invi, Collect, ListDoubleInst(Seq(3.0, 5.0))),
    PowByTestCase(InvInst(LitInst(4.0), LitInst(2.0)), InstanceRep(LitInst(4.0)), DoubleInst(0.5 * 0.5 * 0.5 * 0.5)),

  ) ++ eqls(all_instances) ++ not_eqls(all_instances) ++ struct_not_eqls(all_instances, lhs, rhs) ++ op_equals(all_instances) ++ op_not_equals(all_instances)
}
