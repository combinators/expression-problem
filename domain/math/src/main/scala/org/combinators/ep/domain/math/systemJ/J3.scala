package org.combinators.ep.domain.math.systemJ

import org.combinators.ep.domain.abstractions._
import org.combinators.ep.domain.instances.{DataTypeInstance, InstanceRep}
import org.combinators.ep.domain.math.M0.{AddInst, DoubleInst, Eval, LitInst, addi}
import org.combinators.ep.domain.math.systemJ.J1.{MultByTestCase, subi}
import org.combinators.ep.domain.math.systemJ.J2.{eqls, multi, not_eqls, struct_not_eqls}
import org.combinators.ep.domain.math.{M0, MathDomain}
import org.combinators.ep.domain.{Evolution, GenericModel}

object J3 extends Evolution {
  override implicit def getModel: GenericModel = J2.getModel.evolve("j3", Seq(Neg, Divd), Seq(PrettyP) ++ J2.isOps(Seq(Neg, Divd)))

  // m3:model evolution.
  // -------------------
  lazy val Neg = DataTypeCase.unary("Neg")(MathDomain.getModel)
  lazy val Divd = DataTypeCase.binary("Divd")(MathDomain.getModel)

  def StringInst(s: String): InstanceRep = InstanceRep(TypeRep.String)(s)

  lazy val PrettyP = Operation("prettyp", TypeRep.String)

  // Tests
  def NegInst(inner: DataTypeInstance): DataTypeInstance =
    DataTypeInstance(Neg, Seq(InstanceRep(inner)))

  def DivdInst(left: DataTypeInstance, right: DataTypeInstance): DataTypeInstance =
    DataTypeInstance(Divd, Seq(InstanceRep(left), InstanceRep(right)))

  // Tests
  val negi: DataTypeInstance = NegInst(LitInst(3.0))
  val divdi: DataTypeInstance = DivdInst(LitInst(10.0), LitInst(5.0))

  val negi_same_lhs = NegInst(LitInst(1.0))
  val negi_same_rhs = NegInst(LitInst(-1.0))
  val divdi_same_lhs = DivdInst(LitInst(7.0), LitInst(3.0))
  val divdi_same_rhs = DivdInst(LitInst(3.0), LitInst(5.0))

  val all_instances = J2.all_instances ++ Seq(negi, divdi)
  val lhs = J2.lhs ++ Seq(negi_same_lhs, divdi_same_lhs) // changes on left hand side
  val rhs = J2.rhs ++ Seq(negi_same_rhs, divdi_same_rhs) // changes on right hand side

  def tests: Seq[TestCase] = Seq(
    EqualsTestCase(getModel.baseDataType, negi, Eval, M0.DoubleInst(-3.0)),
    EqualsTestCase(getModel.baseDataType, divdi, Eval, M0.DoubleInst(2.0)),

    EqualsTestCase(getModel.baseDataType, negi, PrettyP, StringInst("-3.0")),
    EqualsTestCase(getModel.baseDataType, divdi, PrettyP, StringInst("(10.0/5.0)")),
    EqualsTestCase(getModel.baseDataType, multi, PrettyP, StringInst("(2.0*3.0)")),
    EqualsTestCase(getModel.baseDataType, addi, PrettyP, StringInst("(1.0+2.0)")),
    EqualsTestCase(getModel.baseDataType, subi, PrettyP, StringInst("(1.0-2.0)")),

    EqualsTestCase(getModel.baseDataType, AddInst(subi, addi), PrettyP, StringInst("((1.0-2.0)+(1.0+2.0))")),
    MultByTestCase(divdi, InstanceRep(LitInst(3.0)), DoubleInst(6.0)),
    MultByTestCase(negi, InstanceRep(LitInst(3.0)), DoubleInst(-9.0)),

  ) ++ eqls(all_instances) ++ not_eqls(all_instances) ++ struct_not_eqls(all_instances, lhs, rhs)
}
