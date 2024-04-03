package org.combinators.ep.domain.math      /*DD:LI:AI*/

import org.combinators.ep.domain._
import org.combinators.ep.domain.abstractions._
import org.combinators.ep.domain.instances._
import org.combinators.ep.domain.math.M0.{Eval, LitInst}
import org.combinators.ep.domain.math.M2.{PrettyP, StringInst}

object M3 extends Evolution {
  override implicit def getModel:GenericModel = M2.getModel.evolve("m3", Seq(Neg, Mult, Divd), Seq.empty)

  // m3:model evolution.
  // -------------------
  lazy val Mult = DataTypeCase.binary("Mult")(MathDomain.getModel)
  lazy val Neg = DataTypeCase.unary("Neg")(MathDomain.getModel)
  lazy val Divd = DataTypeCase.binary("Divd")(MathDomain.getModel)

  def NegInst(inner:DataTypeInstance): DataTypeInstance =
    DataTypeInstance(Neg, Seq(InstanceRep(inner)))
  def MultInst(left:DataTypeInstance, right:DataTypeInstance): DataTypeInstance =
    DataTypeInstance(Mult, Seq(InstanceRep(left), InstanceRep(right)))
  def DivdInst(left:DataTypeInstance, right:DataTypeInstance): DataTypeInstance =
    DataTypeInstance(Divd, Seq(InstanceRep(left), InstanceRep(right)))

  // Tests
  val m3_d1 = NegInst(LitInst(5.0))
  val m3_s1 = NegInst(MultInst(LitInst(2.0), LitInst(3.0)))

  val m3_m1 = NegInst(LitInst(1.0))
  val m3_m2 = MultInst(DivdInst(LitInst(5.0),  LitInst(2.0)), LitInst(4.0))

  override def allTests: Map[GenericModel, Seq[TestCase]] = allPastTests(M2)

  val m3_test_1:TestCase = EqualsTestCase(getModel.baseDataType, m3_m1, Eval, M0.DoubleInst(-1.0))
  val m3_test_2:TestCase = EqualsTestCase(getModel.baseDataType, m3_m1, PrettyP, StringInst("-1.0"))
  val m3_test_3:TestCase = EqualsTestCase(getModel.baseDataType, m3_m2, PrettyP, StringInst("((5.0/2.0)*4.0)"))
  val m3_test_4:TestCase = EqualsTestCase(getModel.baseDataType, m3_m2, Eval, M0.DoubleInst(10.0))
  val m3_test_5:TestCase = EqualsTestCase(getModel.baseDataType, m3_d1, Eval, M0.DoubleInst(-5.0))
  val m3_test_6:TestCase = EqualsTestCase(getModel.baseDataType, m3_s1, PrettyP, StringInst("-(2.0*3.0)"))

  def tests: Seq[TestCase] = Seq(
      m3_test_1,
      m3_test_2,

      m3_test_3,
      m3_test_4,

      m3_test_5,
      m3_test_6
    )

}
