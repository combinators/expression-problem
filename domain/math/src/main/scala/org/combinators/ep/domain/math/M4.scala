package org.combinators.ep.domain.math      /*DD:LI:AI*/

import org.combinators.cogen.InstanceRep
import org.combinators.ep.domain.instances.DataTypeInstanceRep
import org.combinators.cogen.{TestCase, TypeRep}
import org.combinators.ep.domain.*
import org.combinators.ep.domain.abstractions.{DomainTpeRep, EqualsCompositeTestCase, EqualsTestCase, Operation}
import org.combinators.ep.domain.math.M0.{AddInst, Eval, LitInst}
import org.combinators.ep.domain.math.M1.SubInst
import org.combinators.ep.domain.math.M2.{PrettyP, StringInst}
import org.combinators.ep.domain.math.M3.{DivdInst, MultInst, NegInst}

object M4 extends Evolution {
  override implicit def getModel:GenericModel = M3.getModel.evolve("m4", Seq.empty, Seq(Simplify, Collect))

  // this is a producer method (as you can tell by its return type).
  lazy val Simplify = Operation("simplify", DomainTpeRep.DataType(MathDomain.getModel.baseDataType))

  // m4:model evolution.
  // -------------------

  def ListDoubleInst(doubles:Seq[scala.Double]): InstanceRep = InstanceRep(TypeRep.Sequence(TypeRep.Double))(doubles)

  lazy val Collect = Operation("collect", TypeRep.Sequence(TypeRep.Double))

  // Tests
  // (5/7) / (7-(2*3) --> just (5/7)

  val m4_m1 = MultInst(DivdInst(LitInst(5.0), LitInst(2.0)), LitInst(4.0))
  val m4_m2 = MultInst(LitInst(2.0), LitInst(3.0))
  val m4_d2 = DivdInst(DivdInst(LitInst(5.0), LitInst(7.0)), SubInst(LitInst(7.0), m4_m2))

  val m4_s_0 = NegInst(LitInst(0.0))
  val m4_s_5 = AddInst(LitInst(5.0), LitInst(0.0))
  val m4_s_00 = AddInst(LitInst(0.0), LitInst(0.0))
  val m4_s_7 = AddInst(LitInst(0.0), LitInst(7.0))

  // validates simplify ((5+0)+(0+7)) = (5+7)
  val m4_together = AddInst(m4_s_5, m4_s_7)
  val m4_s_13 = MultInst(LitInst(13.0), LitInst(1.0))
  val m4_s_12 = MultInst(LitInst(1.0), LitInst(12.0))
  val m4_s_m0 = SubInst(LitInst(7.0), LitInst(7.0))

  val m4_s_n1 = DivdInst(LitInst(5.0), LitInst(-5.0))
  val m4_s_1 = DivdInst(LitInst(-5.0), LitInst(-5.0))
  val m4_s_d0 = DivdInst(LitInst(0.0), LitInst(-5.0))

  override def allTests: Map[GenericModel, Seq[TestCase]] = allPastTests(M3)

  /**
    * Test cases for Simplify are oddly complicated. The Simplify operation returns a new Exp object, but
    * making test cases depends upon having the ability to PrettyP the result. We therefore want to check
    * equality of (d1 x prettyP) vs. ((d2 x Simplify) x prettyp)
    *
    * Result should support a composite operation
    */
  def tests:Seq[TestCase] = Seq(
    EqualsTestCase(getModel.baseDataType, m4_d2, Collect, ListDoubleInst(Seq(5.0, 7.0, 7.0, 2.0, 3.0))),
    EqualsTestCase(getModel.baseDataType, m4_s_00, Collect, ListDoubleInst(Seq(0.0, 0.0))),
    EqualsTestCase(getModel.baseDataType, m4_s_0, Collect, ListDoubleInst(Seq(0.0))),
    EqualsTestCase(getModel.baseDataType, m4_s_12, Collect, ListDoubleInst(Seq(1.0, 12.0))),
    EqualsTestCase(getModel.baseDataType, m4_s_13, Collect, ListDoubleInst(Seq(13.0, 1.0))),

    EqualsTestCase(getModel.baseDataType, m4_m1, PrettyP, StringInst("((5.0/2.0)*4.0)")),
    EqualsTestCase(getModel.baseDataType, m4_m1, Eval, M0.DoubleInst(10.0)),
  )  ++  M4_simplify_tests//

  def M4_simplify_tests:Seq[TestCase] = Seq(
    EqualsCompositeTestCase(getModel.baseDataType, m4_together, StringInst("(5.0+7.0)"), (Simplify, Seq.empty), (PrettyP, Seq.empty)),
    EqualsCompositeTestCase(getModel.baseDataType, m4_d2,  StringInst("(5.0/7.0)"), (Simplify, Seq.empty), (PrettyP, Seq.empty)),
    EqualsCompositeTestCase(getModel.baseDataType, m4_s_0, M0.DoubleInst(0.0), (Simplify, Seq.empty), (Eval, Seq.empty)),
    EqualsCompositeTestCase(getModel.baseDataType, m4_s_5, M0.DoubleInst(5.0), (Simplify, Seq.empty), (Eval, Seq.empty)),
    EqualsCompositeTestCase(getModel.baseDataType, m4_s_7, M0.DoubleInst(7.0), (Simplify, Seq.empty), (Eval, Seq.empty)),
    EqualsCompositeTestCase(getModel.baseDataType, m4_s_13, M0.DoubleInst(13.0) , (Simplify, Seq.empty), (Eval, Seq.empty)),
    EqualsCompositeTestCase(getModel.baseDataType, m4_s_12, M0.DoubleInst(12.0) , (Simplify, Seq.empty), (Eval, Seq.empty)),
    EqualsCompositeTestCase(getModel.baseDataType, m4_s_m0, M0.DoubleInst(0.0) , (Simplify, Seq.empty), (Eval, Seq.empty)),

    EqualsCompositeTestCase(getModel.baseDataType, m4_s_n1, M0.DoubleInst(-1.0) , (Simplify, Seq.empty), (Eval, Seq.empty)),
    EqualsCompositeTestCase(getModel.baseDataType, m4_s_1, M0.DoubleInst(1.0) , (Simplify, Seq.empty), (Eval, Seq.empty)),
    EqualsCompositeTestCase(getModel.baseDataType, m4_s_d0, M0.DoubleInst(0.0) , (Simplify, Seq.empty), (Eval, Seq.empty)),
  )
}
