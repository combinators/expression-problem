package org.combinators.ep.domain.math    /*DD:LI:AI*/

import org.combinators.ep.domain._
import org.combinators.ep.domain.abstractions._
import org.combinators.ep.domain.instances.InstanceRep
import org.combinators.ep.domain.math.M0.{AddInst, Eval, LitInst}
import org.combinators.ep.domain.math.M1.SubInst
import org.combinators.ep.domain.math.M3.MultInst

object J5 extends Evolution {
  override implicit def getModel:GenericModel = J4.getModel.evolve("j5", Seq.empty, Seq(Simplify, Collect))

  // this is a producer method (as you can tell by its return type).
  lazy val Simplify = Operation("simplify", TypeRep.DataType(MathDomain.getModel.baseDataType))

  // m4:model evolution.
  // -------------------

  def ListDoubleInst(doubles:Seq[scala.Double]): InstanceRep = InstanceRep(TypeRep.Sequence(TypeRep.Double))(doubles)

  lazy val Collect = Operation("collect", TypeRep.Sequence(TypeRep.Double))

  // Tests
  // (5/7) / (7-(2*3) --> just (5/7)

  val m4_m2 = MultInst(LitInst(2.0), LitInst(3.0))

  val m4_s_5 = AddInst(LitInst(5.0), LitInst(0.0))
  val m4_s_00 = AddInst(LitInst(0.0), LitInst(0.0))
  val m4_s_7 = AddInst(LitInst(0.0), LitInst(7.0))

  // validates simplify ((5+0)+(0+7)) = (5+7)
  val m4_together = AddInst(m4_s_5, m4_s_7)
  val m4_s_13 = MultInst(LitInst(13.0), LitInst(1.0))
  val m4_s_12 = MultInst(LitInst(1.0), LitInst(12.0))
  val m4_s_m0 = SubInst(LitInst(7.0), LitInst(7.0))

  /**
    * Test cases for Simplify are oddly complicated. The Simplify operation returns a new Exp object, but
    * making test cases depends upon having the ability to PrettyP the result. We therefore want to check
    * equality of (d1 x prettyP) vs. ((d2 x Simplify) x prettyp)
    *
    * Result should support a composite operation
    */
  def tests:Seq[TestCase] = Seq(
    EqualsTestCase(getModel.baseDataType, m4_s_00, Collect, ListDoubleInst(Seq(0.0, 0.0))),
    EqualsTestCase(getModel.baseDataType, m4_s_12, Collect, ListDoubleInst(Seq(1.0, 12.0))),
    EqualsTestCase(getModel.baseDataType, m4_s_13, Collect, ListDoubleInst(Seq(13.0, 1.0))),

  )  //

  def M4_simplify_tests:Seq[TestCase] = Seq(
    EqualsCompositeTestCase(getModel.baseDataType, m4_s_5, M0.DoubleInst(5.0), (Simplify, Seq.empty), (Eval, Seq.empty)),
    EqualsCompositeTestCase(getModel.baseDataType, m4_s_7, M0.DoubleInst(7.0), (Simplify, Seq.empty), (Eval, Seq.empty)),
    EqualsCompositeTestCase(getModel.baseDataType, m4_s_13, M0.DoubleInst(13.0) , (Simplify, Seq.empty), (Eval, Seq.empty)),
    EqualsCompositeTestCase(getModel.baseDataType, m4_s_12, M0.DoubleInst(12.0) , (Simplify, Seq.empty), (Eval, Seq.empty)),
    EqualsCompositeTestCase(getModel.baseDataType, m4_s_m0, M0.DoubleInst(0.0) , (Simplify, Seq.empty), (Eval, Seq.empty)),
  )
}
