package org.combinators.ep.domain.math      /*DD:LI:AI*/

import org.combinators.ep.domain._
import org.combinators.ep.domain.abstractions._
import org.combinators.ep.domain.instances.{DataTypeInstance, InstanceRep}
import org.combinators.ep.domain.math.M0.{AddInst, LitInst}
import org.combinators.ep.domain.math.M1.SubInst

object M2_ABS extends Evolution {
  override implicit def getModel:GenericModel = M2.getModel.evolve("m2_abs", Seq(Abs), Seq.empty)

  // m1:model evolution.
  // -------------------
  lazy val Abs:DataTypeCase = DataTypeCase.unary("Abs")(MathDomain.getModel)

  val m2_abs_s1 = SubInst(LitInst(1.0), LitInst(2.0))
  val m2_abs_a1 = AbsInst(m2_abs_s1)
  val m2_abs_r1 = M0.DoubleInst(1.0)

  def AbsInst(inner:DataTypeInstance): DataTypeInstance =
    DataTypeInstance(Abs, Seq(InstanceRep(inner)))

  def tests: Seq[TestCase] = Seq(

    EqualsTestCase(getModel.baseDataType, m2_abs_a1, M0.Eval, m2_abs_r1),

    EqualsTestCase(getModel.baseDataType, m2_abs_a1, M2.PrettyP, M2.StringInst("ABS((1.0-2.0))"))
  )
}
