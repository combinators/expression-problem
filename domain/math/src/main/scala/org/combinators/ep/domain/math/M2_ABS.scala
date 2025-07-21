package org.combinators.ep.domain.math      /*DD:LI:AI*/

import org.combinators.ep.domain.instances.DataTypeInstanceRep
import org.combinators.cogen.{InstanceRep, TestCase}
import org.combinators.ep.domain._
import org.combinators.ep.domain.abstractions._
import org.combinators.ep.domain.instances.DataTypeInstance
import org.combinators.ep.domain.math.M0.LitInst
import org.combinators.ep.domain.math.M1.SubInst

object M2_ABS extends Evolution {
  override implicit def getModel:GenericModel = M2.getModel.evolve("m2_abs", Seq(Abs), Seq.empty)

  lazy val Abs:DataTypeCase = DataTypeCase.unary("Abs")(MathDomain.getModel)

  val m2_abs_s1: DataTypeInstance = SubInst(LitInst(1.0), LitInst(2.0))
  val m2_abs_a1: DataTypeInstance = AbsInst(m2_abs_s1)
  val m2_abs_r1: InstanceRep = M0.DoubleInst(1.0)

  def AbsInst(inner:DataTypeInstance): DataTypeInstance =
    DataTypeInstance(Abs, Seq(DataTypeInstanceRep(inner)))

  override def allTests: Map[GenericModel, Seq[TestCase]] = allPastTests(M2)

  def tests: Seq[TestCase] = Seq(

    EqualsTestCase(getModel.baseDataType, m2_abs_a1, M0.Eval, m2_abs_r1),

    EqualsTestCase(getModel.baseDataType, m2_abs_a1, M2.PrettyP, M2.StringInst("ABS((1.0-2.0))"))
  )
}
