package org.combinators.ep.domain.shape   /*DD:LI:AI*/

import org.combinators.cogen.InstanceRep
import org.combinators.cogen.TypeRep
import org.combinators.ep.domain.instances.DataTypeInstanceRep
import org.combinators.cogen.TestCase
import org.combinators.ep.domain.*
import org.combinators.ep.domain.abstractions.*
import org.combinators.ep.domain.shape.S0.ContainsTestCase

object S2 extends Evolution {
  override implicit def getModel:GenericModel = S1.getModel.evolve("s2", Seq.empty, Seq(Shrink))

  // s2:model evolution.
  // -------------------
  // Challenge here is how to "know" that a square has a side, while a circle has a radius. This is a job
  // for the EIPs. This is a producer method.
  val pct = Parameter("pct", TypeRep.Double)
  lazy val Shrink = Operation("shrink",
    DomainTpeRep.DataType(ShapeDomain.getModel.baseDataType), Seq(Parameter("pct", TypeRep.Double)))

  def DoubleInst(d: scala.Double): InstanceRep =
    InstanceRep(TypeRep.Double)(d)

  // TODO: Model test cases for S2
  def tests: Seq[TestCase] = Seq(
    ContainsTestCase(shape.S1.UnionInst(shape.S0.sq1, shape.S0.c1),  shape.S0.p1, result = true),

    // (2.0,2.0) not  within (5x5) square shrunk 50% (centered at 0,0)
    EqualsCompositeTestCase(getModel.baseDataType, shape.S0.sq1, InstanceRep(TypeRep.Boolean)(false), (Shrink, Seq(DoubleInst(0.5))), (shape.S0.ContainsPt, Seq(DoubleInst(2.0), DoubleInst(2.0)))),

    // (1.0,1.0) is within  (5x5) square shrunk 50% (centered at 0,0)
    EqualsCompositeTestCase(getModel.baseDataType, shape.S0.sq1, InstanceRep(TypeRep.Boolean)(true), (Shrink, Seq(DoubleInst(0.5))), (shape.S0.ContainsPt, Seq(DoubleInst(1.0), DoubleInst(1.0)))),
  )

}
