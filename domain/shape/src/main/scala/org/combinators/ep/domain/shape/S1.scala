package org.combinators.ep.domain.shape   /*DD:LI:AI*/


import org.combinators.ep.domain._
import org.combinators.ep.domain.abstractions._

object S1 extends Evolution {
  override implicit def getModel:Model = S0.getModel.evolve("s1", Seq.empty, Seq(Shrink))

  // m1:model evolution.
  // -------------------
  //case object Shrink extends domain.Operation("shrink", Some(domain.Shape), Seq(pct))
  lazy val Shrink = Operation("shrink",
    TypeRep.DataType(ShapeDomain.getModel.baseDataType), Seq(Parameter("pct", TypeRep.Double)))

}
