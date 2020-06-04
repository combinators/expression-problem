package org.combinators.ep.domain.shape   /*DD:LI:AI*/


import org.combinators.ep.domain._
import org.combinators.ep.domain.abstractions._

object S2 extends Evolution {
  override implicit def getModel:GenericModel = S1.getModel.evolve("s2", Seq.empty, Seq(Shrink))

  // s2:model evolution.
  // -------------------
  //case object Shrink extends domain.Operation("shrink", Some(domain.Shape), Seq(pct))\\
  val pct = Parameter("pct", TypeRep.Double)
  lazy val Shrink = Operation("shrink",
    TypeRep.DataType(ShapeDomain.getModel.baseDataType), Seq(Parameter("pct", TypeRep.Double)))

  // TODO: Model test cases for S1
  def tests: Seq[TestCase] = Seq.empty

}
