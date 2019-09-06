package org.combinators.ep.domain.shape   /*DD:LI:AI*/

import org.combinators.ep.domain._

class S1(val s0:S0) extends Evolution {

  val domain:BaseDomain = ShapeDomain
  import domain._
  import s0._

  // m1:model evolution.
  // -------------------
  //case object Shrink extends domain.Operation("shrink", Some(domain.Shape), Seq(pct))
  case object Shrink extends ProducerOperation(baseTypeRep,"shrink", Seq(pct))

  val s1 = Model("s1", Seq.empty, Seq(Shrink), s0.getModel)
  override def getModel:Model = s1

}
