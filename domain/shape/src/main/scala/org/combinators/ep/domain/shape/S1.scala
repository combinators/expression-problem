package org.combinators.ep.domain.shape   /*DD:LI:AI*/

import org.combinators.ep.domain.Evolution

trait S1 extends Evolution {
  self: S0 =>
  val domain: ShapeDomain
  import domain._

  // m1:model evolution.
  // -------------------
  //case object Shrink extends domain.Operation("shrink", Some(domain.Shape), Seq(pct))
  case object Shrink extends ProducerOperation("simplify", Seq(pct))

  val s1 = domain.Model("s1", Seq.empty, Seq(Shrink), s0)
  override def getModel:Model = s1

}
