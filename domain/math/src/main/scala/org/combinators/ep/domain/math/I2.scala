package org.combinators.ep.domain.math      /*DD:LI:AI*/

import org.combinators.ep.domain.Evolution

trait I2 extends Evolution {
  self: M0 with M1 with I1 =>
  val domain: MathDomain

  // i2:model evolution.
  // -------------------
  case object Integer extends domain.TypeRep
  case object Height extends domain.Operation(independent.height, Some(Integer), Seq(domain.Parameter(independent.height, Integer)))
  val i2 = domain.Model("i2", Seq.empty, Seq(Height), last = i1)

  override def getModel = i2

  // TODO: Model test cases for I2
}
