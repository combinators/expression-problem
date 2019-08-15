package org.combinators.ep.domain.math      /*DD:LI:AI*/

import org.combinators.ep.domain._

class I1(val m1:M1) extends Evolution {

  val domain:BaseDomain = MathDomain
  import domain._
  import m1._

  // i1:model evolution.
  // -------------------
  case object Inv extends domain.Unary("Inv")
  val i1 = Model("i1", Seq(Inv), Seq.empty, last = m1.getModel)

  override def getModel = i1

  // TODO: Model test cases for I1
}
