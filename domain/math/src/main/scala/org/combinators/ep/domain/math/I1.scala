package org.combinators.ep.domain.math      /*DD:LI:AI*/

import org.combinators.ep.domain.Evolution

trait I1 extends Evolution {
  self: M0 with M1 =>
  val domain: MathDomain

  // i1:model evolution.
  // -------------------
  object independent {
    val height:String = "height"
  }

  case object Inv extends domain.Unary("Inv")
  val i1 = domain.Model("i1", Seq(Inv), Seq.empty, last = m1)

  override def getModel = i1

  // TODO: Model test cases for I1
}
