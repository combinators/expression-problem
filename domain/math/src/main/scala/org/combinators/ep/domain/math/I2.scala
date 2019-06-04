package org.combinators.ep.domain.math      /*DD:LI:AI*/

import org.combinators.ep.domain.Evolution

trait I2 extends Evolution {
  self: M0 with M1 with I1 =>
  val domain: MathDomain

  // i2:model evolution.
  // -------------------
  object independent {
    val height:String = "height"
  }

  // TODO: Flip this around so there are no parameters in height; rather an atomic data type
  // returns 0 and all others return Max(1+attrubte). Woudl this work?
  //
  // ALternatively: Write necessary code to make test case
  case object Integer extends domain.TypeRep
  case object Height extends domain.Operation(independent.height, Some(Integer), Seq(domain.Parameter(independent.height, Integer)))
  val i2 = domain.Model("i2", Seq.empty, Seq(Height), last = i1)

  override def getModel = i2

  // TODO: Model test cases for I2
}
