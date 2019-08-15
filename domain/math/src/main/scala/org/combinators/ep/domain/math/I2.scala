package org.combinators.ep.domain.math      /*DD:LI:AI*/

import org.combinators.ep.domain._

class I2(val i1:I1) extends Evolution {

  val domain:BaseDomain = MathDomain
  import domain._
  import i1._

  // i2:model evolution.
  // -------------------
  object independent {
    val height:String = "height"
  }

  // TODO: Flip this around so there are no parameters in height; rather an atomic data type
  // returns 0 and all others return Max(1+attrubte). Woudl this work?
  //
  // Alternatively: Write necessary code to make test case
  case object Integer extends TypeRep
  case object Height extends Operation(independent.height, Integer, Seq(Parameter(independent.height, Integer)))
  val i2 = Model("i2", Seq.empty, Seq(Height), last = i1.getModel)

  override def getModel = i2

  // TODO: Model test cases for I2
}
