package org.combinators.ep.domain.math      /*DD:LI:AI*/

import org.combinators.ep.domain._
import org.combinators.ep.domain.abstractions.{Operation, Parameter, TypeRep}

object I2 extends Evolution {
  override implicit def getModel:Model = I1.getModel.evolve("i2", Seq.empty, Seq(Height))
  // i2:model evolution.
  // -------------------
  object independent {
    val height:String = "height"
  }

  // TODO: Flip this around so there are no parameters in height; rather an atomic data type
  // returns 0 and all others return Max(1+attrubte). Woudl this work?
  //
  // Alternatively: Write necessary code to make test case
  lazy val Height = Operation("height", TypeRep.Int, Seq(Parameter(independent.height, TypeRep.Int)))

  // TODO: Model test cases for I2
}
