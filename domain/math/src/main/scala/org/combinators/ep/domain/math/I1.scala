package org.combinators.ep.domain.math      /*DD:LI:AI*/

import org.combinators.ep.domain._
import org.combinators.ep.domain.abstractions.DataTypeCase
import org.combinators.ep.domain.instances.{DataTypeInstance, InstanceRep}

object I1 extends Evolution {
  override implicit def getModel:Model = M1.getModel.evolve("i1", Seq(Inv), Seq.empty)

  lazy val Inv = DataTypeCase.unary("Inv")

  def InvInst(inner:DataTypeInstance): DataTypeInstance =
    DataTypeInstance(Inv, Seq(InstanceRep(inner)))

  // TODO: Model test cases for I1
}
