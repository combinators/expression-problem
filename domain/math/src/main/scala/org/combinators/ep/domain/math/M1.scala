package org.combinators.ep.domain.math      /*DD:LI:AI*/

import org.combinators.ep.domain._
import org.combinators.ep.domain.abstractions._
import org.combinators.ep.domain.instances.{DataTypeInstance, InstanceRep}
import org.combinators.ep.domain.math.M0.{Eval, LitInst}

//class M1(val m0:M0) extends Evolution {
object M1 extends Evolution {
  override implicit def getModel:GenericModel = M0.getModel.evolve("m1", Seq(Sub), Seq.empty)

  // m1:model evolution.
  // -------------------
  lazy val Sub:DataTypeCase = DataTypeCase.binary("Sub")(MathDomain.getModel)

  def SubInst(left:DataTypeInstance, right:DataTypeInstance): DataTypeInstance =
    DataTypeInstance(Sub, Seq(InstanceRep(left), InstanceRep(right)))

  // testing
  def tests: Seq[TestCase] = Seq(
    EqualsTestCase(getModel.baseDataType, SubInst(LitInst(1.0), LitInst(2.0)), Eval, M0.DoubleInst(-1.0)),
  )
}
