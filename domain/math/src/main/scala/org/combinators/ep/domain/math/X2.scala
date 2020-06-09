package org.combinators.ep.domain.math      /*DD:LI:AI*/

import org.combinators.ep.domain._
import org.combinators.ep.domain.abstractions._
import org.combinators.ep.domain.instances.{DataTypeInstance, InstanceRep}
import org.combinators.ep.domain.math.M0.{Eval, LitInst}

object X2 extends Evolution {
  override implicit def getModel:GenericModel = X1.getModel.evolve("x2", Seq(Times), Seq.empty)

  def StringInst(s:String): InstanceRep = InstanceRep(TypeRep.String)(s)

  lazy val Times = DataTypeCase.binary("Times")(MathDomain.getModel)

  def TimesInst(left:DataTypeInstance, right:DataTypeInstance): DataTypeInstance =
    DataTypeInstance(Times, Seq(InstanceRep(left), InstanceRep(right)))

  // Tests
  val x2_1 = TimesInst(LitInst(3.0), LitInst(2.0))
  def tests: Seq[TestCase] = Seq(
    EqualsTestCase(getModel.baseDataType, x2_1, Eval, M0.DoubleInst(6.0)),
  )
}
