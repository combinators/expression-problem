package org.combinators.ep.domain.math      /*DD:LI:AI*/

import org.combinators.cogen.InstanceRep
import org.combinators.ep.domain.instances.DataTypeInstanceRep
import org.combinators.cogen.TestCase
import org.combinators.ep.domain._
import org.combinators.ep.domain.abstractions._
import org.combinators.ep.domain.instances.DataTypeInstance
import org.combinators.ep.domain.math.M0.{Eval, LitInst}
import org.combinators.ep.domain.math.M1.{SubInst, getModel}

object M1 extends Evolution {
  override implicit def getModel:GenericModel = M0.getModel.evolve("m1", Seq(Sub), Seq.empty)

  // m1:model evolution.
  // -------------------
  lazy val Sub:DataTypeCase = DataTypeCase.binary("Sub")(MathDomain.getModel)

  def SubInst(left:DataTypeInstance, right:DataTypeInstance): DataTypeInstance =
    DataTypeInstance(Sub, Seq(DataTypeInstanceRep(left), DataTypeInstanceRep(right)))

  override def allTests: Map[GenericModel, Seq[TestCase]] = allPastTests(M0)

  val m1_test_1:TestCase = EqualsTestCase(getModel.baseDataType, SubInst(LitInst(1.0), LitInst(2.0)), Eval, M0.DoubleInst(-1.0))

  def tests: Seq[TestCase] = Seq(
    m1_test_1,
  )
}
