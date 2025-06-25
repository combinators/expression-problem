package org.combinators.ep.domain.math     /*DD:LI:AI*/

import org.combinators.cogen.InstanceRep
import org.combinators.cogen.abstractions.TestCase
import org.combinators.ep.domain.{Evolution, GenericModel}
import org.combinators.ep.domain.abstractions.{DataTypeCase, EqualsTestCase}
import org.combinators.ep.domain.instances.DataTypeInstance
import org.combinators.ep.domain.math.M0.{Eval, LitInst}
import org.combinators.ep.domain.math.M2.{PrettyP, StringInst}
import org.combinators.ep.domain.math.systemI.I1

// Yes this is "Times" but I wanted to get something quick and dirty. Better to replace
// with Abs(...) which returns absolute value of expression, but wanted this to be binary.
object A1 extends Evolution {
  override implicit def getModel:GenericModel = I1.getModel.evolve("a1", Seq(Times), Seq.empty)

  // a1:model evolution.
  // -------------------
  lazy val Times = DataTypeCase.binary("Times")(MathDomain.getModel)

  def TimesInst(left:DataTypeInstance, right:DataTypeInstance): DataTypeInstance =
    DataTypeInstance(Times, Seq(InstanceRep(left), InstanceRep(right)))

  // Tests
  val a1_m3_m2 = TimesInst(LitInst(5.0),  LitInst(2.0))

  override def allTests: Map[GenericModel, Seq[TestCase]] = allPastTests(I1)

  def tests: Seq[TestCase] = Seq(
    EqualsTestCase(getModel.baseDataType, a1_m3_m2, PrettyP, StringInst("(5.0x2.0)")),
    EqualsTestCase(getModel.baseDataType, a1_m3_m2, Eval, M0.DoubleInst(10.0)),
  )
}
