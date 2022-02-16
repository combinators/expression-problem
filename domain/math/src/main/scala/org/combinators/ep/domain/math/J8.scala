package org.combinators.ep.domain.math     /*DD:LI:AI*/

import org.combinators.ep.domain.abstractions._
import org.combinators.ep.domain.instances.{DataTypeInstance, InstanceRep}
import org.combinators.ep.domain.math.M0.{AddInst, DoubleInst, Eval, LitInst}
import org.combinators.ep.domain.{Evolution, GenericModel}

object J8 extends Evolution {
  override implicit def getModel:GenericModel = J7.getModel.evolve("j8", Seq.empty, Seq(PowBy))

  // m7:model evolution.
  // -------------------
  object j8_extensions {
    val target = "target"
  }

  // add PowBy operation
  lazy val base = TypeRep.DataType(J3.getModel.baseDataType)
  lazy val PowBy = Operation("powBy", base, Seq(Parameter("other", base)))

  object PowByTestCase {
    def apply(instance: DataTypeInstance, argument: InstanceRep, expected:InstanceRep): TestCase = {
      EqualsCompositeTestCase(getModel.baseDataType,
        instance, expected, (PowBy, Seq(argument)), (Eval, Seq.empty))
    }
  }
 // val m7_1 = SqrtInst(LitInst(25.0))
  val m7_2 = AddInst(LitInst(1.0), LitInst(2.0))
  val m7_3 = AddInst(LitInst(1.0), LitInst(2.0))
  val m7_4 = AddInst(m7_2, m7_3)

  val m7_5 = AddInst(LitInst(99.0), LitInst(2.0))
  val m7_6 = AddInst(LitInst(99.0), LitInst(2.0))
  val m7_7 = AddInst(m7_5, m7_6)

  def tests:Seq[TestCase] = Seq(
    PowByTestCase(AddInst(LitInst(1.0), LitInst(12.0)),
      InstanceRep(LitInst(4.0)), DoubleInst(13.0*13.0*13.0*13.0))
  )
}
