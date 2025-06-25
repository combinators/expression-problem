package org.combinators.ep.domain.math   /*DD:LI:AI*/

import org.combinators.cogen.InstanceRep
import org.combinators.cogen.abstractions.TestCase
import org.combinators.ep.domain.*
import org.combinators.ep.domain.abstractions.*
import org.combinators.ep.domain.instances.DataTypeInstance
import org.combinators.ep.domain.math.M0.{AddInst, DoubleInst, Eval, LitInst}

object N1 extends Evolution {
  override implicit def getModel:GenericModel = M3.getModel.evolve("n1", Seq.empty, Seq(PowBy))

  // add PowBy operation

  lazy val base = TypeRep.DataType(M2.getModel.baseDataType)
  lazy val PowBy = Operation("powBy", base, Seq(Parameter("other", base)))

  object PowByTestCase {
    def apply(instance: DataTypeInstance, argument: InstanceRep, expected: InstanceRep): TestCase = {
      EqualsCompositeTestCase(getModel.baseDataType,
        instance, expected, (PowBy, Seq(argument)), (Eval, Seq.empty))
    }
  }

  val n1_2 = AddInst(LitInst(1.0), LitInst(2.0))
  val n1_3 = AddInst(LitInst(1.0), LitInst(2.0))
  val n1_4 = AddInst(n1_2, n1_3)

  val n1_5 = AddInst(LitInst(99.0), LitInst(2.0))
  val n1_6 = AddInst(LitInst(99.0), LitInst(2.0))
  val n1_7 = AddInst(n1_5, n1_6)

  override def allTests: Map[GenericModel, Seq[TestCase]] = allPastTests(M3)

  def tests:Seq[TestCase] = Seq(

    PowByTestCase(AddInst(LitInst(1.0), LitInst(12.0)),
      InstanceRep(LitInst(4.0)), DoubleInst(13.0*13.0*13.0*13.0)),
    PowByTestCase(LitInst(12.0),
      InstanceRep(LitInst(0.0)), DoubleInst(1))
  )  //
}
