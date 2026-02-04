package org.combinators.ep.domain.math   /*DD:LI:AI*/

import org.combinators.cogen.InstanceRep
import org.combinators.ep.domain.instances.DataTypeInstanceRep
import org.combinators.cogen.TestCase
import org.combinators.ep.domain.abstractions.{DomainTpeRep, EqualsCompositeTestCase, Operation, Parameter}
import org.combinators.ep.domain.instances.DataTypeInstance
import org.combinators.ep.domain.{Evolution, GenericModel}
import org.combinators.ep.domain.math.M0.{AddInst, DoubleInst, Eval, LitInst}

object M7 extends Evolution {
  override implicit def getModel:GenericModel = M6.getModel.evolve("m7", Seq.empty, Seq(PowBy))

  object m7_extensions {
    val target = "target"
  }

  lazy val base: DomainTpeRep.DataType = DomainTpeRep.DataType(M2.getModel.baseDataType)
  lazy val PowBy: Operation = Operation("powBy", base, Seq(Parameter("other", base)))

  object PowByTestCase {
    def apply(instance: DataTypeInstance, argument: InstanceRep, expected:InstanceRep): TestCase = {
      EqualsCompositeTestCase(getModel.baseDataType,
        instance, expected, (PowBy, Seq(argument)), (Eval, Seq.empty))
    }
  }

  val m7_2: DataTypeInstance = AddInst(LitInst(1.0), LitInst(2.0))
  val m7_3: DataTypeInstance = AddInst(LitInst(1.0), LitInst(2.0))
  val m7_4: DataTypeInstance = AddInst(m7_2, m7_3)

  val m7_5: DataTypeInstance = AddInst(LitInst(99.0), LitInst(2.0))
  val m7_6: DataTypeInstance = AddInst(LitInst(99.0), LitInst(2.0))
  val m7_7: DataTypeInstance = AddInst(m7_5, m7_6)

  override def allTests: Map[GenericModel, Seq[TestCase]] = allPastTests(M6)

  def tests:Seq[TestCase] = Seq(
    PowByTestCase(AddInst(LitInst(1.0), LitInst(12.0)),
      DataTypeInstanceRep(LitInst(4.0)), DoubleInst(13.0*13.0*13.0*13.0)),
    PowByTestCase(LitInst(12.0),
      DataTypeInstanceRep(LitInst(0.0)), DoubleInst(1))
  )
}
