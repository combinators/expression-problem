package org.combinators.ep.domain.math.systemD    /*DD:LI:AI*/

import org.combinators.ep.domain.abstractions._
import org.combinators.ep.domain.instances.{DataTypeInstance, InstanceRep}
import org.combinators.ep.domain.math.M0.{AddInst, DoubleInst, Eval, LitInst}
import org.combinators.ep.domain.math.{M1, M2}
import org.combinators.ep.domain.{Evolution, GenericModel}

object D1 extends Evolution {
  override implicit def getModel: GenericModel = M1.getModel.evolve("d1", Seq.empty, Seq(MultBy))

  lazy val base = TypeRep.DataType(M2.getModel.baseDataType)
  lazy val MultBy = Operation("multBy", base, Seq(Parameter("other", base)))

  object MultByTestCase {
    def apply(instance: DataTypeInstance, argument: InstanceRep, expected: InstanceRep): TestCase = {
      EqualsCompositeTestCase(getModel.baseDataType,
        instance, expected, (MultBy, Seq(argument)), (Eval, Seq.empty))
    }
  }

  override def allTests: Map[GenericModel, Seq[TestCase]] = allPastTests(M1)

  def tests: Seq[TestCase] = Seq(
    MultByTestCase(AddInst(LitInst(1.0), LitInst(2.0)),
      InstanceRep(LitInst(3.0)), DoubleInst(9.0))
  )
}
