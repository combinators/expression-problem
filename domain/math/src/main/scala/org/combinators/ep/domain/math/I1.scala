package org.combinators.ep.domain.math      /*DD:LI:AI*/

import org.combinators.ep.domain._
import org.combinators.ep.domain.abstractions.{EqualsCompositeTestCase, Operation, Parameter, TestCase, TypeRep}
import org.combinators.ep.domain.instances.{DataTypeInstance, InstanceRep}
import org.combinators.ep.domain.math.M0.{AddInst, DoubleInst, Eval, LitInst}

object I1 extends Evolution {
  override implicit def getModel:Model = M2.getModel.evolve("i1", Seq.empty, Seq(MultBy))

  lazy val base = TypeRep.DataType(M2.getModel.baseDataType)
  lazy val MultBy = Operation("multBy", base, Seq(Parameter("other", base)))

  object MultByTestCase {
    def apply(instance: DataTypeInstance, argument: InstanceRep, expected:InstanceRep): TestCase = {
      EqualsCompositeTestCase(getModel.baseDataType,
        instance, expected, (MultBy, Seq(argument)), (Eval, Seq.empty))
    }
  }

 def tests: Seq[TestCase] = Seq(
    MultByTestCase(AddInst(LitInst(1.0), LitInst(2.0)),
      InstanceRep(LitInst(3.0)), DoubleInst(9.0))
  )
}
