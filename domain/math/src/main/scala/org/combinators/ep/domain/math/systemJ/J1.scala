package org.combinators.ep.domain.math.systemJ    /*DD:LI:AI*/

import org.combinators.cogen.InstanceRep
import org.combinators.ep.domain.instances.DataTypeInstanceRep
import org.combinators.cogen.TestCase
import org.combinators.ep.domain.abstractions._
import org.combinators.ep.domain.instances.DataTypeInstance
import org.combinators.ep.domain.math.M0.{DoubleInst, LitInst, addi, liti}
import org.combinators.ep.domain.math.{M0, MathDomain}
import org.combinators.ep.domain.{Evolution, GenericModel}

object J1 extends Evolution {
  override implicit def getModel: GenericModel = M0.getModel.evolve("j1", Seq(Sub), Seq(MultBy))

  lazy val base = DomainTpeRep.DataType(M0.getModel.baseDataType) // go to previous one, o/w recursion hits
  lazy val MultBy = Operation("multBy", base, Seq(Parameter("other", base)))

  // m1:model evolution.
  // -------------------
  lazy val Sub: DataTypeCase = DataTypeCase.binary("Sub")(MathDomain.getModel)

  def SubInst(left: DataTypeInstance, right: DataTypeInstance): DataTypeInstance =
    DataTypeInstance(Sub, Seq(DataTypeInstanceRep(left), DataTypeInstanceRep(right)))

  object MultByTestCase {
    def apply(instance: DataTypeInstance, argument: InstanceRep, expected: InstanceRep): TestCase = {
      EqualsCompositeTestCase(getModel.baseDataType,
        instance, expected, (MultBy, Seq(argument)), (M0.Eval, Seq.empty))
    }
  }

  val subi: DataTypeInstance = SubInst(LitInst(1.0), LitInst(2.0))

  override def allTests: Map[GenericModel, Seq[TestCase]] = allPastTests(M0)

  def tests: Seq[TestCase] = Seq(
    EqualsTestCase(getModel.baseDataType, subi, M0.Eval, M0.DoubleInst(-1.0)),

    MultByTestCase(liti, DataTypeInstanceRep(LitInst(3.0)), DoubleInst(15.0)),
    MultByTestCase(liti, DataTypeInstanceRep(subi), DoubleInst(-5.0)),
    MultByTestCase(addi, DataTypeInstanceRep(LitInst(3.0)), DoubleInst(9.0)),
    MultByTestCase(subi, DataTypeInstanceRep(LitInst(3.0)), DoubleInst(-3.0)),
    MultByTestCase(LitInst(-2.0), DataTypeInstanceRep(LitInst(3.0)), DoubleInst(-6.0))
  )
}
