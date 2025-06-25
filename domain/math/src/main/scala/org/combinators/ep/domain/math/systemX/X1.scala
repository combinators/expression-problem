package org.combinators.ep.domain.math.systemX     /*DD:LI:AI*/

import org.combinators.cogen.InstanceRep
import org.combinators.cogen.abstractions.TestCase
import org.combinators.ep.domain.abstractions.*
import org.combinators.ep.domain.instances.DataTypeInstance
import org.combinators.ep.domain.math.M0.{AddInst, DoubleInst, Eval, LitInst}
import org.combinators.ep.domain.math.M2.StringInst
import org.combinators.ep.domain.math.{M0, MathDomain}
import org.combinators.ep.domain.{Evolution, GenericModel}

object X1 extends Evolution {
  override implicit def getModel: GenericModel = M0.getModel.evolve("x1", Seq(Sub), Seq(PrettyP, MultBy))

  lazy val base = TypeRep.DataType(M0.getModel.baseDataType)

  // x1:model evolution.
  // -------------------
  lazy val Sub: DataTypeCase = DataTypeCase.binary("Sub")(MathDomain.getModel)
  lazy val PrettyP = Operation("prettyp", TypeRep.String)
  lazy val MultBy = Operation("multBy", base, Seq(Parameter("other", base)))

  object MultByTestCase {
    def apply(instance: DataTypeInstance, argument: InstanceRep, expected: InstanceRep): TestCase = {
      EqualsCompositeTestCase(getModel.baseDataType,
        instance, expected, (MultBy, Seq(argument)), (Eval, Seq.empty))
    }
  }

  // Tests
  val m2_s1 = SubInst(LitInst(1.0), LitInst(2.0))

  def SubInst(left: DataTypeInstance, right: DataTypeInstance): DataTypeInstance =
    DataTypeInstance(Sub, Seq(InstanceRep(left), InstanceRep(right)))

  override def allTests: Map[GenericModel, Seq[TestCase]] = allPastTests(M0)

  // testing
  def tests: Seq[TestCase] = Seq(
    EqualsTestCase(getModel.baseDataType, SubInst(LitInst(1.0), LitInst(2.0)), Eval, M0.DoubleInst(-1.0)),

    EqualsTestCase(getModel.baseDataType, m2_s1, PrettyP, StringInst("(1.0-2.0)")),

    EqualsTestCase(getModel.baseDataType, AddInst(m2_s1, AddInst(LitInst(5.0), LitInst(6.0))),
      PrettyP, StringInst("((1.0-2.0)+(5.0+6.0))")),

    MultByTestCase(AddInst(LitInst(1.0), LitInst(2.0)),
      InstanceRep(LitInst(3.0)), DoubleInst(9.0)),
    MultByTestCase(LitInst(2.0),
      InstanceRep(LitInst(0.0)), DoubleInst(0.0)),
    MultByTestCase(LitInst(0.0),
      InstanceRep(LitInst(3.0)), DoubleInst(0.0))
  )
}
