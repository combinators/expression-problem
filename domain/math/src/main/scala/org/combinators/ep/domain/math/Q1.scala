package org.combinators.ep.domain.math   /*DD:LI:AI*/

import org.combinators.cogen.InstanceRep
import org.combinators.ep.domain.instances.DataTypeInstanceRep
import org.combinators.cogen.{TestCase, TypeRep}
import org.combinators.ep.domain.abstractions.{DataTypeCase, EqualsTestCase, Operation}
import org.combinators.ep.domain.instances.DataTypeInstance
import org.combinators.ep.domain.math.M0.{Eval, LitInst}
import org.combinators.ep.domain.math.M2.{PrettyP, StringInst}
import org.combinators.ep.domain.{Evolution, GenericModel}

object Q1 extends Evolution {
  override implicit def getModel:GenericModel = M3W1.getModel.evolve("q1", Seq(Sqrt), Seq(Operation.asTree, Identifier))

  lazy val Sqrt = DataTypeCase.unary("Sqrt")(MathDomain.getModel)

  lazy val Identifier = Operation("id", TypeRep.Int)

  def SqrtInst(inner:DataTypeInstance): DataTypeInstance =
    DataTypeInstance(Sqrt, Seq(DataTypeInstanceRep(inner)))

  // Tests
  val q1_d1 = SqrtInst(LitInst(5.0))
  val q1_d2 = SqrtInst(LitInst(16.0))

  override def allTests: Map[GenericModel, Seq[TestCase]] = allPastTests(M3W1)

  def tests: Seq[TestCase] = Seq(
    EqualsTestCase(getModel.baseDataType, q1_d1, PrettyP, StringInst("(sqrt 5.0)")),
    EqualsTestCase(getModel.baseDataType, q1_d2, Eval, M0.DoubleInst(4.0))
  )
}
