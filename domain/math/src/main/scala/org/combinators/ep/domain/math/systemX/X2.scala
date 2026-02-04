package org.combinators.ep.domain.math.systemX     /*DD:LI:AI*/

import org.combinators.cogen.InstanceRep
import org.combinators.ep.domain.instances.DataTypeInstanceRep
import org.combinators.cogen.{TestCase, TypeRep}
import org.combinators.ep.domain.abstractions.{DataTypeCase, EqualsTestCase}
import org.combinators.ep.domain.instances.DataTypeInstance
import org.combinators.ep.domain.math.M0.{Eval, LitInst}
import org.combinators.ep.domain.math.{M0, MathDomain}
import org.combinators.ep.domain.{Evolution, GenericModel}

object X2 extends Evolution {
  override implicit def getModel: GenericModel = X1.getModel.evolve("x2", Seq(Times), Seq.empty)

  def StringInst(s: String): InstanceRep = InstanceRep(TypeRep.String)(s)

  lazy val Times: DataTypeCase = DataTypeCase.binary("Times")(MathDomain.getModel)

  def TimesInst(left: DataTypeInstance, right: DataTypeInstance): DataTypeInstance =
    DataTypeInstance(Times, Seq(DataTypeInstanceRep(left), DataTypeInstanceRep(right)))

  // Tests
  val x2_1: DataTypeInstance = TimesInst(LitInst(3.0), LitInst(2.0))

  override def allTests: Map[GenericModel, Seq[TestCase]] = allPastTests(X1)

  def tests: Seq[TestCase] = Seq(
    EqualsTestCase(getModel.baseDataType, x2_1, Eval, M0.DoubleInst(6.0)),
  )
}
